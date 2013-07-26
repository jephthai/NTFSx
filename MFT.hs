--
-- This module parses the MFT entries in NTFS.  It's not exactly the
-- most trivial data structure in the world.  I'm not very expert at
-- building a robust type structure in Haskell, but this appears to be
-- working as I find opportunities to build tools on top of this
-- library.
--
-- Author:  Josh Stone
-- Contact: josh@josho.org
-- Created: 2009
--
--

module MFT where

import BootSector
import Foreign
import TypeSyms
import Blocker
import System.IO
import Debug.Trace
import Text.Printf
import Data.Maybe
import System.Environment
import Data.List
import Data.Bits
import System.Win32.File
import System.Win32.Types
import Control.Monad
import qualified WinBlocks as WB
import qualified Data.ByteString as BS

getMFT :: HANDLE -> Integer -> Integer -> IO (Maybe MFTEntry)
getMFT h base id = do
  WB.setFilePointer h (id * 2 + base)
  readMFTEntry h

getMFTraw h offset = do
  WB.setFilePointer h offset
  readMFTEntry h

getMFTSectors e = aSectors d
  where attrs = mftAttrs e
        datas = filter isData attrs
        isData (DataAttr _ _) = True
        isData _              = False
        d = head datas

getDataBlock :: MFTEntry -> [Block]
getDataBlock e = map aBlock datas
  where attrs = mftAttrs e
        datas = filter isData attrs
        isData (DataAttr _ _) = True
        isData _              = False

--
-- Returns the first filename netry in the MFT.  This was my original
-- implementation until I started getting irritated at the 8.3 names
-- being listed first.  See the next function for an improvement.
--
getMFTName :: MFTEntry -> (Integer, String)
getMFTName e = fromMaybe (-1, "\\null/") findname
  where isname NameAttr{} = True
        isname _          = False
        names = filter isname $ mftAttrs e
        findname = case names of
          [] -> Nothing
          (NameAttr _ p f : _) -> Just (toInteger p,f)

--
-- MFT entries can have multiple $FILENAME attributes, so this
-- function returns them all in a list.
--
getMFTNames :: MFTEntry -> [(Integer, String)]
getMFTNames e = fromMaybe [(-1, "\\null/")] findname
  where isname NameAttr{} = True
        isname _          = False
        names = filter isname $ mftAttrs e
        findname = case names of
          [] -> Nothing
          ns -> Just (map (\(NameAttr _ p f) -> (toInteger p, f)) ns)

getParent [] = Nothing
getParent (NameAttr _ p _ : as) = Just p
getParent (_:as) = getParent as

coolAttrs NameAttr{} = True
coolAttrs DataAttr{} = True
coolAttrs _          = False
  
mftFlags :: MFTEntry -> Maybe Int
mftFlags e = do
  let attrs = reverse $ mftAttrs e
  rec <- find ((==StdInfo).attrType.aHeader) attrs
  return $ length $ aStdFlags rec

--
-- Get the filename from the FileName attribute contained in the
-- provided MFT.  A FileName attribute can be missing (it's not
-- required), so Nothing is returned in that case.
--

mftFileName :: MFTEntry -> Maybe String
mftFileName e = do
  let attrs = reverse $ mftAttrs e
  rec <- find ((==FileName).attrType.aHeader) attrs 
  return $ aFileName rec
 
--
-- An MFTEntry is analogous to an inode in a UFS-derived filesystem.
-- It contains everything necessary to describe an object (either
-- file, directory, etc.).  Every MFT entry in NTFS has a header of
-- fixed fields / format, and a variable number of attributes, which
-- can represent the permissions, file name, contents, etc.
--

data MFTEntry = MFTEntry {
      mftHeader :: EntryHeader,
      mftAttrs  :: [Attribute]
    } deriving (Show, Eq)

readMFTEntry :: HANDLE -> IO (Maybe MFTEntry)
readMFTEntry h = do
  a <- readBlockRel h   -- MFT Entries are two blocks
  b <- readBlockRel h
  let block      = concatBlock a b
      (hdr, rem) = readEntryHeader block
      attrs      = readAttributes rem
      blockOK    = blockStarts block [0x46,0x49,0x4c,0x45]
      hdrOK      = isJust hdr
      entry      = MFTEntry (fromJust hdr) attrs
  return (if blockOK && hdrOK then Just entry else Nothing)

-- 
-- An attribute represents something about the object represented by
-- the MFT entry.  NTFS defines a variety of attributes, which are
-- enumerated here.  Each one, though, has a header that tells you
-- about the attribute itself.  In the minimal case, a "WeirdAttr" has
-- this header and its associated data block.
-- 

data Attribute = 
    NameAttr  { aHeader :: AttrHeader, aParent :: Integer, aFileName :: String } | 
    WeirdAttr { aHeader :: AttrHeader, aBlock :: Block } |
    DataAttr  { aHeader :: AttrHeader, aSectors :: [(Integer,Integer)]} | --aBlock :: Block } |
    ParentAttr { aParent :: Integer } |
    NullAttr | 
    StandardAttr { 
      aHeader       :: AttrHeader,
      aCreateTime   :: [Word8],
      aAlterTime    :: [Word8],
      aMFTAlterTime :: [Word8],
      aAccessTime   :: [Word8],
      aStdFlags     :: [Flag],
      aMaxVers      :: MWord,
      aVerNum       :: MWord,
      aClassID      :: MWord,
      aOwnerID      :: MWord,
      aSecID        :: MWord,
      aQuotaChg     :: [Word8],
      aUSN          :: [Word8]
    }
    deriving (Show, Eq)
    
attrName a =
    case a of
        NameAttr _ p _ -> "NameAttr:" ++ show p
        WeirdAttr _ _ -> "WeirdAttr"
        NullAttr -> "NullAttr"
        ParentAttr n -> "ParentAttr:" ++ show n
        StandardAttr{} -> "StandardAttr"
        DataAttr _ b -> "DataAttr " ++ (show (length b))
    
readAttributes :: Block -> [Attribute]
readAttributes b = if lastAttr then [] else attr : rest
    where (hdr, rem) = readAttrHeader b
          ablock     = takeBlock b (attrLen hdr)
          attr       = readAttribute ablock (attrType hdr) hdr 
          rest       = readAttributes rem
          lastAttr   = blockStarts b (replicate 4 0xff)
          
data Flag = FReadOnly | FHidden | FSystem | FArchive | FDevice | FNormal | 
            FTemporary | FSparse | FReparse | FCompressed | FOffline |
            FNonIndexed | FEncrypted | FInvalid
            deriving (Show, Eq)

flagValues :: [(Int, Flag)]           
flagValues = [(0x0001, FReadOnly),
              (0x0002, FHidden),
              (0x0004, FSystem),
              (0x0020, FArchive),
              (0x0040, FDevice),
              (0x0080, FNormal),
              (0x0100, FTemporary),
              (0x0200, FSparse),
              (0x0400, FReparse),
              (0x0800, FCompressed),
              (0x1000, FOffline),
              (0x2000, FNonIndexed),
              (0x4000, FEncrypted)]

readFlag x = mapMaybe check flagValues
    where check (val, flag)
              | x .&. val > 0 = Just flag
              | otherwise     = Nothing
      

--
-- There are lots of different kinds of attributes.  As I feel like
-- implementing them, this function will specialize on all the kinds.
-- Each one, then, will probably produce a series of other functions
-- to support it...
--

readAttribute :: Block -> AttrType -> AttrHeader -> Attribute
readAttribute b FileName hdr = NameAttr hdr (parseParent b hdr) (parseName b hdr)
readAttribute b ParentID hdr = ParentAttr 0
readAttribute b StdInfo hdr = StandardAttr {
                                aHeader        = hdr,
                                aCreateTime    = getSeq b 0 8,
                                aAlterTime     = getSeq b 8 8,
                                aMFTAlterTime  = getSeq b 16 8,
                                aAccessTime    = getSeq b 24 8,
                                aStdFlags      = readFlag $ getWord b 32,
                                aMaxVers       = getWord b 36,
                                aVerNum        = getWord b 40,
                                aClassID       = getWord b 44,
                                aOwnerID       = getWord b 48,
                                aSecID         = getWord b 52,
                                aQuotaChg      = getSeq b 56 8,
                                aUSN           = getSeq b 64 8}
readAttribute b DataSpec hdr = DataAttr hdr (parseData b)
readAttribute b _ hdr = WeirdAttr hdr b

parseData b = if nonresident == 1 then parseNonresidentData b else []
  where nonresident = getByte b 8

--
-- The runs are represented in NTFS as a list where the first entry
-- describes the block offset of its data run and its extent in
-- sectors.  Each subsequent entry in the DataRun list describes its
-- block offset as a delta from the previous entry.  I want to expand
-- these out so that each one contains its absolute offset from the
-- beginning of the volume, which is what this function will do.
--

expandRuns [] = []
expandRuns (r:[]) = [r]
expandRuns (r:rs) = r : expandRuns' (r:rs)
                    
expandRuns' [] = []
expandRuns' (r:[]) = [r]
expandRuns' ((offset1, count1):(offset2, count2):rs) = current : tail 
  where current = (offset1 + offset2, count2)
        remainder = expandRuns' (current : rs)
        tail = if remainder == [current] then [] else remainder

parseNonresidentData b = expanded
  where runlist  = dropBlock b (64 + offset )
        offset   = getWord b 36
        size     = getLong b 40
        expanded = expandRuns (parseRuns parseRun' runlist)
        output   = "Filesize: " ++ show size ++ " bytes"

parseRuns _ b | blockLen b < 4 = []
parseRuns _ b | getByte b 0 == 0x00 = []
parseRuns f b = run1 : parseRuns parseRun remainder
  where (run1, size) = f b
        remainder    = dropBlock b size

parseRun b = ((toInteger (start * 8), toInteger (len * 8)), 1 + offLen + lenLen)
  where lenspec = getByte b 0
        lenLen = fromIntegral (lowNybble (toInteger lenspec))
        offLen = fromIntegral (highNybble (toInteger lenspec))
        lenOff = 1
        offOff = lenOff + lenLen
        len = getLittleEndianInt lenLen b lenOff
        start = fixSign (getSeq b offOff offLen)

parseRun' b = ((toInteger (start * 8), toInteger (len * 8)), 1 + offLen + lenLen)
  where lenspec = getByte b 0
        lenLen = fromIntegral (lowNybble (toInteger lenspec))
        offLen = fromIntegral (highNybble (toInteger lenspec))
        lenOff = 1
        offOff = lenOff + lenLen
        start = getLittleEndianInt offLen b offOff
        len = getLittleEndianInt lenLen b lenOff

fixSign :: [Word8] -> Integer
fixSign b = if neg then i * (-1) - 1 else i -- + 1
  where neg = last b .&. 0x80 > 0
        b' = if neg
             then map (toInteger.complement) b
             else map toInteger b
        pump x y = x .|. shiftL y 8
        i = foldr pump 0 b'

lowNybble x = x .&. 0xf
highNybble x = (x .&. 0xf0) `shiftL` (-4)

-- TODO need to add namespace adjustment to char length

parseParent :: Block -> AttrHeader -> Integer
parseParent b hdr = toInteger $ getLong b 24

parseName :: Block -> AttrHeader -> String
parseName b hdr = getUnicode nb 66 ((getByte nb 64) * 2)
    where nb = dropBlock b (getInt b 20)

--
-- This is an enumeration of the attribute types defined for NTFS.
-- These happen to correspond directly with integer values in the
-- Attribute header.
--

data AttrType = ParentID | StdInfo | FileName | DataSpec | AttrList | 
                ObjID | ReparsePt | IndexRoot | IndexAlloc | 
                BitmapAttr | UnknownAttr
                deriving (Show, Eq)

--
-- An MFT Entry has a header, as follows:
--

data EntryHeader = EntryHeader {
      hSig      :: String,
      hFixupOff :: MInt,
      hFixupLen :: MInt,
      hLSN      :: MLong,  -- Logfile Sequence Number
      hSeqV     :: MInt,
      hLinkNum  :: MInt,
      hAttrOff  :: MInt,
      hFlags    :: MInt,
      hUsedSz   :: MWord,
      hAllocSz  :: MWord,
      hBaseRef  :: MWord,
      hNextAttr :: MInt
    } deriving (Eq)

instance Show EntryHeader where
    show h = "<MFT-E>"

--
-- readEntryHeader extracts the header information from a Block.
-- There is some error detection in here -- primarily length of the
-- block.  If it's not big enough to *have* an MFT header, then it
-- probably isn't one.
--
-- The other subtlety is the fixups.  NTFS has some error detection
-- features, whereby key bytes are set to known values.  The correct
-- replacements are stored in the entry header.  So, when we're
-- returning the attribute-region for this MFT entry, we want to make
-- sure that we correct the fixup bytes.
--

readEntryHeader :: Block -> (Maybe EntryHeader, Block)
readEntryHeader b = (if ok then Just hdr else Nothing, rem)
    where b'  = doFixups b (hFixupOff hdr, hAttrOff hdr)
          rem = dropBlock b' (hAttrOff hdr)
          ok  = blockLen b >= 16
          hdr = EntryHeader { 
                  hSig      = getASCII b  0 4,
                  hFixupOff = getInt   b  4,
                  hFixupLen = getInt   b  6,
                  hLSN      = getLong  b  8,
                  hSeqV     = getInt   b 16,
                  hLinkNum  = getInt   b 18,
                  hAttrOff  = getInt   b 20,
                  hFlags    = getInt   b 22,
                  hUsedSz   = getWord  b 24,
                  hAllocSz  = getWord  b 28,
                  hBaseRef  = getLong  b 32,
                  hNextAttr = getInt   b 40
                }

--
-- An Attribute has a header as follows:
--

data AttrHeader = AttrHeader {
      attrType     :: AttrType,
      attrLen      :: MWord,
      attrNRF      :: MByte,  -- Non-Resident Flag
      attrNameLen  :: MByte,
      attrNameOff  :: MInt,
      attrFlags    :: MInt,
      attrIdent    :: MInt
    } deriving (Eq)

instance Show AttrHeader where
    show h = "<MFT-A:" ++ (show $ attrType h) ++ ">"

readAttrHeader :: Block -> (AttrHeader, Block)
readAttrHeader b = (hdr, rest)
    where rest = dropBlock b (attrLen hdr)
          hdr = AttrHeader {
                  attrType     = pAttr (getWord b 0),
                  attrLen      = getWord b 4,
                  attrNRF      = getByte b 8,
                  attrNameLen  = getByte b 9,
                  attrNameOff  = getInt  b 10,
                  attrFlags    = getInt  b 12,
                  attrIdent    = getInt  b 14
                }

--
-- UTILITY FUNCTIONS 
--

pAttr :: Int -> AttrType
pAttr 2   = ParentID
pAttr 16  = StdInfo
pAttr 32  = AttrList
pAttr 48  = FileName
pAttr 64  = ObjID
pAttr 128 = DataSpec
pAttr 144 = IndexRoot
pAttr 160 = IndexAlloc
pAttr 176 = BitmapAttr
pAttr 192 = ReparsePt
pAttr _   = UnknownAttr

--
-- a yucky function that patches a block with the appropriate bytes as
-- indicated by the fixup array stored in the MFT entry.
--

doFixups :: Block -> (Int, Int) -> Block
doFixups b (off, len) = setBytes b patches
    where bs = map (\x -> getSeq b (x * 2 + off) 2) [0..len - 1]
          patches = zip (map (\x -> x * 512 - 2) [1..]) (tail bs)



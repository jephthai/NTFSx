--
-- Blocker is a module that provides block-level access to a file,
-- presumably a block device in Linux.  Its intent is to free up
-- higher-level modules from needing to know about the details of
-- binary IO.  It also provides a little help for munging binary data
-- from on-disk representation to nice, happy, Haskell
-- representations.
--
-- Author:  Josh Stone
-- Contact: yakovdk@gmail.com
-- Created: 2009
--
--

module Blocker where

import System.IO
import Foreign
import Text.Printf
import Data.Char
import System.IO.Unsafe
import Data.Ix (inRange)
import System.Win32.Types
import qualified WinBlocks as WB
import qualified Data.ByteString as BS

data Block = Block {
      rbData :: BS.ByteString
    } deriving (Eq)

instance Show Block where
    show b = "<BLOCK " ++ (show $ blockLen b) ++ ">"

    
openBlockFile drive = WB.openDrive drive

-- Return a lazy list of blocks read from the provided binary file
-- handle.  Being lazy, it really helps keep the memory usage down
-- when mapping across the whole file.

--blockList :: HANDLE -> IO [Block]
--blockList h = do
--  block <- unsafeInterleaveIO (readBlockRel h)
--  rest  <- unsafeInterleaveIO (blockList h)
--  return (block : rest)
--  eof   <- hIsEOF h
--  if eof 
--    then return [] 
--    else return (block : rest)

-- given an open binary file handle, retrieve a certain numbered
-- block.  This is not intended to be highly efficient, so I'm OK with
-- the buffer-to-list conversion.

readBlock :: HANDLE -> Integer -> IO Block
readBlock h a = do
  WB.setFilePointer h a -- hSeek h AbsoluteSeek (a * 512)
  b <- readBlockRel h 
  return b

readBlockRel :: HANDLE -> IO Block
readBlockRel h = do
  ws <- WB.readBlock h
  return $ Block $ BS.pack ws


-- change some bytes in a block and return the new copy

setBytes :: Block -> [(Int, [Word8])] -> Block
setBytes b ps = patched
    where patched = foldl patchBlock b ps

patchBlock :: Block -> (Int, [Word8]) -> Block
patchBlock b (off, val) = Block new
    where ws  = rbData b
          len = length val
          pre = BS.take off ws
          mid = BS.pack val
          suf = BS.drop (off + len) ws
          new = BS.concat [pre, mid, suf]

-- concatBlocks takes two blocks and concatenates the data.  Suppose
-- you want to representa 1024-bit segment of the device (maybe an MFT
-- entry?)... read the two blocks, and concat them together.

concatBlock :: Block -> Block -> Block
concatBlock a b = Block (BS.concat [a', b'])
    where a' = rbData a
          b' = rbData b

-- the get<Size> functions are abstractions so that I can change the
-- implementation of a Block when I want to.  They also simplify the
-- endianness issues.  Each will retrieve a value from the data block
-- at the address provided.  It will be returned as an Int.

getLittleEndianInt :: Int -> Block -> Int -> Int
getLittleEndianInt len b off = foldr pump 0 vals
    where bs = BS.drop off (rbData b)
          vals = map (fromIntegral) (BS.unpack (BS.take len bs))
          pump x y = x .|. (shiftL y 8)

getLittleEndianInt' :: Int -> Block -> Int -> Int
getLittleEndianInt' len b off = foldr pump 0 vals
    where bs = BS.drop off (rbData b)
          vals = map (fromIntegral) (BS.unpack (BS.take len bs))
          pump x y = x .|. (shiftL y 8)

getLittleEndianInteger :: Int -> Block -> Int -> Integer
getLittleEndianInteger len b off = foldr pump 0 vals
    where bs = BS.drop off (rbData b)
          vals = map (toInteger) (BS.unpack (BS.take len bs))
          pump x y = x .|. (shiftL y 8)

getByte = getLittleEndianInt 1
getInt  = getLittleEndianInt 2
getWord = getLittleEndianInt 4
getLong = getLittleEndianInt 8

getASCII :: Block -> Int -> Int -> String
getASCII b off len = map (chr.fromIntegral) nums
    where nums = BS.unpack(BS.take len (BS.drop off (rbData b)))

getSeq :: Block -> Int -> Int -> [Word8]
getSeq b off len = BS.unpack $ BS.take len (BS.drop off (rbData b))

dropBlock :: Block -> Int -> Block
dropBlock b n = b { rbData = BS.drop n (rbData b) }

takeBlock :: Block -> Int -> Block
takeBlock b n = b { rbData = BS.take n (rbData b) }

blockStarts :: Block -> [Word8] -> Bool
blockStarts b p = BS.isPrefixOf (BS.pack p) (rbData b)

blockLen :: Block -> Int
blockLen b = BS.length (rbData b)

getUnicode :: Block -> Int -> Int -> String
getUnicode b off len = map (chr.fromIntegral) (filter (>0) seq)
    where seq = getSeq b off len

-- printArray is an ugly function that mimics the behavior of a
-- "hexdump -C" command.  It will print a data block to standard
-- output in a columnified hex / ascii format.

putBlockHex n b = printArray n (BS.unpack $ rbData b)

printArray :: Int -> [Word8] -> IO ()
printArray _ [] = do return ()
printArray n a = do
  let chunk = take 16 a
  printf "%08x  " n
  mapM (printf "%02x ") (take 8 chunk)
  printf " "
  mapM (printf "%02x ") (drop 8 chunk)
  printf " |"
  mapM (printf "%c" . ntc) chunk
  putStrLn "|"
  printArray (n + 16) (drop 16 a)
  
-- A utility function for converting a byte to a char.  In this case,
-- it is intended to be for printing, and lots of bytes don't work.
-- Unprintable bytes become periods.

ntc :: Word8 -> Char
ntc c | inRange (0x20, 0x7e) c = (chr . fromIntegral) c
ntc c = '.'

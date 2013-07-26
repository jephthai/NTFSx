-- 
-- This is a wrap-up module for representing, reading, and using
-- MFT-related data.  The MFT (Master File Table) represents the
-- metadata for an NTFS filesystem.
--
-- Author:  Josh Stone
-- Contact: josh@josho.org
-- Created: 2009
--
--

module NTFS where

import Blocker
import BootSector
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Debug.Trace
import MFT
import System.IO
import System.IO.Unsafe
import System.Win32.File
import System.Win32.Types
import Text.Printf
import TypeSyms
import WinBlocks

type MFTHandle = HANDLE

data NTFS = NTFS {
      ntPath    :: Char,
      ntHandle  :: MFTHandle,
      ntBootSec :: BootSector,
      ntMFTOff  :: Integer,         -- address in sectors
      ntBAKOff  :: Integer,         -- address in sectors
      ntMFT     :: [NTFSFile],
      ntMax     :: Integer,
      ntRoot    :: NTFSFile,
      ntMFrag   :: MFTFrag
    } deriving (Eq, Show)

type MFTID = Integer
type Filename = String
type Parent = MFTID
type Sector = Integer
data NTFSFile = NTFSFile {
  nfID :: MFTID,
  nfName :: Filename,
  nfParent :: Parent,
  nfSectors :: [(Sector, Sector)]
  }
  deriving (Eq)

instance Show NTFSFile where
     show h = "::NTFSFile::\n" ++
              "MFT#:     " ++ show (nfID h) ++ "\n" ++
              "Parent:   " ++ show (nfParent h) ++ "\n" ++
              "Filename: " ++ nfName h ++ "\n" ++
              "Sectors:  " ++ secstr ++
              "\n"
        where secstr = foldl1 ((++).(++ "\n          ")) (map show (nfSectors h))

data MFTFrag = MFTFrag {
  mftFragRuns :: [(Sector, Sector)]
  } deriving (Show, Eq)

calcMFT n = do
  (Just mft) <- getFile n 0
  return (MFTFrag $ nfSectors mft)

calcMFTraw n = calcMFTraw' runs 
  where (MFTFrag runs) = ntMFrag n

calcMFTraw' [] _ = 0
calcMFTraw' [(a,_)] id = a + (id * 2)
calcMFTraw' ((a,b):ps) id 
  | id*2 < b  = a + id * 2
  | otherwise = calcMFTraw' ps (id - (b `div` 2))

ntfsRoot :: MFTID
ntfsRoot = 5

--
-- multiHunt is a more efficient way to find files in the NTFS
-- filesystem.  It makes one pass through the MFT, given a list
-- of the path names it needs to find.  Along the way, it builds
-- a list of all the filename matches for each entry in the
-- target path.  With each new match found, it tests to see if
-- there is a single sequence of parent-child relationships
-- from the beginning to the end of the path.  When found, it
-- returns a list containing the target file/directory.
--

multiHunt n path = do
  (Just root) <- getFile n 5
  let path' = map (map toLower) path
  print path'
  multiHunt' n ("\\" : path') ([root] : replicate (length path) []) 0 0 

multiHunt' n path hist id hits = do
  mft <- getFile n id
  case mft of
    Just file | name `elem` path ->
      case pathDone hist' of
        Just (target:_) -> do
          mhStatus (show path)
          mhStatus name
          return target
        Nothing -> do
          let followfunc ((x,y)) = x `follows` y
              hitcount = length (filter followfunc (zip hist' (drop 1 hist')))
              progress = take hitcount path
          when (hitcount > hits) $ mhStatus (show progress)
          hFlush stdout
          multiHunt' n path hist' (id+1) hitcount
      where name = map toLower $ nfName file
            hist' = pushHist hist (fromMaybe undefined (elemIndex name path)) file
    _ -> multiHunt' n path hist (id+1) hits

mhStatus x = putStrLn ("Found "++x)

pushHist [] _ _ = []
pushHist (x:xs) 0 mft = (mft : x) : xs
pushHist (x:xs) n mft = x : pushHist xs (n-1) mft

pathDone [] = Nothing
pathDone (x:[]) = Just x
pathDone (x:[]:_) = Nothing
pathDone (x:y:path) | x `follows` y = pathDone ([selectChild x y]:path)
pathDone _ = Nothing

follows xs ys = not (null res) -- length res > 0
  where res = mapMaybe (\x -> find (\v -> nfParent v == nfID x) ys) xs

selectChild xs ys = head res
  where res = mapMaybe (\x -> find (\v -> nfParent v == nfID x) ys) xs


--
-- huntMFT searches the NTFS filesystem for the entry representing a
-- file that has a certain name and is a direct child of a given MFT
-- entry.  This can be used to traverse the filesystem, though it is a
-- little inefficient because it starts at MFT #0 every time.
--

huntMFT :: NTFS -> Integer -> String -> IO (Maybe NTFSFile)
huntMFT n parent fname = huntMFT' n parent fname 0

huntMFT' :: NTFS -> Integer -> String -> Integer -> IO (Maybe NTFSFile)
huntMFT' n parent fname id = do
  mft <- getFile n id 
  case mft of
    Just file -> if ischild && isfile then return (Just file) else huntMFT' n parent fname (id + 1)
      where ischild = nfParent file == parent
            fname'  = map toLower fname
            mname   = map toLower (nfName file)
            isfile  = fname' == mname
    otherwise -> huntMFT' n parent fname (id + 1)

readMFTs :: HANDLE -> Integer -> Integer -> IO [NTFSFile]
readMFTs h o s = do
  entry <- unsafeInterleaveIO (getMFT h o s)
  rest <- unsafeInterleaveIO (readMFTs h o (s + 1))
  case entry of
    Nothing -> return rest
    Just e  -> do
      let (fparent, fname) = getMFTName e
          fileobj = NTFSFile s fname fparent (getMFTSectors e)
      return (fileobj : rest)

readNTFS :: Char -> IO NTFS
readNTFS path = do
  h <- openDrive path
  bs <- readBootSector h
  let spc  = bsSPC bs
      mft  = toInteger (spc * (bsMFTOff bs))
      bak  = toInteger (spc * (bsBAKOff bs))
      mfts = []
      frag = MFTFrag []
      start = (NTFS path h bs mft bak mfts 31000)
  mfts <- readMFTs h mft 0
  let n1 = (NTFS path h bs mft bak mfts 31000 (mfts !! 5) frag) -- 242943)
  frags <- calcMFT n1
  return $ n1 { ntMFrag = frags }
    where isJust Nothing = False
          isJust _         = True

getFile :: NTFS -> MFTID -> IO (Maybe NTFSFile)
getFile ntfs id = do
  let (MFTFrag frags) = (ntMFrag ntfs)
      mftraw = calcMFTraw ntfs id
  entry <- case frags of
    [] -> getMFT (ntHandle ntfs) (ntMFTOff ntfs) id
    x -> getMFTraw (ntHandle ntfs) mftraw
  let (Just e) = entry
  case entry of
    Nothing -> return Nothing
    Just e  -> do
      let names   = getMFTNames e
          (fparent, fname) = last names 
          sectors          = getMFTSectors e
          fileobj = NTFSFile id fname fparent sectors
      return (Just fileobj)

getFilePath ntfs file = do
  parent <- getParentFile ntfs file
  if parent == file
    then return []
    else do
      parents <- getFilePath ntfs parent
      return (reverse (file : parents))

getParentFile :: NTFS -> NTFSFile -> IO NTFSFile
getParentFile ntfs (NTFSFile _ _ p _) = do
  file <- getFile ntfs p
  return (fromMaybe undefined file)

findFileByPath ntfs r [] = Just r
findFileByPath ntfs r (f:fs) =
  case findFileChild ntfs r f of
    Just r' -> findFileByPath ntfs r' fs
    Nothing -> Nothing

findFileChild :: NTFS -> NTFSFile -> Filename -> Maybe NTFSFile
findFileChild ntfs parent target = find match (ntMFT ntfs)
    where (NTFSFile id _ _ _) = parent
          match (NTFSFile _ n p _) | n == target && p == id = True
          match _ = False

findFileByName ntfs target = do
  files <- mapM (getFile ntfs) [0..(ntMax ntfs)]
  let valids = filter matchName files
  return (map (fromMaybe undefined) valids)
    where isJust (Just x) = True
          isJust (Nothing) = False
          matchName (Just (NTFSFile _ n _ _)) | n == target = True
          matchName _                                       = False

-- ntfsx.hs:
--
-- This is a simple tool that extracts files from an NTFS filesystem on a running system
-- without requiring SYSTEM rights or permissions to access the files in question.  This
-- turns out to be most useful for copying files that are locked by the operating system.
-- This functionality relies on the ability for an administrator account to open a read-
-- only file handle for a storage volume itself, and then to parse NTFS to identify the
-- sectors on the volume that house the desired data.
--
-- Author:  Josh Stone
-- Contact: yakovdk@gmail.com
-- Date:    2012-06-03
--
--

module Main where

import NTFS
import Data.Maybe
import Text.Printf
import SectorDump
import Data.List 
import Data.Char (toLower)
import Debug.Trace
import System.Environment (getArgs)
import Control.Monad

import qualified WinBlocks as WB
import MFT
import Blocker

main = do
  banner
  args <- getArgs
  n <- readNTFS 'C'
  let (MFTFrag fs) = ntMFrag n
  when (length fs > 1) $ do
    putStrLn "Note: MFT is fragmented, extraction is experimental!"
    putStrLn ""
    putStrLn "MFT Fragments:"
    putStrLn ""
    print fs
    putStrLn ""
  case args of
    ("sam":[])  -> doSAM n
    ("ntds":[]) -> doNTDS n
    (x:y:[])    -> doPath n (split '\\' x) y
    _ -> usage

doSAM n = doPair n sam system
  where sam    = (["Windows", "System32", "config", "SAM"], "SAM.out")
        system = (["Windows", "System32", "config", "SYSTEM"], "SYSTEM.out")

doNTDS n = doPair n ntds system
  where ntds   = (["Windows", "NTDS", "NTDS.dit"], "NTDS.out")
        system = (["Windows", "System32", "config", "SYSTEM"], "SYSTEM.out")

doPair n (p1,o1) (p2,o2) = do
  dumpFile n p1 o1
  dumpFile n p2 o2

split d = unfoldr iterate
  where adjust (i,j) = (i, drop 1 j)
        iterate []   = Nothing
        iterate l    = Just (adjust $ break (==d) l)

doPath n path out = do
  n <- readNTFS 'C'
  putStrLn "NOTE: this file must exist, or I won't terminate"
  putStrLn ""
  dumpFile n path out

dumpFile n path out = do
  -- sam <- huntPath n path
  sam <- multiHunt n path
  let joined = foldl1 ((++).(++ "\\")) path
  putStrLn ("Exporting " ++ joined ++ " to '" ++ out ++ "'...")
  extractFile n (nfID sam) out

extractFile n id fname = do
  (Just f) <- getFile n id
  let raw = calcMFTraw n id
  (Just mft) <- getMFTraw (ntHandle n) raw
  let hdr = mftHeader mft
  putStrLn ""
  print f
  exportRuns (ntHandle n) fname (nfSectors f)

huntPath n = huntPath' n ntfsRoot
huntPath' n p [] = return p
huntPath' n p (f:fs) = do
  putStrLn ("Searching for " ++ show f)
  (Just mft) <- huntMFT n p f
  huntPath' n (nfID mft) fs

usage = do
  putStrLn "  usage: ntfsx ( ntds | sam | [<path> <output>] )"
  putStrLn ""
  putStrLn "     <path>    Backslash-delimited path of file to extract"
  putStrLn "     <output>  EXPERIMENTAL! File to write contents to"
  putStrLn "     ntds      Export NTDS.dit to 'NTDS.out'"
  putStrLn "     sam       Export the SAM to 'SAM.out'"
  putStrLn ""
  putStrLn "  NTFSx is a generalized solution for extracting locked files in"
  putStrLn "  a Microsoft Windows environment.  If you have at least local"
  putStrLn "  administrator rights, you can open a read-only file handle for"
  putStrLn "  a storage volume.  Raw data can be read without enforcement of"
  putStrLn "  access rights, which means we can extract any file by parsing"
  putStrLn "  NTFS and reading the appropriate sectors from the disk"
  putStrLn ""
  putStrLn "  This can be very useful for extracting files like NTDS.DIT or"
  putStrLn "  files that are currently locked (e.g., MSSQL, Exchange, etc.)"
  putStrLn ""
  putStrLn "  NOTE: This code is adequate, but not efficient.  It may take"
  putStrLn "        several minutes to extract files.  Please be patient."
  putStrLn ""
  putStrLn "  NOTE: Files exported will be padded with extra bytes to the"
  putStrLn "        length defined by the NTFS run length.  This is not"
  putStrLn "        typically a problem."
  putStrLn ""
  putStrLn "        Also, I currently only extract files from the C: drive"
  putStrLn ""

banner = do
  putStrLn "-----------------------------------------------------------------"
  putStrLn "        NTFSx - www.josho.org - (C) June 2012 Josh Stone"
  putStrLn "  (This software comes with NO WARRANTY; your mileage may vary)"
  putStrLn "-----------------------------------------------------------------"
  putStrLn ""


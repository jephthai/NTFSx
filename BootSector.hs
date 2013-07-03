module BootSector where

import System.IO
import Blocker
import TypeSyms

data BootSector = BootSector {
      bsJump    :: Int,
      bsName    :: String,
      bsBPS     :: MInt,          -- Bytes Per Sector
      bsSPC     :: MInt,          -- Sectors per Cluster
      bsMFTOff  :: MWord,         
      bsBAKOff  :: MWord          
    } deriving (Eq, Show)

readBootSector h = do
  b <- readBlock h (0::Integer)
  let jump = getLittleEndianInt 3 b 0x00
      name = getASCII b 0x03 8
      bps  = getInt   b 0x0b
      spc  = getInt   b 0x0d
      mft  = getWord  b 0x30
      bak  = getWord  b 0x38
  return (BootSector jump name bps spc mft bak)

getMFTBlock (BootSector _ _ _ s m _) = s * m


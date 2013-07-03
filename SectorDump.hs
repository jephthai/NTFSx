-- SectorDump.hs:
--
-- This is a module that exports block ranges from a disk using the
-- Win32 API.  It expects to have a handle provided which can be
-- created with the "CreateFile()" Win32 API call.
--
--
-- Author:  Josh Stone
-- Contact: yakovdk@gmail.com
-- Created: 2012
--

module SectorDump (
  exportSectors,
  exportRuns
  ) where

import System.Win32.File
import System.Win32.Types
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import Data.Word
import Data.Bits
import Unsafe.Coerce
import Control.Monad

--
-- System.Win32.File does not import the SetFilePointer() function.
-- This is a usable import that will allow me to seek the file HANDLE
-- to whatever byte offset I want.
--

foreign import stdcall unsafe "windows.h SetFilePointer"
    c_setFilePointer :: HANDLE -> DWORD -> Ptr a -> DWORD -> IO ()

--
-- This is a Haskell wrapper for my import above that seeks to a block
-- offset (512-byte blocks).
--

setFilePointer :: HANDLE -> Integer -> IO ()
setFilePointer h offset = do
    p <- mallocBytes 4
    poke (castPtr p) high
    c_setFilePointer h low p 0
    free p
  where block = fromIntegral (offset * 512) :: Word64
        low = unsafeCoerce (block .&. 0xffffffff) :: Word32
        high = unsafeCoerce (block `shift` (-32)) .&. 0xffffffff :: Word32

--
-- Given a buffer, this function will return a single byte at the req-
-- uested offset.
--

secReadByte :: Ptr Word8 -> Int -> IO Word8
secReadByte mem addr = do
    val <- peek offset
    return val

  where offset = mem `plusPtr` addr

--
-- The primary functionality I need, disk-wise, is to read a single
-- block from one HANDLE and write the same data out to another one.
--

readWriteBlock h1 h2 = do
    buffer <- mallocBytes 512
    count <- win32_ReadFile h1 buffer 512 Nothing
    win32_WriteFile h2 buffer count Nothing
    free buffer
    return count

secReadBlock :: HANDLE -> IO [Word8]
secReadBlock h = do
    buffer <- mallocBytes 512
    win32_ReadFile h buffer 512 Nothing
    ws <- mapM (secReadByte buffer) [0..511]
    free buffer
    return ws

exportRuns h outfile runs = do
  out <- createFile outfile gENERIC_WRITE 3 Nothing cREATE_ALWAYS fILE_ATTRIBUTE_NORMAL Nothing
  forM_ runs $ \(offset, count) -> do
    setFilePointer h offset
    forM [1..count] $ \_ -> do
      readWriteBlock h out
  return ()
  
exportSectors h outfile start count = do
    out <- createFile outfile gENERIC_WRITE 3 Nothing cREATE_ALWAYS fILE_ATTRIBUTE_NORMAL Nothing
    setFilePointer h start
    mapM_ (\_ -> readWriteBlock h out) [1..count]
    closeHandle out

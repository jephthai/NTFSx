--
-- Author:  Josh Stone
-- Contact: yakovdk@gmail.com
-- Created: 2012
--

module WinBlocks where

import System.Win32.File
import System.Win32.Types
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import Data.Word
import Data.Char
import Data.Bits
import Unsafe.Coerce
    
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
    --putStrLn ("Offset:  " ++ (show offset))
    --putStrLn ("Block:   " ++ (show block))
    --putStrLn ("Low:     " ++ (show low))
    --putStrLn ("High:    " ++ (show high))
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
        
readByte :: Ptr Word8 -> Int -> IO Word8
readByte mem addr = do
    val <- peek offset
    return val
  where offset = mem `plusPtr` addr
  
-- 
-- The primary functionality I need, disk-wise, is to read a single
-- block from one HANDLE and write the same data out to aonther one.
--

readWriteBlock h1 h2 = do
    buffer <- mallocBytes 512
    count <- win32_ReadFile h1 buffer 512 Nothing
    win32_WriteFile h2 buffer count Nothing
    free buffer
    return count
    
readBlock :: HANDLE -> IO [Word8]
readBlock h = do
    buffer <- mallocBytes 512
    win32_ReadFile h buffer 512 Nothing
    ws <- mapM (readByte buffer) [0..511]
    free buffer
    return ws

openDrive :: Char -> IO HANDLE
openDrive letter = do
    let path = "\\\\.\\" ++ [letter, ':']
    h <- createFile path gENERIC_READ 3 Nothing oPEN_EXISTING fILE_ATTRIBUTE_NORMAL Nothing
    return h

toASCII c | c < 0x20 = '.'
toASCII c | c > 0x7e = '.'
toASCII c            = chr (fromIntegral c)

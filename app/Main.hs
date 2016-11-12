{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.Array.IO
import Data.Word
import Control.Monad.State
import Control.Lens
import Data.Bits
import Data.ByteString as B
import System.IO
import Data.Binary.Get
import Data.Binary

import Core

main = do
    arr <- newArray (0, 2047) 0 :: IO (IOUArray Int Word8)
    handle <- openFile "test.bin" ReadMode
    n <- hGetArray handle arr 128
    print $ "read " ++ show n ++ " bytes"
    hClose handle
--    print contents
--   let n = B.length contents
--    let dat = B.unpack contents
--    arr <- listArray (0, n-1) dat
    --print arr
    --writeArray arr 0x0000 0x01
    let state = S { _mem = arr,  _clock = 0, _regs = R 0 0 0 0 0}
    flip runStateT state (do
        step
        step
        step
        step)

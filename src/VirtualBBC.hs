{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module VirtualBBC where

import qualified Data.ByteString.Internal as BS (c2w, w2c)
import Data.Array.IO
import Control.Monad.State
import Control.Lens
import Data.Bits.Lens
import System.Exit
import Data.Bits
import Data.Word
import Numeric

import Core

data Registers = R {
    _pc :: !Word16,
    _p :: !Word8,
    _a :: !Word8,
    _x :: !Word8,
    _y :: !Word8,
    _s :: !Word8
}

makeLenses ''Registers

{-# INLINE flagC #-}
flagC :: Lens' Registers Bool
flagC = p . bitAt 0

{-# INLINE flagZ #-}
flagZ :: Lens' Registers Bool
flagZ = p . bitAt 1

{-# INLINE flagI #-}
flagI :: Lens' Registers Bool
flagI = p . bitAt 2

{-# INLINE flagD #-}
flagD :: Lens' Registers Bool
flagD = p . bitAt 3

{-# INLINE flagB #-}
flagB :: Lens' Registers Bool
flagB = p . bitAt 4

{-# INLINE flagV #-}
flagV :: Lens' Registers Bool
flagV = p . bitAt 6

{-# INLINE flagN #-}
flagN :: Lens' Registers Bool
flagN = p . bitAt 7

data State6502 = S {
    _mem :: IOUArray Int Word8,
    _clock :: !Int,
    _regs :: !Registers,
    _debug :: !Bool
}

makeLenses ''State6502

newtype Monad6502 a = M { unM :: StateT State6502 IO a }
    deriving (Functor, Applicative, Monad, MonadState State6502, MonadIO)

instance Emu6502 Monad6502 where
    {-# INLINE readMemory #-}
    readMemory addr = do
        -- debugStrLn $ "Reading from addr " ++ showHex addr ""
        -- if addr == 0x8000
        --     then do
        --         c <- liftIO $ getChar
        --         return $ BS.c2w c
        --     else do
        --         m <- use mem
        --         liftIO $ readArray m addr

        m <- use mem
        liftIO $ readArray m (fromIntegral addr)

    {-# INLINE writeMemory #-}
    writeMemory addr v = do
        m <- use mem
        liftIO $ writeArray m (fromIntegral addr) v
        -- debugStrLn $ "Writing " ++ showHex v "" ++ " to addr " ++ showHex addr ""
        -- if addr == 0x8000
        --     then do
        --         liftIO $ putChar (BS.w2c v)
        --     else do
        --         m <- use mem
        --         liftIO $ writeArray m addr v

    {-# INLINE getPC #-}
    getPC = use (regs . pc)
    {-# INLINE tick #-}
    tick n = clock += n
    {-# INLINE putC #-}
    putC b = regs . flagC .= b
    {-# INLINE getC #-}
    getC = use (regs . flagC)
    {-# INLINE putZ #-}
    putZ b = regs . flagZ .= b
    {-# INLINE getZ #-}
    getZ = use (regs . flagZ)
    {-# INLINE putI #-}
    putI b = regs . flagI .= b
    {-# INLINE getI #-}
    getI = use (regs . flagI)
    {-# INLINE putD #-}
    putD b = regs . flagD .= b
    {-# INLINE getD #-}
    getD = use (regs . flagD)
    {-# INLINE putB #-}
    putB b = regs . flagB .= b
    {-# INLINE getB #-}
    getB = use (regs . flagB)
    {-# INLINE putV #-}
    putV b = regs . flagV .= b
    {-# INLINE getV #-}
    getV = use (regs . flagV)
    {-# INLINE putN #-}
    putN b = regs . flagN .= b
    {-# INLINE getN #-}
    getN = use (regs . flagN)
    {-# INLINE getA #-}
    getA = use (regs . a)
    {-# INLINE putA #-}
    putA r = regs . a .= r
    {-# INLINE getS #-}
    getS = use (regs . s)
    {-# INLINE putS #-}
    putS r = regs . s .= r
    {-# INLINE getX #-}
    getX = use (regs . x)
    {-# INLINE putX #-}
    putX r = regs . x .= r
    {-# INLINE getP #-}
    getP = use (regs . p)
    {-# INLINE putP #-}
    putP r = regs . p .= r
    {-# INLINE getY #-}
    getY = use (regs . y)
    {-# INLINE putY #-}
    putY r = regs . y .= r
    {-# INLINE putPC #-}
    putPC r = regs . pc .= r
    {-# INLINE addPC #-}
    addPC n = regs . pc += fromIntegral n

    {-# INLINE debugStr #-}
    debugStr str = do
        d <- use debug
        if d
            then liftIO $ putStr str
            else return ()

    {-# INLINE debugStrLn #-}
    debugStrLn str = do
        d <- use debug
        if d
            then liftIO $ putStrLn str
            else return ()

    {-# INLINE illegal #-}
    illegal i = if i==0x02
        then do
            -- host op
            p0 <- getPC
            op <- readMemory (p0+1)
            --liftIO $ putStrLn $ "Host call with op 0x" ++ showHex op ""
            case op of
                0x00 -> do
                    liftIO $ exitSuccess
                0x01 -> do
                    c <- getA
                    liftIO $ putChar (BS.w2c c)
                    putPC $ p0+2
                0x02 -> do
                    c <- liftIO $ getChar
                    putA (BS.c2w c)
                    putPC $ p0+2
                0x03 -> do
                    lo <- getX
                    hi <- getY
                    let addr = make16 lo hi
                    sAddrLo <- readMemory addr
                    sAddrHi <- readMemory (addr+1)
                    let sAddr = make16 sAddrLo sAddrHi
                    line <- liftIO $ getLine
                    let n = length line
                    forM_ [0..n-1] $ \i -> do
                        writeMemory (sAddr+i16 i) (BS.c2w (line!!i))
                    writeMemory (sAddr+i16 n) 13
                    putC False
                    putY $ i8 n+1
                    putPC $ p0+2
                otherwise -> do
                    error $ "Unknown host op 0x" ++ showHex op "" ++ " at 0x" ++ showHex p0 ""
        else error $ "Illegal opcode 0x" ++ showHex i ""

{-
{-# INLINE writeRAM #-}
writeRAM :: IOUArray Int Word8 -> Int -> Word8 -> Monad6502 ()
writeRAM m addr b = liftIO $ writeArray m addr b

{-# INLINE readRAM #-}
readRAM :: IOUArray Int Word8 -> Int -> Monad6502 Word8
readRAM m addr = liftIO $ readArray m addr
-}

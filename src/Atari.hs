{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Atari where

import Data.Array.IO
import Control.Monad.State
import Control.Lens
import Data.Bits.Lens
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
    readMemory addr =
        if addr >= 0x00 && addr < 0x80
            then do
                liftIO $ putStrLn $ "reading TIA 0x" ++ showHex addr ""
                return 0
            else if addr >= 0x80 && addr < 0x100 || addr >= 0x180 && addr < 0x200
                    then do
                        m <- use mem
                        liftIO $ readArray m (fromIntegral addr .&. 0xff)
                    else if addr >= 0x280 && addr < 0x298
                        then do
                            liftIO $ putStrLn $ "reading PIA 0x" ++ showHex addr ""
                            return 0
                        else if addr >= 0xf000
                            then do
                                m <- use mem
                                liftIO $ readArray m (fromIntegral addr)
                            else do
                                error $ "Mystery read from " ++ showHex addr ""

    {-# INLINE writeMemory #-}
    writeMemory addr v =
        if addr >= 0x00 && addr < 0x80
            then liftIO $ putStrLn $ "writing TIA 0x" ++ showHex addr ""
            else if addr >= 0x80 && addr < 0x100 || addr >= 0x180 && addr < 0x200
                    then do
                        m <- use mem
                        liftIO $ writeArray m (fromIntegral addr .&. 0xff) v
                    else if addr >= 0x280 && addr < 0x298
                            then liftIO $ putStrLn $ "writing PIA 0x" ++ showHex addr ""
                            else if addr >= 0xf000
                                then do
                                    m <- use mem
                                    liftIO $ writeArray m (fromIntegral addr) v
                                else do
                                    error $ "Mystery write to " ++ showHex addr ""

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
    illegal i = error $ "Illegal opcode 0x" ++ showHex i ""

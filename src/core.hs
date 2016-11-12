{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleContexts #-}

-- http://nesdev.com/6502_cpu.txt

module Core where

import Data.Array.IO
import Data.Word
import Control.Monad.State
import Control.Lens
import Data.Bits
import Data.Bits.Lens
import Data.ByteString as B hiding (putStr, putStrLn)
import System.IO
import Data.Binary.Get
import Data.Binary
import Data.Int
import Numeric

data Registers = R {
    _pc :: Word16,
    _p :: Word8,
    _a :: Word8,
    _x :: Word8,
    _y :: Word8
}

makeLenses ''Registers

data State6502 = S {
    _mem :: IOUArray Int Word8,
    _clock :: Int64,
    _regs :: Registers
}

makeLenses ''State6502

flagC :: Lens' Registers Bool
flagC = p . bitAt 0

flagZ :: Lens' Registers Bool
flagZ = p . bitAt 1

flagI :: Lens' Registers Bool
flagI = p . bitAt 2

flagD :: Lens' Registers Bool
flagD = p . bitAt 3

flagB :: Lens' Registers Bool
flagB = p . bitAt 4

flagV :: Lens' Registers Bool
flagV = p . bitAt 6

flagS :: Lens' Registers Bool
flagS = p . bitAt 7

dumpRegisters :: StateT State6502 IO ()
dumpRegisters = do
    tClock <- use clock
    liftIO $ putStr $ "clock = " ++ show tClock
    regPC <- use (regs . pc)
    liftIO $ putStr $ " pc = " ++ showHex regPC ""
    regP <- use (regs . p)
    liftIO $ putStr $ " flags = " ++ showHex regP ""
    liftIO $ putStr $ "(S=" ++ showHex ((regP `shift` (-7)) .&. 1) ""
    liftIO $ putStr $ ",V=" ++ showHex ((regP `shift` (-6)) .&. 1) ""
    liftIO $ putStr $ ",B=" ++ showHex (regP `shift` ((-4)) .&. 1) ""
    liftIO $ putStr $ ",D=" ++ showHex (regP `shift` ((-3)) .&. 1) ""
    liftIO $ putStr $ ",I=" ++ showHex (regP `shift` ((-2)) .&. 1) ""
    liftIO $ putStr $ ",Z=" ++ showHex (regP `shift` ((-1)) .&. 1) ""
    liftIO $ putStr $ ",C=" ++ showHex (regP .&. 1) ""
    regA <- use (regs . a)
    liftIO $ putStr $ ") A = " ++ showHex regA ""
    regX <- use (regs . x)
    liftIO $ putStr $ " X = " ++ showHex regX ""
    regY <- use (regs . y)
    liftIO $ putStrLn $ " Y = " ++ showHex regY ""

dumpMemory :: StateT State6502 IO ()
dumpMemory = do
    regPC <- use (regs . pc)
    m <- use mem
    b0 <- liftIO $ readArray m (fromIntegral regPC)
    b1 <- liftIO $ readArray m (fromIntegral regPC+1)
    b2 <- liftIO $ readArray m (fromIntegral regPC+2)
    liftIO $ putStr $ "(PC) = "
    liftIO $ putStr $ showHex b0 "" ++ " "
    liftIO $ putStr $ showHex b1 "" ++ " "
    liftIO $ putStrLn $ showHex b2 ""

dumpState :: StateT State6502 IO ()
dumpState = do
    dumpMemory
    dumpRegisters

read16 :: Word16 -> StateT State6502 IO Word16
read16 addr = do
    m <- use mem
    lo <- liftIO $ readArray m (fromIntegral addr)
    hi <- liftIO $ readArray m (fromIntegral addr+1)
    return $ (fromIntegral hi `shift` 8)+fromIntegral lo

read16zp :: Word8 -> StateT State6502 IO Word16
read16zp addr = do
    m <- use mem
    lo <- liftIO $ readArray m (fromIntegral addr)
    hi <- liftIO $ readArray m (fromIntegral (addr+1))
    return $ (fromIntegral hi `shift` 8)+fromIntegral lo

-- http://www.emulator101.com/6502-addressing-modes.html

putData :: Word8 -> Word8 -> StateT State6502 IO ()
putData bbb src = case bbb of
    -- (zero page, X)
    0b000 -> do
        m <- use mem
        p0 <- use (regs . pc)
        offsetX <- use (regs . x)
        zpAddr <- liftIO $ readArray m (fromIntegral (p0+1))
        let addrAddr = zpAddr+offsetX :: Word8
        addr <- read16zp addrAddr
        liftIO $ writeArray m (fromIntegral addr) src
        regs . pc .= p0+2
        clock += 6
    -- zero page
    0b001 -> do
        m <- use mem
        p0 <- use (regs . pc)
        addr <- liftIO $ readArray m (fromIntegral (p0+1))
        liftIO $ writeArray m (fromIntegral addr) src
        regs . pc .= p0+2
        clock += 3
    -- immediate
    0b010 -> do
        error "Can't store immediate"
    -- absolute
    0b011 -> do
        m <- use mem
        p0 <- use (regs . pc)
        lo <- liftIO $ readArray m (fromIntegral (p0+1))
        hi <- liftIO $ readArray m (fromIntegral (p0+2))
        let addr = (hi `shift` 8)+lo
        liftIO $ writeArray m (fromIntegral addr) src
        regs . pc .= p0+3
        clock += 4
    -- (zero page), Y
    0b100 -> do
        m <- use mem
        p0 <- use (regs . pc)
        offsetY <- use (regs . y)
        zpAddr <- liftIO $ readArray m (fromIntegral (p0+1))
        addr <- read16zp zpAddr
        let newAddr = addr+fromIntegral offsetY :: Word16
        let carry = (newAddr .&. 0xff00) /= (addr .&. 0xff00)
        liftIO $ writeArray m (fromIntegral addr+fromIntegral offsetY) src
        regs . pc .= p0+2
        clock += 6
    -- zero page, X
    0b101 -> do
        m <- use mem
        p0 <- use (regs . pc)
        offsetX <- use (regs . x)
        zpAddr <- liftIO $ readArray m (fromIntegral (p0+1))
        let addrAddr = zpAddr+offsetX :: Word8
        liftIO $ writeArray m (fromIntegral addrAddr) src
        regs . pc .= p0+2
        clock += 4
    -- absolute, Y
    0b110 -> do
        m <- use mem
        p0 <- use (regs . pc)
        offsetY <- use (regs . y)
        baseAddr <- read16 (fromIntegral (p0+1))
        let addr = baseAddr+fromIntegral offsetY :: Word16
        let carry = (addr .&. 0xff00) /= (baseAddr .&. 0xff00)
        liftIO $ writeArray m (fromIntegral addr) src
        regs . pc .= p0+3
        clock += 5
    -- absolute, X
    0b111 -> do
        m <- use mem
        p0 <- use (regs . pc)
        offsetX <- use (regs . x)
        baseAddr <- read16 (fromIntegral (p0+1))
        let addr = baseAddr+fromIntegral offsetX :: Word16
        let carry = (addr .&. 0xff00) /= (baseAddr .&. 0xff00)
        liftIO $ writeArray m (fromIntegral addr) src
        regs . pc .= p0+3
        clock += 5

getData :: Word8 -> StateT State6502 IO Word8
getData bbb = case bbb of
    -- (zero page, X)
    0b000 -> do
        m <- use mem
        p0 <- use (regs . pc)
        offsetX <- use (regs . x)
        zpAddr <- liftIO $ readArray m (fromIntegral (p0+1))
        let addrAddr = zpAddr+offsetX :: Word8
        addr <- read16zp addrAddr
        src <- liftIO $ readArray m (fromIntegral addr)
        regs . pc .= p0+2
        clock += 6
        return src
    -- zero page
    0b001 -> do
        m <- use mem
        p0 <- use (regs . pc)
        addr <- liftIO $ readArray m (fromIntegral (p0+1))
        src <- liftIO $ readArray m (fromIntegral addr)
        regs . pc .= p0+2
        clock += 3
        return src
    -- immediate
    0b010 -> do
        m <- use mem
        p0 <- use (regs . pc)
        src <- liftIO $ readArray m (fromIntegral (p0+1))
        regs . pc .= p0+2
        clock += 2
        return src
    -- absolute
    0b011 -> do
        m <- use mem
        p0 <- use (regs . pc)
        lo <- liftIO $ readArray m (fromIntegral (p0+1))
        hi <- liftIO $ readArray m (fromIntegral (p0+2))
        let addr = (hi `shift` 8)+lo
        src <- liftIO $ readArray m (fromIntegral addr)
        regs . pc .= p0+3
        clock += 4
        return src
    -- (zero page), Y
    0b100 -> do
        m <- use mem
        p0 <- use (regs . pc)
        offsetY <- use (regs . y)
        zpAddr <- liftIO $ readArray m (fromIntegral (p0+1))
        addr <- read16zp zpAddr
        let newAddr = addr+fromIntegral offsetY :: Word16
        let carry = (newAddr .&. 0xff00) /= (addr .&. 0xff00)
        src <- liftIO $ readArray m (fromIntegral addr+fromIntegral offsetY)
        regs . pc .= p0+2
        clock += if carry then 6 else 5
        return src
    -- zero page, X
    0b101 -> do
        m <- use mem
        p0 <- use (regs . pc)
        offsetX <- use (regs . x)
        zpAddr <- liftIO $ readArray m (fromIntegral (p0+1))
        let addrAddr = zpAddr+offsetX :: Word8
        src <- liftIO $ readArray m (fromIntegral addrAddr)
        regs . pc .= p0+2
        clock += 4
        return src
    -- absolute, Y
    0b110 -> do
        m <- use mem
        p0 <- use (regs . pc)
        offsetY <- use (regs . y)
        baseAddr <- read16 (fromIntegral (p0+1))
        let addr = baseAddr+fromIntegral offsetY :: Word16
        let carry = (addr .&. 0xff00) /= (baseAddr .&. 0xff00)
        src <- liftIO $ readArray m (fromIntegral addr)
        regs . pc .= p0+3
        clock += if carry then 5 else 4
        return src
    -- absolute, X
    0b111 -> do
        m <- use mem
        p0 <- use (regs . pc)
        offsetX <- use (regs . x)
        baseAddr <- read16 (fromIntegral (p0+1))
        let addr = baseAddr+fromIntegral offsetX :: Word16
        let carry = (addr .&. 0xff00) /= (baseAddr .&. 0xff00)
        src <- liftIO $ readArray m (fromIntegral addr)
        regs . pc .= p0+3
        clock += if carry then 5 else 4
        return src

-- Need to separate R/W/RW XXX
withData01 :: Word8 -> Bool -> Bool ->
              (Word8 -> StateT State6502 IO Word8) ->
              StateT State6502 IO ()
withData01 bbb write useY op = case bbb of
    -- immediate
    0b000 -> do
        m <- use mem
        p0 <- use (regs . pc)
        src <- liftIO $ readArray m (fromIntegral (p0+1))
        op src
        regs . pc .= p0+2
        if write
            then error "Can't write immediate"
            else clock += 2
    -- zero page
    0b001 -> do
        m <- use mem
        p0 <- use (regs . pc)
        addr <- liftIO $ readArray m (fromIntegral (p0+1))
        src <- liftIO $ readArray m (fromIntegral addr)
        dst <- op src
        if write
            then do
                liftIO $ writeArray m (fromIntegral addr) dst
                clock += 5
            else
                clock += 3
        regs . pc .= p0+2
    -- accumulator -- XXX
    0b010 -> do
        p0 <- use (regs . pc)
        src <- use (regs . a)
        dst <- op src
        regs . a .= dst
        regs . pc .= p0+1
        if write
            then clock += 2
            else error "Must write back to A"
    -- absolute
    0b011 -> do
        m <- use mem
        p0 <- use (regs . pc)
        lo <- liftIO $ readArray m (fromIntegral (p0+1))
        hi <- liftIO $ readArray m (fromIntegral (p0+2))
        let addr = (hi `shift` 8)+lo
        src <- liftIO $ readArray m (fromIntegral addr)
        dst <- op src
        regs . pc .= p0+3
        if write
            then do
                liftIO $ writeArray m (fromIntegral addr) dst
                clock += 6
            else clock += 4
    -- zero page, X
    0b101 -> do
        m <- use mem
        p0 <- use (regs . pc)
        offsetX <- if useY then use (regs . y) else use (regs . x)
        zpAddr <- liftIO $ readArray m (fromIntegral (p0+1))
        let addr = zpAddr+offsetX :: Word8
        src <- liftIO $ readArray m (fromIntegral addr)
        dst <- op src
        regs . pc .= p0+2
        if write
            then do
                liftIO $ writeArray m (fromIntegral addr) dst
                clock += 6
            else clock += 4
    -- absolute, X
    0b111 -> do
        m <- use mem
        p0 <- use (regs . pc)
        offsetX <- if useY then use (regs . y) else use (regs . x)
        baseAddr <- read16 (fromIntegral (p0+1))
        let addr = baseAddr+fromIntegral offsetX :: Word16
        let carry = (addr .&. 0xff00) /= (baseAddr .&. 0xff00)
        src <- liftIO $ readArray m (fromIntegral addr)
        dst <- op src
        regs . pc .= p0+3
        if write
            then do
                liftIO $ writeArray m (fromIntegral addr) dst
                clock += 7
            else
                clock += if carry then 5 else 4

    otherwise -> error "Unknown addressing mode"

ins_ora :: Word8 -> StateT State6502 IO ()
ins_ora bbb = do
    src <- getData bbb
    oldA <- use (regs . a)
    let newA = oldA .|. src
    regs . a .= newA
    regs . flagS .= (newA .&. 0x80 > 0)
    regs . flagZ .= (newA == 0)
    liftIO $ print $ "A = " ++ show newA

ins_and :: Word8 -> StateT State6502 IO ()
ins_and bbb = do
    src <- getData bbb
    oldA <- use (regs . a)
    let newA = oldA .&. src
    regs . a .= newA
    regs . flagS .= (newA .&. 0x80 > 0)
    regs . flagZ .= (newA == 0)
    liftIO $ print $ "A = " ++ show newA

ins_xor :: Word8 -> StateT State6502 IO ()
ins_xor bbb = do
    src <- getData bbb
    oldA <- use (regs . a)
    let newA = oldA `xor` src
    regs . a .= newA
    regs . flagS .= (newA .&. 0x80 > 0)
    regs . flagZ .= (newA == 0)
    liftIO $ print $ "A = " ++ show newA

ins_lda :: Word8 -> StateT State6502 IO ()
ins_lda bbb = do
    newA <- getData bbb
    regs . a .= newA
    regs . flagS .= (newA .&. 0x80 > 0)
    regs . flagZ .= (newA == 0)
    liftIO $ print $ "A = " ++ show newA

ins_sta :: Word8 -> StateT State6502 IO ()
ins_sta bbb = do
    oldA <- use (regs . a)
    putData bbb oldA

ins_adc :: Word8 -> StateT State6502 IO ()
ins_adc bbb = do
    src <- getData bbb
    oldA <- use (regs . a)
    carry <- use (regs . flagC)
    let newA = fromIntegral oldA+fromIntegral src+if carry then 1 else 0 :: Word16
    decimal <- use (regs . flagD)
    regs . flagZ .= (newA .&. 0xff == 0)
    if decimal
        then do
            let adjustedA = if (oldA .&. 0xf) + (src .&. 0xf) + (if carry then 1 else 0) > 9
                                then newA+6
                                else newA
            regs . flagS .= (adjustedA .&. 0x80 > 0)
            regs . flagV .= ((complement (fromIntegral oldA `xor` fromIntegral src) .&. 0x80) .&. ((fromIntegral oldA `xor` adjustedA) .&. 0x80) /= 0)
            let readjustedA = if adjustedA > 0x99 then adjustedA+96 else adjustedA
            regs . flagC .= (readjustedA > 0xff)
            regs . a .= fromIntegral (readjustedA .&. 0xff)
        else do
            regs . flagS .= (newA .&. 0x80 > 0)
            regs . flagV .= ((complement (fromIntegral oldA `xor` fromIntegral src) .&. 0x80) .&. ((fromIntegral oldA `xor` newA) .&. 0x80) /= 0)
            regs . flagC .= (newA > 0xff)
            regs . a .= fromIntegral (newA .&. 0xff)
    liftIO $ print $ "A = " ++ show newA

ins_sbc :: Word8 -> StateT State6502 IO ()
ins_sbc bbb = do
    src <- getData bbb
    oldA <- use (regs . a)
    carry <- use (regs . flagC)
    let newA = fromIntegral oldA-fromIntegral src-if carry then 0 else 1 :: Word16
    regs . flagS .= (newA .&. 0x80 > 0)
    regs . flagZ .= (newA .&. 0xff == 0)
    regs . flagV .= (((fromIntegral oldA `xor` fromIntegral src) .&. 0x80) .&. ((fromIntegral oldA `xor` newA) .&. 0x80) /= 0)
    decimal <- use (regs . flagD)
    if decimal
        then do
            let adjustedA = if (oldA .&. 0xf)-(if carry then 0 else 1) < (src .&. 0xf)
                                then newA - 6
                                else newA
            let readjustedA = if adjustedA > 0x99
                                then adjustedA-0x60
                                else adjustedA
            regs . a .= fromIntegral (readjustedA .&. 0xff)
        else
            regs . a .= fromIntegral (newA .&. 0xff)
    regs . flagC .= (newA < 0x100)
    liftIO $ print $ "A = " ++ show newA

ins_asl :: Word8 -> StateT State6502 IO ()
ins_asl bbb = withData01 bbb True False $ \src -> do
    regs . flagC .= (src .&. 0x80 > 0)
    let new = src `shift` 1
    regs . flagS .= (new .&. 0x80 > 0)
    regs . flagZ .= (new == 0)
    return new

ins_rol :: Word8 -> StateT State6502 IO ()
ins_rol bbb = withData01 bbb True False $ \src -> do
    fc <- use (regs . flagC)
    regs . flagC .= (src .&. 0x80 > 0)
    let new = (src `shift` 1) + if fc then 1 else 0
    regs . flagS .= (new .&. 0x80 > 0)
    regs . flagZ .= (new == 0)
    return new

ins_lsr :: Word8 -> StateT State6502 IO ()
ins_lsr bbb = withData01 bbb True False $ \src -> do
    regs . flagC .= (src .&. 0x01 > 0)
    let new = src `shift` (-1)
    regs . flagS .= False
    regs . flagZ .= (new == 0)
    return new

ins_ror :: Word8 -> StateT State6502 IO ()
ins_ror bbb = withData01 bbb True False $ \src -> do
    fc <- use (regs . flagC)
    regs . flagC .= (src .&. 0x01 > 0)
    let new = (src `shift` (-1))+if fc then 0x80 else 0x00
    regs . flagS .= (new .&. 0x80 > 0)
    regs . flagZ .= (new == 0)
    return new

ins_stx :: Word8 -> StateT State6502 IO ()
ins_stx bbb = withData01 bbb True True $ \src -> do
    new <- use (regs . x)
    return new

ins_ldx :: Word8 -> StateT State6502 IO ()
ins_ldx bbb = withData01 bbb False True $ \src -> do
    regs . x .= src
    return 0 -- Unused, I hope

ins_dec :: Word8 -> StateT State6502 IO ()
ins_dec bbb = withData01 bbb True False $ \src -> do
    let new = src-1
    regs . flagS .= (new .&. 0x80 > 0)
    regs . flagZ .= (new == 0)
    return new

ins_inc :: Word8 -> StateT State6502 IO ()
ins_inc bbb = withData01 bbb True False $ \src -> do
    let new = src+1
    regs . flagS .= (new .&. 0x80 > 0)
    regs . flagZ .= (new == 0)
    return new

step :: StateT State6502 IO ()
step = do
    liftIO $ print "------"
    dumpState
    m <- use mem
    p <- use (regs . pc)
    liftIO $ print $ "pc = " ++ showHex p ""
    i <- liftIO $ readArray m (fromIntegral p)
    liftIO $ print $ "instruction = " ++ showHex i ""
    let cc = i .&. 0b11
    liftIO $ print $ "cc = " ++ show cc
    case cc of
        0b01 -> do
            liftIO $ print "0b01 instruction"
            let aaa = (i `shift` (-5)) .&. 0b111
            let bbb = (i `shift` (-2)) .&. 0b111
            case aaa of

                -- ORA
                0b000 -> ins_ora bbb
                -- AND
                0b001 -> ins_and bbb
                -- EOR
                0b010 -> ins_xor bbb
                -- ADC
                0b011 -> ins_adc bbb
                -- LDA
                0b101 -> ins_lda bbb
                -- STA
                0b100 -> ins_sta bbb
                -- SBC
                0b111 -> ins_sbc bbb

                otherwise -> error "Unknown aaa"
        0b10 -> do
            liftIO $ print "0b10 instruction"
            let aaa = (i `shift` (-5)) .&. 0b111
            let bbb = (i `shift` (-2)) .&. 0b111
            case aaa of

                -- ASL
                0b000 -> ins_asl bbb
                -- ROL
                0b001 -> ins_rol bbb
                -- LSR
                0b010 -> ins_lsr bbb
                -- ROR
                0b011 -> ins_ror bbb
                -- STX
                0b100 -> ins_stx bbb
                -- LDX
                0b101 -> ins_ldx bbb
                -- DEC
                0b110 -> ins_dec bbb
                -- INC
                0b111 -> ins_inc bbb
        otherwise -> error "Unknown instruction class (cc)"
    dumpState
    return ()


{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

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
import qualified Data.ByteString.Internal as BS (c2w, w2c)

data Registers = R {
    _pc :: Word16,
    _p :: Word8,
    _a :: Word8,
    _x :: Word8,
    _y :: Word8,
    _s :: Word8
}

makeLenses ''Registers

data State6502 = S {
    _mem :: IOUArray Int Word8,
    _clock :: Int64,
    _regs :: Registers,
    _debug :: Bool
}

makeLenses ''State6502

writeRAM :: IOUArray Int Word8 -> Int -> Word8 -> StateT State6502 IO ()
writeRAM m addr b = liftIO $ writeArray m addr b

readRAM :: IOUArray Int Word8 -> Int -> StateT State6502 IO Word8
readRAM m addr = liftIO $ readArray m addr

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

debugStr str = do
    d <- use debug
    if d
        then liftIO $ putStr str
        else return ()

debugStrLn str = do
    d <- use debug
    if d
        then liftIO $ putStrLn str
        else return ()

dumpRegisters :: StateT State6502 IO ()
dumpRegisters = do
    tClock <- use clock
    debugStr $ "clock = " ++ show tClock
    regPC <- use (regs . pc)
    debugStr $ " pc = " ++ showHex regPC ""
    regP <- use (regs . p)
    debugStr $ " flags = " ++ showHex regP ""
    debugStr $ "(S=" ++ showHex ((regP `shift` (-7)) .&. 1) ""
    debugStr $ ",V=" ++ showHex ((regP `shift` (-6)) .&. 1) ""
    debugStr $ ",B=" ++ showHex (regP `shift` ((-4)) .&. 1) ""
    debugStr $ ",D=" ++ showHex (regP `shift` ((-3)) .&. 1) ""
    debugStr $ ",I=" ++ showHex (regP `shift` ((-2)) .&. 1) ""
    debugStr $ ",Z=" ++ showHex (regP `shift` ((-1)) .&. 1) ""
    debugStr $ ",C=" ++ showHex (regP .&. 1) ""
    regA <- use (regs . a)
    debugStr $ ") A = " ++ showHex regA ""
    regX <- use (regs . x)
    debugStr $ " X = " ++ showHex regX ""
    regY <- use (regs . y)
    debugStrLn $ " Y = " ++ showHex regY ""

readMemory :: Int -> StateT State6502 IO Word8
readMemory addr = do
    debugStrLn $ "Reading from addr " ++ showHex addr ""
    if addr == 0x8000
        then do
            c <- liftIO $ getChar
--            liftIO $ print $ "Read char " ++ show (BS.c2w c)
            return $ BS.c2w c
        else do
            m <- use mem
            liftIO $ readArray m addr

writeMemory :: Int -> Word8 -> StateT State6502 IO ()
writeMemory addr v = do
    debugStrLn $ "Writing " ++ showHex v "" ++ " to addr " ++ showHex addr ""
    if addr == 0x8000
        then do
            liftIO $ putChar (BS.w2c v)
        else do
            m <- use mem
            liftIO $ writeArray m addr v

dumpMemory :: StateT State6502 IO ()
dumpMemory = do
    regPC <- use (regs . pc)
    b0 <- readMemory (fromIntegral regPC)
    b1 <- readMemory (fromIntegral regPC+1)
    b2 <- readMemory (fromIntegral regPC+2)
    debugStr $ "(PC) = "
    debugStr $ showHex b0 "" ++ " "
    debugStr $ showHex b1 "" ++ " "
    debugStrLn $ showHex b2 ""

dumpState :: StateT State6502 IO ()
dumpState = do
    dumpMemory
    dumpRegisters

read16 :: Word16 -> StateT State6502 IO Word16
read16 addr = do
    lo <- readMemory (fromIntegral addr)
    hi <- readMemory (fromIntegral addr+1)
    return $ (i16 hi `shift` 8)+i16 lo

read16zp :: Word8 -> StateT State6502 IO Word16
read16zp addr = do
    lo <- readMemory (fromIntegral addr)
    hi <- readMemory (fromIntegral (addr+1))
    return $ (fromIntegral hi `shift` 8)+fromIntegral lo

-- http://www.emulator101.com/6502-addressing-modes.html

i8 :: Integral a => a -> Word8
i8 = fromIntegral

i16 :: Integral a => a -> Word16
i16 = fromIntegral

putData :: Word8 -> Word8 -> StateT State6502 IO ()
putData bbb src = do
    p0 <- use (regs . pc)
    case bbb of
        -- (zero page, X)
        0b000 -> do
            offsetX <- use (regs . x)
            zpAddr <- readMemory (fromIntegral (p0+1))
            let addrAddr = zpAddr+offsetX :: Word8
            addr <- read16zp addrAddr
            writeMemory (fromIntegral addr) src
            regs . pc .= p0+2
            clock += 6
        -- zero page
        0b001 -> do
            addr <- readMemory (fromIntegral (p0+1))
            writeMemory (fromIntegral addr) src
            regs . pc .= p0+2
            clock += 3
        -- immediate
        0b010 -> do
            error "Can't store immediate"
        -- absolute
        0b011 -> do
            addr <- read16 (p0+1)
            writeMemory (fromIntegral addr) src
            regs . pc .= p0+3
            clock += 4
        -- (zero page), Y
        0b100 -> do
            offsetY <- use (regs . y)
            zpAddr <- readMemory (fromIntegral (p0+1))
            addr <- read16zp zpAddr
            let newAddr = addr+fromIntegral offsetY :: Word16
            let carry = (newAddr .&. 0xff00) /= (addr .&. 0xff00)
            writeMemory (fromIntegral addr+fromIntegral offsetY) src
            regs . pc .= p0+2
            clock += 6
        -- zero page, X
        0b101 -> do
            offsetX <- use (regs . x)
            zpAddr <- readMemory (fromIntegral (p0+1))
            let addrAddr = zpAddr+offsetX :: Word8
            writeMemory (fromIntegral addrAddr) src
            regs . pc .= p0+2
            clock += 4
        -- absolute, Y
        0b110 -> do
            offsetY <- use (regs . y)
            baseAddr <- read16 (fromIntegral (p0+1))
            let addr = baseAddr+fromIntegral offsetY :: Word16
            let carry = (addr .&. 0xff00) /= (baseAddr .&. 0xff00)
            writeMemory (fromIntegral addr) src
            regs . pc .= p0+3
            clock += 5
        -- absolute, X
        0b111 -> do
            offsetX <- use (regs . x)
            baseAddr <- read16 (fromIntegral (p0+1))
            let addr = baseAddr+fromIntegral offsetX :: Word16
            let carry = (addr .&. 0xff00) /= (baseAddr .&. 0xff00)
            writeMemory (fromIntegral addr) src
            regs . pc .= p0+3
            clock += 5

getData :: Word8 -> StateT State6502 IO Word8
getData bbb = case bbb of
    -- (zero page, X)
    0b000 -> do
        p0 <- use (regs . pc)
        offsetX <- use (regs . x)
        zpAddr <- readMemory (fromIntegral (p0+1))
        let addrAddr = zpAddr+offsetX :: Word8
        addr <- read16zp addrAddr
        src <- readMemory (fromIntegral addr)
        regs . pc .= p0+2
        clock += 6
        return src
    -- zero page
    0b001 -> do
        p0 <- use (regs . pc)
        addr <- readMemory (fromIntegral (p0+1))
        src <- readMemory (fromIntegral addr)
        regs . pc .= p0+2
        clock += 3
        return src
    -- immediate
    0b010 -> do
        p0 <- use (regs . pc)
        src <- readMemory (fromIntegral (p0+1))
        regs . pc .= p0+2
        clock += 2
        return src
    -- absolute
    0b011 -> do
        p0 <- use (regs . pc)
        addr <- read16 (fromIntegral (p0+1))
        addr <- read16 (p0+1)
        debugStrLn $ "Absolute read from " ++ showHex addr ""
        src <- readMemory (fromIntegral addr)
        regs . pc .= p0+3
        clock += 4
        return src
    -- (zero page), Y
    0b100 -> do
        p0 <- use (regs . pc)
        offsetY <- use (regs . y)
        zpAddr <- readMemory (fromIntegral (p0+1))
        addr <- read16zp zpAddr
        let newAddr = addr+fromIntegral offsetY :: Word16
        let carry = (newAddr .&. 0xff00) /= (addr .&. 0xff00)
        src <- readMemory (fromIntegral addr+fromIntegral offsetY)
        regs . pc .= p0+2
        clock += if carry then 6 else 5
        return src
    -- zero page, X
    0b101 -> do
        p0 <- use (regs . pc)
        offsetX <- use (regs . x)
        zpAddr <- readMemory (fromIntegral (p0+1))
        let addrAddr = zpAddr+offsetX :: Word8
        src <- readMemory (fromIntegral addrAddr)
        regs . pc .= p0+2
        clock += 4
        return src
    -- absolute, Y
    0b110 -> do
        p0 <- use (regs . pc)
        offsetY <- use (regs . y)
        baseAddr <- read16 (fromIntegral (p0+1))
        let addr = baseAddr+fromIntegral offsetY :: Word16
        let carry = (addr .&. 0xff00) /= (baseAddr .&. 0xff00)
        src <- readMemory (fromIntegral addr)
        regs . pc .= p0+3
        clock += if carry then 5 else 4
        return src
    -- absolute, X
    0b111 -> do
        p0 <- use (regs . pc)
        offsetX <- use (regs . x)
        baseAddr <- read16 (fromIntegral (p0+1))
        let addr = baseAddr+fromIntegral offsetX :: Word16
        let carry = (addr .&. 0xff00) /= (baseAddr .&. 0xff00)
        src <- readMemory (fromIntegral addr)
        regs . pc .= p0+3
        clock += if carry then 5 else 4
        return src

{-# INLINE ins_bra #-}
ins_bra :: Lens' Registers Bool -> Bool -> StateT State6502 IO ()
ins_bra flag value = do
    f <- use (regs . flag)
    p0 <- use (regs . pc)
    let oldP = p0+2
    if value && f || not value && not f
        then do
            debugStrLn "Taking branch"
            offset <- readMemory (fromIntegral (p0+1)) -- XXX or ^^^
            let newP = if offset < 0x80 then oldP+i16 offset else oldP+i16 offset-0x100
            clock += if newP .&. 0xff00 == oldP .&. 0xff00 then 3 else 4
            regs . pc .= newP
        else do
            debugStrLn "Not taking branch"
            clock += 2
            regs . pc .= oldP

ins_set :: Lens' Registers Bool -> Bool -> StateT State6502 IO ()
ins_set flag value = do
    p0 <- use (regs . pc)
    regs . flag .= value
    regs . pc .= p0+1
    clock += 2

ins_nop :: StateT State6502 IO ()
ins_nop = do
    regs . pc += 1
    clock += 2

{-# INLINE ins_jmp #-}
ins_jmp :: Word8 -> StateT State6502 IO ()
ins_jmp _ = do
    p0 <- use (regs . pc)
    addr <- read16 (p0+1)
    regs . pc .= addr
    clock += 3

{-# INLINE ins_jmp_indirect #-}
ins_jmp_indirect :: Word8 -> StateT State6502 IO ()
ins_jmp_indirect _ = do
    p0 <- use (regs . pc)
    addrAddr <- read16 (p0+1)
    addr <- read16 addrAddr
    regs . pc .= addr
    clock += 5

-- Need to separate R/W/RW XXX
withData01 :: Word8 -> Bool -> Bool ->
              (Word8 -> StateT State6502 IO Word8) ->
              StateT State6502 IO ()
withData01 bbb write useY op = case bbb of
    -- immediate
    0b000 -> do
        p0 <- use (regs . pc)
        src <- readMemory (fromIntegral (p0+1))
        regs . pc .= p0+2
        op src
        if write
            then error "Can't write immediate"
            else clock += 2
    -- zero page
    0b001 -> do
        p0 <- use (regs . pc)
        addr <- readMemory (fromIntegral (p0+1))
        src <- readMemory (fromIntegral addr)
        regs . pc .= p0+2
        dst <- op src
        if write
            then do
                writeMemory (fromIntegral addr) dst
                clock += 5
            else
                clock += 3
    -- accumulator -- XXX
    0b010 -> do
        p0 <- use (regs . pc)
        src <- use (regs . a)
        regs . pc .= p0+1
        dst <- op src
        regs . a .= dst
        if write
            then clock += 2
            else error "Must write back to A"
    -- absolute
    0b011 -> do
        p0 <- use (regs . pc)
        lo <- readMemory (fromIntegral (p0+1))
        hi <- readMemory (fromIntegral (p0+2))
        let addr = (i16 hi `shift` 8)+i16 lo
        src <- readMemory (fromIntegral addr)
        regs . pc .= p0+3
        dst <- op src
        if write
            then do
                writeMemory (fromIntegral addr) dst
                clock += 6
            else clock += 4
    -- zero page, X
    0b101 -> do
        p0 <- use (regs . pc)
        offsetX <- if useY then use (regs . y) else use (regs . x)
        zpAddr <- readMemory (fromIntegral (p0+1))
        let addr = zpAddr+offsetX :: Word8
        src <- readMemory (fromIntegral addr)
        regs . pc .= p0+2
        dst <- op src
        if write
            then do
                writeMemory (fromIntegral addr) dst
                clock += 6
            else clock += 4
    -- absolute, X
    0b111 -> do
        p0 <- use (regs . pc)
        offsetX <- if useY then use (regs . y) else use (regs . x)
        baseAddr <- read16 (fromIntegral (p0+1))
        let addr = baseAddr+fromIntegral offsetX :: Word16
        let carry = (addr .&. 0xff00) /= (baseAddr .&. 0xff00)
        src <- readMemory (fromIntegral addr)
        regs . pc .= p0+3
        dst <- op src
        if write
            then do
                writeMemory (fromIntegral addr) dst
                clock += 7
            else
                clock += if carry then 5 else 4

    otherwise -> error "Unknown addressing mode"

{-# INLINE ins_ora #-}
ins_ora :: Word8 -> StateT State6502 IO ()
ins_ora bbb = do
    src <- getData bbb
    oldA <- use (regs . a)
    let newA = oldA .|. src
    regs . a .= newA
    regs . flagS .= (newA .&. 0x80 > 0)
    regs . flagZ .= (newA == 0)
    debugStrLn $ "A = " ++ show newA

{-# INLINE ins_and #-}
ins_and :: Word8 -> StateT State6502 IO ()
ins_and bbb = do
    src <- getData bbb
    oldA <- use (regs . a)
    let newA = oldA .&. src
    regs . a .= newA
    regs . flagS .= (newA .&. 0x80 > 0)
    regs . flagZ .= (newA == 0)
    debugStrLn $ "A = " ++ show newA

{-# INLINE ins_xor #-}
ins_xor :: Word8 -> StateT State6502 IO ()
ins_xor bbb = do
    src <- getData bbb
    oldA <- use (regs . a)
    let newA = oldA `xor` src
    regs . a .= newA
    regs . flagS .= (newA .&. 0x80 > 0)
    regs . flagZ .= (newA == 0)
    debugStrLn $ "A = " ++ show newA

{-# INLINE ins_lda #-}
ins_lda :: Word8 -> StateT State6502 IO ()
ins_lda bbb = do
    debugStrLn $ "LDA instruction with address mode " ++ showHex bbb ""
    newA <- getData bbb
    regs . a .= newA
    regs . flagS .= (newA .&. 0x80 > 0)
    regs . flagZ .= (newA == 0)
    debugStrLn $ "A = " ++ show newA

{-# INLINE ins_sta #-}
ins_sta :: Word8 -> StateT State6502 IO ()
ins_sta bbb = do
    oldA <- use (regs . a)
    putData bbb oldA

{-# INLINE ins_adc #-}
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
    debugStrLn $ "A = " ++ show newA

{-# INLINE ins_sbc #-}
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
    debugStrLn $ "A = " ++ show newA

ins_cmp :: Word8 -> StateT State6502 IO ()
ins_cmp bbb = do
    src <- getData bbb
    oldA <- use (regs . a)
    let new = i16 oldA-i16 src
    debugStrLn $ "Comparing " ++ showHex oldA "" ++ " to " ++ showHex src ""
    regs . flagS .= (new .&. 0x80 > 0)
    regs . flagC .= (new < 0x100)
    regs . flagZ .= (new .&. 0xff == 0)

{-# INLINE ins_asl #-}
ins_asl :: Word8 -> StateT State6502 IO ()
ins_asl bbb = withData01 bbb True False $ \src -> do
    debugStrLn $ "into asl = " ++ show src
    regs . flagC .= (src .&. 0x80 > 0)
    let new = src `shift` 1
    regs . flagS .= (new .&. 0x80 > 0)
    regs . flagZ .= (new == 0)
    debugStrLn $ "out of asl = " ++ show new
    return new

{-# INLINE ins_rol #-}
ins_rol :: Word8 -> StateT State6502 IO ()
ins_rol bbb = withData01 bbb True False $ \src -> do
    debugStrLn $ "into rol = " ++ show src
    fc <- use (regs . flagC)
    regs . flagC .= (src .&. 0x80 > 0)
    let new = (src `shift` 1) + if fc then 1 else 0
    regs . flagS .= (new .&. 0x80 > 0)
    regs . flagZ .= (new == 0)
    debugStrLn $ "out of rol = " ++ show new
    return new

{-# INLINE ins_lsr #-}
ins_lsr :: Word8 -> StateT State6502 IO ()
ins_lsr bbb = withData01 bbb True False $ \src -> do
    regs . flagC .= (src .&. 0x01 > 0)
    let new = src `shift` (-1)
    regs . flagS .= False
    regs . flagZ .= (new == 0)
    return new

{-# INLINE ins_ror #-}
ins_ror :: Word8 -> StateT State6502 IO ()
ins_ror bbb = withData01 bbb True False $ \src -> do
    fc <- use (regs . flagC)
    regs . flagC .= (src .&. 0x01 > 0)
    let new = (src `shift` (-1))+if fc then 0x80 else 0x00
    regs . flagS .= (new .&. 0x80 > 0)
    regs . flagZ .= (new == 0)
    return new

{-# INLINE ins_stx #-}
ins_stx :: Word8 -> StateT State6502 IO ()
ins_stx bbb = withData01 bbb True True $ \src -> do
    new <- use (regs . x)
    return new

{-# INLINE ins_ldx #-}
ins_ldx :: Word8 -> StateT State6502 IO ()
ins_ldx bbb = withData01 bbb False True $ \src -> do
    regs . x .= src
    return 0 -- Unused, I hope

{-# INLINE ins_dec #-}
ins_dec :: Word8 -> StateT State6502 IO ()
ins_dec bbb = withData01 bbb True False $ \src -> do
    let new = src-1
    regs . flagS .= (new .&. 0x80 > 0)
    regs . flagZ .= (new == 0)
    return new

{-# INLINE ins_inc #-}
ins_inc :: Word8 -> StateT State6502 IO ()
ins_inc bbb = withData01 bbb True False $ \src -> do
    let new = src+1
    regs . flagS .= (new .&. 0x80 > 0)
    regs . flagZ .= (new == 0)
    return new

{-# INLINE ins_bit #-}
ins_bit :: Word8 -> StateT State6502 IO ()
ins_bit bbb = withData01 bbb False False $ \src -> do
    regs . flagS .= (src .&. 0x80 > 0)
    regs . flagV .= (src .&. 0x40 > 0)
    regs . flagZ .= (src == 0)
    return 0 -- unused

{-# INLINE ins_sty #-}
ins_sty :: Word8 -> StateT State6502 IO ()
ins_sty bbb = withData01 bbb True False $ \src -> do
    new <- use (regs . y)
    return new

{-# INLINE ins_ldy #-}
ins_ldy :: Word8 -> StateT State6502 IO ()
ins_ldy bbb = withData01 bbb False False $ \src -> do
    regs . y .= src
    return 0 -- Unused, I hope

{-# INLINE ins_cpx #-}
ins_cpx :: Word8 -> StateT State6502 IO ()
ins_cpx bbb = withData01 bbb False False $ \src -> do
    rx <- use (regs . x)
    let new = i16 rx-i16 src
    regs . flagS .= (src .&. 0x80 > 0)
    regs . flagC .= (new < 0x100)
    regs . flagZ .= (new .&. 0xff == 0)
    return 0 -- unused

{-# INLINE ins_cpy #-}
ins_cpy :: Word8 -> StateT State6502 IO ()
ins_cpy bbb = withData01 bbb False False $ \src -> do
    ry <- use (regs . y)
    let new = i16 ry-i16 src
    regs . flagS .= (src .&. 0x80 > 0)
    regs . flagC .= (new < 0x100)
    regs . flagZ .= (new .&. 0xff == 0)
    return 0 -- unused

ins_transfer :: Lens' Registers Word8 -> Lens' Registers Word8 ->
                StateT State6502 IO ()
ins_transfer vsrc vdst = do
    v0 <- use (regs . vsrc)
    regs . vdst .= v0
    regs . pc += 1
    clock += 2

ins_incr :: Lens' Registers Word8 -> StateT State6502 IO ()
ins_incr v = do
    v0 <- use (regs . v)
    let v1 = v0+1

    regs . flagS .= (v1 .&. 0x80 > 0)
    regs . flagZ .= (v1 == 0)
    
    regs . v .= v1
    regs . pc += 1
    clock += 2

ins_decr :: Lens' Registers Word8 -> StateT State6502 IO ()
ins_decr v = do
    v0 <- use (regs . v)
    let v1 = v0-1

    regs . flagS .= (v1 .&. 0x80 > 0)
    regs . flagZ .= (v1 == 0)
    
    regs . v .= v1
    regs . pc += 1
    clock += 2

ins_brk :: StateT State6502 IO ()
ins_brk = do
    regs . pc += 2
    regs . flagB .= True
    nmi True

irq :: StateT State6502 IO ()
irq = do
    fi <- use (regs . flagI)
    if not fi
        then nmi False
        else return ()

push :: Word8 -> StateT State6502 IO ()
push v = do
    sp <- use (regs . s)
    writeMemory (0x100+fromIntegral sp) v
    regs . s -= 1

pull :: StateT State6502 IO Word8
pull = do
    regs . s += 1
    sp <- use (regs . s)
    readMemory (0x100+fromIntegral sp)

ins_push :: Lens' Registers Word8 -> StateT State6502 IO ()
ins_push v = do
    v0 <- use (regs . v)
    push v0
    regs . pc += 1
    clock += 3

ins_pull :: Lens' Registers Word8 -> StateT State6502 IO ()
ins_pull v = do
    v0 <- pull
    regs . v .= v0
    regs . pc += 1
    clock += 4

nmi :: Bool -> StateT State6502 IO ()
nmi sw = do
    p0 <- use (regs . pc)
    push (i8 (p0 `shift` (-8)))
    push (i8 (p0 .&. 0xff))
    regs . flagB .= sw
    s0 <- use (regs . s)
    push s0
    addr <- read16 0xfffe
    regs . pc .= addr
    clock += 7

ins_rti :: StateT State6502 IO ()
ins_rti = do
    s0 <- pull
    regs . s .= s0 -- clear B
    lo <- pull
    hi <- pull
    regs . pc .= i16 lo+(i16 hi `shift` 8)
    clock += 6

ins_jsr :: StateT State6502 IO ()
ins_jsr = do
    p0 <- use (regs . pc)
    addr <- read16 (fromIntegral (p0+1))
    let p2 = p0+2
    push (i8 (p2 `shift` (-8)))
    push (i8 (p2 .&. 0xff))
    regs . pc .= addr
    clock += 6

ins_rts :: StateT State6502 IO ()
ins_rts = do
    lo <- pull
    hi <- pull
    let addr = i16 lo+(i16 hi `shift` 8)+1
    regs . pc .= addr
    clock += 6

step :: StateT State6502 IO ()
step = do
    debugStrLn "------"
    dumpState
    p0 <- use (regs . pc)
    debugStrLn $ "pc = " ++ showHex p0 ""
    i <- readMemory (fromIntegral p0)
    debugStrLn $ "instruction = " ++ showHex i ""
    case i of
        0x00 -> ins_brk
        0x08 -> ins_push p
        0x10 -> ins_bra flagS False
        0x18 -> ins_set flagC False
        0x20 -> ins_jsr
        0x28 -> ins_pull p
        0x30 -> ins_bra flagS True
        0x38 -> ins_set flagC True
        0x40 -> ins_rti
        0x48 -> ins_push a
        0x50 -> ins_bra flagV False
        0x58 -> ins_set flagI False
        0x60 -> ins_rts
        0x68 -> ins_pull a
        0x70 -> ins_bra flagV True
        0x78 -> ins_set flagI True
        0x88 -> ins_decr y
        0x8a -> ins_transfer x a
        0x90 -> ins_bra flagC False
        0x98 -> ins_transfer y a
        0x9a -> ins_transfer x s
        0xa8 -> ins_transfer a y
        0xaa -> ins_transfer a x
        0xb0 -> ins_bra flagC True
        0xb8 -> ins_set flagV False
        0xba -> ins_transfer s x
        0xc8 -> ins_incr y
        0xca -> ins_decr x
        0xd0 -> ins_bra flagZ False
        0xd8 -> ins_set flagD False
        0xe8 -> ins_incr x
        0xea -> ins_nop
        0xf0 -> ins_bra flagZ True
        0xf8 -> ins_set flagD True

        otherwise -> do
            let cc = i .&. 0b11
            debugStrLn $ "cc = " ++ show cc
            case cc of
                0b00 -> do
                    debugStrLn "cc=0b00 instruction"
                    let aaa = (i `shift` (-5)) .&. 0b111
                    let bbb = (i `shift` (-2)) .&. 0b111
                    case aaa of
                        0b001 -> ins_bit bbb
                        0b010 -> ins_jmp bbb
                        0b011 -> ins_jmp_indirect bbb
                        0b100 -> ins_sty bbb
                        0b101 -> ins_ldy bbb
                        0b110 -> ins_cpy bbb
                        0b111 -> ins_cpx bbb

                0b01 -> do
                    debugStrLn "0b01 instruction"
                    let aaa = (i `shift` (-5)) .&. 0b111
                    let bbb = (i `shift` (-2)) .&. 0b111
                    case aaa of

                        0b000 -> ins_ora bbb
                        0b001 -> ins_and bbb
                        0b010 -> ins_xor bbb
                        0b011 -> ins_adc bbb
                        0b100 -> ins_sta bbb
                        0b101 -> ins_lda bbb
                        0b110 -> ins_cmp bbb
                        0b111 -> ins_sbc bbb

                        otherwise -> error "Unknown aaa"
                0b10 -> do
                    debugStrLn "0b10 instruction"
                    let aaa = (i `shift` (-5)) .&. 0b111
                    let bbb = (i `shift` (-2)) .&. 0b111
                    case aaa of

                        0b000 -> ins_asl bbb
                        0b001 -> ins_rol bbb
                        0b010 -> ins_lsr bbb
                        0b011 -> ins_ror bbb
                        0b100 -> ins_stx bbb
                        0b101 -> ins_ldx bbb
                        0b110 -> ins_dec bbb
                        0b111 -> ins_inc bbb

                otherwise -> error "Unknown instruction class (cc)"
    dumpState
    return ()


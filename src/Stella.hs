{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BinaryLiterals #-}

module Stella where

import Core
import Binary
import Foreign.C.Types
import Data.Int
import Foreign.Ptr
import Foreign.Storable
import Control.Applicative
import Data.Monoid
import Data.Word
import System.Random
import Control.Monad
import Control.Monad.State
import Control.Lens
import SDL.Video.Renderer
import TIAColors
import Data.Array.Unboxed
import Numeric
import Data.Array.IO
import Data.Bits
import Data.Bits.Lens

data Stella = Stella {
    _stellaClock :: !Int64,
    _hpos :: !CInt,
    _vpos :: !CInt,
    _tvSurface :: !Surface,
    _vblank :: !Word8,
    _vsync :: !Word8,
    _wsync :: !Word8,
    _colubk :: !Word8,
    _colupf :: !Word8,
    _pf0 :: !Word8,
    _pf1 :: !Word8,
    _pf2 :: !Word8,
    _ctrlpf :: !Word8,
    _colup0 :: ! Word8,
    _colup1 :: !Word8,
    _ppos0 :: !CInt,
    _ppos1 :: !CInt,
    _grp0 :: !Word8,
    _grp1 :: !Word8,
    _swcha :: !Word8,
    _swchb :: !Word8,
    _enam0 :: !Word8,
    _enam1 :: !Word8,
    _hmp0 :: !Word8,
    _hmp1 :: !Word8,
    _nusiz0 :: !Word8,
    _nusiz1 :: !Word8,
    _inpt4 :: !Word8,
    _cxm0p :: !Word8,
    _cxm1p :: !Word8,
    _cxp0fb :: !Word8,
    _cxp1fb :: !Word8,
    _cxm0fb :: !Word8,
    _cxm1fb :: !Word8,
    _cxblpf :: !Word8,
    _cxppmm :: !Word8,
    _enabl :: !Word8,
    _mpos0 :: !CInt,
    _mpos1 :: !CInt,
    _bpos :: !CInt,
    _resmp0 :: !Word8,
    _resmp1 :: !Word8,
    _resbl :: !Word8,
    _hmm0 :: !Word8,
    _hmm1 :: !Word8,
    _hmbl :: !Word8,
    _inpt5 :: !Word8,
    _intim :: !Word8,
    _subtimer :: !CInt,
    _interval :: !CInt
}

$(makeLenses ''Stella)

{-# INLINABLE playfield #-}
playfield :: (MonadIO m, MonadState Stella m) => Int -> m Bool
playfield i | i >= 0 && i < 4 = do
                pf0' <- use pf0
                return $ (pf0' `shift` (i-4)) .&. 1 > 0
            | i >=4 && i < 12 = do
                pf1' <- use pf1
                return $ (pf1' `shift` (i-11)) .&. 1 > 0
            | i >= 12 && i < 20 = do
                pf2' <- use pf2
                return $ (pf2' `shift` (12-i)) .&. 1 > 0
playfield i | i >= 20 && i < 40 = do
                ctrlpf' <- use ctrlpf
                playfield $ if ctrlpf' .&. 0b00000001 > 0 then 39-i else i-20

{-# INLINABLE stretchPlayer #-}
stretchPlayer :: Word8 -> CInt -> Word8 -> Bool
stretchPlayer sizeCopies o grp0' =
    case sizeCopies of
        0b000 -> -- one copy
            if o >= 0 && o < 8
                then (grp0' `shift` (fromIntegral o-7)) .&. 1 /= 0
                else False
        0b001 -> -- two copies close
            if o >= 0 && o < 8 || o >= 16 && o < 24
                then (grp0' `shift` (fromIntegral (o .&. 7)-7)) .&. 1 /= 0
                else False
        0b010 -> -- two copies - med
            if o >= 0 && o < 8 || o >= 32 && o < 40
                then (grp0' `shift` (fromIntegral (o .&. 7)-7)) .&. 1 /= 0
                else False
        0b011 -> -- three copies close
            if o >= 0 && o < 8 || o >= 16 && o < 24 || o >= 32 && o < 40
                then (grp0' `shift` (fromIntegral (o .&. 7)-7)) .&. 1 /= 0
                else False
        0b100 -> -- two copies wide
            if o >= 0 && o < 8 || o >= 64 && o < 72
                then (grp0' `shift` (fromIntegral (o .&. 7)-7)) .&. 1 /= 0
                else False
        0b101 -> -- double size player
            if o >= 0 && o < 16
                then (grp0' `shift` ((fromIntegral o `shift` (-1))-7)) .&. 1 /= 0
                else False
        0b110 -> -- three copies medium
            if o >= 0 && o < 8 || o >= 32 && o < 40 || o >= 64 && o < 72
                then (grp0' `shift` (fromIntegral (o .&. 7)-7)) .&. 1 /= 0
                else False
        0b111 -> -- quad sized player
            if o >= 0 && o < 32
                then (grp0' `shift` ((fromIntegral o `shift` (-2))-7)) .&. 1 /= 0
                else False

-- Stella programmer's guide p.40
{-# INLINABLE player0 #-}
player0 :: (MonadIO m, MonadState Stella m) => m Bool
player0 = do
    hpos' <- use hpos
    ppos0' <- use ppos0
    let o = hpos'-ppos0' :: CInt
    sizeCopies <- (0b111 .&.) <$> use nusiz0
    grp0' <- use grp0
    return $ stretchPlayer sizeCopies o grp0'

{-# INLINABLE player1 #-}
player1 :: (MonadIO m, MonadState Stella m) => m Bool
player1 = do
    hpos' <- use hpos
    ppos1' <- use ppos1
    let o = hpos'-ppos1' :: CInt
    sizeCopies <- (0b111 .&.) <$> use nusiz0
    grp1' <- use grp1
    return $ stretchPlayer sizeCopies o grp1'

-- Stella programmer's guide p.22
{-# INLINABLE missile0 #-}
missile0 :: (MonadIO m, MonadState Stella m) => m Bool
missile0 = do
    enabled <- use (enam0 . bitAt 1)
    if enabled
        then do
            hpos' <- use hpos
            mpos1' <- use mpos1
            let o = hpos'-mpos1' :: CInt
            nusiz0' <- use nusiz0
            let missileSize = 1 `shift` (fromIntegral ((nusiz0' `shift` (fromIntegral $ -4)) .&. 0b11))
            return $ o >= 0 && o < missileSize
        else return False

{-# INLINABLE missile1 #-}
missile1 :: (MonadIO m, MonadState Stella m) => m Bool
missile1 = do
    enabled <- use (enam1 . bitAt 1)
    if enabled
        then do
            hpos' <- use hpos
            mpos1' <- use mpos1
            let o = hpos'-mpos1' :: CInt
            nusiz1' <- use nusiz1
            let missileSize = 1 `shift` (fromIntegral ((nusiz1' `shift` (fromIntegral $ -4)) .&. 0b11))
            return $ o >= 0 && o < missileSize
        else return False

{-# INLINABLE ball #-}
ball :: (MonadIO m, MonadState Stella m) => m Bool
ball = do
    enabled <- use (enabl . bitAt 1)
    if enabled
        then do
            o <- (-) <$> use hpos <*> use bpos
            ctrlpf' <- use ctrlpf
            let ballSize = 1 `shift` (fromIntegral ((ctrlpf' `shift` (fromIntegral $ -4)) .&. 0b11))
            return $ o >= 0 && o < ballSize
        else return False

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (160, 192)

{-# INLINE clockMove #-}
clockMove :: Word8 -> CInt
clockMove i = fromIntegral ((fromIntegral i :: Int8) `shift` (-4))

{-# INLINE stellaHmclr #-}
stellaHmclr :: (MonadIO m, MonadState Stella m) => m ()
stellaHmclr = do
    hmp0 .= 0
    hmp1 .= 0
    hmm0 .= 0
    hmm1 .= 0
    hmbl .= 0

{-# INLINE stellaCxclr #-}
stellaCxclr :: (MonadIO m, MonadState Stella m) => m ()
stellaCxclr = do
    cxm0fb .= 0
    cxm1fb .= 0
    cxp0fb .= 0
    cxp1fb .= 0
    cxm0fb .= 0
    cxm1fb .= 0
    cxblpf .= 0
    cxppmm .= 0

{-# INLINE stellaHmove #-}
stellaHmove :: (MonadIO m, MonadState Stella m) => m ()
stellaHmove = do
    poffset0 <- use hmp0
    ppos0 -= clockMove poffset0
    poffset1 <- use hmp1
    ppos1 -= clockMove poffset1
    moffset0 <- use hmm0
    mpos0 -= clockMove moffset0
    moffset1 <- use hmm1
    mpos1 -= clockMove moffset1
    boffset <- use hmbl
    bpos -= clockMove boffset

{-# INLINE stellaResmp0 #-}
stellaResmp0 :: (MonadIO m, MonadState Stella m) => m ()
stellaResmp0 = use ppos0 >>= (mpos0 .=) -- XXX

{-# INLINE stellaResmp1 #-}
stellaResmp1 :: (MonadIO m, MonadState Stella m) => m ()
stellaResmp1 = use ppos1 >>= (mpos1 .=) -- XXX

{-# INLINE stellaWsync #-}
stellaWsync :: (MonadIO m, MonadState Stella m) => m ()
stellaWsync = do
    hpos' <- use hpos
    stellaTick (228-fromIntegral hpos')

{-# INLINE stellaVsync #-}
stellaVsync :: (MonadIO m, MonadState Stella m) => Word8 -> m ()
stellaVsync v = do
    return ()

{-# INLINE stellaVblank #-}
stellaVblank :: (MonadIO m, MonadState Stella m) => Word8 -> m ()
stellaVblank v = do
    vold <- use vblank
    -- Set latches for INPT4 and INPT5
    when (vold .&. 0b01000000 /= 0) $ do
        inpt4 . bitAt 7 .= True
        inpt5 . bitAt 7 .= True
    --liftIO $ putStrLn $ show vold ++ " -> " ++ show v
    if (vold .&. 0b00000010 /= 0) && (v .&. 0b00000010 == 0)
        then do
            --liftIO $ print "VBLANK"
            hpos .= 0
            vpos .= 0
        else return ()
    vblank .= v

-- player0

picy :: CInt
picy = 40
picx :: CInt
picx = 68

data Pixel = Pixel { plogic :: !Bool, pcolor :: !Word8 }

instance Monoid Pixel where
    {-# INLINE mappend #-}
    mempty = Pixel False 0
    _ `mappend` pixel@(Pixel True _) = pixel
    pixel `mappend` (Pixel False _) = pixel

{-# INLINABLE compositeAndCollide #-}
compositeAndCollide :: (MonadIO m, MonadState Stella m) => CInt -> m Word8
compositeAndCollide x = do
    -- Assemble colours
    pbackground <- Pixel True <$> use colubk
    pplayfield <- Pixel <$> playfield (fromIntegral $ x `shift` (-2)) <*> use colupf
    pplayer0 <- Pixel <$> player0 <*> use colup0
    pplayer1 <- Pixel <$> player1 <*> use colup1
    pmissile0 <- Pixel <$> missile0 <*> use colup0
    pmissile1 <- Pixel <$> missile1 <*> use colup1
    pball <- Pixel <$> ball <*> use colupf

    cxm0p . bitAt 7 ||= (plogic pmissile0 && plogic pplayer1)
    cxm0p . bitAt 6 ||= (plogic pmissile0 && plogic pplayer0)
    cxm1p . bitAt 7 ||= (plogic pmissile1 && plogic pplayer0)
    cxm1p . bitAt 6 ||= (plogic pmissile1 && plogic pplayer1)
    cxp0fb . bitAt 7 ||= (plogic pplayer0 && plogic pplayfield)
    cxp0fb . bitAt 6 ||= (plogic pplayer0 && plogic pball)
    cxp1fb . bitAt 7 ||= (plogic pplayer1 && plogic pplayfield)
    cxp1fb . bitAt 6 ||= (plogic pplayer1 && plogic pball)
    cxm0fb . bitAt 7 ||= (plogic pmissile0 && plogic pplayfield)
    cxm0fb . bitAt 6 ||= (plogic pmissile0 && plogic pball)
    cxm1fb . bitAt 7 ||= (plogic pmissile1 && plogic pplayfield)
    cxm1fb . bitAt 6 ||= (plogic pmissile1 && plogic pball)
    cxblpf . bitAt 7 ||= (plogic pball && plogic pplayfield)
    cxppmm . bitAt 7 ||= (plogic pplayer0 && plogic pplayer1)
    cxppmm . bitAt 6 ||= (plogic pmissile0 && plogic pmissile1)
    
    -- Get ordering priority
    ctrlpf' <- use ctrlpf
    let Pixel _ final = pbackground `mappend`
                        if ctrlpf' .&. 0b00000100 /= 0
                            then mconcat [pplayer1, pmissile1, pplayer0, pmissile0, pplayfield, pball]
                            else mconcat [pball, pplayfield, pplayer1, pmissile1, pplayer0, pmissile0]
    return final

stellaTick :: (MonadIO m, MonadState Stella m) => Int -> m ()
stellaTick 0 = return ()
stellaTick n = do
    -- Interval timer
    stellaClock += 1
    subtimer' <- use subtimer
    let subtimer'' = subtimer'-1
    subtimer .= subtimer''
    when (subtimer' == 0) $ do
        interval' <- use interval
        subtimer .= 3*interval'-1
        intim' <- use intim
        let intim'' = intim'-1
        intim .= intim''
        when (intim' == 0) $ do
            subtimer .= 3*1-1
            interval .= 1
    
    -- Display
    hpos' <- use hpos
    vpos' <- use vpos
    when (vpos' >= picy && vpos' < picy+192 && hpos' >= picx) $ do
        surface <- use tvSurface
        ptr <- liftIO $ surfacePixels surface
        let ptr' = castPtr ptr :: Ptr Word32
        let x = hpos'-picx
        let y = vpos'-picy
        let i = screenWidth*y+x

        final <- compositeAndCollide x

        liftIO $ pokeElemOff ptr' (fromIntegral i) (lut!(final `shift` (-1)))
    hpos += 1
    hpos' <- use hpos
    when (hpos' >= picx+160) $ do
        hpos .= 0
        vpos += 1
        vpos' <- use vpos
        when (vpos' >= picy+192) $ vpos .= 0
    stellaTick (n-1)

renderFrame :: (MonadIO m, MonadState Stella m) => m ()
renderFrame = do
    surface <- use tvSurface
    ptr <- liftIO $ surfacePixels surface
    let ptr' = castPtr ptr :: Ptr Word32
    liftIO $ lockSurface surface
    forM_ [0..screenHeight-1] $ \row -> do
        forM_ [0..screenWidth-1] $ \col -> do
            let i = screenWidth*row+col
            liftIO $ pokeElemOff ptr' (fromIntegral i) (fromIntegral col)
            return ()
    liftIO $ unlockSurface surface

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

data StateAtari = S {
    _mem :: IOUArray Int Word8,
    _clock :: !Int,
    _regs :: !Registers,
    _debug :: !Bool,
    _stella :: Stella
}

makeLenses ''StateAtari

newtype MonadAtari a = M { unM :: StateT StateAtari IO a }
    deriving (Functor, Applicative, Monad, MonadState StateAtari, MonadIO)

{-# SPECIALIZE player0 :: Int -> StateT Stella IO Bool #-}
{-# SPECIALIZE player1 :: Int -> StateT Stella IO Bool #-}
{-# SPECIALIZE stellaWsync :: StateT Stella IO () #-}
{-# SPECIALIZE stellaVsync :: Word8 -> StateT Stella IO () #-}
{-# SPECIALIZE stellaVblank :: Word8 -> StateT Stella IO () #-}
{-# SPECIALIZE stellaTick :: Int -> StateT Stella IO () #-}

vsync_addr :: Word16
vsync_addr = 0x00
vblank_addr :: Word16
vblank_addr = 0x01
wsync_addr :: Word16
wsync_addr = 0x02
colubk_addr :: Word16
colubk_addr = 0x09
pf0_addr :: Word16
pf0_addr = 0x0d
pf1_addr :: Word16
pf1_addr = 0x0e
pf2_addr :: Word16
pf2_addr = 0x0f

--  XXX Do this! If reset occurs during horizontal blank, the object will appear at the left side of the television screen

{-# INLINE usingStella #-}
usingStella :: MonadState StateAtari m =>
               StateT Stella m a -> m a
usingStella m = do
    stella' <- use stella
    (a, stella'') <- flip runStateT stella' m
    stella .= stella''
    return a

{-# INLINE writeStella #-}
writeStella :: (MonadIO m, MonadState Stella m) =>
               Word16 -> Word8 -> m ()
writeStella addr v = 
    case addr of
       0x00 -> stellaVsync v             -- VSYNC
       0x01 -> stellaVblank v            -- VBLANK
       0x02 -> stellaWsync               -- WSYNC
       0x04 -> nusiz0 .= v               -- NUSIZ0
       0x05 -> nusiz1 .= v               -- NUSIZ1
       0x06 -> colup0 .= v               -- COLUP0
       0x07 -> colup1 .= v               -- COLUP1
       0x08 -> colupf .= v               -- COLUPF
       0x09 -> colubk .= v               -- COLUBK
       0x0a -> ctrlpf .= v               -- COLUPF
       0x0d -> pf0 .= v                  -- PF0
       0x0e -> pf1 .= v                  -- PF1
       0x0f -> pf2 .= v                  -- PF2
       0x10 -> use hpos >>= (ppos0 .=)   -- RESP0
       0x11 -> use hpos >>= (ppos1 .=)   -- RESP1
       0x12 -> use hpos >>= (mpos0 .=)   -- RESM0
       0x13 -> use hpos >>= (mpos1 .=)   -- RESM1
       0x14 -> use hpos >>= (bpos .=)    -- RESBL
       0x1b -> grp0 .= v                 -- GRP0
       0x1c -> grp1 .= v                 -- GRP1
       0x1d -> (liftIO $ putStrLn $ "ENAM0="++showHex v "") >> enam0 .= v                -- ENAM0
       0x1e -> (liftIO $ putStrLn $ "ENAM1="++showHex v "") >> enam1 .= v                -- ENAM1
       0x1f -> (liftIO $ putStrLn $ "ENABL="++showHex v "") >> enabl .= v                -- ENABL
       0x20 -> hmp0 .= v                 -- HMP0
       0x21 -> hmp1 .= v                 -- HMP1
       0x22 -> hmm0 .= v                 -- HMM0
       0x23 -> hmm1 .= v                 -- HMM1
       0x24 -> hmbl .= v                 -- HMBL
       0x28 -> do
        resmp0' <- use (resmp0 . bitAt 1)
        when (resmp0') $ use ppos0 >>= (mpos0 .=)  -- RESMP0
       0x29 -> do
        resmp1' <- use (resmp1 . bitAt 1)
        when (resmp1') $ use ppos1 >>= (mpos1 .=)  -- RESMP1
       0x2a -> stellaHmove               -- HMOVE
       0x2b -> stellaHmclr               -- HMCLR
       0x2c -> stellaCxclr               -- CXCLR
       0x294 -> do                       -- TIM1T
        interval .= 1
        subtimer .= 1*3-1
        intim .= v
       0x295 -> do                       -- TIM8T
        interval .= 8
        subtimer .= 8*3-1
        intim .= v
       0x296 -> do                       -- TIM64T
        interval .= 64
        subtimer .= 64*3-1
        intim .= v
       0x297 -> do                       -- TIM1024T
        interval .= 1024
        subtimer .= 1024*3-1
        intim .= v
       otherwise -> return ()
        --liftIO $ putStrLn $ "writing TIA 0x" ++ showHex addr ""

{-# INLINE readStella #-}
readStella :: (MonadIO m, MonadState Stella m) =>
              Word16 -> m Word8
readStella addr = 
    case addr of
        0x00 -> use cxm0p
        0x01 -> use cxm1p
        0x02 -> use cxp0fb
        0x03 -> use cxp1fb
        0x04 -> use cxm0fb
        0x05 -> use cxm1fb
        0x06 -> use cxblpf
        0x07 -> use cxppmm
        0x0c -> use inpt4
        0x10 -> use cxm0p
        0x11 -> use cxm1p
        0x12 -> use cxp0fb
        0x13 -> use cxp1fb
        0x14 -> use cxm0fb
        0x15 -> use cxm1fb
        0x16 -> use cxblpf
        0x17 -> use cxppmm
        0x1c -> use inpt4
        0x20 -> use cxm0p
        0x21 -> use cxm1p
        0x22 -> use cxp0fb
        0x23 -> use cxp1fb
        0x24 -> use cxm0fb
        0x25 -> use cxm1fb
        0x26 -> use cxblpf
        0x27 -> use cxppmm
        0x2c -> use inpt4
        0x30 -> use cxm0p
        0x31 -> use cxm1p
        0x32 -> use cxp0fb
        0x33 -> use cxp1fb
        0x34 -> use cxm0fb
        0x35 -> use cxm1fb
        0x36 -> use cxblpf
        0x37 -> use cxppmm
        0x3c -> use inpt4
        0x280 -> use swcha
        0x282 -> use swchb
        otherwise -> return 0

instance Emu6502 MonadAtari where
    {-# INLINE readMemory #-}
    readMemory addr =
        if addr >= 0x00 && addr < 0x80
            then usingStella $ readStella addr
            else if addr >= 0x80 && addr < 0x100 || addr >= 0x180 && addr < 0x200
                    then do
                        m <- use mem
                        liftIO $ readArray m (fromIntegral addr .&. 0xff)
                    else if addr >= 0x280 && addr < 0x298
                        then usingStella $ readStella addr
                        else if addr >= 0xf000
                            then do
                                m <- use mem
                                liftIO $ readArray m (fromIntegral addr)
                            else return 0 --do
                                --error $ "Mystery read from " ++ showHex addr ""


    {-# INLINE writeMemory #-}
    writeMemory addr v =
        if addr >= 0x00 && addr < 0x80
            then usingStella $ writeStella addr v
            else if addr >= 0x80 && addr < 0x100 || addr >= 0x180 && addr < 0x200
                    then do
                        m <- use mem
                        liftIO $ writeArray m (fromIntegral addr .&. 0xff) v
                    else if addr >= 0x280 && addr < 0x298
                            then usingStella $ writeStella addr v
                            else if addr >= 0xf000
                                then do
                                    m <- use mem
                                    liftIO $ writeArray m (fromIntegral addr) v
                                else do
                                    return ()
                                    --liftIO $ print $ "Mystery write to " ++ showHex addr ""

    {-# INLINE getPC #-}
    getPC = use (regs . pc)
    {-# INLINE tick #-}
    tick n = do
        clock += n
        usingStella $ stellaTick (3*n)
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


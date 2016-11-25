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
    _vclock :: !Int,
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
    _pos0 :: !CInt,
    _pos1 :: !CInt,
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
    _cxppmm :: !Word8
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

-- Stella programmer's guide p.40
{-# INLINABLE player0 #-}
player0 :: (MonadIO m, MonadState Stella m) => Int -> m Bool
player0 i = do
    hpos' <- use hpos
    pos0' <- use pos0
    let o = hpos'-pos0' :: CInt
    sizeCopies <- (0b111 .&.) <$> use nusiz0
    case sizeCopies of
        0b000 -> -- one copy
            if o >= 0 && o < 8
                then do
                    grp0' <- use grp0
                    return $ (grp0' `shift` (fromIntegral o-7)) .&. 1 /= 0
                else return False
        0b001 -> -- two copies close
            if o >= 0 && o < 8 || o >= 16 && o < 24
                then do
                    grp0' <- use grp0
                    return $ (grp0' `shift` (fromIntegral (o .&. 7)-7)) .&. 1 /= 0
                else return False
        0b010 -> -- two copies - med
            if o >= 0 && o < 8 || o >= 32 && o < 40
                then do
                    grp0' <- use grp0
                    return $ (grp0' `shift` (fromIntegral (o .&. 7)-7)) .&. 1 /= 0
                else return False
        0b011 -> -- three copies close
            if o >= 0 && o < 8 || o >= 16 && o < 24 || o >= 32 && o < 40
                then do
                    grp0' <- use grp0
                    return $ (grp0' `shift` (fromIntegral (o .&. 7)-7)) .&. 1 /= 0
                else return False
        0b100 -> -- two copies wide
            if o >= 0 && o < 8 || o >= 64 && o < 72
                then do
                    grp0' <- use grp0
                    return $ (grp0' `shift` (fromIntegral (o .&. 7)-7)) .&. 1 /= 0
                else return False
        0b101 -> -- double size player
            if o >= 0 && o < 16
                then do
                    grp0' <- use grp0
                    return $ (grp0' `shift` ((fromIntegral o `shift` (-1))-7)) .&. 1 /= 0
                else return False
        0b110 -> -- three copies medium
            if o >= 0 && o < 8 || o >= 32 && o < 40 || o >= 64 && o < 72
                then do
                    grp0' <- use grp0
                    return $ (grp0' `shift` (fromIntegral (o .&. 7)-7)) .&. 1 /= 0
                else return False
        0b111 -> -- quad sized player
            if o >= 0 && o < 32
                then do
                    grp0' <- use grp0
                    return $ (grp0' `shift` ((fromIntegral o `shift` (-2))-7)) .&. 1 /= 0
                else return False

{-# INLINABLE player1 #-}
player1 :: (MonadIO m, MonadState Stella m) => Int -> m Bool
player1 i = do
    hpos' <- use hpos
    pos1' <- use pos1
    let o = hpos'-pos1' :: CInt
    if o >= 0 && o < 8
        then do
            grp1' <- use grp1
            --when (o == 7) $ pos1 .= 9999
            return $ (grp1' `shift` (fromIntegral o-7)) .&. 1 /= 0
        else return False

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (160, 192)

{-# INLINE clockMove #-}
clockMove :: Word8 -> CInt
clockMove i = fromIntegral ((fromIntegral i :: Int8) `shift` (-4))

{-# INLINE stellaHmove #-}
stellaHmove :: (MonadIO m, MonadState Stella m) => m ()
stellaHmove = do
    offset <- use hmp0
    pos0 -= clockMove offset
    pos1 -= clockMove offset

{-# INLINE stellaWsync #-}
stellaWsync :: (MonadIO m, MonadState Stella m) => m ()
stellaWsync = do
    hpos' <- use hpos
    stellaIdle (228-fromIntegral hpos')

{-# INLINE stellaVsync #-}
stellaVsync :: (MonadIO m, MonadState Stella m) => Word8 -> m ()
stellaVsync v = do
    return ()

{-# INLINE stellaVblank #-}
stellaVblank :: (MonadIO m, MonadState Stella m) => Word8 -> m ()
stellaVblank v = do
    vold <- use vblank
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

data Pixel = Pixel !Bool !Word8

instance Monoid Pixel where
    mempty = Pixel False 0
    _ `mappend` pixel@(Pixel True _) = pixel
    pixel `mappend` (Pixel False _) = pixel

{-# INLINE stellaIdle #-}
stellaIdle :: (MonadIO m, MonadState Stella m) => Int -> m ()
stellaIdle 0 = return ()
stellaIdle n = do
    hpos' <- use hpos
    vpos' <- use vpos
    when (vpos' >= picy && vpos' < picy+192 && hpos' >= picx) $ do
        surface <- use tvSurface
        ptr <- liftIO $ surfacePixels surface
        let ptr' = castPtr ptr :: Ptr Word32
        let x = hpos'-picx
        let y = vpos'-picy
        let i = screenWidth*y+x

        -- Assemble colours
        pbackground <- Pixel True <$> use colubk
        pplayfield <- Pixel <$> playfield (fromIntegral $ x `shift` (-2)) <*> use colupf
        pplayer0 <- Pixel <$> player0 (fromIntegral hpos') <*> use colup0
        pplayer1 <- Pixel <$> player1 (fromIntegral hpos') <*> use colup1
        
        -- Get ordering priority
        ctrlpf' <- use ctrlpf
        let Pixel _ final = pbackground `mappend`
                            if ctrlpf' .&. 0b00000100 /= 0
                                then pplayer1 `mappend` pplayer0 `mappend` pplayfield
                                else pplayfield `mappend` pplayer1 `mappend` pplayer0

        liftIO $ pokeElemOff ptr' (fromIntegral i) (lut!(final `shift` (-1)))
    hpos += 1
    hpos' <- use hpos
    when (hpos' >= picx+160) $ do
        hpos .= 0
        vpos += 1
        vpos' <- use vpos
        when (vpos' >= picy+192) $ vpos .= 0
    stellaIdle (n-1)

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
{-# SPECIALIZE stellaIdle :: Int -> StateT Stella IO () #-}

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
       0x00 -> stellaVsync v    -- VSYNC
       0x01 -> stellaVblank v   -- VBLANK
       0x02 -> stellaWsync      -- WSYNC
       0x04 -> nusiz0 .= v      -- NUSIZ0
       0x05 -> nusiz1 .= v      -- NUSIZ1
       0x06 -> colup0 .= v      -- COLUP0
       0x07 -> colup1 .= v      -- COLUP1
       0x08 -> colupf .= v      -- COLUPF
       0x09 -> colubk .= v      -- COLUBK
       0x0a -> ctrlpf .= v      -- COLUPF
       0x0d -> pf0 .= v         -- PF0
       0x0e -> pf1 .= v         -- PF1
       0x0f -> pf2 .= v         -- PF2
       0x10 -> use hpos >>= (pos0 .=)   -- RESP0
       0x11 -> use hpos >>= (pos1 .=)   -- RESP1
       0x1b -> grp0 .= v        -- GRP0
       0x1c -> grp1 .= v        -- GRP1
       0x1d -> enam0 .= v       -- ENAM0
       0x1e -> enam1 .= v       -- ENAM1
       0x20 -> hmp0 .= v        -- HMP0
       0x21 -> hmp1 .= v        -- HMP1
       0x2a -> stellaHmove      -- HMOVE
       otherwise -> return ()
        --liftIO $ putStrLn $ "writing TIA 0x" ++ showHex addr ""

{-# INLINE readStella #-}
readStella :: (MonadIO m, MonadState Stella m) =>
              Word16 -> m Word8
readStella addr = 
    case addr of
        0x0c -> use inpt4
        0x1c -> use inpt4
        0x2c -> use inpt4
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
                            then return ()
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
        usingStella $ stellaIdle (3*n)
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


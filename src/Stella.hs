{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BinaryLiterals #-}

module Stella where

import Core
import Control.Concurrent (threadDelay)
import Binary
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import SDL.Vect
import SDL.Video.Renderer
import Data.Word
import System.Random
import Control.Monad
import Control.Monad.State
import qualified SDL
import Control.Lens
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
    -- _resp0 :: !Word8,
    -- _resp1 :: !Word8,
    _pos0 :: !CInt,
    _pos1 :: !CInt,
    _grp0 :: !Word8,
    _grp1 :: !Word8
}

$(makeLenses ''Stella)

-- 4->7 11->0
-- add reverse later XXX
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



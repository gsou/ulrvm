{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Interface.Class where

import Brick

import Control.Lens
import Control.Wire

import Data.Time.Clock

import Control.Lens
import Control.Lens.TH

import Data.Bits
import Data.Word (Word8, Word16, Word32)


import Text.Printf

newtype Id = Id Word32 deriving (Num, Eq, Ord, Show, Read)

-- | Can compatible packet
data Packet = Packet {_canId :: Id, _payload :: [Word8]}
makeLenses ''Packet

instance Show Packet where
    show (Packet (Id id) payload) = printf ((++) "0x%08X " $ unwords $ map (printf "%02X") payload) id

-- * UI configurable elements

type O = String
type W = Wire (Timed Double ()) () IO
type S = (W (BrickEvent O Packet) [Widget O], UTCTime, [Widget O])

filterRaw w = filterE (\p -> p ^. canId == w)


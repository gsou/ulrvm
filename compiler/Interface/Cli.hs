{-# LANGUAGE Arrows, ForeignFunctionInterface, TemplateHaskell, TypeApplications, OverloadedStrings #-}
module Interface.Cli where

import Brick
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.ProgressBar

import Control.Wire hiding (id, (.), Max)
import qualified Control.Wire.Unsafe.Event as WireUnsafe
import Control.Concurrent hiding (yield)
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Lens
import Control.Lens.TH

import qualified Data.Array as A
import Conduit
import qualified Data.Conduit.Combinators as CC
import Data.ByteString.Char8 (pack, unpack)
import Data.Char (chr, ord, isDigit)
import Data.Foldable
import Data.Maybe
import Data.Char (chr)
import Data.Int (Int64, Int32)
import qualified Data.Sequence as Seq
import Data.List.Split (chunksOf)
import Data.Time.Clock
import Data.Word (Word8, Word32)

import Graphics.Vty hiding (Event, (<|>))
import Graphics.Vty.Attributes
import qualified Graphics.Vty.Input as VI
import Graphics.Vty.Config

import Numeric (readHex)

import Text.Printf

import System.IO

import Interface.Class
import qualified System.Hardware.Serialport as S

-- * Helper threads

receive :: BChan Packet -> S.SerialPort -> IO ()
receive chan s = runConduit $ gen s .| makePacket .| consume
 where gen :: S.SerialPort -> ConduitT () Word8 IO ()
       gen s = forever $ yieldMany =<< (liftIO $ S.recv s 1024)
       makePacket = do
        CC.dropWhile (/= fromIntegral (ord ':'))
        await
        packet <- runMaybeC $ do
            idx <- maybeC $ readNum []
            len <- maybeC $ readNum []
            payload <- readPayload len
            pure $ Packet (Id idx) payload
        case packet of
            Just a -> yield a
            Nothing -> pure ()
        makePacket
       consume = forever $ do
         packet <- await
         case packet of
           Just p -> liftIO $ writeBChan chan p
           Nothing -> pure ()
       readNum n = do
        Just a <- await
        if isHexDigit $ chr $ fromIntegral a
            then readNum $ n ++ [chr $ fromIntegral a]
            else pure $ case readHex n of
                [(a,[])] -> Just a
                _ -> Nothing
       readPayload 0 = pure []
       readPayload len = replicateM len $ maybeC $ do
            Just a <- await
            Just b <- await
            pure $ case readHex [chr $ fromIntegral a, chr $ fromIntegral b] of 
                [(a,[])] -> Just a
                _ -> Nothing
       isHexDigit = (||) <$> isDigit <*> (`elem` ("ABCDEF"::String))

-- * Interface state

data IState = IState {
  _canMessages :: Seq.Seq Packet
                     }
makeLenses ''IState


appevent a (VtyEvent (EvKey (KChar 'c') [MCtrl])) = halt a
appevent is (AppEvent pack) = do
  continue $ is & (canMessages %~ (Seq.take 1024 . (pack Seq.:<|)))
appevent is _ = continue is

appattr _ = attrMap (white `on` black) [("ok", green `on` black), ("err", red `on` black), (progressCompleteAttr, white `on` blue)]

drawapp :: IState -> [Widget String]
drawapp is = pure $ hBox []

-- * Interface start

initInterface :: String -> S.SerialPortSettings -> IO ()
initInterface port serialSettings = do
  hSetBuffering stdout NoBuffering
  chan <- newBChan 256 :: IO (BChan Packet)
  S.withSerial port serialSettings $ \s -> do
    forkIO (receive chan s)
    time <- getCurrentTime
    customMain
      (mkVty defaultConfig)
      (Just chan)
      (App drawapp (\_ _ -> Nothing) appevent pure appattr)
      (IState Seq.empty)
  pure ()


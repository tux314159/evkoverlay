module System.Evdev.Event
  ( Event (..),
    KeyState (..),
    KeyEvent (..),
    readEvent,
    getKeyEvent,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as B
import System.Evdev.KeyCode
import System.IO

-- https://www.kernel.org/doc/html/v6.11/input/input.html

data Event = Event
  { evTimeSecs :: Int,
    evTimeUsecs :: Int,
    evType :: Int,
    evCode :: Int,
    evValue :: Int
  }
  deriving (Show)

data KeyState = KeyDepress | KeyHold | KeyRelease deriving (Show)

data KeyEvent = KeyEvent {keyState :: KeyState, keyCode :: KeyCode} deriving (Show)

packWord :: B.ByteString -> Int
packWord = B.foldl' f 0 . B.reverse
  where
    f a b = a `shiftL` 8 .|. fromIntegral b

readEvent :: (MonadIO m) => Int -> Int -> Handle -> m Event
readEvent timeTSize susecondsTSize device = do
  Event
    <$> evGet device timeTSize -- time_t.tv_sec
    <*> evGet device susecondsTSize -- time_t.tv_usec
    <*> evGet device 2 -- type
    <*> evGet device 2 -- code
    <*> evGet device 4 -- value
  where
    evGet dev n = packWord <$> liftIO (B.hGetNonBlocking dev n)

getKeyEvent :: Event -> Maybe KeyEvent
getKeyEvent (Event _ _ 0x01 key 1) = KeyEvent KeyDepress <$> fromKeyCode key
getKeyEvent (Event _ _ 0x01 key 0) = KeyEvent KeyRelease <$> fromKeyCode key
getKeyEvent (Event _ _ 0x01 key 2) = KeyEvent KeyHold <$> fromKeyCode key
getKeyEvent _ = Nothing

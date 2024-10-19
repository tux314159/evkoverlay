{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module System.Evdev.KeyCode
  ( KeyCode (..),
    fromKeyCode,
  )
where

import System.Evdev.TH

$( genKeyCodes "KeyCode" "fromKeyCode"
     =<< readLinuxInputHdr "/usr/include/linux/input-event-codes.h"
 )

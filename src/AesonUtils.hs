{-# LANGUAGE OverloadedStrings #-}

module AesonUtils where

import Control.Lens
import Data.Aeson.Lens
import qualified Data.ByteString as B

asd :: IO ()
asd = do
  bs <- B.readFile "src/test.json"
  print bs
  print "ASDASDASDAWSD"
  print $ bs ^.. key "tekija" 

module Test where

import Data.Scientific
import Data.Aeson
import Data.String
import Control.Lens
import qualified Data.Aeson.Lens as L

jtn = putStrLn "hei"


jsonD = decode "{\"age\": 10}" :: Maybe Value

asd = case (jsonD ^.. each . L.key (fromString "age")) of
    [(Number a)] -> a
    _            -> scientific 1 2


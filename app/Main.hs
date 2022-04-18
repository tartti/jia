{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
-- import Data.Text hiding (head, drop)
import GHC.Generics
import System.Environment
import JiaParser
import JiaInterpreter
import JiaJsonDecoder
import Text.Megaparsec
import qualified Data.Map.Strict as Map



main :: IO ()
main = do
    args <- getArgs
    file <- readFile (head args)
    ast <- runParser jiaParser (head args) <$> readFile (head args) --jäsennetään koodi komentoriviparametrina annetusta tiedostosta
    jsonFileAsString <- readFile (head $ drop 1 args)
    print "AST BEGIN"
    print ast
    print "AST END"
    print "----"
    print "RESULT PRINT"
    either (putStr <$> errorBundlePretty) (print <$> (testInterpret jsonFileAsString)) ast --joko tulostetaan virheilmoitukset tai siirrytään eteenpäin (mikäli virheitä ei ollut)
    -- env <- testInterpret jsonFileAsString ast
    -- print "ACTUAL RETURN:"
    -- printReturn env


printReturn :: Env -> IO ()
printReturn env = print $ case Map.lookup "return" env of
    Just a -> a
    _ -> error "return not found"



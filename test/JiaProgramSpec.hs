-- Tests for parsing and running complete programs
module JiaProgramSpec where

import Data.Maybe

import System.Directory

import JiaParser
import JiaInterpreter

import Text.Megaparsec
import Text.Megaparsec.Char
import Test.Hspec
import Test.Hspec.Megaparsec

spec :: Spec
spec = do
    describe "Basic parser and interpreter test" $ do
        it "parses and returns jia program with boolean conditions" $ do
            let jiaFilePath =  "resources/test/jia/booleanCondition.jia"
            let jsonFilePath = "resources/test/json/booleanConditionExample.json"
            let expectedReturnValue = "alsoCorrect"
            returnStmt <- runTest jiaFilePath jsonFilePath
            returnStmt `shouldBe` expectedReturnValue


-- Takes filepaths for jia and json and returns output 
-- from running jia query against input json
runTest :: FilePath -> FilePath -> IO String
runTest jia json = do
    jiaFileAsString <- readFile jia
    jsonFileAsString <- readFile json
    ast <- runParser jiaParser jia <$> readFile jia
    return $ either errorBundlePretty (testInterpret jsonFileAsString) ast



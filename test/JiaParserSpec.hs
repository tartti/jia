--Testejä jäsentimelle
module JiaParserSpec where

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
    describe "JiaParser basic tests" $ do
        it "works with single line input" $
            parse jiaParser "" `shouldSucceedOn` "{\"firstName\":\"X\"}<-InputExample"

        it "works with taking first elem from list" $
            parse jiaParser "" `shouldSucceedOn` "[Eka.. ] <- Alkiot"

        it "works with empty list" $
            parse jiaParser "" `shouldSucceedOn` "[] <- Alkiot"

        it "works with extract first element of each nested list" $
            parse jiaParser "" `shouldSucceedOn` "{*: [ Alkiot.. ]} <- InputExample"

        it "works with multiple conditions" $
            parse jiaParser "" `shouldSucceedOn` "{\"age\": X, \"address\":{\"test\":Y}, \"koti\":S, \"children\": []} <- Input"

        it "works with nested value" $
            parse jiaParser "" `shouldSucceedOn` "{\"faxNumbers\":{\"number\":\"Y\"}}<-InputExample"

        it "works with multiple values" $
            parse jiaParser "" `shouldSucceedOn` "{\"firstName\":\"X\", \"lastName\":\"Y\"}<- Input"

        it "works with single value" $
            parse jiaParser "" `shouldSucceedOn` "{\"firstName\":\"X\"}<-InputExample"

        it "works with single value2" $
            parse jiaParser "" `shouldSucceedOn` "{\"number\": X} <- Obj"

        it "works with single nested values" $
            parse jiaParser "" `shouldSucceedOn` "{\"phoneNumbers\":{\"number\":X}}<-InputExample"

    describe "JiaParser tests with conditions" $ do
        it "parses multiple conditions" $
            parse jiaParser "" `shouldSucceedOn` "{\"name\": x, \"age\": 10, \"address\": \"Helsinki\"} <- Input\nreturn x"

        it "parses nested conditions" $
            parse jiaParser "" `shouldSucceedOn` "{\"age\": x, \"address\":{\"city\": \"Helsinki\"}} <- Input\nreturn x"

        it "parses multiple conditions including nested condition" $
            parse jiaParser "" `shouldSucceedOn` "{\"age\": x, \"address\":{\"city\": \"Helsinki\"}, \"isAlive\": true, \"children\": []} <- Input\nreturn x"


    describe "JiaParser return stmt tests" $ do
        it "works with single return statement" $
            parse jiaParser "" `shouldSucceedOn` "return X"

        it "works with multi return" $
            parse jiaParser "" `shouldSucceedOn` "return (X,Y) -- John, Smith, limit ei tee mitaan?"


        it "works with single return -- comments" $
            parse jiaParser "" `shouldSucceedOn` "return Eka --listan ekat alkiot"

        it "works with single return" $
            parse jiaParser "" `shouldSucceedOn` "return X"

        it "works with single return statement2" $
            parse jiaParser "" `shouldSucceedOn` "return X -- palauttaa x"

    describe "JiaParser sql tests" $ do
        it "works with sql-like input" $
            parse jiaParser "" `shouldSucceedOn` "LIMIT 1"

    describe "JiaParser comments and ws tests" $ do
        it "works with just -- comment line" $
            parse jiaParser "" `shouldSucceedOn` "-- kommentti"

    describe "JiaParser multiline tests" $ do
        it "works with multiline input with no whitespace in beginning of lines" $
            parse jiaParser "" `shouldSucceedOn` "{\"firstName\":\"X\"}<-InputExample\nreturn x"


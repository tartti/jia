{-# LANGUAGE OverloadedStrings #-}
module JiaInterpreter where

import JiaParser
import JiaJsonDecoder
import Data.Map.Strict as Map (insert, lookup, singleton)
import Data.Text
import Control.Lens
import Data.String
import Data.Aeson
import Data.Aeson.Lens


-- key used inv env for original input json, should never be used as variable name
inputJsonKey :: String
inputJsonKey = "input"

-- Key used in env for return value
returnJsonKey :: String
returnJsonKey = "return"

-- Testi --

testEnv :: Env
testEnv = Map.singleton "X" (TextValue "success")


-- Testi loppu --

testInterpret :: InputJson -> Input -> String
testInterpret inputJson (IInput inputLines) = case Map.lookup returnJsonKey env of
    Just (AesonValues aesonValues) -> aesonValuesAsString (Prelude.head aesonValues)
    Just (TextValue str) -> str
    Nothing -> error "Return not found"
    where env = exec inputLines (Map.insert inputJsonKey (AesonValues (myAesonWrapper inputJson)) testEnv)

interpret :: Input -> Env -> Env
interpret (IInput inputLines) env = exec inputLines env


exec :: [InputLine] -> Env -> Env
exec [] env = env
exec (x:xs) env = exec xs (execInput x env)


aesonValuesAsString :: Value -> String
aesonValuesAsString x = case x of
    String x -> (unpack x)
    _ -> "error: value couldn't be converted to a string"


execInput :: InputLine -> Env -> Env
execInput (ISql sql) env                = execSql sql env
execInput (IInputLineReturn rtrnStmt) env   = execReturn rtrnStmt env
execInput (IInputLine ps rs) env = execPattern (ps, rs) env

execSql :: Sql -> Env -> Env
execSql (SqlKw SqlKeywordLimit limit) env = case Map.lookup returnJsonKey env of
    Nothing -> env
    Just (TextValue retVal) -> Map.insert returnJsonKey newValue env where
        newValue = (TextValue (Prelude.unlines (Prelude.take limit (Prelude.lines retVal))))

--data Sql = SqlKw SqlKeyword Int deriving (Show)

-- Return-lauseiden suoritus
execReturn :: ReturnStmt -> Env -> Env
execReturn (ReturnStmt (RetVal (RetOpNone, (PatternVar var)))) env = case Map.lookup var env of
    Nothing -> error ("Undefined return variable " ++ var ++ "\nENV: " ++ show env)
    Just value -> Map.insert returnJsonKey value env
execReturn (ReturnStmt (RetVal (_, var))) env = undefined
execReturn (ReturnStmt (RetValList [x])) env = execReturn (ReturnStmt x) env
execReturn _ _ = undefined


execPattern :: (PatternSide, RightSide) -> Env -> Env
execPattern (PsJsonPattern (JsonPattern (JsonElement jsonValue)), (Rside (PatternVar rSideInputName))) env = decodePattern aesonValues jsonValue env where 
    aesonValues = case Map.lookup rSideInputName env of -- here decodePattern should be given input json and env aswell
        (Just (AesonValues x)) -> x
        _ -> error "technical error, there was invalid value for inputJson"
execPattern _ _ = undefined



myAesonWrapper :: InputJson -> [Value]
myAesonWrapper inputJson = case decode $ fromString inputJson :: Maybe Value of
    Just aesonValues -> [aesonValues]
    Nothing -> error "Decoding input-json failed"


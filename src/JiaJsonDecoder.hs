{-# LANGUAGE OverloadedStrings #-}

module JiaJsonDecoder where

import Control.Lens
import Data.Aeson
import qualified Data.Scientific as Scientific
import qualified Data.Map.Strict as Map
import qualified Data.Aeson.Lens as L
import Data.String
import JiaParser

type Env = Map.Map String EnvValue -- TODO: doesnt work with multiple values with same key
type InputJson = String
type EnvKey = String


data EnvValue =   AesonValues [Value]  -- data for stored value in Env
                | TextValue String deriving Show -- TODO: TextValue constr. is not used?

-- main = do
--     a <- decodeFileStrict "src/test.json" :: IO (Maybe Value)
--     printResult a

printResult :: Maybe Value -> IO ()
printResult a = case a of
    Nothing -> error "print result gone wrong"
    Just x -> print result where
       result = testFunction x

testFunction :: Value -> [String]
testFunction _ = ["kissa"]

-- myAesonWrapper :: InputJson -> [Value]
-- myAesonWrapper inputJson = case (decode inputJson) of
--     Just values -> values
--     Nothing -> error "Decoding input-json failed"

-- idea here is to simultaneously go through parsed pattern (JsonValue) and input json (Value),
-- passing on always same level data structure as we go deeper and deeper
-- f.e. there is pattern has condition on nested object, we only pass that nested object from value, not the whole value
decodePattern :: [Value] -> JsonValue -> Env -> Env
decodePattern values pat env =  case pat of
    (JvJsonObject (JoMembers members)) -> decodeMembersPattern  values members env  -- We top level element is object
    _ -> error "not implemented"



decodeMembersPattern :: [Value] -> JsonMembers -> Env -> Env
-- value must have key corresponding to given JsonString in here
decodeMembersPattern values (JsonMembersSingle jsonMember) env = decodeSingleMemberPattern values jsonMember env
decodeMembersPattern values (JsonMembersMulti member members) env = decodeMembersPattern values members (decodePattern values (JvJsonObject (JoMembers (JsonMembersSingle member))) env)

decodeSingleMemberPattern :: [Value] -> JsonMember -> Env -> Env
decodeSingleMemberPattern values (JsonMember (JsonString string) jsonElement) env = decodeJsonElement values string jsonElement env


-- Decodes jsonElement seaching given string as key from current [Value] object
-- or passing forward
decodeJsonElement :: [Value] -> String -> JsonElement -> Env -> Env
decodeJsonElement values jsonKey (JsonElement (JvPatternVar (PatternVar varName))) env = Map.insert varName (AesonValues value) env where
    value = case (values ^.. each . L.key (fromString jsonKey)) of
        [] -> error ("Key" ++ show jsonKey ++ "not found on values:" ++ show values) -- TODO: better error handling
        x  -> x
decodeJsonElement values jsonKey (JsonElement (JvJsonObject jsonObject)) env = decodeJsonObject values jsonKey jsonObject env
decodeJsonElement values jsonKey (JsonElement jsonValue) env = case checkCondition values jsonKey jsonValue env of
    Left a -> a
    Right b -> Map.insert  "errorDescription" (TextValue ("key not found: " ++ jsonKey)) env-- TODO: real error handling
decodeJsonElement values jsonKey (JsonElement jsonValue) env = case checkCondition values jsonKey jsonValue env of
    Left a -> a
    Right b -> Map.singleton  "errorDescription" (TextValue ("key not found: " ++ jsonKey))-- TODO: real error handling
decodeJsonElement _ _ _ _ = undefined


decodeJsonObject :: [Value] -> String -> JsonObject -> Env -> Env
decodeJsonObject _ _ JoEmpty env = env
decodeJsonObject values jsonKey (JoMembers jsonMembers) env = decodeMembersPattern (values ^.. each . L.key (fromString jsonKey)) jsonMembers env

-- throws error if some condition isnt met
checkCondition :: [Value] -> EnvKey -> JsonValue -> Env -> Either Env String
checkCondition values jsonKey (JvJsonString (JsonString jsonString)) env = case values ^.. each . L.key (fromString jsonKey) of
    [(String a)] -> case (a == (fromString jsonString)) of
        True -> Left env
        False -> error $ "error: condition for " ++ jsonKey ++ " : " ++ show a ++ " = " ++ jsonKey ++ " not met\nEnv : " ++ show env
checkCondition values jsonKey (JvJsonNumber (JsonNumber jsonNumber)) env = case values ^.. each . L.key (fromString jsonKey) of
    [(Number a)] -> case ((Scientific.coefficient a) == toInteger jsonNumber) of
        True -> Left env
        False -> error $ "error: condition for " ++ jsonKey ++ " : " ++ show (Scientific.coefficient a) ++ " = " ++ show (toInteger jsonNumber) ++ " not met\nEnv : " ++ show env
    _ -> undefined
checkCondition values jsonKey (JvBooleanValue BooleanValueTrue) env = case values ^.. each . L.key (fromString jsonKey) of
    [(Bool True)] -> Left env
    _ -> error $ "[true] expected but was " ++ show (values ^.. each . L.key (fromString jsonKey))
checkCondition values jsonKey (JvBooleanValue BooleanValueFalse) env = case values ^.. each . L.key (fromString jsonKey) of
    [(Bool False)] -> Left env
    _ -> error $ "[false] expected but was " ++ show (values ^.. each . L.key (fromString jsonKey))

-- checkCondition :: [Value] -> Int -> Int -> Env -> Either Env Int
-- -- checkCondition values jsonKey jsonNumber env = case values ^.. each . L.key (fromString jsonKey)

    -- (JsonMembersSingle (JsonMember wrappedName wrappedValue)))) -> insert key val env where
    --     key = "a"
    --     val = "b"
    -- _ -> undefined -- TODO: ERROR HANDLING
    
--    (JsonElement (JvJsonObject (JoMembers (JsonMembersSingle (JsonMember (JsonString "tekija") (JsonElement (JvPatternVar (PatternVar "x"))))))))

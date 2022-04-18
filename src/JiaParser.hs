module JiaParser where

import Control.Applicative hiding (many, some)
import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Input = IInput [InputLine] deriving (Show)

data InputLine = ISql Sql
               | IInputLineReturn ReturnStmt
               | IInputLine PatternSide RightSide deriving (Show)
               -- | IInputLineMulti InputLine InputLine deriving (Show)
             --             | patternSide right-side lineBreak inputLine -- no need coz input is list of inputlines?

data PatternSide = PsJsonPattern JsonPattern
                 | PsVarJsonPattern PatternVar JsonPattern
                 | PsSqlValuePair [SqlValuePair]
                 | PsSql Sql
                 | PsReturn ReturnStmt deriving (Show)

data SqlValuePair = SqlValPair (Sql, JsonPattern) deriving (Show)

data JsonPattern = JsonPattern JsonElement deriving (Show)

data Id = Id String deriving (Show)

data JsonValue = JvPatternVar        PatternVar
               | JvJsonObject        JsonObject
               | JvJsonArray         JsonArray
               | JvJsonString        JsonString
               | JvJsonNumber        JsonNumber
               | JvBooleanValue      BooleanValue
               | JvNullValue         NullValue deriving (Show)

data BooleanValue = BooleanValueTrue | BooleanValueFalse deriving (Show) -- Bad way to implement, but cba to fix

data NullValue = NullValue deriving (Show)

data JsonElement = JsonElement JsonValue deriving (Show)

data JsonObject = JoEmpty
                | JoMembers JsonMembers deriving (Show)

data JsonMembers = JsonMembersSingle JsonMember
                 | JsonMembersMulti JsonMember JsonMembers deriving (Show)

data JsonMember = JsonMember JsonString JsonElement deriving (Show)

data JsonArray = JaEmpty
               | JaElements JsonElements deriving (Show)

data JsonNumber = JsonNumber Int deriving (Show)

data JsonString = JsonString String 
                | JsonWildcard 
                | JsonStringRoL PatternVar deriving (Show)

data JsonElements = JsonElements [JsonElement] deriving (Show)

data Sql = SqlKw SqlKeyword Int deriving (Show)

-- Insert possible new sqlKeywords here by constructor
data SqlKeyword = SqlKeywordLimit deriving (Show)

-- No known valueless keywords at the moment
--data SqlValuelessKeyword = SqlValuelessKeyword String

data ReturnStmt = ReturnStmt ReturnValue deriving (Show)

data ReturnOperator = RetOpNone -- used if there is only single returned var etc.
                    | ReturnOpPlus deriving (Show) -- operator for concatenation

data RetWord = RetWord deriving (Show)

data RightSide = Rside PatternVar deriving (Show)

data PatternVar = PatternVar String deriving (Show)

data ReturnValue = RetVal (ReturnOperator, PatternVar)
                 | RetValList [ReturnValue] deriving (Show)

-- how to make it so there can be multiple returnOperands
-- retVal = retVal | retOp retVal | [retVal] | patternVar

type Parser = Parsec Void String


-- poistaa whitespacen 
whitespace :: Parser ()
whitespace = L.space space1 lineCmnt empty
    where
        lineCmnt = L.skipLineComment "--"

whitespaceNoNewline :: Parser ()
whitespaceNoNewline = L.space (void $ some (char ' ' <|> char '\t')) lineCmnt empty
    where
        lineCmnt = L.skipLineComment "--"

-- poistaa whitespacen jokaisen lexemen lopusta
lexeme :: Parser a -> Parser a
lexeme = L.lexeme whitespaceNoNewline

-- poistaa whitespacen jokaisen symboln lopusta
symbol :: String -> Parser String
symbol = L.symbol whitespaceNoNewline

-- pilkkoo sulkujen sisällön syötteestä
sulut :: Parser a -> Parser a
sulut = between (symbol "(") (symbol ")")

-- pilkkoo aaltosulkujen sisällön syötteestä
asulut :: Parser a -> Parser a
asulut = between (symbol "{") (symbol "}")

-- varmistaa onko seuraavana tulossa luku ja ottaa sen
integer :: Parser Int
integer = lexeme L.decimal

-- used to make sure reserved words aren't used as beginning of id
resWord :: String -> Parser ()
resWord r = (lexeme . try) (string r *> notFollowedBy alphaNumChar)

-- reserved word list
rwl :: [String] 
rwl = ["true", "false", "LIMIT"]

-- parses id from input
-- TODO: jsonissa voi olla muutakin kuin id:t, fixme
-- NOTE! will eat everything
identifier :: Parser Id
identifier = (lexeme . try) (p >>= check)
    where
        p       = (:) <$> letterChar <*> many alphaNumChar
        check x = if elem x rwl 
                     then fail $ "keyword " ++ show x ++ "cannot be used for some reason TODO: fixme"
                    else return ((Id x))

jvPatternVar :: Parser JsonValue
jvPatternVar = do
    patternVar <- patternVar
    return (JvPatternVar patternVar)

-- TODO: fixme
patternVar :: Parser PatternVar
patternVar = (lexeme . try) (p >>= check)
    where
        p       = (:) <$> letterChar <*> many alphaNumChar
        check x = if elem x rwl 
                     then fail $ "keyword " ++ show x ++ "cannot be used for some reason TODO: fixme"
                    else return ((PatternVar x))

returnWrapper :: Parser ReturnValue
returnWrapper = multiReturn <|> singleReturn

multiReturn:: Parser ReturnValue
multiReturn= do
    _ <- symbol "("
    retVal <- sepBy returnValue (symbol ",")
    _ <- symbol ")"
    return (RetValList retVal)

singleReturn:: Parser ReturnValue
singleReturn= do
    retStmt <- returnValue
    return (RetValList [retStmt])

returnValue :: Parser ReturnValue
returnValue = do
    retOp <- returnOperator
    retVal <- patternVar
    return (RetVal (retOp, retVal))

returnOperator :: Parser ReturnOperator
returnOperator = retOpValuePlus <|> retOpValueNone

retOpValueNone :: Parser ReturnOperator
retOpValueNone = do
    return RetOpNone

retOpValuePlus :: Parser ReturnOperator
retOpValuePlus = do
    _ <- symbol "++"
    return (ReturnOpPlus)
    

jiInput :: Parser Input
jiInput = do
    inputLines <- sepEndBy inputLine newline
    return (IInput inputLines)

inputLine :: Parser InputLine
inputLine = inputLineSql <|> inputLineReturn <|> inputLineNormal

inputLineNormal :: Parser InputLine
inputLineNormal = do
    patternSide <- patternSide
    _ <- symbol "<-"
    rightSide <- rightSide
    return (IInputLine patternSide rightSide)

inputLineReturn :: Parser InputLine
inputLineReturn = do
    returnStmt <- returnStmt
    return (IInputLineReturn returnStmt)

inputLineSql :: Parser InputLine
inputLineSql = do
    parsedSql <- sqlParser
    return (ISql parsedSql)
    

rightSide :: Parser RightSide
rightSide = do
    varName <- patternVar
    return (Rside varName)

patternSide :: Parser PatternSide
patternSide =  jsonPatternSide
           <|> basicMatching -- not sure if there is any point of having this

jsonPatternSide :: Parser PatternSide
jsonPatternSide = do
    jsonPattern <- jsonPattern
    return (PsJsonPattern jsonPattern)

jsonPattern :: Parser JsonPattern
jsonPattern = do
    jsonElem <- jsonElem
    return (JsonPattern jsonElem)

basicMatching :: Parser PatternSide
basicMatching = do
   varName <- patternVar
   _ <- symbol "@"
   jsonPattern <- jsonPattern
   return (PsVarJsonPattern varName jsonPattern)

jsonElem :: Parser JsonElement
jsonElem = do
    jsonValue <- jsonValue
    return (JsonElement jsonValue)

jsonValue :: Parser JsonValue
jsonValue = jsonStringValue 
         <|> jvPatternVar
         <|> jsonObjectValue
         <|> jsonArrayValue
         <|> jsonNumberValue
         <|> booleanValueTrue
         <|> booleanValueFalse
         <|> nullValue

jsonObjectValue :: Parser JsonValue
jsonObjectValue = do
    jsonObject <- jsonObject
    return (JvJsonObject jsonObject)

jsonObject :: Parser JsonObject
jsonObject = jsonObjectWithMembers <|> emptyJsonObject

emptyJsonObject :: Parser JsonObject
emptyJsonObject = do
    _ <- symbol "{"
    _ <- symbol "}"
    return (JoEmpty)
-- TODO: check what to return if anything

jsonObjectWithMembers :: Parser JsonObject
jsonObjectWithMembers = do
    _ <- symbol "{"
    jsonMembers <- jsonMembers
    _ <- symbol "}"
    return (JoMembers jsonMembers)

jsonMembers :: Parser JsonMembers
jsonMembers = try (multiJsonMembers) <|> singleJsonMembers -- try:n avulla mennään multikokonaan loppuun, ja ei kaaduta ongelmaan jos eka parsinta onnistuu molemmissa

singleJsonMembers :: Parser JsonMembers
singleJsonMembers = do
    jsonMember <- jsonMember
    return (JsonMembersSingle jsonMember)

multiJsonMembers :: Parser JsonMembers
multiJsonMembers = do
    jsonMember <- jsonMember
    _ <- symbol ","
    jsonMembers <- jsonMembers
    return (JsonMembersMulti jsonMember jsonMembers)

jsonMember :: Parser JsonMember
jsonMember = do
    jsonString <- jsonStringWrapper
    _ <- symbol ":"
    jsonElem <- jsonElem
    return (JsonMember jsonString jsonElem)

jsonArrayValue :: Parser JsonValue
jsonArrayValue = do
    jsonArrayVal <- jsonArray
    return (JvJsonArray jsonArrayVal)

jsonArray :: Parser JsonArray
jsonArray = jaElements <|> jaEmpty

jaEmpty :: Parser JsonArray
jaEmpty = do
    _ <- symbol "["
    _ <- symbol "]"
    return (JaEmpty)

jaElements :: Parser JsonArray
jaElements = do
    jsonElements <- jsonElements
    return (JaElements jsonElements)

jsonElements :: Parser JsonElements
jsonElements = do
    _ <- symbol "["
    jsonElements <- sepBy jsonElem (symbol ",")
    _ <- symbol "]"
    return (JsonElements jsonElements)

jsonStringValue :: Parser JsonValue
jsonStringValue = do
    stringValue <- jsonStringWrapper
    return (JvJsonString stringValue)

jsonStringWrapper :: Parser JsonString
jsonStringWrapper = try(jsonRestOfList) <|> jsonString  <|> jsonWildcard

jsonWildcard :: Parser JsonString
jsonWildcard = do
    _ <- symbol "*"
    return JsonWildcard

jsonString :: Parser JsonString
jsonString = do
    _ <- symbol "\""
    stringValue <- manyTill anySingle (symbol "\"")
    return (JsonString stringValue)

jsonRestOfList :: Parser JsonString
jsonRestOfList = do
    patternVar <- patternVar
    _ <- symbol ".."
    return (JsonStringRoL patternVar)

jsonNumberValue :: Parser JsonValue
jsonNumberValue = do
    numValue <- jsonNumber
    return (JvJsonNumber numValue)

-- For now this only parses integers
jsonNumber :: Parser JsonNumber
jsonNumber = do
    numValue <- integer
    return (JsonNumber numValue)

booleanValueTrue :: Parser JsonValue
booleanValueTrue = do
    resWord "true"
    return (JvBooleanValue BooleanValueTrue)

booleanValueFalse :: Parser JsonValue
booleanValueFalse = do
    resWord "false"
    return (JvBooleanValue BooleanValueFalse)

-- TODO: fixme?
nullValue :: Parser JsonValue
nullValue = do
    resWord "null"
    return (JvNullValue NullValue)

sqlParser :: Parser Sql
sqlParser = do
    resWord "LIMIT"
    intValue  <- integer
    return (SqlKw SqlKeywordLimit intValue)

singleSql :: Parser PatternSide   
singleSql = undefined

returnStmt :: Parser ReturnStmt
returnStmt = do
    resWord "return"
    returnVar <- returnWrapper
    return (ReturnStmt returnVar)

jiaParser :: Parser Input
jiaParser = between whitespace eof jiInput

-- main = do
--     input <- fmap head getArgs
--     parseTest jiInput input

module LispParser where

import Data.Functor ((<$>))
import Data.Complex (Complex ((:+)))
import Data.Ratio ((%))
import Data.Array (listArray)
import Control.Applicative ((<*>))
import Control.Monad.Error (throwError)
import Numeric (readOct, readHex, readFloat)
import Text.ParserCombinators.Parsec (
  Parser,
  parse,
  letter, char, string, digit, space, octDigit, hexDigit,
  anyChar, alphaNum,
  oneOf, noneOf,
  endBy, sepBy, notFollowedBy, sepEndBy,
  many, many1, skipMany, skipMany1,
  between,
  try,
  (<|>))
import LispData

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapedChars :: Parser Char
escapedChars = convert <$> (char '\\' >> oneOf "\\\"nrt")
  where convert 'n' = '\n'
        convert 'r' = '\r'
        convert 't' = '\t'
        convert x = x

parseAtom :: Parser LispVal
parseAtom = Atom <$>
  ((:) <$> (letter <|> symbol) <*> many (letter <|> digit <|> symbol))

parseString :: Parser LispVal
parseString = String <$> between (char '"') (char '"') contents
  where contents = many $ escapedChars <|> noneOf "\"\\"

-- TODO: Error occurred with to evaluate the '\n
parseQuoted :: Parser LispVal
parseQuoted = quote <$> (char '\'' >> parseExpr)
  where quote x = List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = quasiquote <$> (char '`' >> parseExpr)
  where quasiquote x = List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = unquote <$> (char ',' >> parseExpr)
  where unquote x = List [Atom "unquote", x]

-- TODO: '(1. 2) -> (1 . 2)
parseList :: Parser LispVal
parseList =
  do char '(' >> skipMany space
     contents <- parseExpr `sepEndBy` spaces
     parseDot contents <|> parseEnd contents
  where parseDot _head =
          do _tail <- char '.' >> spaces >> parseExpr
             closeParen >> return (DottedList _head _tail)
        parseEnd h = closeParen >> return (List h)
        closeParen = skipMany space >> char ')'

parseComplex :: Parser LispVal
parseComplex =
  do x <- parseRealNumber
     _ <- skipMany space >> char '+' >> skipMany space
     y <- parseRealNumber
     _ <- char 'i'
     return $ Complex (toDouble x :+ toDouble y)
  where parseRealNumber = try parseFloat <|> parseDecimal1

parseFloat :: Parser LispVal
parseFloat =
  do x <- many1 digit
     dot <- string "."
     y <- many1 digit
     return . strToFloat $ concat [x, dot, y]
  where strToFloat = Float . fst . head . readFloat

parseRatio :: Parser LispVal
parseRatio =
  do x <- many1 digit
     _ <- char '/'
     y <- many1 digit
     return $ Ratio ((read x) % (read y))

toDouble :: LispVal -> Double
toDouble (Float f) = realToFrac f
toDouble (Number n) = fromIntegral n

parseInteger :: Parser LispVal
parseInteger =
  parseDecimal1
  <|> parseDecimal2
  <|> parseHex
  <|> parseOct
  <|> parseBin

parseDecimal1 :: Parser LispVal
parseDecimal1 = Number . read <$> many1 digit

parseDecimal2 :: Parser LispVal
parseDecimal2 = try (string "#d") >> parseDecimal1

parseHex :: Parser LispVal
parseHex = try (string "#x") >> (Number . hex2dig <$> many1 hexDigit)

parseOct :: Parser LispVal
parseOct = try (string "#o") >> (Number . oct2dig <$> many1 octDigit)

parseBin :: Parser LispVal
parseBin = try (string "#b") >> (Number . bin2dig <$> many1 (oneOf "10"))

oct2dig x = fst $ readOct x !! 0

hex2dig x = fst $ readHex x !! 0

bin2dig  = bin2dig' 0

bin2dig' digint "" = digint
bin2dig' digint (x:xs) =
  let old = 2 * digint + (if x == '0' then 0 else 1)
  in bin2dig' old xs

parseNumber :: Parser LispVal
parseNumber = try parseComplex
  <|> try parseFloat
  <|> try parseRatio
  <|> parseInteger

parseBool :: Parser LispVal
parseBool = Bool . toBool <$> (try (string "#t") <|> try (string "#f"))
  where toBool "#t" = True
        toBool "#f" = False

parseCharacter :: Parser LispVal
parseCharacter =
  Character . convert
  <$> (try (string "#\\") >>
       (try (string "newline") <|> try (string "space") <|> oneAlphaNum))
  where oneAlphaNum = do { x <- anyChar; notFollowedBy alphaNum; return [x] }
        convert value =
          case value of
            "space" -> ' '
            "newline" -> '\n'
            _ -> head value

-- TODO: Error occurred with to evaluate #(1  )
parseVector :: Parser LispVal
parseVector = toVector <$> between beg end (parseExpr `sepBy` spaces)
  where beg = try (string "#(") >> skipMany space
        end = skipMany space >> char ')'
        toVector s = Vector (listArray (0 ,length s - 1) s)

parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> parseString
  <|> parseQuoted
  -- <|> parseQuasiQuoted TODO:
  -- <|> parseUnQuote TODO:
  <|> parseList
  <|> parseNumber
  <|> try parseBool
  <|> try parseCharacter
  <|> try parseVector

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val

readExpr = readOrThrow parseExpr
readExprList = readOrThrow $ endBy parseExpr spaces

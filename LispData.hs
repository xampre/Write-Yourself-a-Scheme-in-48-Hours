module LispData where

import System.IO (Handle)
import Control.Monad.Error -- TODO: (Error)
import Data.Array (Array)
import Data.Complex (Complex)
import Data.IORef (IORef)
import Text.ParserCombinators.Parsec (ParseError)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Vector (Array Int LispVal)
             | Complex (Complex Double)
             | Float Double
             | Ratio Rational
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String], vararg :: Maybe String,
                      body :: [LispVal], closure :: Env }
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

type ThrowsError = Either LispError
type IOThrowsError = ErrorT LispError IO
type Env = IORef [(String, IORef LispVal)]

instance Show LispVal where show = showVal

instance Show LispError where show = showError
instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Complex contents) = show contents
showVal (Float contents) = show contents
showVal (Ratio contents) = show contents
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Character contents) = show contents
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList _head _tail) = "(" ++ unwordsList _head ++ " . " ++ showVal _tail ++ ")"
showVal (Vector contents) = show contents
showVal (PrimitiveFunc _) = "<primitive>"
showVal Func {params = args, vararg = varargs, body = _, closure = _} =
   "(lambda (" ++ unwords (map show args) ++
      (case varargs of
         Nothing -> ""
         Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Port _)   = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

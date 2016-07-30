import Data.Functor ((<$>))
import Control.Applicative ((<*>))
import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import LispParser
import LispData
import LispPrimitives
import REPL (runRepl, evalAndPrint)

evalString :: Env -> String -> IO String
evalString env expr =
  runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env

runOne :: [String] -> IO ()
runOne args =
  do env <- primitiveBindings >>=
            flip bindVars [("args", List $ map String $ drop 1 args)]
     res <- runIOThrows $
            show <$> eval env (List [Atom "load", String (head args)])
     hPutStrLn stderr res

interactive = primitiveBindings >>= runRepl "quit" "Lisp > " . evalString

main :: IO ()
main =
  do args <- getArgs
     if null args then interactive else runOne args

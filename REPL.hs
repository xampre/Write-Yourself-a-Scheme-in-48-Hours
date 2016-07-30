module REPL where

import System.IO (hFlush, stdout)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalAndPrint :: (String -> IO String) -> String -> IO ()
evalAndPrint evalString expr = evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action =
  do result <- prompt
     if pred result
       then return ()
       else action result >> until_ pred prompt action

runRepl :: String -> String -> (String -> IO String) -> IO ()
runRepl quitStr promptStr evalString =
  until_ (== quitStr) (readPrompt promptStr) (evalAndPrint evalString)

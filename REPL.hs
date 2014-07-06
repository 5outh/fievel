{-# LANGUAGE NoMonomorphismRestriction #-}

import System.Console.Haskeline
import Parser
import Types
import Text.ParserCombinators.Parsec
import Control.Applicative hiding (many, optional, (<|>))
import Control.Monad.Trans.State
import Control.Monad.State.Class hiding (put, get)
import Control.Monad.IO.Class
import Control.Monad.Trans
import qualified Data.Map as M

-- Command line command types
data Command = 
    Quit
  | Evaluate String
  | GetType String
  | Print String
  | Load String
    deriving Show

getType  = string ":t" *> optional spaces *> liftA GetType (many anyChar)
quit     = string ":q" *> return Quit
evaluate = liftA Evaluate (many anyChar)
putLn    = string ":p" *> optional spaces *> liftA Print (many anyChar) 
load     = string ":l" *> optional spaces *> liftA (Load . trim) (many anyChar)
  where trim = filter (`notElem` " \t\r\n")

command = try getType 
      <|> try quit
      <|> try putLn
      <|> try load
      <|> evaluate

parseCommand = parse command "(command)"

runQuit = lift $ outputStrLn "Goodbye!"

runGetType str = do
  (FievelState _ types) <- get
  case M.lookup str types of 
    Just t  -> lift $ outputStrLn $ show t 
    Nothing -> do
      let out = parseFievel str
      lift $ case out of 
        Left (Parser err) -> outputStrLn str >> outputStrLn err
        Right (t, _)      -> outputStrLn $ show t

runPrint str = do
  (FievelState exprs _) <- get
  case M.lookup str exprs of
    Just e -> lift $ outputStrLn $ show e
    Nothing -> do
      let out = parseFievel str
      lift $ case out of 
        Left (Parser err) -> outputStrLn str >> outputStrLn err
        Right (_, e)      -> outputStrLn $ show e

runLoad path = do
  out <- liftIO $ parseFievelFile path
  case out of
    Left (Parser err) -> lift $ outputStrLn err
    Right st -> do
      put st
      st' <- get
      liftIO $ print st'

runEvaluate str = lift $ outputStrLn "Todo: Evaluation"

loop :: StateT FievelState (InputT IO) ()
loop = do
    minput <- lift $ getInputLine "> "
    case minput of
        Nothing -> runQuit
        Just cmd -> do 
          let parsed = parseCommand cmd
          case parsed of
            Left err -> lift $ outputStrLn $ show err
            Right c  -> case c of
              Quit          -> runQuit
              GetType str   -> runGetType  str  >> loop
              Print str     -> runPrint    str  >> loop
              Load path     -> runLoad     path >> loop
              Evaluate str  -> runEvaluate str  >> loop

main :: IO ()
main = runInputT defaultSettings (evalStateT loop emptyState)

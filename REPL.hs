{-# LANGUAGE NoMonomorphismRestriction #-}

import System.Console.Haskeline
import Text.ParserCombinators.Parsec
import Control.Applicative hiding (many, optional, (<|>))
import Control.Monad.Trans.State hiding (get, put)
import Control.Monad.State.Class
import Control.Monad.IO.Class
import Control.Monad.Trans
import qualified Data.Map as M
import Control.Lens

import Parser
import Types
import TypeChecker

-- Command line command types
data Command = 
    Quit
  | Evaluate String
  | GetType String
  | Print String
  | Load String
  | Let String
    deriving Show

getType  = string ":t" *> optional spaces *> liftA GetType (many anyChar)
quit     = string ":q" *> return Quit
evaluate = liftA Evaluate (many anyChar)
putLn    = string ":p" *> optional spaces *> liftA Print (many anyChar) 
load     = string ":l" *> optional spaces *> liftA (Load . trim) (many anyChar)
  where trim = filter (`notElem` " \t\r\n")
bind     = string ":let" *> optional spaces *> liftA Let (many anyChar)

command = try getType 
      <|> try quit
      <|> try putLn
      <|> try bind
      <|> try load
      <|> evaluate

parseCommand = parse command "(command)"

runQuit = lift $ outputStrLn "Goodbye!"

runGetType str = do
  fs@(FievelState _ types) <- get
  case M.lookup str types of 
    Just t  -> lift $ outputStrLn $ show t 
    Nothing -> do
      let out = parseSingleExpr str
      lift $ case out of 
        Left (Parser err) -> outputStrLn str >> outputStrLn err
        Right e           -> case typeOf e of
          Right t  -> outputStrLn $ show t
          Left (TypeError err) -> outputStrLn err

runPrint str = do
  fs@(FievelState exprs _) <- get
  case M.lookup str exprs of
    Just e -> lift $ outputStrLn $ show e
    Nothing -> do
      let out = parseSingleExpr str
      lift $ case out of 
        Left (Parser err) -> outputStrLn str >> outputStrLn err
        Right e    -> outputStrLn $ show e

runLoad path = do
  fs <- get
  out <- liftIO $ parseFievelFile fs path
  case out of
    Left (Parser err) -> lift $ outputStrLn err
    Right st -> do
      put st
      lift $ (outputStrLn $ show st)

runBind :: String -> StateT FievelState (InputT IO) ()
runBind str = do
  fs@(FievelState exprs types) <- get
  let out = parseFievel fs str
  case out of 
    Left (Parser err) -> lift $ (outputStrLn str >> outputStrLn err)
    Right (res, fs') -> do
      lift $ outputStrLn (show res)
      put fs'

runEvaluate str = lift $ outputStrLn "Todo: Evaluation"

loop :: StateT FievelState (InputT IO) ()
loop = do
    minput <- lift $ getInputLine "Fievel*> "
    case minput of
        Nothing -> runQuit
        Just "" -> loop
        Just cmd -> do 
          let parsed = parseCommand cmd
          lift $ outputStrLn $ show parsed
          case parsed of
            Left err -> lift $ outputStrLn $ show err
            Right c  -> case c of
              Quit          -> runQuit
              GetType str   -> runGetType  str  >> loop
              Print str     -> runPrint    str  >> loop
              Let str       -> runBind     str  >> loop
              Load path     -> runLoad     path >> loop
              Evaluate str  -> runEvaluate str  >> loop

main :: IO ()
main = runInputT defaultSettings (evalStateT loop emptyState)

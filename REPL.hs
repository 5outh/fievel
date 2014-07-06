{-# LANGUAGE NoMonomorphismRestriction #-}

import System.Console.Haskeline
import Parser
import Types
import Text.ParserCombinators.Parsec
import Control.Applicative hiding (many, optional, (<|>))
import Control.Monad.Trans.State hiding (get, put)
import Control.Monad.State.Class
import Control.Monad.IO.Class
import Control.Monad.Trans
import qualified Data.Map as M
import Control.Lens

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

runBind :: String -> StateT FievelState (InputT IO) ()
runBind str = do
  let out = parseFievel str
  (FievelState exprs types) <- get
  case out of 
    Left (Parser err) -> lift $ (outputStrLn str >> outputStrLn err)
    Right (t, e) -> do
      case t of
        Nothing -> return ()
        Just t  -> let tpl = typeToTuple t in case tpl of 
          Nothing      -> return ()
          Just (s, t') -> do
            let types' = M.insert s t' types
            put (FievelState exprs types')
      let tpl = defToTuple e in case tpl of
        Nothing      -> return ()
        Just (s, e') -> do
          let exprs' = M.insert s e' exprs
          put (FievelState exprs' types)

runEvaluate str = lift $ outputStrLn "Todo: Evaluation"

loop :: StateT FievelState (InputT IO) ()
loop = do
    minput <- lift $ getInputLine "> "
    case minput of
        Nothing -> runQuit
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

{-# LANGUAGE NoMonomorphismRestriction #-}
import System.Console.Haskeline
import Parser
import Types
import Text.ParserCombinators.Parsec
import Control.Applicative hiding (many, optional, (<|>))

-- Command line command types
data Command = 
    Quit
  | Evaluate String
  | GetType String
  | Print String

getType  = string ":t" *> optional spaces *> liftA GetType (many anyChar)
quit     = string ":q" *> return Quit
evaluate = liftA Evaluate (many anyChar)
putLn    = string ":p" *> optional spaces *> liftA Print (many anyChar) 

command = try getType 
      <|> try quit
      <|> try putLn
      <|> evaluate

parseCommand = parse command "(command)"

main :: IO ()
main = runInputT defaultSettings loop
  where 
    loop :: InputT IO ()
    loop = do
        minput <- getInputLine "> "
        case minput of
            Nothing -> return ()
            Just cmd -> do 
              case parseCommand cmd of
                Left err -> outputStrLn $ show err
                Right c -> case c of
                  Quit -> return ()
                  GetType str  -> do 
                    let out = parseFievel str
                    case out of 
                      Left (Parser err) -> outputStrLn str >> outputStrLn err
                      Right (t, _)      -> outputStrLn $ show t
                    loop
                  Print str  -> do
                    let out = parseFievel str
                    case out of 
                      Left (Parser err) -> outputStrLn str >> outputStrLn err
                      Right (_, e)      -> outputStrLn $ show e
                    loop
                  Evaluate str -> outputStrLn "Todo: Evaluation" >> loop

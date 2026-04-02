module Main where

import           Evaluation
import           Helper
import           Lexer
import           Parser
import           Syntax
import           System.Console.Haskeline

getStratData :: String -> Maybe EvalStrat
getStratData ":norm" = Just NormalOrder
getStratData ":appl" = Just ApplicativeOrder
getStratData _       = Nothing

splitInput :: Maybe (String -> (String, String))
splitInput = Just $ \txt -> (takeWhile (' ' /=) txt, dropWhile (' ' /=) txt)

commandList :: String
commandList =
  "------------------------------------"
    ++ ":?, :h, :help -> for help\n"
    ++ ":q, :quit -> for closing the program\n"
    ++ ":appl -> for using applicative order reduction\n"
    ++ ":norm -> for using normal order reduction\n"
    ++ "<lambda-term> -> for showing PLCI's interpretation of the term, as well as evaluating it"
    ++ "------------------------------------"

main :: IO ()
main = do
  putStrLn "Type :?, :h or :help for help"
  runInputT defaultSettings $ repl ApplicativeOrder
  where
    repl :: EvalStrat -> InputT IO ()
    repl strat = do
      maybeInput <- getInputLine "plci> "
      case splitInput <*> maybeInput of
        Nothing -> pure ()
        Just (h, _) | elem h [":?", ":h", ":help"] -> outputStrLn commandList
        Just (q, _) | elem q [":q", ":quit"] -> return ()
        Just (order, _) | elem order [":norm", ":appl"] -> do
          case (getStratData order) of
            Just order' -> do
              outputStrLn ("Now using " ++ show order')
              repl order'
            Nothing -> pure ()
        Just (txt1, txt2) -> do
          case parser $ alexScanTokens (txt1 ++ txt2) of
            Left e -> outputStrLn e
            Right ast -> do
              maybeAST <- validateDeBruijn (genIndex ast)
              tryMaybeAST maybeAST (\ast' -> outputStrLn ("Showing:\n" ++ show ast' ++ "\n"))
              let nextStep2 maybeAST' = tryMaybeAST maybeAST' (\ast'' -> tryMaybeAST (fixNames ast'') (\ast''' -> outputStrLn ("Evaluating:\n" ++ show ast''')))
                  nextStep1 ast' = (validateDeBruijn ast' >>= nextStep2)
              tryMaybeAST (Just (eval strat) <*> maybeAST) nextStep1
          repl strat

validateDeBruijn :: TermNode -> InputT IO (Maybe TermNode)
validateDeBruijn ast =
  if areIndexesBroken ast
    then outputStrLn "There was an error with the internal representation" >>= \_ -> pure Nothing
    else pure $ Just ast

tryMaybeAST :: Maybe TermNode -> (TermNode -> InputT IO ()) -> InputT IO ()
tryMaybeAST maybeAST act = case maybeAST of Just ast -> act ast; Nothing -> pure ()

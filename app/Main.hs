module Main where

import           Data.Char
import           Data.List
import           Evaluation
import           Helper
import           Lexer
import           Parser
import           Syntax
import           System.Console.Haskeline

type Environment = [(Name, TermNode)]

getStratData :: String -> Maybe EvalStrat
getStratData ":norm" = Just NormalOrder
getStratData ":appl" = Just ApplicativeOrder
getStratData _       = Nothing

commandList :: String
commandList =
  "-----------------------------------------------------------------------------------\n"
    ++ "| The default reduction strategy is applicative                                   |\n"
    ++ "| :?, :h, :help -> for help                                                       |\n"
    ++ "| :q, :quit     -> for closing the program                                        |\n"
    ++ "| <lambda-term> -> shows the term and then evaluates it                           |\n"
    ++ "| :appl         -> for using applicative order reduction                          |\n"
    ++ "| :norm         -> for using normal order reduction                               |\n"
    ++ "| :let <var> = <lambda-term> -> assigns a lambda-term to an environtment variable |\n"
    ++ "| :s, :show <var> -> shows the contents of a variable                             |\n"
    ++ "| :e, :eval <var> -> evaluates a variable up-to normal form (in-place)            |\n"
    ++ "| :e, :eval <number> <var> -> evaluates a variable n times (in-place)             |\n"
    ++ "| :a, :alpha <var1> <var2> -> checks if var1 and var2 are alpha equivalent        |\n"
    ++ "| :showenv -> shows the list of names of environment variables                    |\n"
    ++ "| :b, :bnf, :beta, :normalform <var> -> checks if var is in beta-normal-form      |\n"
    ++ "-----------------------------------------------------------------------------------"

main :: IO ()
main = do
  putStrLn "Type :?, :h or :help for help"
  runInputT defaultSettings $ repl ApplicativeOrder []
  where
    repl :: EvalStrat -> Environment -> InputT IO ()
    repl strat env = do
      maybeInput <- getInputLine "plci> "
      case Just words <*> maybeInput of
        Nothing -> pure ()
        Just (h : []) | elem h [":?", ":h", ":help"] -> outputStrLn commandList >>= \_ -> repl strat env
        Just (q : []) | elem q [":q", ":quit"] -> return ()
        Just (order : []) | elem order [":norm", ":appl"] -> do
          case (getStratData order) of
            Just order' -> do
              outputStrLn ("Now using " ++ show order')
              repl order' env
            Nothing -> pure ()
        Just (":let" : x : "=" : txt)
          | and (map isAlpha x) ->
              getTermNode (intercalate " " txt) >>= \maybeAST -> tryMaybeAST maybeAST (\ast -> repl strat ((x, ast) : env))
        Just (":showenv" : []) -> outputStrLn $ show $ nub $ map fst env
        Just (beta : x : []) | lookup x env /= Nothing && elem beta [":b", ":beta", ":bnf", ":normalform"] -> case lookup x env of Just ast -> outputStrLn (x ++ " is" ++ (if hasRedex ast then " not in " else " in ") ++ "beta-normal-form") >>= \_ -> repl strat env; Nothing -> pure ()
        Just (e : x : [])
          | lookup x env /= Nothing && elem e [":e", ":eval"] ->
              showEvalTermNode strat Total (lookup x env) >>= \ast -> case ast of Just ast' -> repl strat ((x, ast') : env); Nothing -> repl strat env
        Just (e : n : x : [])
          | lookup x env /= Nothing && and (map isDigit n) && elem e [":e", ":eval"] ->
              showEvalTermNode strat (Partial $ read n) (lookup x env) >>= \ast -> case ast of Just ast' -> repl strat ((x, ast') : env); Nothing -> repl strat env
        Just (s : x : [])
          | lookup x env /= Nothing && elem s [":s", ":show"] ->
              tryMaybeAST (lookup x env) (\ast -> outputStrLn $ "Showing:\n" ++ show ast) >>= \_ -> repl strat env
        Just (alpha : x1 : x2 : []) | lookup x1 env /= Nothing && lookup x2 env /= Nothing && elem alpha [":a", ":alpha"] -> do
          case (lookup x1 env, lookup x2 env) of
            (Just ast1, Just ast2) | ast1 == ast2 -> outputStrLn (x1 ++ " and " ++ x2 ++ " are alpha equivalent")
            _ -> outputStrLn (x1 ++ " and " ++ x2 ++ " are not alpha equivalent")
          repl strat env
        Just ((':' : _) : _) -> outputStrLn "Command invalid: either variable does not exist, wrong number of arguments or wrong command" >>= \_ -> repl strat env
        Just txt -> do
          maybeAST1 <- getTermNode (intercalate " " txt)
          tryMaybeAST maybeAST1 (\ast -> outputStrLn ("Showing:\n" ++ show ast ++ "\n"))
          maybeAST2 <- showEvalTermNode strat Total maybeAST1
          repl strat env

isJust :: Maybe a -> Bool
isJust m = case m of Just _ -> True; _ -> False

showEvalTermNode :: EvalStrat -> EvalNumber -> (Maybe TermNode) -> InputT IO (Maybe TermNode)
showEvalTermNode strat n maybeAST1 = do
  maybeAST2 <- case maybeAST1 of Just ast -> validateDeBruijn (eval strat n ast); Nothing -> pure Nothing
  tryMaybeAST maybeAST2 (\ast -> outputStrLn ("Evaluating:\n" ++ show ast))
  pure maybeAST2

getTermNode :: String -> InputT IO (Maybe TermNode)
getTermNode txt = do
  case parser $ alexScanTokens txt of
    Left e    -> outputStrLn e >>= \_ -> pure Nothing
    Right ast -> validateDeBruijn $ genIndex ast

validateDeBruijn :: TermNode -> InputT IO (Maybe TermNode)
validateDeBruijn ast =
  if areIndexesBroken ast
    then outputStrLn "There was an error with the internal representation" >>= \_ -> pure Nothing
    else pure (Just ast)

tryMaybeAST :: Maybe TermNode -> (TermNode -> InputT IO ()) -> InputT IO ()
tryMaybeAST maybeAST act = case maybeAST of Just ast -> act ast; Nothing -> pure ()

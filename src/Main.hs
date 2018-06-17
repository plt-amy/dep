{-# LANGUAGE FlexibleContexts, ConstraintKinds, ApplicativeDo, ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.Map.Strict as Map

import Control.Monad.State.Strict
import Control.Exception

import System.Console.Haskeline
import System.Environment
import System.IO

import Data.List
import Data.Char

import Parser

import Term.Context
import Term.Value
import Term

import Infer

main :: IO ()
main = flip evalStateT (Ctx mempty mempty) $ runInputT settings run where
  run :: InputT (StateT (Ctx Value) IO) ()
  run = do
    x <- liftIO getArgs
    case x of
      [file] -> load file
      ["-l", file] -> load file *> loop
      _ -> loop
  settings :: Settings (StateT (Ctx Value) IO)
  settings = Settings { historyFile = Just ".dep", complete = completion, autoAddHistory = True }

  loop :: InputT (StateT (Ctx Value) IO) ()
  loop = do
    stmt <- getInputLine "Π "
    case stmt of
      Nothing -> pure ()
      Just "" -> loop
      Just x | ":l " `isPrefixOf` x -> load (drop (length ":l ") x) *> loop
      Just i -> runStmt loop i

  printError er = do
    outputStrLn "\x1b[31merror:\x1b[0m"
    outputStr . unlines . map ("  " ++) . lines $ er

  runStmt k i =
    case parseStmt "<interactive>" i of
      Left e -> outputStrLn e *> k 
      Right (Define v ty x) -> do
        ctx <- lift get
        let cont = case ty of
                     Just t -> \e -> do
                       _ <- inferLevel ctx t
                       check ctx e t
                       pure t
                     Nothing -> infer ctx
        case runTc (cont x) of
          Left e -> printError (e ++ "\nIn definition of " ++ show v) *> k
          Right ty -> do
            lift $ put Ctx{ typings = Map.insert v ty (typings ctx)
                          , bindings = Map.insert v (eval (bindings ctx) x)
                                        (bindings ctx) }
            k
      Right (Postulate v ty) -> do
        ctx <- lift get
        case runTc (inferLevel ctx ty) of
          Left e -> printError (e ++ "\nIn definition of " ++ show v) *> k
          Right _ -> do
            lift $ put ctx{ typings = Map.insert v ty (typings ctx) }
            k
      Right (Infer ex) -> do
        ctx <- lift get
        case runTc (infer ctx ex) of
          Left e -> printError e *> k
          Right ty -> do
            outputStrLn (" " ++ show (reduce ctx ex) ++ " : " ++ show ty)
            k

  load :: String -> InputT (StateT (Ctx Value) IO) ()
  load x = do
    acs <- liftIO . withFile x ReadMode $ \h -> do
      l <- lines <$> hGetContents h
      !_ <- evaluate (length l)
      pure . map (runStmt (pure ())) . filter (/= "") . map (dropWhile isSpace) $ l
    sequence_ acs

  completion :: CompletionFunc (StateT (Ctx Value) IO)
  completion = completeWord Nothing " \t\v" $ \x -> do
    ctx <- get
    let mp = Map.filterWithKey (\k _ -> x `isPrefixOf` k) (bindings ctx)
    pure (map simpleCompletion (Map.keys mp))

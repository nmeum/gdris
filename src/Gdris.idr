module Gdris

import Gopher
import Parser

import System
import System.File

import Data.Fin
import Data.Vect
import Data.Strings

import Network.Socket

-- Valid REPL input commands.
data Command = Goto Integer | Exit | Unknown

-- REPL execution context.
record Context where
    constructor MkCtx
    menu   : (List Item)

showMenu : (List Item) -> String
showMenu xs = trim $ showMenu' 0 xs
    where
        showMenu' : Nat -> (List Item) -> String
        showMenu' _ [] = ""
        showMenu' n (x :: xs) = show n ++ ": " ++ (show x) ++ "\n" ++ (showMenu' (n + 1) xs)

-- XXX: If this returns a Maybe Monad the totality checker doesn't terminate
lineToCmd : String -> Command
lineToCmd input = case words input of
    ["goto", x] => Goto $ cast x
    ["exit"]    => Exit
    _ => Unknown

readCommand : String -> IO Command
readCommand prompt
   = do eof <- fEOF stdin
        if eof
          then pure Exit
          else do putStr prompt
                  fflush stdout
                  x <- getLine
                  pure $ lineToCmd x

getItem : Context -> Integer -> Maybe Item
getItem ctx n = let idx = integerToFin n (length ctx.menu) in
    case idx of
        Just f  => Just $ index f (fromList ctx.menu)
        Nothing => Nothing

createClient : Address -> IO (Either ResultCode Socket)
createClient addr = do
    Right sock <- socket AF_INET Stream 0
        | Left fail => pure $ Left fail
    res <- connect sock (Hostname $ fst addr) $ (cast $ snd addr)
    if res /= 0
        then pure $ Left res
        else pure $ Right sock

sendAndRecv : Socket -> String -> IO (Either ResultCode String)
sendAndRecv sock input = do
    n <- send sock input
    case n of
        Right _  => do r <- recvMsg sock
                       case r of
                        Right x => pure $ Right x
                        Left err => pure $ Left err
        Left err => pure $ Left err

makeReq : Address -> String -> IO (Either ResultCode String)
makeReq addr input = do
    sock <- createClient addr
    case sock of
        Right s => do out <- sendAndRecv s (input ++ "\r\n")
                      close s
                      pure out
        Left err => pure $ Left err

newCtx : Context -> (List Item) -> Context
newCtx _ items = MkCtx items

execTrans : Context -> Item -> IO (Context, String)
execTrans ctx (MkItem Document _ s addr) = do
    out <- makeReq addr s
    pure $ MkPair ctx $ case out of
        Right out => out
        Left err  => "makeReq failed: " ++ show err
execTrans ctx (MkItem Directory _ s addr) = do
    out <- makeReq addr s
    case out of
        Right o => do i <- parseAll o
                      pure $ case i of
                         Right it => MkPair (newCtx ctx it) (showMenu it)
                         Left err => MkPair ctx $ show err
        Left err => pure $ MkPair ctx $ "makeReq failed: " ++ show err
execTrans ctx _ = do
    pure $ MkPair ctx "not implemented"

execGoto : Context -> Integer -> IO (Context, String)
execGoto ctx n =
    case item of
        Just i  => do execTrans ctx i
        Nothing => do pure $ (MkPair ctx "unknown menu item")
    where
        item : Maybe Item
        item = getItem ctx n

runREPL : Context -> IO ()
runREPL ctx = do
    cmd <- readCommand "> "
    case cmd of
        Goto x  => do p <- execGoto ctx x
                      putStrLn $ snd p
                      runREPL $ fst p
        Unknown => do putStrLn "unknown command"
                      runREPL ctx
        Exit => pure ()

    pure ()

runClient : Address -> IO ()
runClient addr = do
    Right out <- makeReq addr ""
        | Left err => do putStrLn $ "makeReq failed: " ++ show err
                         exitFailure
    Right items <- parseAll out
        | Left err => do putStrLn $ "Parsing failed: " ++ show err
                         exitFailure

    ctx <- pure $ MkCtx items
    putStrLn $ showMenu ctx.menu

    runREPL ctx

main : IO ()
main = do
    [prog, host, port] <- getArgs
        | _ => do putStrLn "USAGE: gdris HOST PORT"
                  exitFailure

    runClient (MkPair host (stringToNatOrZ port))

module Gdris

import Gopher
import Parser

import System
import System.File

import Data.Fin
import Data.Vect
import Data.Strings
import Data.String.Parser

import Network.Socket

-- Valid REPL input commands.
data Command = Goto Integer | Exit | Unknown

-- REPL execution context.
record Context where
    constructor MkCtx
    socket : Socket
    menu   : (List Item)

showMenu : (List Item) -> String
showMenu xs = showMenu' 0 xs
    where
        showMenu' : Nat -> (List Item) -> String
        showMenu' _ [] = ""
        showMenu' n (x :: xs) = show n ++ ": " ++ (show x) ++ (showMenu' (n + 1) xs)

-- XXX: If this returns a Maybe Monad the totality checker doesn't terminate
lineToCmd : String -> Command
lineToCmd input = case words input of
    ["goto", x] => Goto $ cast x
    ["exit"]    => Exit
    _ => Unknown

readCommand : HasIO io => String -> io Command
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

sendAndRecv : HasIO io => Socket -> Selector -> io (Maybe String)
sendAndRecv sock selector = do
    n <- send sock selector
    case n of
        Right _  => do r <- recvAll sock
                       case r of
                        Right x => pure $ Just x
                        Left  _ => pure Nothing
        Left err => pure Nothing

execGoto : HasIO io => Context -> Integer -> io Context
execGoto ctx n =
    case item of
        Just (MkItem t d s a) => do putStrLn $ "desc: " ++ d
                                    pure $ ctx
        Nothing => do putStrLn "unknown menu item"
                      pure $ ctx
    where
        item : Maybe Item
        item = getItem ctx n

runREPL : HasIO io => Context -> io ()
runREPL ctx = do
    cmd <- readCommand "> "
    case cmd of
        Goto x  => do ctx <- execGoto ctx x
                      runREPL ctx
        Unknown => do putStrLn "unknown command"
                      runREPL ctx
        Exit => pure ()

    pure ()

createClient : HasIO io => Address -> io (Maybe Socket)
createClient addr = do
    Right sock <- socket AF_INET Stream 0
        | Left fail => pure $ Nothing
    res <- connect sock (Hostname $ fst addr) $ (cast $ snd addr)
    if res /= 0
        then pure $ Nothing
        else pure $ Just sock

runClient : Address -> IO ()
runClient addr = do
    Just s <- createClient addr
        | _ => do putStrLn "Failed to create socket"
                  exitFailure

    Just out <- sendAndRecv s "\r\n"
        | _ => do putStrLn "Failed to retrieve initial menu"
                  exitFailure
    Right (items, _) <- parseT parseItems out
        | Left err => do putStrLn $ "Parsing failed: " ++ show err
                         exitFailure

    ctx <- pure $ MkCtx s items
    putStrLn $ showMenu ctx.menu

    runREPL ctx

main : IO ()
main = do
    [prog, host, port] <- getArgs
        | _ => do putStrLn "USAGE: gdris HOST PORT"
                  exitFailure

    runClient (MkPair host (stringToNatOrZ port))

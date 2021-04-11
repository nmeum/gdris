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
    socket : Socket
    menu   : (List Item)

-- XXX: If this returns a Maybe Monad the totality checker doesn't terminate
lineToCmd : String -> Command
lineToCmd input = case words input of
    ["goto", x] => let n = stringToNatOrZ x in
                      if n == 0
                         then Unknown -- Gotos must start 1
                         else Goto $ natToInteger n
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

    runREPL (MkCtx s [])

main : IO ()
main = do
    [prog, host, port] <- getArgs
        | _ => do putStrLn "USAGE: gdris HOST PORT"
                  exitFailure

    runClient (MkPair host (stringToNatOrZ port))

module Gdris

import Gopher
import Parser

import System
import System.REPL
import Data.String.Parser
import Data.Strings

internalState : Type
internalState = String

processInput : internalState -> String -> Maybe (String, internalState)
processInput _ _ = Nothing

runClient : Address -> IO ()
runClient addr = do
	putStrLn ("host: " ++ (fst addr))
	putStrLn ("port: " ++ (show (snd addr)))
	replWith "" "> " processInput

main : IO ()
main = do
	[prog, host, port] <- getArgs
		| _ => do putStrLn "USAGE: gdris HOST PORT"
		          exitFailure
	runClient (MkPair host (stringToNatOrZ port))
	pure ()

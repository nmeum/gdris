module Gdris

import Gopher
import Parser

import System
import Data.String.Parser
import Data.Strings

runClient : Address -> IO ()
runClient addr = do
	putStrLn ("host: " ++ (fst addr))
	putStrLn ("port: " ++ (show (snd addr)))

main : IO ()
main = do
	[prog, host, port] <- getArgs
		| _ => do putStrLn "USAGE: gdris HOST PORT"
		          exitFailure
	runClient (MkPair host (stringToNatOrZ port))
	pure ()

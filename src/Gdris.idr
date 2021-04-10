module Gdris

import Gopher
import Parser

import System
import System.REPL

import Data.String.Parser
import Data.Strings

import Network.Socket
import Network.Socket.Data
import Network.Socket.Raw

internalState : Type
internalState = Socket

processInput : internalState -> String -> Maybe (String, internalState)
processInput _ _ = Nothing

createClient : HasIO io => Address -> io (Maybe Socket)
createClient addr = do
  Right sock <- socket AF_INET Stream 0
    | Left fail => pure $ Nothing
  res <- connect sock (Hostname "localhost") 8000 -- TODO: take from tuple
  if res /= 0
    then pure $ Nothing
    else pure $ Just sock

runClient : Address -> IO ()
runClient addr = do
	putStrLn ("host: " ++ (fst addr))
	putStrLn ("port: " ++ (show (snd addr)))

	(Just s) <- createClient addr
	  | _ => do putStrLn "Failed to create socket"
	            exitFailure
	replWith s "> " processInput

main : IO ()
main = do
	[prog, host, port] <- getArgs
		| _ => do putStrLn "USAGE: gdris HOST PORT"
		          exitFailure
	runClient (MkPair host (stringToNatOrZ port))
	pure ()

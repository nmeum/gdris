module Client

import Gopher
import Network.Socket

public export
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

public export
makeReq : Address -> String -> IO (Either ResultCode String)
makeReq addr input = do
    sock <- createClient addr
    case sock of
        Right s => do out <- sendAndRecv s (input ++ "\r\n")
                      close s
                      pure out
        Left err => pure $ Left err

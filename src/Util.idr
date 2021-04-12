module Util

import Data.Strings
import Network.Socket

bufsiz : ByteLength
bufsiz = 4096

delim : String
delim = "\r\n.\r\n"

-- Ugly hack to read an entire gopher message form the socket.
export
recvMsg : HasIO io => (sock : Socket) -> io (Either SocketError String)
recvMsg sock = recvRec sock []
  where
    partial
    recvRec : Socket -> List String -> io (Either SocketError String)
    recvRec sock acc = do res <- recv sock bufsiz
                          case res of
                            Left err => if err == 0
                                            then pure $ Right (concat acc)
                                            else pure $ Left err
                            Right (str, n) => if isSuffixOf delim str || n == 0
                                                then pure (Right (concat $ acc ++ [str]))
                                                else recvRec sock (str :: acc)

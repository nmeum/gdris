module Gopher

import Builtin
import Data.Strings
import Network.Socket

-- Valid gopher item types as defined in RFC 1436.
public export
data ItemType = Document |
    Directory |
    PhoneBook |
    Error |
    BinHex |
    PCDOS |
    UnixUuencoded |
    SearchService |
    TelnetSession |
    Binary |
    Duplicated |
    Gif |
    Image |
    Tn3270Session |
    InfoLine |
    HTML

public export
Show ItemType where
    show Document  = "Document"
    show Directory = "Directory"
    show PhoneBook = "Phonebook"
    show Error = "Error"
    show BinHex = "BINHEX"
    show PCDOS = "PCDOS"
    show UnixUuencoded = "Uuencoded"
    show SearchService = "SearchService"
    show TelnetSession = "Telnet"
    show Binary = "Binary"
    show Duplicated = "Duplicated"
    show Gif = "Gif"
    show Image = "Image"
    show Tn3270Session ="Tn3270"
    show InfoLine = "Info"
    show HTML = "HTML"

-- Selector used to retrieve a document from a server.
public export
Selector : Type
Selector = String

-- Address of a server which stores a given document.
public export
Address : Type
Address = (String, Nat)

-- Product type for a Gopher menu entry.
public export
data Item = MkItem ItemType String Selector Address

public export
Show Item where
    show (MkItem ty desc select addr) = "[" ++ show ty ++ "]" ++ "\t" ++ desc ++ "\t"

public export
unmarshalType : Char -> Maybe ItemType
unmarshalType '0' = Just Document
unmarshalType '1' = Just Directory
unmarshalType '2' = Just PhoneBook
unmarshalType '3' = Just Error
unmarshalType '4' = Just BinHex
unmarshalType '5' = Just PCDOS
unmarshalType '6' = Just UnixUuencoded
unmarshalType '7' = Just SearchService
unmarshalType '8' = Just TelnetSession
unmarshalType '9' = Just Binary
unmarshalType '+' = Just Duplicated
unmarshalType 'g' = Just Gif
unmarshalType 'I' = Just Image
unmarshalType 'i' = Just InfoLine
unmarshalType 'T' = Just Tn3270Session
unmarshalType 'h' = Just HTML
unmarshalType _   = Nothing

public export
isUnAscii : Char -> Bool
isUnAscii '\r' = False
isUnAscii '\n' = False
isUnAscii '\t' = False
isUnAscii '\0' = False
isUnAscii _    = True

public export
isHostPart : Char -> Bool
isHostPart c = (isUnAscii c) && (c /= '.')

partial
recvMsg' : HasIO io => Socket -> List String -> io (Either SocketError String)
recvMsg' sock acc = do
    res <- recv sock bufsiz
    case res of
        Left err => if err == 0
                      then pure $ Right (concat acc)
                      else pure $ Left err
        Right (str, n) => if isSuffixOf delim str
                            then pure (Right (concat $ acc ++ [str]))
                            else recvMsg' sock (acc ++ [str])
    where
        bufsiz : ByteLength
        bufsiz = 4096

        delim : String
        delim = "\r\n.\r\n"

export
recvMsg : HasIO io => (sock : Socket) -> io (Either SocketError String)
recvMsg sock = recvMsg' sock []

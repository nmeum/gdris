module Gopher

import Builtin
import Data.String.Parser

-- Valid gopher item types as defined in RFC 1436.
data ItemType = Document | Directory | SearchService

-- Selector used to retrieve a document from a server.
Selector : Type
Selector = String

-- Address of a server which stores a given document.
Address : Type
Address = (String, Nat)

-- Product type for a Gopher menu entry.
data Item = MkItem ItemType String Selector Address

marshalType : ItemType -> Char
marshalType Document      = '0'
marshalType Directory     = '1'
marshalType SearchService = '7'

unmarshalType : Char -> Maybe ItemType
unmarshalType '0' = Just Document
unmarshalType '1' = Just Directory
unmarshalType '7' = Just SearchService
unmarshalType _   = Nothing

isUnAscii : Char -> Bool
isUnAscii '\r' = False
isUnAscii '\n' = False
isUnAscii '\t' = False
isUnAscii '\0' = False
isUnAscii _    = True

parseUnAscii : ParseT IO Char
parseUnAscii = satisfy isUnAscii <?> "any character except: Tab, CR-LF, NUL"

parseType : ParseT IO ItemType
parseType = do
	res <- parseUnAscii
	case (unmarshalType res) of
		Just a  => pure $ a
		Nothing => fail "unknown item type"

parseDesc : ParseT IO String
parseDesc = do
	res <- (many parseUnAscii)
	pure $ pack res

parseSelector : ParseT IO Selector
parseSelector = parseDesc

parseHost' : ParseT IO String
parseHost' = do
	r1 <- (many (satisfy (\x => x /= '.')))
	r2 <- (string ".")
	pure $ ((pack r1) ++ r2)
parseHost : ParseT IO String
parseHost = do
	name <- (many parseHost')
	tld  <- (many (satisfy (\x => x /= '.')))
	pure $ (concat name) ++ (pack tld)

parsePort : ParseT IO Nat
parsePort = natural

parseTab : ParseT IO Char
parseTab = satisfy (\x => x == '\t') <?> "tab character"

parseDelim : ParseT IO String
parseDelim = string "\r\n"

parseItem : ParseT IO Item
parseItem = do
	type <- parseType
	desc <- parseDesc
	ignore $ parseTab
	select <- parseSelector
	ignore $ parseTab
	host <- parseHost
	ignore $ parseTab
	port <- parsePort
	ignore $ parseDelim
	pure $ MkItem type desc select (MkPair host port)

module Parser

import Gopher
import Data.String.Parser

many1 : Monad m => ParseT m a -> ParseT m (List a)
many1 p = do
	x  <- p
	xs <- (many p)
	pure $ (x :: xs)

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
	res <- (many1 parseUnAscii)
	pure $ pack res

parseSelector : ParseT IO Selector
parseSelector = parseDesc

parseHost' : ParseT IO String
parseHost' = do
	r1 <- many1 $ satisfy isHostPart
	r2 <- (string ".")
	pure $ ((pack r1) ++ r2)
parseHost : ParseT IO String
parseHost = do
	name <- (many1 parseHost')
	tld  <- many1 $ satisfy isHostPart
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
	pure $ MkItem type desc select (MkPair host port)

public export
parseItems : ParseT IO (List Item)
parseItems = parseItem `sepBy` parseDelim

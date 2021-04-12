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
		Nothing => fail $ "unknown item type: " ++ show res

parseDesc : ParseT IO String
parseDesc = do
	res <- many parseUnAscii
	pure $ pack res

parseSelector : ParseT IO Selector
parseSelector = parseDesc

parseHost' : ParseT IO String
parseHost' = do
	r1 <- many $ satisfy isHostPart
	r2 <- (string ".")
	pure $ ((pack r1) ++ r2)
parseHost : ParseT IO String
parseHost = do
	name <- many parseHost'
	tld  <- many $ satisfy isHostPart
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

parseItems : ParseT IO (List Item)
parseItems = many1 parseItem

public export
parseAll : String -> IO (Either String (List Item))
parseAll input = do
	r <- parseT parseItems input
	pure $ case r of
		Left err => Left err
		Right (items, n) => if n /= (cast $ length input)
			then Left $ "not all data consumed: " ++ (show (n - (cast $ length input)))
			else Right items

module Gopher

import Builtin

-- Valid gopher item types as defined in RFC 1436.
public export
data ItemType = Document | Directory | SearchService

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
marshalType : ItemType -> Char
marshalType Document      = '0'
marshalType Directory     = '1'
marshalType SearchService = '7'

public export
unmarshalType : Char -> Maybe ItemType
unmarshalType '0' = Just Document
unmarshalType '1' = Just Directory
unmarshalType '7' = Just SearchService
unmarshalType _   = Nothing

public export
isUnAscii : Char -> Bool
isUnAscii '\r' = False
isUnAscii '\n' = False
isUnAscii '\t' = False
isUnAscii '\0' = False
isUnAscii _    = True

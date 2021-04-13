module Menu

import Gopher

import Data.Nat
import Data.Strings

-- String used to seperate metadata from primary data in menu entries.
sep : String
sep = " │ "

-- Returns the maximum amount of characters required to display an entry type.
maxTypeWidth : (List Item) -> Nat
maxTypeWidth = foldr (\(MkItem ty _ _ _), acc => max (length $ show ty) acc) 0

-- Adds spaces as padding to the end of the given String.
padToWidth : String -> Nat -> String
padToWidth str n = if (Strings.length str) >= n
                    then str
                    else padToWidth (str ++ " ") n

-- Returns amount of digits required to display number in decimal.
digitsReq : Nat -> Nat
digitsReq n = if n `div` 10 > 0
    then 1 + (digitsReq $ n `div` 10)
    else 1

public export
showMenu : (List Item) -> String
showMenu xs = trim $ showMenu' (digitsReq $ length xs) (maxTypeWidth xs) 0 xs
    where
        showItem : Nat -> Item -> String
        showItem max (MkItem ty desc _ _) = (padToWidth (show ty) max) ++ sep ++ desc

        showMenu' : Nat -> Nat -> Nat -> (List Item) -> String
        showMenu' _ _ _ [] = ""
        showMenu' maxNum maxTy n (x :: xs) = (padToWidth (show n) maxNum) ++
                                             " " ++ showItem maxTy x ++ "\n" ++
                                             (showMenu' maxNum maxTy (n + 1) xs)

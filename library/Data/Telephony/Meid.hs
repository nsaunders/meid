module Data.Telephony.Meid (Meid (HexMeid, DecMeid), Data.Telephony.Meid.showHex, showDec) where

import Data.Char (toUpper)
import Numeric (readHex, showHex)

data Meid = HexMeid [Char] | DecMeid [Char]
showHex (HexMeid m) = m
showHex (DecMeid m) = uppercase $ (hex (firstPart $ DecMeid m)) ++ (hex (secondPart $ DecMeid m))
showDec (DecMeid m) = m
showDec (HexMeid m) = (++)
    (unwrap $ readHex (firstPart $ HexMeid m))
    (zeropad 8 (unwrap $ readHex (secondPart $ HexMeid m)))

uppercase (x:xs) = (toUpper x) : (uppercase xs)
uppercase [] = []
hex n = Numeric.showHex (read n) ""
unwrap [(a, b)] = show a
zeropad n s = if (length s) == n then s else zeropad n ("0" ++ s)

firstPart :: Meid -> [Char]
firstPart (DecMeid m) = take 10 m
firstPart (HexMeid m) = take 8 m

secondPart :: Meid -> [Char]
secondPart (DecMeid m) = reverse (take 8 $ reverse m)
secondPart (HexMeid m) = reverse (take 6 $ reverse m)
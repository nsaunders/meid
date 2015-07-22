{-|

Module      : Data.Telephony.Meid
Description : Contains a data structure that represents a Mobile Equipment Identifier and
              various MEID functions.
Copyright   : (c) 2015 Nick Saunders
License     : MIT
Maintainer  : nick@saunde.rs

-}
module Data.Telephony.Meid (Meid (HexMeid, DecMeid), Data.Telephony.Meid.showHex, showDec) where

import Data.Char (toUpper)
import Numeric (readHex, showHex)

-- | The 'Meid' type represents a Mobile Equipment Identifier (MEID).
data Meid
    = HexMeid [Char] -- ^ Constructs a MEID given a MEID string in hexadecimal format.
    | DecMeid [Char] -- ^ Constructs a MEID given a MEID string in decimal format.

-- | The 'showHex' function shows a MEID in hexadecimal format.
showHex :: Meid -- ^ The MEID to show in hexadecimal format
        -> [Char] -- ^ The MEID in hexadecimal format
showHex (HexMeid m) = m
showHex (DecMeid m) = uppercase $ (hex (firstPart $ DecMeid m)) ++ (hex (secondPart $ DecMeid m))

-- | The 'showDec' function shows a MEID in decimal format.
showDec :: Meid -- ^ The MEID to show in decimal format
        -> [Char] -- ^ The MEID in decimal format
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
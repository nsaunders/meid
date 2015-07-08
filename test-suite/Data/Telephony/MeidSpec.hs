module Data.Telephony.MeidSpec (spec) where

import Data.Telephony.Meid
import Test.Hspec

spec = do
    describe "showHex" $ do
        it "should show a HEX MEID as a HEX MEID" $ do
            (showHex $ HexMeid "A100003C5433F4") `shouldBe` "A100003C5433F4"
        it "should show a DEC MEID as a HEX MEID" $ do
            (showHex $ DecMeid "270113183605518324") `shouldBe` "A100003C5433F4"
    describe "showDec" $ do
        it "should show a DEC MEID as a DEC MEID" $ do
            (showDec $ DecMeid "270113183605518324") `shouldBe` "270113183605518324"
        it "should show a HEX MEID as a DEC MEID" $ do
            (showDec $ HexMeid "A100003C5433F4") `shouldBe` "270113183605518324"
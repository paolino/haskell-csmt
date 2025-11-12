module CSMT.Backend.RocksDB.KeySpec (spec) where

import CSMT.Backend.RocksDB.Key
    ( RocksPathKey (..)
    , indirectToRocksValue
    , keyToRocksPathKey
    , rocksPathKeyToKey
    , rocksValueToIndirect
    )
import CSMT.Hashes (mkHash)
import CSMT.Interface
    ( Direction (L, R)
    , Indirect (Indirect, jump, value)
    , Key
    )
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Gen, Testable (property), forAll)
import Test.QuickCheck.Gen (elements, listOf)

genKeys :: Gen Key
genKeys = listOf $ elements [L, R]

genValues :: Gen ByteString
genValues = fmap B.pack $ listOf $ elements [0 .. 255]

spec :: Spec
spec = describe "RocksDBKey" $ do
    it "converts empty key to RocksPathKey and back" $ do
        let key = []
            rkey = keyToRocksPathKey key
        rkey
            `shouldBe` RocksPathKey
                { byteString = ""
                , keyLength = 0
                }
        let key' = rocksPathKeyToKey rkey
        key' `shouldBe` key

    it "converts single-direction keys to RocksPathKey and back" $ do
        let keyL = [L]
            rkeyL = keyToRocksPathKey keyL
        rkeyL
            `shouldBe` RocksPathKey
                { byteString = "\x00"
                , keyLength = 1
                }
        rocksPathKeyToKey rkeyL `shouldBe` keyL

    it "converts keys to RocksPathKey and back"
        $ property
        $ forAll genKeys
        $ \key -> do
            let rkey = keyToRocksPathKey key
                key' = rocksPathKeyToKey rkey
            key' `shouldBe` key

    it "converts a fake empty Indirect to RocksDB key and back" $ do
        let indirect = Indirect{jump = [], value = "0" :: ByteString}
            rkey = indirectToRocksValue indirect
        rkey `shouldBe` "\x00\x00\x00\x01\x30"
        let indirect' = rocksValueToIndirect rkey
        indirect' `shouldBe` indirect

    it "converts an Indirect of a Hash to RocksDB key and back"
        $ property
        $ forAll genKeys
        $ \key ->
            forAll genValues $ \value -> do
                let hash = mkHash value
                    indirect = Indirect{jump = key, value = hash}
                    rkey = indirectToRocksValue indirect
                    indirect' = rocksValueToIndirect rkey
                indirect' `shouldBe` indirect

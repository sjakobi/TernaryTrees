{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Main where

import Data.Map.StringMap
import Data.Binary.Serialise.CBOR.Properties
import Data.Serialize
import Test.Hspec
import Test.Hspec.SmallCheck
import Test.SmallCheck.Series

main :: IO ()
main = hspec $ do
  describe "StringMap" $ do
    context "cereal" $ do
      it "decode . encode is id" $
        property $ \m -> decode (encode m) == Right (m :: StringMap Bool)
    context "binary-serialise-cbor" $ do
      it "serialiseIdentity" $
        property $ \m -> serialiseIdentity (m :: StringMap Int)
      it "flatTermIdentity" $
        property $ \m -> flatTermIdentity (m :: StringMap Bool)
      it "hasValidFlatTerm" $
        property $ \m -> hasValidFlatTerm (m :: StringMap Int)

instance Serial m v => Serial m (StringMap v) where
  series = fromList <$> series


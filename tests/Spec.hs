{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Main where

import Data.Map.StringMap
import Data.Map.StringMap.Internal
import Data.Binary.Serialise.CBOR.Properties
import Data.Serialize
import Test.Hspec
import qualified Test.Hspec.SmallCheck as SC
import qualified Test.QuickCheck.Property as QC
import Test.QuickCheck
import Test.SmallCheck.Series

main :: IO ()
main = hspec $ do
  describe "StringMap" $ parallel $ do

    context "cereal" $ do
      it "decode . encode is id (smallcheck)" $
        SC.property $ \m -> decode (encode m) == Right (m :: StringMap Bool)
      it "decode . encode is id (QuickCheck)" $
        QC.property $ \m -> decode (encode m) == Right (m :: StringMap Bool)

    context "binary-serialise-cbor" $ do
      describe "serialiseIdentity" $ do
        it "with smallcheck" $
          SC.property $ \m -> serialiseIdentity (m :: StringMap Int)
        it "QuickCheck" $
          QC.property $ \m -> serialiseIdentity (m :: StringMap Int)
        it "Node ch End End End" $
          serialiseIdentity (Node 'ö' End End End :: StringMap Int)


      it "flatTermIdentity" $
        SC.property $ \m -> flatTermIdentity (m :: StringMap Bool)
      it "hasValidFlatTerm" $
        SC.property $ \m -> hasValidFlatTerm (m :: StringMap Int)

instance Serial m v => Serial m (StringMap v) where
  series = fromList <$> series

instance Arbitrary v => Arbitrary (StringMap v) where
  arbitrary = fromList <$> arbitrary

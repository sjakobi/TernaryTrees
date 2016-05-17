{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module Main where

import Data.Map.StringMap
import Data.Map.StringMap.Internal
import Data.Binary.Serialise.CBOR.Properties
import Data.Serialize
import Test.Hspec
import qualified Test.Hspec.SmallCheck as SC
import qualified Test.QuickCheck.Property as QC
import qualified Test.SmallCheck as SC
import Test.QuickCheck
import Test.SmallCheck.Series

main :: IO ()
main = hspec $ do
  describe "StringMap" $ parallel $ do

    context "cereal" $ do
      describe "decode . encode is id" $
        smallAndQuick $ \m -> decode (encode m) == Right (m :: StringMap Bool)

    context "binary-serialise-cbor" $ do
      describe "serialiseIdentity" $ do
        smallAndQuick $ \m -> serialiseIdentity (m :: StringMap Int)
        it "Node ch End End End" $
          serialiseIdentity (Node 'ö' End End End :: StringMap Int)

      describe "flatTermIdentity" $ do
        smallAndQuick $ \m -> flatTermIdentity (m :: StringMap Bool)

      describe "hasValidFlatTerm" $ do
        smallAndQuick $ \m -> hasValidFlatTerm (m :: StringMap Int)

  describe "CompactStringMap" $ parallel $ do

    context "binary-serialise-cbor" $ do
      describe "serialiseIdentity" $ do
        smallAndQuick $ \m -> serialiseIdentity (m :: CompactStringMap Int)
      describe "flatTermIdentity" $ do
        smallAndQuick $ \m -> flatTermIdentity (m :: CompactStringMap Bool)
      -- no valid flat term here!

smallAndQuick :: (SC.Testable IO prop, Testable prop) => prop -> SpecWith ()
smallAndQuick testable = do
  it "with smallcheck" $
    SC.property testable
  it "with QuickCheck" $
    QC.property testable

instance Serial m v => Serial m (StringMap v) where
  series = fromList <$> series

instance Arbitrary v => Arbitrary (StringMap v) where
  arbitrary = fromList <$> arbitrary

instance Serial m v => Serial m (CompactStringMap v) where
  series = CompactStringMap <$> series

instance Arbitrary v => Arbitrary (CompactStringMap v) where
  arbitrary = CompactStringMap <$> arbitrary

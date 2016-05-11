{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Main where

import Data.Map.StringMap
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

instance Serial m v => Serial m (StringMap v) where
  series = fromList <$> series


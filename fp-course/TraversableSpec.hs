{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.TraversableSpec where


import           Test.Hspec        (Spec, describe, it, shouldBe)

import           Course.Core
import           Course.ExactlyOne (ExactlyOne (ExactlyOne))
import           Course.Functor    ((<$>))
import           Course.List       (List (..), length, listh, reverse)
import           Course.Optional   (Optional (..))

-- import           Course.Extend     (cojoin, (<<=))
import           Course.Traversable (traverse, sequenceA)
spec :: Spec
spec = do
  describe "sequenceA Tests" $ do
    it "List of ExactlyOne instance" $ 
      (sequenceA (ExactlyOne 7 :. ExactlyOne 8 :. ExactlyOne 9 :. Nil)) 
      `shouldBe` (ExactlyOne $ listh [7,8,9])
    it "Optional of ExactlyOne Instance" $      
      (sequenceA (Full (ExactlyOne 7))) `shouldBe` (ExactlyOne (Full 7))
    it "Optional Instance" $
      (sequenceA (Full (*10)) 6) `shouldBe` (Full 60)

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.ParserSpec where

import           Test.Hspec        (Spec, describe, it, shouldBe)
import           Course.Core
-- import           Course.ExactlyOne (ExactlyOne (ExactlyOne))
import           Course.Person
import           Course.Functor    ((<$>))
import           Course.List       (List (..), length, listh, reverse)
-- import           Course.Optional   (Optional (..))
import           Course.Applicative ((*>), (<*))
import           Course.Monad      ((=<<))
import           Course.Parser 

import Data.Char(isUpper)

spec :: Spec
spec = do
  describe "character tests" $ do
    it "parse character abc" $ 
      (parse character $ listh "abc") `shouldBe` (Result (listh "bc") 'a')
    it "parse character empty" $ 
      (isErrorResult (parse character Nil)) `shouldBe` True

  describe "fmap" $ do
    it "toUpper" $
      (parse (toUpper <$> character) $ listh "amz")
      `shouldBe` (Result (listh "mz") 'A')

  describe "value parser" $ do
    it "const 3" $
      (parse (valueParser 3) $ listh "abc")
      `shouldBe` (Result (listh "abc") 3)
       
  describe "bind" $ do
    it "bind1" $
      parse ((\c -> if c == 'x' then character else valueParser 'v') =<< character) (listh "a")
      `shouldBe` Result Nil 'v'
    it "bind2" $
      parse ((\c -> if c == 'x' then character else valueParser 'v') =<< character) (listh "abc")
      `shouldBe` Result (listh "bc") 'v'
    it "bind3" $
      (isErrorResult
       (parse ((\c -> if c == 'x' then character else valueParser 'v') =<< character) Nil)) `shouldBe` True
    it "bind4" $ 
       (isErrorResult (parse ((\c -> if c == 'x' then character else valueParser 'v') =<< character) $ listh "x")) `shouldBe` True

  describe "alterantive |||" $ do
    it "right" $
      parse (character ||| valueParser 'v') Nil
      `shouldBe` Result Nil 'v'
    it "right2" $
      parse (constantParser UnexpectedEof ||| valueParser 'v') Nil
      `shouldBe` Result Nil 'v'
    it "left" $
      (parse (character ||| valueParser 'v') $ listh "abc")
      `shouldBe` Result (listh "bc") 'a'

  describe "satisfy" $ do
    it "isUpper" $
      parse (satisfy isUpper) (listh "Abc")
      `shouldBe` Result (listh "bc") 'A'
    it "not isUpper" $
      isErrorResult (parse (satisfy isUpper) $ listh "abc")
      `shouldBe` True

  describe "list" $ do 
    it "empty list" $
      parse (list character) Nil
      `shouldBe` Result Nil Nil
    it "valueParser" $
      (parse (list (character *> valueParser 'v')) (listh "abc"))
      `shouldBe` (Result Nil $ listh "vvv")

  describe "list1" $ do
    it "chars" $
      parse (list1 (character *> valueParser 'v')) (listh "abc")
      `shouldBe`(Result Nil $ listh "vvv")
    it "error" $
      isErrorResult (parse (list1 (character *> valueParser 'v')) Nil)
      `shouldBe` True

  describe "sequence" $ do
    it "sequence1" $
      parse (sequenceParser (character :. is 'x' :. upper :. Nil)) (listh "axCdef")
     `shouldBe` Result (listh "def") (listh "axC")
    it "sequence2" $
      isErrorResult (parse (sequenceParser (character :. is 'x' :. upper :. Nil)) $ listh "abCdef")
      `shouldBe` True

  describe "thisMany" $ do
    it "4 times" $
      parse (thisMany 4 upper) (listh "ABCDef")
      `shouldBe` Result (listh "ef") (listh "ABCD")

  describe "names" $ do
    it "first name" $
      parse firstNameParser (listh "Abc")
      `shouldBe` Result Nil (listh "Abc")
    it "last name" $
      parse surnameParser (listh "Abcdef") 
      `shouldBe` Result Nil (listh "Abcdef")
    it "last name fail" $
      isErrorResult (parse surnameParser $ listh "Abc")
      `shouldBe` True

  describe "phone" $ do
     it "phone body" $
       parse phoneBodyParser (listh "123-4a56")
       `shouldBe` Result (listh "a56") (listh "123-4")
     it "phone" $
       parse phoneParser (listh "123-456#abc")
       `shouldBe` Result (listh "abc") (listh "123-456")

  describe "person" $ do
    it "empty" $
      isErrorResult (parse personParser Nil)
      `shouldBe` True
    it "error 1" $ do
      isErrorResult (parse personParser $ listh "12x Fred Clarkson y 123-456.789#")
      `shouldBe` True
    it "error 2" $ do
      isErrorResult (parse personParser $ listh "123 Fred Clarkson y -123-456.789#")
      `shouldBe` True
    it "complete" $ do
      parse personParser (listh "123  Fred   Clarkson    y     123-456.789#")
      `shouldBe` Result Nil (Person 123 (listh "Fred") (listh "Clarkson") True (listh "123-456.789"))

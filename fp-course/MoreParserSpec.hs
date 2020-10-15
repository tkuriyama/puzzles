{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.MoreParserSpec where

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
import           Course.MoreParser 

import Data.Char(isUpper)

spec :: Spec
spec = do
  describe "spaces" $ do
    it "zero" $ 
      parse spaces (listh "abc") `shouldBe` Result (listh "abc") Nil
    it "some" $ 
      parse spaces (listh "   abc") `shouldBe` Result (listh "abc") (listh "   ")

  describe "tok" $ do
    it "with space" $
      parse (tok (is 'a')) (listh "a bc") `shouldBe` Result (listh "bc") 'a'
    it "without space" $
      parse (tok (is 'a')) (listh "abc") `shouldBe` Result (listh "bc") 'a'
    it "quote" $
      parse quote (listh "\"abc") `shouldBe` Result (listh "abc") '\"'
      

  describe "string" $ do
    it "substring" $
      parse (string $ listh "abc") (listh "abcdef") `shouldBe` Result (listh "def") (listh "abc")
    it "string tok" $
      parse (stringTok $ listh "abc") (listh "abc  ") `shouldBe` Result Nil (listh "abc")
    it "strokg tok fail" $ 
      (isErrorResult (parse (stringTok $ listh "abc") $ listh "bc  ")) `shouldBe` True

  describe "option" $ do
    it "option" $
      parse (option 'x' character) Nil `shouldBe` Result Nil 'x'
    it "normal parse" $
      parse (option 'x' character) (listh "abc") `shouldBe` Result (listh "bc") 'a'

  describe "oneof / noneof" $ do
    it "oneof" $
      isErrorResult (parse (oneof $ listh "abcd") $ listh "abc") `shouldBe` False
    it "noneoe" $
      isErrorResult (parse (noneof $ listh "abcd") $ listh "abc") `shouldBe` True

  describe "between" $ do
    it "char between []" $
      parse (between (is '[') (is ']') character) (listh "[a]") `shouldBe` Result Nil 'a'
    it "error" $
      isErrorResult (parse (between (is '[') (is ']') character) (listh "[abc")) `shouldBe` True
    it "with charTok" $
      parse (betweenCharTok '[' ']' character) (listh "[  a]  ") `shouldBe` Result Nil 'a'

  describe "hex" $ do
    it "readHex" $
      parse hex (listh "0a1f") `shouldBe` Result Nil '\2591'
    it "hexu" $
      parse hexu (listh "u0a1f") `shouldBe` Result Nil '\2591'
    it "hexu fail" $
      isErrorResult (parse hexu $ listh "0010") `shouldBe` True

  describe "sepby family" $ do
    it "sepby1 succeed" $
      parse (sepby1 character (is ',')) (listh "a,b,c") `shouldBe` Result Nil (listh "abc")
    it "sepby1 fail" $
      isErrorResult (parse (sepby1 character (is ',')) Nil) `shouldBe` True
    it "sepby nil" $
      parse (sepby character (is ',')) Nil `shouldBe` Result Nil Nil

    it "sepby succeed" $
      parse (sepby character (is ',')) (listh "a") `shouldBe` Result Nil (listh "a")

    it "sepby longer example" $
      parse (sepby character (is ',')) (listh "a,b,c,,def") `shouldBe` Result (listh "def") (listh "abc,")

  describe "satisfies..." $ do
    it "satisfyAll" $
      parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) (listh "ABC") `shouldBe` Result (listh "BC") 'A'

    it "satisfyAny" $ do
      parse (satisfyAny (isLower :. (/= 'X') :. Nil)) (listh "abc") `shouldBe` Result (listh "bc") 'a'

  describe "betweenSepbyComma" $ do
    it "char lower" $
      parse (betweenSepbyComma '[' ']' lower) (listh "[a]") `shouldBe` Result Nil (listh "a")
  
    it "string sep" $ 
      parse (betweenSepbyComma '[' ']' character) (listh "[a,b,c,d,e]") `shouldBe` Result Nil (listh "abcde")

    it "error1" $
      isErrorResult (parse (betweenSepbyComma '[' ']' lower) $ listh "[A]") `shouldBe` True

  it "error2" $
    isErrorResult (parse (betweenSepbyComma '[' ']' lower) (listh "a]")) `shouldBe` True

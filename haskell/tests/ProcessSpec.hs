{-# LANGUAGE OverloadedStrings #-}

module ProcessSpec (spec) where

import           Test.Hspec

import qualified Data.Text                  as T
import           Data.Either                (isRight)

import           Control.Monad.Trans.Either

import           Process

spec :: Spec
spec = do

  describe "runProcess (wanting output)" $ do
    it "fails if the process can't be created" $ do
      r <- runEitherT $ runCommand WantOutput "test" "poop"
      r `shouldSatisfy` isError
    it "fails if the process returns an error" $ do
      r <- runEitherT $ runCommand WantOutput "test" "false"
      r `shouldSatisfy` isError
    it "fails if the process returns successfully but with no output" $ do
      r <- runEitherT $ runCommand WantOutput "test" "true"
      r `shouldSatisfy` isError
    it "succeeds if the process returns successfully and has output" $ do
      r <- runEitherT $ runCommand WantOutput "test" "printf \\#a29cd5"
      r `shouldBe` (Right "#a29cd5")

  describe "runProcess (discarding output)" $ do
    it "fails if the process can't be created" $ do
      r <- runEitherT $ runCommand NoOutput "test" "poop"
      r `shouldSatisfy` isError
    it "fails if the process returns an error" $ do
      r <- runEitherT $ runCommand NoOutput "test" "false"
      r `shouldSatisfy` isError
    it "succeeds if the process returns successfully with no output" $ do
      r <- runEitherT $ runCommand NoOutput "test" "true"
      r `shouldSatisfy` isRight
    it "succeeds if the process returns successfully and has output" $ do
      r <- runEitherT $ runCommand NoOutput "test" "printf \\#a29cd5"
      r `shouldSatisfy` isRight

  where
    isError (Left t) = T.length t > 0
    isError _ = False

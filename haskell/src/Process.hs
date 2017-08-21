{-# LANGUAGE OverloadedStrings #-}

module Process where

import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           Data.Monoid                ((<>))
import           Data.Either.Combinators    (mapLeft)

import           Control.Exception          (try, IOException)
import           Control.Monad.Trans.Either

import           System.IO                  (hGetContents)
import           System.Exit                (ExitCode(..))
import           System.Process

err :: Int -> String -> Text -> EitherT Text IO Text
err exitCode processName errorMsg = left $ "Error while " <> T.pack processName <> ". Exit code: " <> T.pack (show exitCode) <> ". Error: " <> errorMsg

data WantOutput = WantOutput | NoOutput

runCommand :: WantOutput -> String -> String -> EitherT Text IO Text
runCommand wantOutput name process = do
  (_, Just stdout, Just stderr, ph) <- tryIO $ createProcess (shell process) {
      std_out = CreatePipe
    , std_err = CreatePipe
  }
  result <- tryIO $ waitForProcess ph
  errMsg <- tryIO $ T.pack <$> hGetContents stderr
  outMsg <- tryIO $ T.pack <$> hGetContents stdout
  case (result, wantOutput) of
    (ExitSuccess, WantOutput) | T.length outMsg == 0 -> err 0 name "no output"
                              | otherwise            -> right outMsg
    (ExitSuccess, NoOutput)                          -> right ""
    (ExitFailure n, _)        | T.length errMsg == 0 -> err n name "no error message"
                              | otherwise            -> err n name errMsg

tryIO :: IO a -> EitherT Text IO a
tryIO m = EitherT $ mapLeft showIOException <$> try m
  where
    showIOException = T.pack . show :: IOException -> Text

{-# LANGUAGE OverloadedStrings #-}

module Upload where

import qualified Web.Scotty as S 

import Network.Wai.Parse

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T

import System.FilePath ((</>), takeBaseName, takeExtension)
import System.Directory

import Control.Monad.Trans.Either
import Control.Exception
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))

import ParseCNF
import Kiss
import Shell
import Data.Aeson.Encode (encode) 

processSet :: [S.File] -> EitherT T.Text IO [KissCell]
processSet files = do
  file <- getFile files
  let fname = fst file
  let fcontents = snd file
  let staticDir = "static/sets/" <> takeBaseName fname
  tryIO $ B.writeFile ("static/sets" </> fname) fcontents
  tryIO $ createDirectoryIfMissing ("create parents" == "false") staticDir
  unzipFile fname staticDir
  cnf <- getCNF staticDir
  kissData <- getKissData cnf
  let json = "var kissJson = " <> encode kissData
  kissCels <- getKissCels cnf
  -- just using first palette found for now
  kissPalette <- getKissPalette kissData
  tryIO $ B.writeFile (staticDir <> "/setdata.js") json
  convertCels kissPalette (map celName kissCels) staticDir
  return kissCels

tryIO :: IO () -> EitherT T.Text IO ()
tryIO f = do
  result <-liftIO (try f :: IO (Either IOException ()))
  case result of
    Right () -> return ()
    Left ex  -> EitherT $ return $ Left (T.pack $ show ex)

uploadSet :: [S.File] -> FilePath -> EitherT T.Text IO ()
uploadSet files staticDir = do 
  file <- getFile files
  let fname = fst file  
  let fcontents = snd file
  tryIO $ B.writeFile (staticDir </> "sets" </> fname) fcontents

unzipSet :: [S.File] -> FilePath -> EitherT T.Text IO ()
unzipSet files staticDir = do
  file <- getFile files
  let fname = fst file
  let dir = "static/sets/" <> takeBaseName fname
  tryIO $ createDirectoryIfMissing ("create parents" == "false") dir
  --unzipFile fname dir

{--
writeJson :: [S.File] -> FilePath -> EitherT T.Text IO ()
writeJson files staticDir = do
  file <- getFile files
  let fname = fst file
  let dir = "static/sets/" <> takeBaseName fname
  cnf <- getCNF dir
  kissData <- getKissData cnf
  let json = "var kissJson = " <> encode kissData
  tryIO $ B.writeFile (dir <> "/setdata.js") json

convertSet :: [S.File] -> FilePath -> EitherT T.Text IO ()
convertSet files staticDir = do
  file <- getFile files
  let fname = fst file
  let dir = "static/sets/" <> takeBaseName fname
  cnf <- getCNF dir
  kissCels <- getKissCels cnf
  -- just using first palette found for now
  kissPalette <- getKissPalette kissData
  convertCels kissPalette (map celName kissCels) dir

listCels :: [S.File] -> FilePath -> EitherT T.Text IO ()
listCels files staticDir = do
  file <- getFile files
  let fname = fst file
  let dir = "static/sets/" <> takeBaseName fname
  cnf <- getCNF dir
  return (getKissCels cnf)
--}

getFile :: [S.File] -> EitherT T.Text IO (String, B.ByteString)
getFile files = EitherT $ return $
  case files of 
    [(_, b)]  -> Right (BS.unpack (fileName b), fileContent b)
    _         -> Left "Please upload exactly one file."

getRelDir :: [S.File] -> EitherT T.Text IO FilePath
getRelDir files = do
  file <- getFile files
  let fname = fst file
  return $ "sets/" ++ takeBaseName fname

-- for now, only looks at first cnf listed
getCNF :: FilePath -> EitherT T.Text IO String
getCNF dir = do
  files <- liftIO $ getDirectoryContents dir
  let cnfs = filter (\x -> takeExtension x == ".cnf") files 
  case cnfs of
    (x:_)    -> liftIO $ readFile $ dir </> x
    _        -> EitherT $ return $ Left "No configuration file found."

{-# LANGUAGE OverloadedStrings #-}

module Shell where

import           Control.Monad              (void)
import           Control.Monad.Trans.Either
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T

import           Kiss
import           ParseCNF
import           Process

-- Gets the transparency color from a kcf palette.
transColor :: PaletteFilename -> EitherT T.Text IO String
transColor paletteLoc =
  let name = "finding transparency color"
      command = "cel2pnm -t " <> paletteLoc in
  T.unpack <$> runCommand WantOutput name command


-- Gets an indexed color from a kcf palette.
colorByIndex :: Int -> PaletteFilename -> EitherT T.Text IO String
colorByIndex colorNum paletteLoc =
  let name = "finding background color"
      command = "cel2pnm -c " <> show colorNum <> " " <> paletteLoc in
  T.unpack <$> runCommand WantOutput name command

-- Convert a whole list of cels given a palette. Put the files in target directory. Return list of cels with offset information
convertCels :: Palettes
            -> [CNFKissCell]
            -> String
            -> EitherT T.Text IO [ (String, (Int, Int)) ]
convertCels pals cels base = do
  mapM (\(CNFKissCell _ name pal _ _) -> convertCel pals pal name base) cels

-- Convert cel to pnm, pnm to png, delete pnm
convertCel :: Palettes
           -> Int
           -> CelFilename
           -> String
           -> EitherT T.Text IO (String, (Int, Int))
convertCel palettes palNum cel base = do
  pal <- lookupPalette palNum palettes
  trans <- transColor $ base ++ "/" ++ pal
  let celFile = base <> "/" <> cel <> ".cel"
  let pngFile = base <> "/" <> cel <> ".png"
  let paletteFile = base <> "/" <> pal
  offsets <- convertAndGetOffsets paletteFile celFile
  void $ runCommand NoOutput ("converting to png " <> pngFile)
                    ("pnmtopng " <> " -transparent " <> trans <> " pnm > " <> pngFile)
  void $ runCommand NoOutput "removing temp file" "rm pnm"
  return (cel, offsets)

-- Gets the transparency color from a kcf palette.
convertAndGetOffsets :: PaletteFilename -> CelFilename -> EitherT T.Text IO (Int, Int)
convertAndGetOffsets paletteFile celFile = do
  let name = "converting cel " <> celFile
      command = "cel2pnm -o " <> paletteFile <> " " <> celFile <> " pnm"
  offsetText  <- runCommand WantOutput name command
  case T.words offsetText of
    (x: y: _ ) -> right (read $ T.unpack x, read $ T.unpack y)
    _          -> left "Bad offset output "

unzipFile :: FilePath -> FilePath -> EitherT T.Text IO ()
unzipFile name dir = void $ runCommand NoOutput ("decompressing archive " <> name)
                                       ("lha -xw=" <> dir <> " static/sets/" <> name)

{-# LANGUAGE OverloadedStrings, DeriveAnyClass, OverloadedRecordDot #-}
module Main where

import Arxiv.Client
import Arxiv.Download
import Arxiv.Entry
import Arxiv.Filters
import Arxiv.Query
import Arxiv.Query.Algebraic
import Arxiv.Query.Parser
import Control.Monad (when, forM_, unless)
import Data.Maybe
import Data.Time (UTCTime(..), secondsToDiffTime, fromGregorian, Day)
import Options.Generic
import System.Directory
import System.FilePath ((</>))
import System.Process (system)
import Text.Megaparsec (errorBundlePretty)
import qualified Data.Text as T
import qualified Data.Text.IO as T

data ArxivCliArgs = ArxivCliArgs
  { query       :: T.Text
  , downloadPdf :: Bool
  , downloadSrc :: Bool
  , ungzip      :: Bool -- automatically ungzip source files (by running "tar -xzf")
  , concise     :: Bool -- concise output
  , detail      :: Bool -- detailed output
  , abstract    :: Bool -- show abstract
  , downloadDir :: Maybe FilePath
  , after       :: Maybe Day
  , before      :: Maybe Day
  , maxResult   :: Maybe Int
  , page        :: Maybe Int
  } deriving (Show, Generic, ParseRecord)

fromDay :: Integer -> Int -> Int -> UTCTime
fromDay y m d = UTCTime (fromGregorian y m d) (secondsToDiffTime 0)

tshow :: Show a => a -> T.Text
tshow = T.pack . show

main :: IO ()
main = do
  args :: ArxivCliArgs <- getRecord "Arxiv Client CLI"

  let nResultPage = fromMaybe 25   args.maxResult
      directory   = fromMaybe "./" args.downloadDir
      entryFilter = foldr (.) id
        [ maybe id (publishedAfter  . (`UTCTime` 0)) args.after
        , maybe id (publishedBefore . (`UTCTime` 0)) args.before
        ]

  arxivQuery <- case parseQueryTerm (query args) of
    Left err -> error ("Failed to parse query: " ++ errorBundlePretty err)
    Right qt -> pure
      $ emptyQuery
      & applyQueryTerm qt
      & setPaging (maybe 0 (* nResultPage) args.page) nResultPage
      & setSort SubmittedDate Desc

  entries0 <- queryArxivIO arxivQuery
  let entries = entryFilter entries0

  putStrLn $ "Total received : " <> show (length entries0) <> ", after filtering : " <> show (length entries)
  forM_ entries $ \e -> do
    T.putStrLn $ "• " <> title e
    when (args.detail || not args.concise) $ do
      T.putStrLn $ "  Authors    : "    <> T.intercalate ", " e.authors
      T.putStrLn $ "  Published  : "  <> tshow              e.published
      T.putStrLn $ "  Categories : " <> T.intercalate ", " e.categories
      T.putStrLn $ "  Link       : " <> e.pdfUrl
    when (args.detail || args.abstract) $ do
      T.putStrLn   "  Abstract   : "
      T.putStrLn $ "    " <> T.replace "\n" "\n    " (summary e)
    unless args.concise $ putStrLn ""

  if null entries
    then putStrLn "No result found."
    else putStrLn $ "Total papers found: " <> show (length entries)

  when args.downloadPdf $ do
   createDirectoryIfMissing True directory
   putStrLn "Downloading recent papers (PDF)"
   mapM_ (\en -> downloadPdfToFile en (directory </> defaultFileName ".pdf" en)) entries
  when args.downloadSrc $ do
   createDirectoryIfMissing True directory
   putStrLn "Downloading recent papers (Source .tar.gz)"
   mapM_ (\en -> downloadSourceToFile en (directory </> defaultFileName ".tar.gz" en)) entries
   when args.ungzip $ do
     putStrLn "Unzipping downloaded source files"
     forM_ entries $ \en -> do
       let filePath' = "\"" <> directory </> defaultFileName ".tar.gz" en <> "\""
           destDir   = directory </> defaultFileName "_src" en
           destDir'  = "\"" <> destDir <> "\""
       createDirectoryIfMissing True destDir
       let cmd = "tar -xzf " ++ filePath' ++ " -C " ++ destDir' ++ " && rm " ++ filePath'
       putStrLn $ "Running: " ++ cmd
       _ <- system cmd
       return ()

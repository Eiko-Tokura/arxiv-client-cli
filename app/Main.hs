{-# LANGUAGE OverloadedStrings, MultilineStrings, OverloadedRecordDot, DataKinds #-}
module Main where

import Arxiv.Client
import Arxiv.Download
import Arxiv.Entry
import Arxiv.Filters
import Arxiv.Query
import Arxiv.Query.Algebraic
import Arxiv.Query.Parser
import Control.Monad (when, forM_, unless, guard)
import Data.Aeson
import qualified Data.Aeson.KeyMap as K
import Data.IORef
import Data.Maybe
import Data.Time (UTCTime(..), Day)
import Options.Generic
import System.Directory
import System.FilePath ((</>))
import System.Process (system)
import System.Exit (ExitCode(..))
import Text.Megaparsec (errorBundlePretty)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T

queryHelp :: [Char]
queryHelp =
  """
  --query parameter is mandatory.
  Query string is constructed by using

  * <field> <match> <value>
    <field> := title | author | abstract | category | anywhere
    (<match>, <value>) :=
      is <string>          -- exact match
      has <string>         -- substring match
      any [<string>, ...]  -- any of the strings (ors)
      all [<string>, ...]  -- all of the strings (ands)

  * You can write a single string "value" without specifying field and match,
    which is equivalent to: anywhere has "value"

  * Logical operators:
    &&      -- and (note that these operators are right associative, use brackets for clarity)
    ||      -- or  (note that these operators are right associative, use brackets for clarity)
    ands [<queryTerm>, ...]  -- and multiple terms
    ors  [<queryTerm>, ...]  -- or  multiple terms
    not <queryTerm>          -- negate term

  Examples:
    --query 'title has "quantum" && author is "Albert Einstein"'
    --query 'author any ["john doe", "jane smith"]'
    --query 'ands [title is "coleman", author has "doe"]'
    --query 'ors [category is "math.NT", category is "math.AG"]'
  """

data ArxivCliArgs w = ArxivCliArgs
  { query       :: w ::: Maybe T.Text   <#> "q" <?> "Search query string"
  , helpQuery   :: w ::: Bool           <#> "H" <?> "Display help for constructing query strings"
  , downloadPdf :: w ::: Bool           <#> "p" <?> "Download PDF files for each entry"
  , downloadSrc :: w ::: Bool           <#> "s" <?> "Download source .tar.gz files for each entry"
  , ungzip      :: w ::: Bool           <#> "u" <?> "Automatically ungzip source files (by running \"tar -xzf\")"
  , concise     :: w ::: Bool           <#> "c" <?> "Concise output (only titles)"
  , detail      :: w ::: Bool           <#> "v" <?> "Detailed output"
  , abstract    :: w ::: Bool           <#> "a" <?> "Include abstracts in the output"
  , json        :: w ::: Bool           <#> "j" <?> "Output results in JSON format, turn off human-readable output"
  , downloadDir :: w ::: Maybe FilePath <#> "d" <?> "Directory path to save downloaded files" <!> "./"
  , after       :: w ::: Maybe Day              <?> "Only include papers published after this date (YYYY-MM-DD)"
  , before      :: w ::: Maybe Day              <?> "Only include papers published before this date (YYYY-MM-DD)"
  , maxResult   :: w ::: Maybe Int      <#> "m" <?> "Maximum results per page"                <!> "25"
  , page        :: w ::: Maybe Int      <#> "n" <?> "Page number to retrieve (starting from 0)"
  } deriving (Generic)

instance ParseRecord (ArxivCliArgs Wrapped)

tshow :: Show a => a -> T.Text
tshow = T.pack . show

main :: IO ()
main = do
  args <- unwrap <$> getRecord "Arxiv Client CLI"
  let mQuery = do
        guard (not args.helpQuery)
        args.query
  maybe (putStrLn queryHelp) (arxivCli args) mQuery

arxivCli :: ArxivCliArgs Unwrapped -> Text -> IO ()
arxivCli args query = do
  let nResultPage = fromMaybe 25   args.maxResult
      directory   = fromMaybe "./" args.downloadDir
      entryFilter = foldr (.) id
        [ maybe id (publishedAfter  . (`UTCTime` 0)) args.after
        , maybe id (publishedBefore . (`UTCTime` 0)) args.before
        ]

  arxivQuery <- case parseQueryTerm query of
    Left err -> error ("Failed to parse query: " ++ errorBundlePretty err)
    Right qt -> pure
      $ emptyQuery
      & applyQueryTerm qt
      & setPaging (maybe 0 (* nResultPage) args.page) nResultPage
      & setSort SubmittedDate Desc

  entries0 <- queryArxivIO arxivQuery
  let entries = entryFilter entries0
  jsonObj <- newIORef (object ["results" .= entries])

  unless args.json $ do
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
    if args.json
      then addDownloadedPdfPaths entries directory jsonObj
      else putStrLn "Downloading recent papers (PDF)"
    mapM_ (\en -> downloadPdfToFile en (directory </> defaultFileName ".pdf" en)) entries

  when args.downloadSrc $ do
    createDirectoryIfMissing True directory
    if args.json
      then addDownloadedSourcePaths entries directory jsonObj
      else putStrLn "Downloading recent papers (Source .tar.gz)"
    mapM_ (\en -> downloadSourceToFile en (directory </> defaultFileName ".tar.gz" en)) entries
    when args.ungzip $ do
      unless args.json $ putStrLn "Unzipping downloaded source files"
      forM_ entries $ \en -> do
        let filePath' = "\"" <> directory </> defaultFileName ".tar.gz" en <> "\""
            destDir   = directory </> defaultFileName "_src" en
            destDir'  = "\"" <> destDir <> "\""
        createDirectoryIfMissing True destDir
        let cmd = "tar -xzf " ++ filePath' ++ " -C " ++ destDir' ++ " && rm " ++ filePath'
        unless args.json $ putStrLn $ "Running: " ++ cmd
        code <- system cmd
        case code of
          ExitSuccess   -> addUngzippedSourcePath en destDir jsonObj
          ExitFailure c -> unless args.json $ putStrLn $ "Command failed with exit code: " ++ show c
        return ()

  when args.json $ do
    finalObj <- readIORef jsonObj
    B.putStrLn $ encode finalObj
  where
    addDownloadedPdfPaths :: [ArxivEntry] -> FilePath -> IORef Value -> IO ()
    addDownloadedPdfPaths entries dir jsonRef = do
      let paths = entries `zip` map (\en -> dir </> defaultFileName ".pdf" en) entries
          downloads = toJSON $ map (\(en, path) -> object
            [ "arxiv_id" .= arxivId en
            , "pdf_path" .= path
            ]) paths
      modifyIORef jsonRef $ \obj ->
        case obj of
          Object o -> Object $ K.insert "downloadPdf" downloads o
          _        -> obj
    addDownloadedSourcePaths :: [ArxivEntry] -> FilePath -> IORef Value -> IO ()
    addDownloadedSourcePaths entries dir jsonRef = do
      let paths = entries `zip` map (\en -> dir </> defaultFileName ".tar.gz" en) entries
          downloads = toJSON $ map (\(en, path) -> object
            [ "arxiv_id"   .= arxivId en
            , "source_path" .= path
            ]) paths
      modifyIORef jsonRef $ \obj ->
        case obj of
          Object o -> Object $ K.insert "downloadSrc" downloads o
          _        -> obj
    addUngzippedSourcePath :: ArxivEntry -> FilePath -> IORef Value -> IO ()
    addUngzippedSourcePath en destDir jsonRef = do
      let entryObj = object
            [ "arxiv_id"    .= arxivId en
            , "ungzipped_path" .= destDir
            ]
          appendUngzipped (Array a1) (Array a2) = Array (a1 <> a2)
          appendUngzipped _ _ = error "Expected arrays when appending ungzippedSrc entries"
      modifyIORef jsonRef $ \obj ->
        case obj of
          Object o -> Object $ K.insertWith appendUngzipped "ungzippedSrc" (toJSON [entryObj]) o
          _        -> obj

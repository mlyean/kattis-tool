{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Codec.Archive.Zip (unpackInto, withArchive)
import Control.Exception
import Control.Monad
import Control.Monad.Loops
import qualified Data.ByteString as BS
import qualified Data.Conduit.List as CL
import Data.Default
import Data.Hashable (hash)
import Data.List
import Data.Maybe.HT (toMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Version (showVersion)
import Network.HTTP.Simple
import Numeric (showHex, showInt)
import Paths_kattis_tool (version)
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process
import qualified Text.PrettyPrint.ANSI.Leijen as PP

--------------------------------------------------------------------------------
-- DATA TYPES
--------------------------------------------------------------------------------
type ProblemID = String

type TestCase = (T.Text, [T.Text])

data Flags = Flags
  { lazyRun :: Bool -- Abort running tests on first failure
  , floatPrec :: Int -- Floating point precision
  }

instance Default Flags where
  def = Flags {lazyRun = False, floatPrec = 8}

--------------------------------------------------------------------------------
-- CONSTANTS
--------------------------------------------------------------------------------
author :: String
author = "mlyean"

description :: String
description = "Test Kattis solutions locally"

kattisDir :: FilePath
kattisDir = "/tmp/kattis/"

--------------------------------------------------------------------------------
-- FILE HELPER FUNCTIONS
--------------------------------------------------------------------------------
testCaseUrl :: ProblemID -> String
testCaseUrl pId =
  showString "https://open.kattis.com/problems/" . showString pId $
  "/file/statement/samples.zip"

problemDir :: ProblemID -> FilePath
problemDir pId = showString kattisDir . showString pId $ "/"

pIdHash :: ProblemID -> Word
pIdHash = fromIntegral . hash

--------------------------------------------------------------------------------
-- FILE MANAGEMENT
--------------------------------------------------------------------------------
genTmpOutFilePath :: ProblemID -> IO (FilePath, Handle)
genTmpOutFilePath pId = do
  dlTime <- round <$> getPOSIXTime
  let file =
        showString kattisDir .
        showString pId .
        showChar '-' . showHex (pIdHash pId) . showChar '-' . showHex dlTime $
        ".zip.part"
  hin <- openBinaryFile file WriteMode
  return (file, hin)

getTestCases :: ProblemID -> IO String
getTestCases pId = do
  alreadyExists <- doesFileExist outFile
  if alreadyExists
    then return outFile
    else do
      createDirectoryIfMissing True kattisDir
      request <- parseRequest . testCaseUrl $ pId
      (tmpFile, hin) <- genTmpOutFilePath pId
      success <-
        handle
          (\(_ :: HttpException) ->
             errorWithoutStackTrace "Failed to retrieve test cases") $
        httpSink request $ \response -> do
          let success = getResponseStatusCode response == 200
          when success $ CL.mapM_ (BS.hPut hin)
          return success
      hClose hin
      if success
        then renameFile tmpFile outFile
        else removeFile tmpFile
      return outFile
  where
    outFile :: FilePath
    outFile =
      showString kattisDir .
      showString pId . showChar '-' . showHex (pIdHash pId) $
      ".zip"

extractTestCase :: ProblemID -> FilePath -> IO ()
extractTestCase pId zipPath = withArchive zipPath (unpackInto $ problemDir pId)

getAndExtract :: ProblemID -> IO ()
getAndExtract pId = do
  res <- getTestCases pId
  extractTestCase pId res

cleanTmp :: IO ()
cleanTmp = do
  dirExists <- doesDirectoryExist kattisDir
  when dirExists $ removeDirectoryRecursive kattisDir

--------------------------------------------------------------------------------
-- TEST CASE PARSING
--------------------------------------------------------------------------------
listTestCases :: ProblemID -> IO [(FilePath, FilePath)]
listTestCases pId = do
  files <- sort <$> getDirectoryContents (problemDir pId)
  return $ zip (f ".in" files) (f ".ans" files)
  where
    fp = problemDir pId
    prependDir dir = map (dir ++)
    filterExt ext = filter (ext `isSuffixOf`)
    f ext = prependDir fp . filterExt ext

readTestCases :: ProblemID -> IO [TestCase]
readTestCases pId = do
  paths <- listTestCases pId
  mapM f paths
  where
    f (x, y) = do
      x' <- T.stripEnd <$> T.readFile x
      y' <- map T.stripEnd . T.lines <$> T.readFile y
      return (x', y')

countTestCases :: ProblemID -> IO Int
countTestCases pId = length <$> listTestCases pId

--------------------------------------------------------------------------------
-- TEXT FORMATTING
--------------------------------------------------------------------------------
titleFormat :: PP.Doc -> PP.Doc
titleFormat x = PP.text "--" PP.<+> x PP.<+> PP.text "--"

--------------------------------------------------------------------------------
-- MISCELLANEOUS
--------------------------------------------------------------------------------
getExitCode :: ExitCode -> Int
getExitCode (ExitFailure n) = n
getExitCode ExitSuccess = 0

parseFlags :: [String] -> Flags
parseFlags ("-l":xs) = (parseFlags xs) {lazyRun = True}
parseFlags (('-':'p':'=':prec):xs) = (parseFlags xs) {floatPrec = read prec}
parseFlags ("-p":prec:xs) = (parseFlags xs) {floatPrec = read prec}
parseFlags (x:_) = errorWithoutStackTrace ("Unknown flag " ++ x)
parseFlags [] = def

--------------------------------------------------------------------------------
-- TEST CASE RUNNING
--------------------------------------------------------------------------------
runTestCase :: FilePath -> TestCase -> IO Bool
runTestCase execPath (inp, ans) =
  withCreateProcess
    (shell execPath)
      { std_in = CreatePipe
      , std_out = CreatePipe
      , std_err = CreatePipe
      , delegate_ctlc = True
      } $ \(Just hin) (Just hout) (Just herr) hproc -> do
    T.hPutStr hin inp
    hClose hin
    exitCode <- waitForProcess hproc
    out <- map T.stripEnd . T.lines <$> T.hGetContents hout
    err <- T.hGetContents herr
    let outputMatched = out == ans
        hasErrorMsg = not . T.null $ err
        hasZeroExit = exitCode == ExitSuccess
        passed = outputMatched && not hasErrorMsg && hasZeroExit
    putChar '\n'
    if passed
      then PP.putDoc $ header (PP.green $ PP.text "PASSED") <> PP.line
      else do
        PP.putDoc $ header (PP.red $ PP.text "FAILED") <> PP.line
        unless outputMatched $
          PP.putDoc $
          PP.vsep
            [ PP.text "Input:"
            , indent $ T.lines inp
            , PP.text "Expected output:"
            , indent ans
            , PP.text "Your output:"
            , indent out
            ] <>
          PP.line
        when hasErrorMsg $
          PP.putDoc $
          PP.text "Error message:" PP.<$> indent (T.lines err) <> PP.line
        unless hasZeroExit $
          PP.putDoc $
          PP.text "Exit code:" PP.<+> PP.int (getExitCode exitCode) <> PP.line
    return passed
  where
    header x = titleFormat (PP.text "Test Case" PP.<+> x)
    indent =
      PP.vsep . map (\x -> PP.blue PP.rangle PP.<+> (PP.text . T.unpack $ x))

runAllTestCases :: ProblemID -> FilePath -> IO Int
runAllTestCases pId execPath = do
  testCases <- readTestCases pId
  outcomes <- mapM (runTestCase execPath) testCases
  return $ length . filter id $ outcomes

runPartialTestCases :: ProblemID -> FilePath -> IO Int
runPartialTestCases pId execPath = do
  testCases <- readTestCases pId
  outcomes <- takeWhileM (runTestCase execPath) testCases
  return $ length outcomes

--------------------------------------------------------------------------------
-- USER INTERFACE
--------------------------------------------------------------------------------
helpText :: String -> PP.Doc
helpText prog =
  PP.vsep
    [ PP.text prog PP.<+> PP.char '-' PP.<+> PP.text description
    , PP.text ""
    , PP.text "Usage:" PP.<+> PP.text prog PP.<+> PP.text usageStr
    , PP.text ""
    , PP.text "Available options:"
    , PP.indent 2 (PP.vsep optionInfo)
    , PP.text ""
    , PP.text "Available commands:"
    , PP.indent 2 (PP.vsep commandInfo)
    ] <>
  PP.line
  where
    usageStr = "[ --help | --version | COMMAND [ OPTIONS ] ]"
    optionInfo =
      map
        f
        [ ("--help, -h", ["Show this help message."])
        , ("--version, -v", ["Show program version."])
        , ("--lazy, -l", ["Abort running tests on first failure."])
        ]
    commandInfo =
      map
        f
        [ ("test ID EXEC", ["Run test cases of the given problem ID on EXEC."])
        , ( "get ID"
          , [ "Download test cases for the given problem ID."
            , "This is automatically done by 'test'."
            ])
        , ("clean", ["Remove all temporary files."])
        ]
    f (x, y) =
      PP.fill 16 (PP.text x) PP.<+> PP.align (PP.fillCat $ map PP.text y)

showHelp :: IO ()
showHelp = do
  prog <- getProgName
  PP.putDoc $ helpText prog

verText :: String -> PP.Doc
verText prog =
  PP.text prog PP.<+> PP.blue (PP.text $ showVersion version) PP.<$>
  PP.text "Author:" PP.<+>
  PP.blue (PP.text author) <>
  PP.line

showVer :: IO ()
showVer = do
  prog <- getProgName
  PP.putDoc $ verText prog

testAndSummarize :: Flags -> ProblemID -> FilePath -> IO ()
testAndSummarize flags pId exec = do
  getAndExtract pId
  total <- countTestCases pId
  totalPassed <-
    if lazyRun flags
      then runPartialTestCases pId exec
      else runAllTestCases pId exec
  putChar '\n'
  showSummary total totalPassed

showSummary :: Int -> Int -> IO ()
showSummary total passed =
  PP.putDoc $
  titleFormat (PP.text "SUMMARY") PP.<$>
  (if passed == total
     then PP.text "All test cases passed!"
     else PP.int passed PP.<+> PP.text "of" PP.<+> PP.int total PP.<+>
          PP.text "test cases passed.") <>
  PP.line

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("-h":_) -> showHelp
    ("--help":_) -> showHelp
    ["-v"] -> showVer
    ["--version"] -> showVer
    ("test":pId:exec:flags) -> testAndSummarize (parseFlags flags) pId exec
    ("get":pId:flags) -> getAndExtract pId
    ("clean":flags) -> cleanTmp
    _ -> showHelp >> exitFailure

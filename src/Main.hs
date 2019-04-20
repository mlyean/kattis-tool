{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Codec.Archive.Zip (unpackInto, withArchive)
import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.Conduit.List as CL
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

type ProblemID = String

type TestCase = (T.Text, [T.Text])

kattisDir :: FilePath
kattisDir = "/tmp/kattis/"

testCaseUrl :: ProblemID -> String
testCaseUrl pId =
  showString "https://open.kattis.com/problems/" . showString pId $
  "/file/statement/samples.zip"

getTestCases :: ProblemID -> IO (Maybe String)
getTestCases pId = do
  createDirectoryIfMissing True kattisDir
  ft <- round <$> getPOSIXTime
  let fh = fromIntegral . hash $ pId :: Word
      outFile =
        showString kattisDir . showString pId . showChar '-' . showHex fh $
        ".zip"
  alreadyExists <- doesFileExist outFile
  if alreadyExists
    then return (Just outFile)
    else do
      let tmpOutFile =
            showString kattisDir .
            showString pId .
            showChar '-' . showHex fh . showChar '-' . showHex ft $
            ".zip.part"
      request <- parseRequest . testCaseUrl $ pId
      file <- openBinaryFile tmpOutFile WriteMode
      success <-
        handle (\(_ :: HttpException) -> return False) $
        httpSink request $ \response -> do
          let success = getResponseStatusCode response == 200
          when success $ CL.mapM_ (BS.hPut file)
          return success
      hClose file
      if success
        then renameFile tmpOutFile outFile
        else removeFile tmpOutFile
      return $ toMaybe success outFile

problemDir :: ProblemID -> FilePath
problemDir pId = showString kattisDir . showString pId $ "/"

extractTestCase :: ProblemID -> FilePath -> IO ()
extractTestCase pId zipPath = withArchive zipPath (unpackInto $ problemDir pId)

listTestCases :: ProblemID -> IO [(FilePath, FilePath)]
listTestCases pId = do
  files <- sort <$> getDirectoryContents (problemDir pId)
  return $ zip (f ".in" files) (f ".ans" files)
  where
    fp = problemDir pId
    f ext = map (fp ++) . filter (ext `isSuffixOf`)

readTestCases :: ProblemID -> IO [TestCase]
readTestCases pId = do
  paths <- listTestCases pId
  mapM f paths
  where
    f (x, y) = do
      x' <- T.stripEnd <$> T.readFile x
      y' <- map T.stripEnd . T.lines <$> T.readFile y
      return (x', y')

titleFormat :: PP.Doc -> PP.Doc
titleFormat x = PP.text "--" PP.<+> x PP.<+> PP.text "--"

getExitCode :: ExitCode -> Int
getExitCode (ExitFailure n) = n
getExitCode ExitSuccess = 0

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
        hasNonZeroExit = exitCode /= ExitSuccess
        hasFailed = not outputMatched || hasErrorMsg || hasNonZeroExit
    putChar '\n'
    if hasFailed
      then do
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
        when hasNonZeroExit $
          PP.putDoc $
          PP.text "Exit code:" PP.<+> PP.int (getExitCode exitCode) <> PP.line
      else PP.putDoc $ header (PP.green $ PP.text "PASSED") <> PP.line
    return hasFailed
  where
    header x = titleFormat (PP.text "Test Case" PP.<+> x)
    indent =
      PP.vsep . map (\x -> PP.blue PP.rangle PP.<+> (PP.text . T.unpack $ x))

runTestCases :: ProblemID -> FilePath -> IO Int
runTestCases pId execPath = do
  testCases <- readTestCases pId
  outcomes <- mapM (runTestCase execPath) testCases
  return $ sum . map fromEnum $ outcomes

countTestCases :: ProblemID -> IO Int
countTestCases pId = length <$> listTestCases pId

cleanTmp :: IO ()
cleanTmp = do
  dirExists <- doesDirectoryExist kattisDir
  when dirExists $ removeDirectoryRecursive kattisDir

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
    description = "Test Kattis solutions locally"
    usageStr = "[ --help | --version | COMMAND ] [ OPTIONS ]"
    optionInfo =
      map
        f
        [ ("--help, -h", ["Show this help message."])
        , ("--version, -v", ["Show program version."])
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
  PP.text prog PP.<+> PP.blue (PP.text $ showVersion version) <> PP.line <>
  PP.text "Author:" PP.<+>
  PP.blue (PP.text author) <>
  PP.line
  where
    author = "mlyean"

showVer :: IO ()
showVer = do
  prog <- getProgName
  PP.putDoc $ verText prog

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("-h":_) -> showHelp
    ("--help":_) -> showHelp
    ["-v"] -> showVer
    ["--version"] -> showVer
    ("test":pId:exec:opt) -> testAndSummarize pId exec
    ("get":pId:opt) -> getAndExtract pId
    ("clean":opt) -> cleanTmp
    _ -> showHelp >> exitFailure
  where
    getAndExtract pId = do
      res <- getTestCases pId
      maybe
        (putStrLn "Error: Failed to retrieve test cases." >> exitFailure)
        (extractTestCase pId)
        res
    testAndSummarize pId exec = do
      getAndExtract pId
      total <- countTestCases pId
      totalFailed <- runTestCases pId exec
      putChar '\n'
      showSummary total totalFailed
    showSummary t f = do
      PP.putDoc $ titleFormat (PP.text "SUMMARY") <> PP.line
      if f == 0
        then putStrLn "All test cases passed!"
        else putStrLn $
             showInt f . showString " of " . showInt t $ " test cases failed"

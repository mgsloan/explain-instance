import Control.Monad
import System.Directory
import System.Exit
import System.IO
import System.Process.Typed
import System.FilePath

main :: IO ()
main = do
  examples <- listDirectory "examples"
  forM_ examples $ \example ->
    when (takeExtensions example == ".hs") $
      expectExampleSuccess ("examples" </> dropExtensions example)
  failingExamples <- listDirectory "failing-examples"
  forM_ failingExamples $ \example ->
    when (takeExtensions example == ".hs") $
      expectExampleFailure ("failing-examples" </> dropExtensions example)

expectExampleSuccess :: String -> IO ()
expectExampleSuccess name =
  expectProcessSuccess
    "runghc"
    [name ++ ".hs"]
    (name ++ ".stdout")
    (name ++ ".stderr")

expectExampleFailure :: String -> IO ()
expectExampleFailure name =
  expectProcessFailure
    "runghc"
    [name ++ ".hs"]
    (name ++ ".stdout")
    (name ++ ".stderr")

expectProcessSuccess :: String -> [String] -> String -> String -> IO ()
expectProcessSuccess cmd args stdoutFile stderrFile = do
  ec <- runHelper cmd args stdoutFile stderrFile
  case ec of
    ExitFailure code -> failAndShowOutput stdoutFile stderrFile $
      "Expected success, but instead got exit " ++ show code
    ExitSuccess -> return ()

expectProcessFailure :: String -> [String] -> String -> String -> IO ()
expectProcessFailure cmd args stdoutFile stderrFile = do
  ec <- runHelper cmd args stdoutFile stderrFile
  case ec of
    ExitFailure _ -> return ()
    ExitSuccess -> failAndShowOutput stdoutFile stderrFile
      "Expected failure, but instead exited successfully"

failAndShowOutput :: FilePath -> FilePath -> String -> IO ()
failAndShowOutput stdoutFile stderrFile msg = do
  out <- readFileIfExists stdoutFile
  err <- readFileIfExists stderrFile
  fail $ unlines
    [ msg
    , ""
    , "stdout:"
    , out
    , ""
    , "stderr:"
    , err
    ]

runHelper :: String -> [String] -> FilePath -> FilePath -> IO ExitCode
runHelper cmd args stdoutFile stderrFile = do
  putStrLn $ unwords ("Running" : cmd : args)
  exitCode <-
    withFile stdoutFile WriteMode $ \stdoutHandle ->
      withFile stderrFile WriteMode $ \stderrHandle ->
        runProcess $
          setStdout (useHandleOpen stdoutHandle) $
          setStderr (useHandleOpen stderrHandle) $
          proc cmd args
  deleteIfEmpty stdoutFile
  deleteIfEmpty stderrFile
  return exitCode

readFileIfExists :: String -> IO String
readFileIfExists path = do
  exists <- doesFileExist path
  if exists then readFile path else return ""

deleteIfEmpty :: String -> IO ()
deleteIfEmpty path = do
  exists <- doesFileExist path
  when exists $ do
    contents <- readFile path
    when (null (filter (not . null) (lines contents))) $ removeFile path

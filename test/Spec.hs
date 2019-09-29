import Control.Monad
import System.Directory
import System.Exit
import System.IO
import System.Process.Typed

main :: IO ()
main = do
  expectExampleSuccess "Test"
  expectExampleSuccess "Test2"
  expectExampleSuccess "Test3"
  expectExampleFailure "Test4"
  expectExampleSuccess "Test5"
  expectExampleSuccess "Test6"
  expectExampleSuccess "Test7"
  expectExampleSuccess "Test8"
  expectExampleSuccess "Test9"

expectExampleSuccess :: String -> IO ()
expectExampleSuccess name =
  expectProcessSuccess
    "runghc"
    ["examples/" ++ name ++ ".hs"]
    ("examples/" ++ name ++ ".stdout")
    ("examples/" ++ name ++ ".stderr")

expectExampleFailure :: String -> IO ()
expectExampleFailure name =
  expectProcessFailure
    "runghc"
    ["examples/" ++ name ++ ".hs"]
    ("examples/" ++ name ++ ".stdout")
    ("examples/" ++ name ++ ".stderr")

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
  out <- readFile stdoutFile
  err <- readFile stderrFile
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

deleteIfEmpty :: String -> IO ()
deleteIfEmpty path = do
  contents <- readFile path
  when (null (filter (not . null) (lines contents))) $ removeFile path

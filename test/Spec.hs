import System.Directory (createDirectoryIfMissing)
import System.IO
import System.Exit
import System.Process.Typed

main :: IO ()
main = do
  createDirectoryIfMissing True "examples-output/"
  expectExampleSuccess "Test"
  expectExampleSuccess "Test2"
  expectExampleSuccess "Test3"
  expectExampleFailure "Test4"
  expectExampleSuccess "Test5"
  expectExampleFailure "Test6"
  expectExampleSuccess "Test7"
  expectExampleSuccess "Test8"
  expectExampleSuccess "Test9"

expectExampleSuccess :: String -> IO ()
expectExampleSuccess name =
  expectProcessSuccess
    "runghc"
    ["-ddump-splices", "examples/" ++ name ++ ".hs"]
    ("examples-output/" ++ name ++ ".stdout")
    ("examples-output/" ++ name ++ ".stderr")

expectExampleFailure :: String -> IO ()
expectExampleFailure name =
  expectProcessFailure
    "runghc"
    ["examples/" ++ name ++ ".hs"]
    ("examples-output/" ++ name ++ ".stdout")
    ("examples-output/" ++ name ++ ".stderr")

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
  withFile stdoutFile WriteMode $ \stdoutHandle ->
    withFile stderrFile WriteMode $ \stderrHandle ->
      runProcess $
        setStdout (useHandleOpen stdoutHandle) $
        setStderr (useHandleOpen stderrHandle) $
        proc cmd args

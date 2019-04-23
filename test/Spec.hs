import System.Exit
import System.Process.Typed

main :: IO ()
main = do
  expectSuccess "ghc" ["-ddump-splices", "examples/Test.hs"]

expectSuccess :: String -> [String] -> IO ()
expectSuccess cmd args = do
  putStrLn $ unwords ("Running" : cmd : args)
  ec <- runProcess $ proc cmd args
  case ec of
    ExitFailure code -> fail $ "Expected success, but instead got exit " ++ show code
    ExitSuccess -> return ()

expectFailure :: String -> [String] -> IO ()
expectFailure cmd args = do
  putStrLn $ unwords ("Running" : cmd : args)
  ec <- runProcess $ proc cmd args
  case ec of
    ExitFailure _ -> return ()
    ExitSuccess -> fail $ "Expected failure, but instead exited successfully"

module Main where

import System.Win32.HPPath (winPath, unWinPath, clearPath, addHPPath,
  isVersionOpt)
import System.Environment (getArgs, lookupEnv, setEnv)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.Process (callProcess)
import Data.Maybe (fromMaybe)
import Data.Version (showVersion)
import Paths_win_hp_path (version)

usage :: IO ()
usage = putStrLn (unlines msg) >> exitWith (ExitFailure 1)
  where
    msg =
      [ "use-hppath version " ++ showVersion version
      , "Usage:"
      , "  use-hppath <path-to-hp>"
      , "Example:"
      , "  use-hppath " ++
        "\"C:\\Program Files (x86)\\Haskell Platform\\2014.2.0.0\""
      ]

main = fmap parseCmd getArgs >>= maybe usage useHPPath

useHPPath :: FilePath -> IO ()
useHPPath hp = do
  oldPath <- fmap (winPath . fromMaybe []) $ lookupEnv "PATH"
  let newPath = addHPPath hp . clearPath $ oldPath
  setEnv "PATH" $ unWinPath newPath
  putStrLn $ "Using Haskell Platform at " ++ hp
  putStrLn $ "Enter 'exit' when done."
  callProcess "cmd.exe" []

parseCmd :: [String] -> Maybe FilePath
parseCmd [v]
 | isVersionOpt v = Nothing
 | otherwise      = Just v
parseCmd _        = Nothing

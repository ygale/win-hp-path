module Main where

import System.Win32.HPPath (winPath, unWinPath, clearPath, addHPPath,
  isVersionOpt)
import System.Win32.HPPath.Reg (modifySysPath, modifyUserPath)
import System.Exit (exitWith, ExitCode(ExitFailure))
import Data.Version (showVersion)
import Paths_win_hp_path (version)

usage :: IO ()
usage = putStrLn (unlines msg) >> exitWith (ExitFailure 1)
  where
    msg =
      [ "Usage:"
      , "  use-hppath \"<path-to-hp>\""
      , "Example:"
      , "  use-hppath " ++
        "\"C:\\Program Files (x86)\\Haskell Platform\\2014.2.0.0\""
      ]

main = fmap parseCmd getArgs >>= either usage activate

activate :: FilePath -> IO ()
activate hp = do
  putStrLn $ "Activating Haskell Platform at " ++ hp
  modifyUserPath clearPath
  modifySysPath (addHPPath hp . clearPath)
  putStrLn "This version of the Haskell Platform will now be used by default"
  putStrLn "when you open new command prompt windows."

parseCmd :: [String] -> Maybe FilePath
parseCmd [v]
 | isVersionOpt v = Nothing
 | otherwise      = Just v
parseCmd _        = Nothing

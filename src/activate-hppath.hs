module Main where

import System.Environment (getArgs)
import System.Win32.HPPath (mapWinPath, clearPath, addHPPath, isVersionOpt)
import System.Win32.HPPath.Registry (modifySysPath, modifyUserPath)
import System.Exit (exitWith, ExitCode(ExitFailure))
import Data.Version (showVersion)
import Paths_win_hp_path (version)

usage :: IO ()
usage = putStrLn (unlines msg) >> exitWith (ExitFailure 1)
  where
    msg =
      [ "Usage:"
      , "  use-hppath <path-to-hp>"
      , "Example:"
      , "  use-hppath " ++
        "\"C:\\Program Files (x86)\\Haskell Platform\\2014.2.0.0\""
      ]

main = fmap parseCmd getArgs >>= maybe usage activate

activate :: FilePath -> IO ()
activate hp = do
  putStrLn $ "Activating Haskell Platform at " ++ hp
  modifyUserPath $ mapWinPath clearPath
  modifySysPath . mapWinPath $ addHPPath hp . clearPath
  putStrLn "This version of the Haskell Platform will now be used by default"
  putStrLn "when you open new command prompt windows."

parseCmd :: [String] -> Maybe FilePath
parseCmd [v]
 | isVersionOpt v = Nothing
 | otherwise      = Just v
parseCmd _        = Nothing

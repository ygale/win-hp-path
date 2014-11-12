-- | Utility functions for manipulating Windows paths related to the
-- Haskell Platform.
module System.Win32.HPPath
  ( winPath
  , unWinPath
  , mapWinPath
  , clearPath
  , addHPPath
  , isVersionOpt
  , WinPath(getWinPath)
  )
 where

import Data.List (intercalate, isInfixOf)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)

-- | A Windows path that has been split into its components.
newtype WinPath = WinPath { getWinPath :: [FilePath] }

-- | A directory which can contain command line executables related to
-- Haskell. It always exists near the beginning of the Windows PATH,
-- so the executables here override other executables with the same
-- name.
hpMaster :: FilePath
hpMaster = "C:\\Program Files\\Haskell\\bin"

-- | Subpaths of a Haskell Platform installation that are placed near
-- the beginning of the Windows PATH.
hpInitialSubpaths :: [FilePath]
hpInitialSubpaths =
  [ "lib\\extralibs\\bin"
  , "bin"
  ]

-- | Subpaths of a Haskell Platform installation that are placed near
-- the end of the Windows PATH.
hpFinalSubpaths :: [FilePath]
hpFinalSubpaths = ["mingw\\bin"]

-- | Split a Windows PATH 'String' into a 'WinPath'.
winPath :: String -> WinPath
winPath = WinPath . splitOn ";"

-- | Join the paths of a 'WinPath' into a Windows PATH 'String'.
unWinPath :: WinPath -> String
unWinPath = intercalate ";" . getWinPath

-- | Apply a function on 'WithPath' to a list of 'FilePaths'.
mapWinPath :: (WinPath -> WinPath) -> String -> String
mapWinPath f = unWinPath . f . winPath

-- | Remove all Haskell-related paths from a 'WinPath'.
clearPath :: WinPath -> WinPath
clearPath = WinPath . filter (not . ("Haskell" `isInfixOf`)) . getWinPath

-- | Install all required paths for the Haskell Platform based at the
-- given 'FilePath' into a 'WinPath'. The initial 'WinPath' must not
-- contain any Haskell-related paths.
addHPPath :: FilePath -> WinPath -> WinPath
addHPPath hp =
    WinPath
    . (hpMaster :)
    . (map prependHp hpInitialSubpaths ++)
    . (++ map prependHp hpFinalSubpaths)
    . getWinPath
  where
    prependHp = (hp' ++)
    hp'
     | endsWith '\\' hp = unquote hp
     | otherwise        = unquote hp ++ "\\"

-- | Remove double quotes, if they exist, from around a 'String'.
unquote :: String -> String
unquote = unbracket '"' '"'

-- | Remove the specified left and right brackets from around a list.
unbracket :: Eq a => a -> a -> [a] -> [a]
unbracket l r xs@(x:xs') | x == l = fromMaybe xs $ dropFromEnd r xs'
unbracket _ _ xs                  = xs

-- | Remove the specified value from the end of the list, if possible.
dropFromEnd :: Eq a => a -> [a] -> Maybe [a]
dropFromEnd x [y] | x == y = Just []
dropFromEnd x (y:ys)       = fmap (y:) $ dropFromEnd x ys
dropFromEnd _ _            = Nothing

-- | Test whether the last element of a list exists and matches the
-- given value.
endsWith :: Eq a => a -> [a] -> Bool
endsWith x [y]    = x == y
endsWith x (_:ys) = endsWith x ys
endsWith _ _      = False

-- | Check whether the given argument to a command on Windows looks
-- like a request either for the version number or for command syntax
-- help.
isVersionOpt :: String -> Bool
isVersionOpt =
  (`elem`
    [ "/?"
    , "/v"
    , "/version"
    , "/h"
    , "/help"
    , "-v"
    , "--version"
    , "-h"
    , "--help"
    ]
  )

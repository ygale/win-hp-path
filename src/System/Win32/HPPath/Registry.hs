-- | Utility functions for manipulating the user and system paths in
-- the Windows registry.
module System.Win32.HPPath.Registry
  ( modifyUserPath
  , modifySysPath
  )
  where

import System.Win32.Types (HKEY)
import System.Win32.Registry (regQueryValue, regSetStringValue,
  regOpenKeyEx, regCloseKey,
  hKEY_LOCAL_MACHINE, hKEY_CURRENT_USER
  kEY_QUERY_VALUE, kEY_SET_VALUE)
import Data.Bits ((.|.))

-- | The location of the system path in the Windows registry.
sysPathKey :: (HKEY, String)
sysPathKey =
  ( hKEY_LOCAL_MACHINE
  , "SYSTEM\\CurrentControlSet\\Control\\Session Manager\\Environment"
  )

-- | The location of the system path in the Windows registry.
userPathKey :: (HKEY, String)
userPathKey =(hKEY_CURRENT_USER, "Environment")

-- | Use the given function to modify the system path in the Windows
-- registry.
modifySysPath :: (String -> String) -> IO ()
modifySysPath = uncurry $ modifyRegPath sysPathKey

-- | Use the given function to modify the user path in the Windows
-- registry.
modifyUserPath :: (String -> String) -> IO ()
modifyUserPath = uncurry $ modifyRegPath userPathKey

-- | Use the given function to modify a value of the @PATH@ value in
-- an entry in the Windows registry.
modifyRegPath :: (String -> String) -> HKEY ->  String -> IO ()
modifyRegPath f rootKey keyPath = do
    hkey <- regOpenKeyEx rootKey keyPath perms
    -- Note that reqQueryValue is not the win32 function
    -- RegQueryValue(). It is a souped-up version of
    -- RegQueryValueEx().
    oldValue <- regQueryValue hkey (Just "PATH")
    _ <- regSetStringValue hkey "PATH" $ f oldValue
    _ <- regCloseKey hkey
  where
    perms = kEY_QUERY_VALUE .|. kEY_SET_VALUE

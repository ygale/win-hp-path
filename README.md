## win-hp-path: Work with multiple Haskell Platform versions on Windows

This project provides the `use-hp` command for working with multiple
versions of the Haskell Platform on Windows.

### Quick start

#### Installation

1. Make sure the folder `C:\Program Files\Haskell\bin` exists.
1. Get the latest Haskell Platform. Run `cabal install win-hp-path` and copy the resulting executable `use-hppath.exe` into `C:\Program Files\Haskell\bin`.
1. Download `use-hp.bat` from this GitHub project and copy it into `C:\Program Files\Haskell\bin`.
1. Install whatever other Haskell Platform versions you need. Ignore the warning about having more than one Haskell Platform version.
1. Download `find-hp.bat` from this GitHub project. Edit it so that the paths point to where you have Haskell Platform versions installed. Copy it into `C:\Program Files\Haskell\bin`.
1. Optional - if you want to use the same version of cabal.exe with all of your Haskell Platform versions, copy it into `C:\Program Files\Haskell\bin`.

In the future, when you install or remove Haskell Platform versions,
edit `find-hp.bat` and keep the paths up-to-date.

#### Usage

1. Open a command prompt window.
1. Run `use-hp <ghc-version-number>`. For example: `use-hp 7.8.3`

Optional - pick one version of Haskell Platform that you want to be
your default. Do the above steps to activate it in one window.  Run
`echo %PATH%`, and use that information to correct the values of your
path permanently in System Properties. Now any new command prompt will
work with your default Haskell Platform right from the
start without having to run `use-hp`.

Running `use-hp` enters a "sub-shell" within the same command prompt
window and temporarily modifies the path to use the Haskell Platform
you specified.  To return to the original environment, enter the
command `exit`.

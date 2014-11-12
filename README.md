## win-hp-path: Work with multiple Haskell Platform versions on Windows

Get it on [Hackage](http://hackage.haskell.org/package/win-hp-path).

This package provides the `use-hppath` program for working with
multiple versions of the Haskell Platform on Windows.

Warning! The use of this program removes any folders containing the
string `"Haskell"` from your PATH. For Haskell Platform users on
Windows, any such folder is only put into the PATH by Haskell Platform
installations, so that behavior is what you want.

When run in a command prompt window, the `use-hppath` program enters a
sub-shell where the PATH is modified to use the Haskell Platform
installed at the path specified on the comand line. In other words, it
changes the version of the Haskell Platform in the current command
prompt window only. To return to the original command line
environment, enter the command `exit`.

Example usage:

    use-hppath "C:\\Program Files (x86)\\Haskell Platform\\2014.2.0.0"

Please note: `use-hppath` always ensures that the hard-coded path
`C:\Program Files\\Haskell\\bin` is located on the PATH and overrides
all other Haskell-related paths. This is similar to, but slightly
different from, the behavior of the Haskell Platform itself. This can
be a good place to put these executables and the batch files described
below.

### Batch files

This package also provides batch files that allow you to select an
active Haskell Platform version by specifying the GHC version, similar
to how the corresponding shell scripts work on Linux and Mac OS X.

The batch file `use-hp.bat` is a wrapper for `use-hppath`. The script
`find-hp.bat` provides the mapping from GHC version to Haskell
Platform installation folder. Currently, you must edit `find-hp.bat`
by hand.

Place the two batch files somewhere on the PATH, such as
`C:\\Program Files\\Haskell\\bin`.
Edit `find-hp.bat` to match the paths on your own PC when you first
install it, and whenever you modify the set of Haskell Platform
versions installed on your PC.

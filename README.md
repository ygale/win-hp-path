==win-hp-path: Work with multiple Haskell Platform versions on Windows

Get it on [Hackage](http://hackage.haskell.org/package/win-hp-path).

This package provides two programs for working
with multiple versions of the Haskell Platform on
Windows.

The `activate-hspath` program, when run in a
command prompt window, modifies the PATH
variables stored in the Windows registry to use
the Haskell Platform installed at the path
specified on the command line. This essentially
sets the default Haskell Platform that you will
usually work with. It also cleans up the mess
that is made in the PATH when you install or
uninstall an additional version of Haskell
Platform. Note, however, that this command does
*not* change the version of Haskell Platform in
any currently open command prompt window,
including the one you run the command in. It only
changes it for future command prompt windows that
you open.

Example usage:

    activate-hspath "C:\\Program Files (x86)\\Haskell Platform\\2014.2.0.0"

The `use-hppath` program, when run in a command
prompt window, enters a sub-shell where the PATH
is modified to use the Haskell Platform installed
at the path specified on the comand line. In
other words, it changes the version of the
Haskell Platform in the current command prompt
window only. To return to the original command
line environment, enter the command `exit`.

Example usage:

    use-hppath "C:\\Program Files (x86)\\Haskell Platform\\2014.2.0.0"

Please note: These programs always ensure that
the hard-coded path
`C:\Program Files\\Haskell\\bin`
is located on the PATH and overrides all other
Haskell-related paths. This is similar to, but
slightly different from, the behavior of the
Haskell Platform itself. This can be a good place
to put these executables and the batch files
described below.

Batch files: This package also provides batch
files that allow you to select an active Haskell
Platform version by specifying the GHC version,
similar to how the corresponding shell scripts
work on Linux and Mac OS X. The batch files
`activate-hp.bat` and `use-hp.bat`, respectively,
are wrappers for the above two programs. They
both use a third script, `find-hp.bat`, which
provides the mapping from GHC version to Haskell
Platform installation folder. Currently, you must
edit find-hp.bat by hand. Place the three batch
files somewhere on the PATH, such as
`C:\\Program Files\\Haskell\\bin`.
Edit `find-hp.bat` to match the paths on your own
PC when you first install it, and whenever you
modify the set of Haskell Platform versions
installed on your PC.

# mondo

This is a simple Common Lisp REPL, just like SLIME REPL that works on terminal.  
It's not intended to achieve the same behavior, but it aims to provide its functionality outside of Emacs.

## Prequisite

* [Roswell](https://github.com/roswell/roswell)
* [GNU Readline](https://tiswww.case.edu/php/chet/readline/rltop.html)

## Features

* Complete function/macro names
* Show an argument list
* Run a Lisp implementation by name and version
* Connect to a running Swank server

## Installation

```
$ ros install fukamachi/mondo
```

Be sure that `~/.roswell/bin` is added to `PATH`.

## Usage

```
$ mondo --help
Usage: mondo [OPTIONS...] [DIRECTORY]

OPTIONS:
    -L, --lisp [NAME]
        Run the specific Lisp implementation (Default: sbcl-bin)
    -S, --source-registry [SOURCE REGISTRY]
        Overwrite source registry of ASDF with the argument
    -Q, --quicklisp [QUICKLISP HOME]
        Use the different Quicklisp home from the default one.
        Would be useful when using Qlot.
    -h, --host [NAME]
        Hostname of the running Swank server to connect to
    -p, --port [PORT]
        Port of the running Swank server to connect to
    --no-color
        Disable colors
    --version
        Print version
    --help
        Print this message
    --debug
        Print debug logs

ARGUMENTS:
    DIRECTORY
        Optional.  If specified, add the directory path to ASDF source registry,
        and use its local Quicklisp if exists.
        ex) `mondo .` is equivalent to `mondo -S . -Q ./.qlot`.
```

## License

GNU General Public License v3.0

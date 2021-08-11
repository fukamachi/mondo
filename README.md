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
Usage: mondo [OPTIONS...]

OPTIONS:
    -L [NAME], --lisp [NAME]
        Run the specific Lisp implementation (Default: sbcl-bin)
    -h [NAME], --host [NAME]
        Hostname of the running Swank server to connect to
    -p [PORT], --port [PORT]
        Port of the running Swank server to connect to
    --no-color
        Disable colors
    --version
        Print version
    --help
        Print this message
    --debug
        Print debug logs
```

## License

GNU General Public License v3.0

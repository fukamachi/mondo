# mondo

Simple Common Lisp REPL just like SLIME REPL but on terminal.

## Prequisite

* [Roswell](https://github.com/roswell/roswell)
* [GNU Readline](https://tiswww.case.edu/php/chet/readline/rltop.html)

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

## Related work

* [CL-REPL](https://github.com/koji-kojiro/cl-repl)
  * More feature-rich, like highlighting, colorized output and shell-command execution

## License

GNU General Public License v3.0

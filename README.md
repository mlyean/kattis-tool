# Kattis Tool

## About

`kat` is a simple command line tool written in Haskell to easily
download and run the provided sample test cases on your machine.

## Installation

Ensure you already have Haskell `stack` installed.

To install, clone this repository and run `stack install`.

Note this has only been tested to work on Linux, and will not
(currently) work on Windows.

## Usage

For usage, run `kat --help`:

```
$ kat --help
kat - Test Kattis solutions locally

Usage: kat [ --help | --version | COMMAND ]

Available options:
  --help, -h       Show this help message.
  --version, -v    Show program version.

Available commands:
  test ID EXEC     Run test cases of the given problem ID on EXEC.
  get ID           Download test cases for the given problem ID.
                   This is automatically done by 'test'.
  clean            Remove all temporary files.
```

The main command here is `kat test`. For example, to test your solution
`main.cpp` on the problem with ID `abc`, you first need to compile your
solution.
```sh
g++ -o ./main -O2 main.cpp
kat test hello ./main
```

The test cases are downloaded into `/tmp/kattis/`. If for some reason
the download was corrupted (which should never happen, it should just
fail), you can run `kat clean` to clear the cached files.

If the test cases were previously downloaded and still exists, they will
be reused on subsequent runs.

You need an internet connection to download the test cases. You can
download and cache the files off-line using `kat get ID`.

## Ideas

A list of features I may implement in the future:

* Submit solutions directly from command line
* Check floating point numbers to a certain precision

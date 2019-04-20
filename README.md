# Kattis Tool

## About

This is a simple command line tool written in Haskell to easily
download and run the provided sample test cases on your machine.

## Installation

Ensure you already have Haskell `stack` installed.

To install, clone this repository and run `stack install`.

Note this has only been tested on Linux, and will not (currently)
work on Windows.

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

The main command here is `kat test`. To test your solution `main.o`
on the problem with ID `hello`, for example, run the following
command:
```
kat test hello ./main.o
```
Note that `EXEC` is the command to run your (already compiled) program.

The test cases are downloaded into `/tmp/kattis/`. If for some reason
the test cases got corrupted (which should never happen, the download
would probably just fail), you can run `kat clean` to clear the
downloads.

If the test cases were already downloaded and still exists, they will
be reused on subsequent runs.

You need an internet connection to download the test cases. You can
download and cache the test cases off-line using `kat get ID`.

## Ideas

A list of features I may implement in the future:

* Upload file directly from command line
* Check floating point numbers to a certain accuracy


# csv2xml

Utility for converting CSV (comma separated values) files to XML representation.

## How to build

The easiest way to build this project is to use the `stack` utility.
This is one of the most common tools for building projects written in *Haskell*.
To install `stack`, follow the [instructions](https://docs.haskellstack.org/en/stable/README/) from the official project website.

After the `stack` is installed, the project can be built and installed like this:

```sh
cd csv2xml/
stack install
```

## How to use

After this you can use `csv2xml` command line utility:

```sh
$ csv2xml --help
CSV to XML converter version 0.1.0.0 (haskell)

csv2xml [OPTIONS] [INPUT_FILE OUTPUT_FILE]

Common flags:
  -i    --input-encoding=ENC   Encoding of input CSV file. If not specified
                               then default system encoding will be used
  -o -e --output-encoding=ENC  Encoding of resulting XML file. If not
                               specified then UTF-8 will be used as default
  -?    --help                 Display help message
  -V    --version              Print version information
        --numeric-version      Print just the version number

Converts CSV file to XML representation.
```

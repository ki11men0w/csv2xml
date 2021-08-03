
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
$ csv2xml --help=100
CSV to XML converter version 0.1.0.0 (haskell)

csv2xml [OPTIONS] [INPUT_FILE [OUTPUT_FILE]]

Common flags:
  -i    --input-encoding=ENC        Encoding of input CSV file. If not specified then default
                                    system encoding will be used
  -o -e --output-encoding=ENC       Encoding of resulting XML file. If not specified then UTF-8
                                    will be used as default
  -r    --record-tag-name=TAG_NAME  Name for the record XML elements. If not specified the name
                                    of `r` will be used
  -f    --field-tag-name=TAG_NAME   Name for the field XML elements. If not specified the name of
                                    `f` will be used
  -x    --indexed-fields            By default all field elements are created with the same name.
                                    Use this option to add index number suffix to the field names
  -n    --namespace=XML_NAME_SPACE  XML namespace for all elements of the output XML document. By
                                    default no namespace is used.
  -t    --tab-delimited             Specify this option if the fields in the input CSV file are
                                    separated by a tab character. By default, it is assumed that
                                    the fields are separated by a comma
  -s    --store-records-source      If specified, each record will be supplemented with an
                                    additional element `sourceRecord`, which will contain the
                                    source text of the corresponding CSV record
  -?    --help                      Display help message
  -V    --version                   Print version information
        --numeric-version           Print just the version number

Converts CSV file to XML representation.
If an INPUT/OUTPUT file is not specified, STDIN/STDOUT will be used, respectively. You can also
explicitly specify the use of standard IO streams: to do this, you need to specify a hyphen ("-")
instead of the file names.
```

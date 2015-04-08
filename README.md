hmatrix-csv
===========
[![Build Status](https://travis-ci.org/grtlr/hmatrix-csv.svg)](https://travis-ci.org/grtlr/hmatrix-csv) [![Hackage](https://img.shields.io/hackage/v/hmatrix-csv.svg?style=flat)](https://hackage.haskell.org/package/hmatrix-csv)

Information
-----------

If you want to help improve this library, feel free to file an issue or send a pull-request on github. Every feedback is appreciated.
As of now only matrices of type Double are supported.

Example
-------

For decoding we have to specify, if there is a header. This can be done with the HasHeader/NoHeader type:

    >>> decodeMatrix NoHeader "1.0,2.0,3.0\r\n4.0,5.0,6.0\r\n7.0,8.0,9.0\r\n"
    (3><3)
    [ 1.0, 2.0, 3.0
    , 4.0, 5.0, 6.0
    , 7.0, 8.0, 9.0 ]

Cassava, which is used for parsing the .csv files uses overloaded string literals. If you try this in ghci, make sure to start it with the right flag:

    >>> ghci -XOverloadedStrings

Encoding a file works pretty much the same, except that we do not need to specify a header.

    >>> encodeMatrix $ matrix 3 [1,2,3,4,5,6,7,8,9]
    "1.0,2.0,3.0\r\n4.0,5.0,6.0\r\n7.0,8.0,9.0\r\n"


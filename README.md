# hw-json-standard-cursor
[![master](https://circleci.com/gh/haskell-works/hw-json-standard-cursor/tree/master.svg?style=svg)](https://circleci.com/gh/haskell-works/hw-json-standard-cursor/tree/master)

`hw-json-standard-cursor` is support library for `hw-json`, a succinct JSON parsing library.

It is used to create "interest-bits" (`*.ib.idx`) files and
"balanced-parenthesis" (`*.bp.idx`) files.  These can then be loaded by the `hw-json`
library to enable parsing of parts of the original JSON file without deserialising
the entire JSON file into objects in memory.

For more information see [`hw-json`](https://github.com/haskell-works/hw-json).

## References

* [Semi-Indexing Semi-Structured Data in Tiny Space](http://www.di.unipi.it/~ottavian/files/semi_index_cikm.pdf)
* [Succinct Data Structures talk by Edward Kmett](https://www.youtube.com/watch?v=uA0Z7_4J7u8)
* [Typed Tagless Final Interpreters](http://okmij.org/ftp/tagless-final/course/lecture.pdf)

## Special mentions

* [Sydney Paper Club](http://www.meetup.com/Sydney-Paper-Club/)

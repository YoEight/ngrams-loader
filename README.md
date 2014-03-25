ngrams-loader
=============

Ngrams loader based on http://www.ngrams.info format

[![Build Status](https://travis-ci.org/YoEight/ngrams-loader.png?branch=master)](https://travis-ci.org/YoEight/ngrams-loader)

Installation
------------
Supposed you have at least `cabal 1.18` installed

```
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal configure
$ cabal install

-- program located in ~/.cabal-sandbox/bin
```

Usage
-----

```
usage: ngrams-loader [options] <n-grams file> <SQLite file>
  [-2,--bigram]     Parses bigrams
  [-3,--trigram]    Parses trigrams
  [-4,--quadgram]   Parses 4-grams
  [-5,--pentagram]  Parses 5-grams
  [-c,--create]     Creates table before inserts
  <n-grams file>    N-grams file
  <SQLite file>     SQlite db file
```

Example
-------

```
ngrams-loader --bigram --create w2.txt bigram.db

```
It parses each line of `w2.txt` as a bigram, create bigram table before performing inserts and saves everything in `bigram.db`

Figures
-------

Specs

- Core i7 3770 @ 3.4GHz
- Gentoo with 3.12.13 Linux kernel (64bits)
- 1.055.386 lines bigram file
 
`ngrams-loader --bigram --create w2.txt bigram.db` gets

```
real	0m16.244s
user	0m15.597s
sys	  0m0.143s

```

Sql Schemas
-----------

Bigram

```sql
create table bigrams(
  frequence int,
  word1 varchar(100),
  word2 varchar(100)
);
```

Trigram

```sql
create table tridgrams(
  frequence int,
  word1 varchar(100),
  word2 varchar(100),
  word3 varchar(100)
);
```

4-gram

```sql
create table quadgrams(
  frequence int,
  word1 varchar(100),
  word2 varchar(100),
  word3 varchar(100),
  word4 varchar(100)
);
```

5-gram

```sql
create table pentagrams(
  frequence int,
  word1 varchar(100),
  word2 varchar(100),
  word3 varchar(100),
  word4 varchar(100),
  word5 varchar(100)
);
```

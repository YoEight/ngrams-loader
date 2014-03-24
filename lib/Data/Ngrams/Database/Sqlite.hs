{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module : Data.Ngrams.Database.Sqlite
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
----------------------------------------------------------------------------
module Data.Ngrams.Database.Sqlite
    ( Command
    , cmdExec
    , cmdCreateTable
    , bigramCommand
    , trigramCommand
    , quadgramCommand
    , pentagramCommand
    ) where

----------------------------------------------------------------------------
import Database.SQLite.Simple

----------------------------------------------------------------------------
import Data.Ngrams.Type

----------------------------------------------------------------------------
data Command a =
    Command
    { cmdCreateTable :: Query
    , cmdExec        :: a -> Connection -> IO ()
    }

----------------------------------------------------------------------------
bigramCommand :: Command Bigram
bigramCommand = Command bigramCreateTable bigramEx

----------------------------------------------------------------------------
trigramCommand :: Command Trigram
trigramCommand = Command trigramCreateTable trigramEx

----------------------------------------------------------------------------
quadgramCommand :: Command Quadgram
quadgramCommand = Command quadgramCreateTable quadgramEx

----------------------------------------------------------------------------
pentagramCommand :: Command Pentagram
pentagramCommand = Command pentagramCreateTable pentagramEx

----------------------------------------------------------------------------
bigramCreateTable :: Query
bigramCreateTable =
    "create table bigrams(\
    \  frequence int,\
    \  word1 varchar(100),\
    \  word2 varchar(100)\
    \);"

----------------------------------------------------------------------------
trigramCreateTable :: Query
trigramCreateTable =
    "create table trigrams(\
    \  frequence int,\
    \  word1 varchar(100),\
    \  word2 varchar(100),\
    \  word3 varchar(100)\
    \);"

----------------------------------------------------------------------------
quadgramCreateTable :: Query
quadgramCreateTable =
    "create table quadgrams(\
    \  frequence int,\
    \  word1 varchar(100),\
    \  word2 varchar(100),\
    \  word3 varchar(100),\
    \  word4 varchar(100)\
    \);"

----------------------------------------------------------------------------
pentagramCreateTable :: Query
pentagramCreateTable =
    "create table pentagrams(\
    \  frequence int,\
    \  word1 varchar(100),\
    \  word2 varchar(100),\
    \  word3 varchar(100),\
    \  word4 varchar(100),\
    \  word5 varchar(100)\
    \);"

----------------------------------------------------------------------------
bigramInsert :: Query
bigramInsert =
    "insert into bigrams (frequence, word1, word2) values (?,?,?);"

----------------------------------------------------------------------------
trigramInsert :: Query
trigramInsert =
    "insert into trigrams (frequence, word1, word2, word3) values (?,?,?,?);"

----------------------------------------------------------------------------
quadgramInsert :: Query
quadgramInsert =
    "insert into quadgrams (frequence, word1, word2, word3, word4) values \
    \(?,?,?,?,?);"

----------------------------------------------------------------------------
pentagramInsert :: Query
pentagramInsert =
    "insert into pentagrams (frequence, word1, word2, word3, word4, word5) \
    \values (?,?,?,?,?);"

----------------------------------------------------------------------------
bigramEx:: Bigram -> Connection -> IO ()
bigramEx (Bigram freq w1 w2) con =
    execute con bigramInsert (freq, w1, w2)

----------------------------------------------------------------------------
trigramEx :: Trigram -> Connection -> IO ()
trigramEx (Trigram freq w1 w2 w3) con =
    execute con trigramInsert (freq, w1, w2, w3)

----------------------------------------------------------------------------
quadgramEx :: Quadgram -> Connection -> IO ()
quadgramEx (Quadgram freq w1 w2 w3 w4) con =
    execute con quadgramInsert (freq, w1, w2, w3, w4)

----------------------------------------------------------------------------
pentagramEx :: Pentagram -> Connection -> IO ()
pentagramEx (Pentagram freq w1 w2 w3 w4 w5) con =
    execute con pentagramInsert (freq, w1, w2, w3, w4, w5)

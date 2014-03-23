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
    , cmdBind
    , cmdCreateTable
    , cmdInsert
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
    , cmdInsert      :: Query
    , cmdBind        :: a -> Statement -> IO ()
    }

----------------------------------------------------------------------------
bigramCommand :: Command Bigram
bigramCommand = Command bigramCreateTable bigramInsert bigramBind

----------------------------------------------------------------------------
trigramCommand :: Command Trigram
trigramCommand = Command trigramCreateTable trigramInsert trigramBind

----------------------------------------------------------------------------
quadgramCommand :: Command Quadgram
quadgramCommand = Command quadgramCreateTable quadgramInsert quadgramBind

----------------------------------------------------------------------------
pentagramCommand :: Command Pentagram
pentagramCommand = Command pentagramCreateTable pentagramInsert pentagramBind

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
    "insert into bigrams (frequence, word1, word2) values (?,?,?)"

----------------------------------------------------------------------------
trigramInsert :: Query
trigramInsert =
    "insert into trigrams (frequence, word1, word2, word3) values (?,?,?,?)"

----------------------------------------------------------------------------
quadgramInsert :: Query
quadgramInsert =
    "insert into quadgrams (frequence, word1, word2, word3, word4) values \
    \(?,?,?,?,?)"

----------------------------------------------------------------------------
pentagramInsert :: Query
pentagramInsert =
    "insert into pentagrams (frequence, word1, word2, word3, word4, word5) \
    \values (?,?,?,?,?)"

----------------------------------------------------------------------------
bigramBind:: Bigram -> Statement -> IO ()
bigramBind (Bigram freq w1 w2) st =
    bind st (freq, w1, w2)

----------------------------------------------------------------------------
trigramBind :: Trigram -> Statement -> IO ()
trigramBind (Trigram freq w1 w2 w3) st =
    bind st (freq, w1, w2, w3)

----------------------------------------------------------------------------
quadgramBind :: Quadgram -> Statement -> IO ()
quadgramBind (Quadgram freq w1 w2 w3 w4) st =
    bind st (freq, w1, w2, w3, w4)

----------------------------------------------------------------------------
pentagramBind :: Pentagram -> Statement -> IO ()
pentagramBind (Pentagram freq w1 w2 w3 w4 w5) st =
    bind st (freq, w1, w2, w3, w4, w5)

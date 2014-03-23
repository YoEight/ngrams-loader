-----------------------------------------------------------------------------
-- |
-- Module : Data.Ngrams.Type
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
----------------------------------------------------------------------------
module Data.Ngrams.Type where

----------------------------------------------------------------------------
import Data.Text (Text)

----------------------------------------------------------------------------
data Bigram =
    Bigram
    { biFreq  :: !Integer
    , biWord1 :: !Text
    , biWord2 :: !Text
    }

data Trigram =
    Trigram
    { triFreq  :: !Integer
    , triWord1 :: !Text
    , triWord2 :: !Text
    , triWord3 :: !Text
    }

data Quadgram =
    Quadgram
    { quadFreq  :: !Integer
    , quadWord1 :: !Text
    , quadWord2 :: !Text
    , quadWord3 :: !Text
    , quadWord4 :: !Text
    }

data Pentagram =
    Pentagram
    { pentaFreq  :: !Integer
    , pentaWord1 :: !Text
    , pentaWord2 :: !Text
    , pentaWord3 :: !Text
    , pentaWord4 :: !Text
    , pentaWord5 :: !Text
    }

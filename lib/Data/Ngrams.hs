-----------------------------------------------------------------------------
-- |
-- Module : Data.Ngrams
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
----------------------------------------------------------------------------
module Data.Ngrams
    ( module Data.Ngrams.Database.Sqlite
    , module Data.Ngrams.Parser
    , module Data.Ngrams.Process
    ) where

----------------------------------------------------------------------------
import Data.Ngrams.Database.Sqlite
import Data.Ngrams.Parser
import Data.Ngrams.Process

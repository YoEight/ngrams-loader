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
    , mkProcess
    , runProcess_
    ) where

----------------------------------------------------------------------------
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Attoparsec.Text         (Parser)
import Data.Machine                 (ProcessT, runT_, (~>))
import Data.Text                    (Text)

----------------------------------------------------------------------------
import Data.Ngrams.Database.Sqlite
import Data.Ngrams.Parser
import Data.Ngrams.Process

----------------------------------------------------------------------------
mkProcess :: FilePath
          -> FilePath
          -> Bool
          -> Parser a
          -> Command a
          -> ProcessT (ResourceT IO) Text ()
mkProcess file db create parser cmd =
    sourceLines file ~> parseLine parser ~> saveDB create db cmd

----------------------------------------------------------------------------
runProcess_ :: ProcessT (ResourceT IO) Text a -> IO ()
runProcess_ = runResourceT . runT_

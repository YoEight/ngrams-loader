{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
-----------------------------------------------------------------------------
-- |
-- Module : Data.Ngrams.Process
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
----------------------------------------------------------------------------
module Data.Ngrams.Process where

----------------------------------------------------------------------------
import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Exception
import Data.Typeable
import System.IO (IOMode(..), hClose, hIsEOF, openFile)

----------------------------------------------------------------------------
import           Data.Attoparsec.Text (Parser, parseOnly)
import           Database.SQLite.Simple
import           Control.Monad.Trans (lift, liftIO)
import           Control.Monad.Trans.Resource
import           Data.Machine
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T

----------------------------------------------------------------------------
import Data.Ngrams.Database.Sqlite

----------------------------------------------------------------------------
data ParseException = ParseException !String deriving (Show, Typeable)

instance Exception ParseException

----------------------------------------------------------------------------
-- | Creates a `Source` from the lines of a file, using the `ResourceT`
--   monad to ensure the file is closed when processing the stream of lines
--   is finished
sourceLines :: FilePath -> SourceT (ResourceT IO) T.Text
sourceLines path = construct $ do
    (key, h) <- lift $ allocate (openFile path ReadMode) hClose
    go key h
  where
    go key h =
        let readLine = lift $ liftIO $ T.hGetLine h
            isEOF    = lift $ liftIO $ hIsEOF h

            closing = do
                lift $ release key
                stop

            yielding = do
                line <- readLine
                yield line
                go key h in

        isEOF >>= \eof -> if eof then closing else yielding

----------------------------------------------------------------------------
-- | Applies a `Parser` on each line. If an error occurs, an Exception is
--   raised
parseLine :: MonadThrow m => Parser a -> ProcessT m T.Text  a
parseLine p = repeatedly $ do
    line <- await
    case parseOnly p line of
        Left e  -> lift $ monadThrow $ ParseException e
        Right a -> yield a

----------------------------------------------------------------------------
-- | Saves each entry in a SQLite database with submitted `Command`
saveDB :: Bool -> FilePath -> Command a -> ProcessT (ResourceT IO) a ()
saveDB create path c = construct $ do
    (ckey, con) <- lift $ allocate (open path) close
    when create $ liftIO $ execute_ con createTable
    lift $ liftIO $ execute_ con "begin"
    loop ckey con
  where
    exec        = cmdExec c
    createTable = cmdCreateTable c

    loop ckey con =
        let closing = do
                lift $ liftIO $ do
                    execute_ con "end"
                    release ckey
                stop in

        do a <- await <|> closing
           lift $ liftIO $ exec a con
           loop ckey con

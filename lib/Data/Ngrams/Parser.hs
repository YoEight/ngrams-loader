-----------------------------------------------------------------------------
-- |
-- Module : Data.Ngrams.Parser
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
----------------------------------------------------------------------------
module Data.Ngrams.Parser
    ( parserBigram
    , parserTrigram
    , parserQuadgram
    , parserPentagram
    ) where

----------------------------------------------------------------------------
import Data.Char (isSpace)

----------------------------------------------------------------------------
import Data.Attoparsec.Text (Parser, decimal, skipSpace, takeWhile1)
import Data.Text            (Text)

----------------------------------------------------------------------------
import Data.Ngrams.Type

----------------------------------------------------------------------------
parserBigram :: Parser Bigram
parserBigram = do
    freq <- decimal
    skipSpace
    w1 <- _word
    skipSpace
    w2 <- _word
    return $ Bigram freq w1 w2

----------------------------------------------------------------------------
parserTrigram :: Parser Trigram
parserTrigram = do
    Bigram freq w1 w2 <- parserBigram
    skipSpace
    w3 <- _word
    return $ Trigram freq w1 w2 w3

----------------------------------------------------------------------------
parserQuadgram :: Parser Quadgram
parserQuadgram = do
    Trigram freq w1 w2 w3 <- parserTrigram
    skipSpace
    w4 <- _word
    return $ Quadgram freq w1 w2 w3 w4

----------------------------------------------------------------------------
parserPentagram :: Parser Pentagram
parserPentagram = do
    Quadgram freq w1 w2 w3 w4 <- parserQuadgram
    skipSpace
    w5 <- _word
    return $ Pentagram freq w1 w2 w3 w4 w5

----------------------------------------------------------------------------
_word :: Parser Text
_word = takeWhile1 (not . isSpace)

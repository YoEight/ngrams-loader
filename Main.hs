-----------------------------------------------------------------------------
-- |
-- Module : Main
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
----------------------------------------------------------------------------
module Main where

----------------------------------------------------------------------------
import Data.List (find)

----------------------------------------------------------------------------
import System.Console.ParseArgs

----------------------------------------------------------------------------
import Data.Ngrams

----------------------------------------------------------------------------
-- | Command-line Option type
data Option = Bi
            | Tri
            | Quad
            | Penta
            | Create
            | File
            | DB deriving (Ord, Eq, Show, Enum)

----------------------------------------------------------------------------
main :: IO ()
main = do
    as <- parseArgsIO ArgsComplete desc
    let nType  = parserType as
        file   = getRequiredArg as File
        db     = getRequiredArg as DB
        create = gotArg as Create
        mk     = mkProcess file db create

        ngram Bi    = mk parserBigram bigramCommand
        ngram Tri   = mk parserTrigram trigramCommand
        ngram Quad  = mk parserQuadgram quadgramCommand
        ngram Penta = mk parserPentagram pentagramCommand
        ngram _     = error "The impossible happens"

    case nType of
        Nothing -> print "Please toggled one of N-grams type"
        Just p  -> runProcess_ $ ngram p

----------------------------------------------------------------------------
-- | Command-line Options parser description
desc :: [Arg Option]
desc = [ Arg { argIndex = Bi
             , argAbbr  = Just '2'
             , argName  = Just "bigram"
             , argData  = Nothing
             , argDesc  = "Parses bigrams"
             }
       , Arg { argIndex = Tri
             , argAbbr  = Just '3'
             , argName  = Just "trigram"
             , argData  = Nothing
             , argDesc  = "Parses trigrams"
             }
       , Arg { argIndex = Quad
             , argAbbr  = Just '4'
             , argName  = Just "quadgram"
             , argData  = Nothing
             , argDesc  = "Parses 4-grams"
             }
       , Arg { argIndex = Penta
             , argAbbr  = Just '5'
             , argName  = Just "pentagram"
             , argData  = Nothing
             , argDesc  = "Parses 5-grams"
             }
       , Arg { argIndex = Create
             , argAbbr  = Just 'c'
             , argName  = Just "create"
             , argData  = Nothing
             , argDesc  = "Creates table before inserts"
             }
       , Arg { argIndex = File
             , argAbbr  = Nothing
             , argName  = Nothing
             , argData  = argDataRequired "n-grams file" ArgtypeString
             , argDesc  = "N-grams file"
             }
       , Arg { argIndex = DB
             , argAbbr  = Nothing
             , argName  = Nothing
             , argData  = argDataRequired "SQLite file" ArgtypeString
             , argDesc  = "SQlite db file"
             }
       ]

----------------------------------------------------------------------------
-- | Gets toggled Ngrams flag
parserType :: Args Option -> Maybe Option
parserType as = find (gotArg as) [Bi .. Penta]

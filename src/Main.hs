{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 
module Main where
import SphinxEscape (escapeSphinxQueryString, parseQuery, extractTagFilters
                    ,formatQuery, formatFilters)
import System.Environment
import Data.List
import Options.Applicative
import Control.Applicative


data Options = Options {
    optMode :: Mode
  , optInput :: Maybe String
  }

data Mode = Convert | Parse | Extract deriving Read

mode :: Parser Options
mode = Options
  <$> (read <$>
      (strOption 
          (
             (long "mode" <>
              short 'm' <>
              help "Mode [Convert | Parse | Extract]" <>
              value "Convert")
          ))
      )
  <*> (
          (Just <$> strArgument (metavar "RAW-STRING" <> help "sphinx raw input expression"))
          <|> pure Nothing
      )

opt :: ParserInfo Options
opt = info (helper <*> mode) (header "sphinxesc")
  

main = do
  Options{..} <- execParser opt
  input <- maybe getContents return optInput
  let p = parseQuery input
      (ts, qs) = extractTagFilters p
      (tags, q) = (formatFilters ts, formatQuery qs)
  case optMode of 
      Extract -> do
        let output = extractTagFilters . parseQuery $ input
        putStrLn $ show output
      Convert -> do
        let output = q ++ (if null tags then "" else (" " ++ show tags))
        putStrLn output
      Parse -> do
        print $ parseQuery input
  

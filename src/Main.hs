{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 
module Main where
import SphinxEscape (escapeSphinxQueryString, parseQuery, extractFilters
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
      (ts, as, qs)    = extractFilters p
      (tags, authors) = formatFilters ts as
      q               = formatQuery qs
  case optMode of 
      Extract -> do
        let output = extractFilters . parseQuery $ input
        putStrLn $ show output
      Convert -> do
        let indent     = if null q then "" else " "
            tagsStr    = show tags
            authorsStr = show authors
            output     = q ++ indent ++ tagsStr ++ authorsStr
        putStrLn output
      Parse -> do
        print $ parseQuery input
  

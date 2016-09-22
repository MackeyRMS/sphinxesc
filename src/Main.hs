{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 
module Main where
import SphinxEscape (parseQuery, parseQuery', extractFilters
                    ,formatQuery, formatQuery', formatFilters
                    ,transformQuery)
import System.Environment
import Data.List
import Options.Applicative
import Control.Applicative


data Options = Options {
    optMode :: Mode
  , optInput :: Maybe String
  }

data Mode = Convert | Parse | Extract | Transform deriving Read

mode :: Parser Options
mode = Options
  <$> (read <$>
      (strOption 
          (
             (long "mode" <>
              short 'm' <>
              help "Mode [Convert | Parse | Extract | Transform]" <>
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
      q               = formatQuery $ qs
      p'              = parseQuery' q
      q'              = formatQuery' p'
  case optMode of 
      Transform -> let (_, _, q) = transformQuery input in putStrLn q
      Parse     -> print p >> print p'
      Extract   -> putStrLn $ show $ (ts, as, qs)
      Convert   -> do
        let indent     = if null q' then "" else " "
            tagsStr    = show tags
            authorsStr = show authors
            output     = if null q' && null tags && null authors
                           then ""
                           else q' ++ indent ++ tagsStr ++ authorsStr
        putStrLn output
  

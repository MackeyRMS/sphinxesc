{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables, FlexibleContexts #-} 
module SphinxEscape where
import Control.Applicative
import Data.Char
import Data.Functor.Identity (Identity)
import Data.List
import Data.String.Utils (strip)
import Text.Parsec hiding (many, (<|>)) 
 
-- | Extract tag and author filters and prepare resulting
--   query string for submission to Sphinx.
transformQuery :: String                        -- ^ Original query string
               -> ([String], [String], String)  -- ^ tag names, author names, query
transformQuery q = (ts', as', q')
  where 
    (ts, as, qs) = extractFilters $ parseQuery q
    (ts', as')   = formatFilters ts as
    q'           = formatQuery . parseQuery' . formatQuery $ qs

extractFilters :: [Expression] -> ([Expression], [Expression], [Expression])
extractFilters es = (ts, as, q')
  where
    (ts, q)  = partition isTagFilter es
    (as, q') = partition isAuthorFilter q

formatQuery :: [Expression] -> String
formatQuery = strip . intercalate " " . map (strip . rawString)

formatQuery' :: [Expression] -> String
formatQuery' = strip . intercalate " " . map (strip . expressionToString)

formatFilters :: [Expression] -> [Expression] -> ([String], [String])
formatFilters ts as = (map tagNameFromExpression ts, map authorNameFromExpression as)

isTagFilter :: Expression -> Bool
isTagFilter (TagFilter _) = True
isTagFilter _             = False

tagNameFromExpression :: Expression -> String
tagNameFromExpression (TagFilter t) = t
tagNameFromExpression _             = error "tagNameFromExpression: not tag"

isAuthorFilter :: Expression -> Bool
isAuthorFilter (AuthorFilter _) = True
isAuthorFilter _                = False

authorNameFromExpression :: Expression -> String
authorNameFromExpression (AuthorFilter t) = t
authorNameFromExpression _                = error "authorNameFromExpression: not author"

-- Just a simplified syntax tree. Besides this, all other input has its
-- non-alphanumeric characters stripped, including double and single quotes and
-- parentheses

data Expression = 
        TagFilter String
      | AuthorFilter String
      | Literal String
      | Phrase String
      | AndOrExpr Conj Expression Expression
  deriving Show

data Conj = And | Or
  deriving Show

parseQuery :: String -> [Expression]
parseQuery  inp =
  case Text.Parsec.parse (many expression) "" inp of
    Left x   -> error $ "parser failed: " ++ show x
    Right xs -> xs

parseQuery' :: String -> [Expression]
parseQuery'  inp =
  case Text.Parsec.parse (many expression') "" inp of
    Left x   -> error $ "parser failed: " ++ show x
    Right xs -> xs

rawString :: Expression -> String
rawString (TagFilter s)    = "tag:" ++ maybeQuote s
rawString (AuthorFilter s) = "author:" ++ maybeQuote s
rawString (Literal s)      = s
rawString (Phrase s)       = quote s -- no need to escape the contents
rawString (AndOrExpr c a b) =
   let a' = rawString a
       b' = rawString b
       c' = conjToString c
   -- if either a' or b' is just whitespace, just choose one or the other
   in case (all isSpace a', all isSpace b') of
       (True, False) -> b'
       (False, True) -> a'
       (False, False) -> a' ++ c' ++ b'
       _  -> ""

-- escapes expression to string to pass to sphinx
expressionToString :: Expression -> String
expressionToString (TagFilter s)    = "tag:" ++ maybeQuote (escapeString s)
expressionToString (AuthorFilter s) = "author:" ++ maybeQuote (escapeString s)
expressionToString (Literal s)      = escapeString s
expressionToString (Phrase s)       = quote s -- no need to escape the contents
expressionToString (AndOrExpr c a b) =
   let a' = expressionToString a
       b' = expressionToString b
       c' = conjToString c
   -- if either a' or b' is just whitespace, just choose one or the other
   in case (all isSpace a', all isSpace b') of
       (True, False) -> b'
       (False, True) -> a'
       (False, False) -> a' ++ c' ++ b'
       _  -> ""

quote :: String -> String
quote s = "\"" ++ s ++ "\""

maybeQuote :: String -> String
maybeQuote s = if any isSpace s then quote s else s

conjToString :: Conj -> String
conjToString And = " & "
conjToString Or  = " | "

-- removes all non-alphanumerics from literal strings that could be parsed
-- mistakenly as Sphinx Extended Query operators
escapeString :: String -> String
escapeString = map stripAlphaNum

stripAlphaNum :: Char -> Char
stripAlphaNum s | isAlphaNum s = s
                | otherwise    = ' '


type Parser' = ParsecT String () Identity 

-----------------------------------------------------------------------
-- Parse filters

expression :: Parser' Expression
expression = try tagFilter <|> try authorFilter <|> try phrase <|> literal 

tagFilter :: Parser' Expression
tagFilter = do
   try (string "tag:") <|> try (string "@(tag_list)") <|> string "@tag_list"
   many space
   x <- (try phrase <|> literal)
   let
     s = case x of
       Phrase p  -> p
       Literal l -> l
       otherwise -> "" -- will never be returned (parse error)
   return $ TagFilter s

authorFilter :: Parser' Expression
authorFilter = do
   string "author:"
   many space
   x <- (try phrase <|> literal)
   let
     s = case x of
       Phrase p  -> p
       Literal l -> l
       otherwise -> "" -- will never be returned (parse error)
   return $ AuthorFilter s

phrase :: Parser' Expression
phrase = do
    char '"'
    xs <- manyTill anyChar (char '"')
    return . Phrase $ xs

literalStop :: Parser' ()
literalStop = (choice [ 
    lookAhead (tagFilter >> return ()) 
  , lookAhead (authorFilter >> return ()) 
  , lookAhead (phrase >> return ())
  , (space >> return ())
  , eof
  ])
  <?> "literalStop"

literal :: Parser' Expression
literal = do
    a  <- anyChar
    xs <- manyTill anyChar (try literalStop)
    return . Literal $ a:xs

-----------------------------------------------------------------------
-- Parse query string

expression' :: Parser' Expression
expression' = try andOrExpr <|> try phrase <|> literal'

andOrExpr :: Parser' Expression
andOrExpr = do 
   a <- (try phrase <|> literal')
   x <- try conjExpr
   b <- expression'  -- recursion
   return $ AndOrExpr x a b

conjExpr :: Parser' Conj
conjExpr = andExpr <|> orExpr

andExpr :: Parser' Conj
andExpr = mkConjExpr ["and", "AND", "&"] And

orExpr :: Parser' Conj
orExpr = mkConjExpr ["|", "or", "OR"] Or

mkConjExpr :: [String] -> Conj -> Parser' Conj
mkConjExpr xs t = 
    try (many1 space >> choice (map (string . (++" ")) xs))
    >> return t

literalStop' :: Parser' ()
literalStop' = (choice [
    lookAhead (conjExpr >> return ())
  , lookAhead (phrase >> return ())
  , (space >> return ())
  , eof
  ])
  <?> "literalStop'"

literal' :: Parser' Expression
literal' = do
    a  <- anyChar
    xs <- manyTill anyChar (try literalStop')
    return . Literal $ a:xs


{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables, FlexibleContexts #-} 
module SphinxEscape where
import Control.Applicative
import Data.Functor.Identity (Identity )
import Text.Parsec hiding (many, (<|>)) 
import Data.Char
import Data.List
 

-- Main function
escapeSphinxQueryString :: String -> String
escapeSphinxQueryString s = intercalate " " . map expressionToString . parseQuery $ s


-- Just a simplified syntax tree. Besides this, all other input has its
-- non-alphanumeric characters stripped, including double and single quotes and
-- parentheses

data Expression = 
        TagFieldSearch String 
      | Literal String
      | Phrase String
      | AndOrExpr Conj Expression Expression 
  deriving Show

data Conj = And | Or
  deriving Show

parseQuery :: String -> [Expression]
parseQuery  inp =
  case Text.Parsec.parse (many expression) "" inp of
    Left x -> error $ "parser failed: " ++ show x
    Right xs -> xs

-- escapes expression to string to pass to sphinx
expressionToString :: Expression -> String
expressionToString (TagFieldSearch s) = "@tag_list" ++ escapeString s
expressionToString (Literal s) = escapeString s
expressionToString (Phrase s) = "\"" ++ s ++ "\"" -- no need to escape the contents
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

conjToString :: Conj -> String
conjToString And = " & "
conjToString Or = " | "

-- removes all non-alphanumerics from literal strings that could be parsed
-- mistakenly as Sphinx Extended Query operators
escapeString :: String -> String
escapeString s = map (stripAlphaNum) s

stripAlphaNum :: Char -> Char
stripAlphaNum s | isAlphaNum s = s
                | otherwise = ' '


type Parser' = ParsecT String () Identity 

-- | can be literal or tag field or nothing, followed an expression
topLevelExpression :: Parser' [Expression]
topLevelExpression = do
    a <- option [] ((:[]) <$> (tagField <|> literal))
    xs <- many expression
    return $ a ++ xs


expression :: Parser' Expression
expression = (try andOrExpr) <|> try tagField <|> try phrase <|> literal 

tagField :: Parser' Expression
tagField = do
   char '@'
   string "tag_list" <|> string "(tag_list)"
   s <- manyTill anyChar (try literalStop)
   return $ TagFieldSearch s


andOrExpr :: Parser' Expression
andOrExpr = do 
    a <- (try tagField <|> try phrase <|> literal)
    x <- try conjExpr
    b <- expression  -- recursion
    return $ AndOrExpr x a b

conjExpr :: Parser' Conj
conjExpr = andExpr <|> orExpr

andExpr :: Parser' Conj
andExpr = mkConjExpr ["and", "AND", "&"] And

orExpr :: Parser' Conj
orExpr = mkConjExpr ["or", "OR", "|"] Or


mkConjExpr :: [String] -> Conj -> Parser' Conj
mkConjExpr xs t = 
    try (many1 space >> choice (map (string . (++" ")) xs))
    >> return t

phrase :: Parser' Expression
phrase = do
    _ <- char '"'
    xs <- manyTill anyChar (char '"')
    return . Phrase $ xs

literalStop :: Parser' ()
literalStop = (choice [ 
    lookAhead (tagField >> return ()) 
  , lookAhead (conjExpr >> return ())
  , lookAhead (phrase >> return ())
  , eof
  ])
  <?> "literalStop"

literal :: Parser' Expression
literal = do
    a <- anyChar
    notFollowedBy literalStop
    xs <- manyTill anyChar (try literalStop)
    return . Literal $ a:xs



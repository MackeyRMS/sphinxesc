module Main where
import SphinxEscape
import Test.HUnit 

main :: IO Counts
main = runTestTT . test $ [
    "7/11" ==> ([], [], "7 11")
  , "hello 7/11" ==> ([], [], "hello 7 11")
  , "hello OR 7/11" ==> ([], [], "hello | 7 11")
  , "hello or 7/11" ==> ([], [], "hello | 7 11")
  , "hello | 7/11" ==> ([], [], "hello | 7 11")
  , "hello AND 7/11" ==> ([], [], "hello & 7 11")
  , "@tag_list fox tango 7/11" ==> (["fox"], [], "tango 7 11")
  , "@(tag_list) fox tango 7/11" ==> (["fox"], [], "tango 7 11")
  , "@(tag_list) AND" ==> (["AND"], [], "")
  , "@other_field AND" ==> ([], [], "other field AND")
  , "hello & @other_field AND" ==> ([], [], "hello &  other field AND")
  , "hello &" ==> ([], [], "hello")
  , "& hello &" ==> ([], [], "hello")
  , "& & hello &" ==> ([], [], "hello")
  , "| | hello |" ==> ([], [], "hello")
  , "\"hello\" hello" ==> ([], [], "\"hello\" hello")
  , "\"hello\" @tag_list fox" ==> (["fox"], [], "\"hello\"")
  , "@tag_list tea \"hello fox\"" ==> (["tea"], [], "\"hello fox\"")
  , "@tag_list \"hello fox\" tea" ==> (["hello fox"], [], "tea")

  , "author:aa author:bb xx" ==> ([], ["aa", "bb"], "xx")
  , "tag: aa xx author: \"bb cc\"" ==> (["aa"], ["bb cc"], "xx")

  -- NEW. make quoted phrase search out of any words with periods in them
  -- to avoid sphinx whitespace breaking; e.g. AAPL.O

  , "aapl.o foo" ==> ([], [], "\"aapl.o\" foo")
  , "aapl o foo" ==> ([], [], "aapl o foo") -- no phrase boundary
  ]

(==>) :: String -> ([String], [String], String) -> Test
input ==> expect = 
    (input ++ " => " ++ show expect) 
      ~: (transformQuery input) @?= expect




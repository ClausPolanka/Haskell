module Aufgabe5 where

import Data.List 

-- Beispiel 1

ctxlines :: [String]
ctxlines = ["Test", "Test", "Test\nTest\nTest"]

ctxunlines :: [[String]]
ctxunlines = [["Test1\nT"], ["Test2\nT"], ["Test3\nT"], ["T\nT"]]

ctxwords :: [String]
ctxwords = ["Test2 Test3  T", "T  Test1 Test2 Test3", "Test1  T Test2 Test3"]

-- Beispiel 2

unixtac :: String -> String
unixtac s = withoutLastNewLine text
    where text = unlines . reverse . lines $ s

withoutLastNewLine :: String -> String
withoutLastNewLine s = take (length s - 1) s 

unixhead :: Int -> String -> String
unixhead n text = withoutLastNewLine nLines
    where nLines = unlines . take n  $ lines text

unixtail :: Int -> String -> String
unixtail n text = withoutLastNewLine nLines
    where 
        nLines = unlines . drop (length linesAsList - n) $ linesAsList
        linesAsList = lines text

unixgrep :: String -> String -> String
unixgrep searchString text
    | searchString == "" = text
unixgrep searchString text = withoutLastNewLine linesContainingString
    where 
        linesContainingString = unlines [line | line <- (lines text), 
                                                line `contains` searchString]

contains :: String -> String -> Bool
contains [] _ = False
contains sentence@(x:xs) word
    | word `isPrefixOf` sentence = True
    | otherwise = contains xs word

-- Beispiel 3

aslines :: ([String]->[String]) -> String -> String
aslines f s = withoutLastNewLine . unlines . f . lines $ s

unixtac' :: String -> String
unixtac' = aslines reverse

unixhead' :: Int -> String -> String
unixhead' n s = aslines (take n) s

unixtail' :: Int -> String -> String
unixtail' n s = aslines (drop (length (lines s) - n)) s

unixgrep' :: String -> String -> String
unixgrep' = unixgrep

-- Beispiel 4

unixrev :: String -> String
unixrev s = withoutLastNewLine . unlines $ map reverse $ lines s

wordrev :: String -> String
wordrev s = withoutLastNewLine . unlines $ [reverseWordsOf line | line <- lines s]
    where reverseWordsOf = unwords . reverse . words

-- Beispiel 5

unixwcw :: String -> Int
unixwcw text = length . words $ text

unixwc :: String -> (Int, Int, Int)
unixwc s = (length . lines $ s, unixwcw s, sum $ map length $ lines s)
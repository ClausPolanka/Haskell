module Main where

import QuickCheck
import Test.QuickCheck

main = 
    do
        quickCheck (prop_idempotent :: [Integer] -> Bool)
        quickCheck (prop_minimum :: [Integer] -> Property)
        quickCheck (prop_ordered :: [Integer] -> Bool)
        quickCheck (prop_permutation :: [Integer] -> Bool)
        quickCheck (prop_maximum :: [Integer] -> Property)
        quickCheck (prop_append :: [Integer] -> [Integer] -> Property)
        quickCheck (prop_sort_model :: [Integer] -> Bool)
        
        
        
        
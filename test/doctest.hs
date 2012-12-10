module Main where

import Test.DocTest

main :: IO ()
main = doctest ["-idist/build/autogen/", "./Data/Array/ZipWithN/Internal.hs"]
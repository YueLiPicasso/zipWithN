module Main where

import Test.DocTest

main :: IO ()
main = doctest ["-idist/build/autogen/", "./src/Data/Array/ZipWithN/Internal.hs"]
module Main (main) where

import Test.Framework

import AuthTests

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [authTests]


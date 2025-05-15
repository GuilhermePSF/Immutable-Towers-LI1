module Main (main) where

import Tarefa1Spec
import Tarefa2Spec
import Tarefa3Spec
import Test.HUnit

testSuite :: Test
testSuite =
  TestLabel "Spec Test Suit" $
    test
      [ testesTarefa1,
        testesTarefa2,
        testesTarefa3
      ]

main :: IO ()
main = runTestTTAndExit $ test [testSuite]

module Main where

import Test.Tasty
import ConnectednessTest

main = defaultMain ConnectednessTest.connectednessProps

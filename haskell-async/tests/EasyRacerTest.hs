{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import EasyRacerClient
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as Tasty
import TestContainers.Tasty qualified as TC

data EasyRacerServerConfig = EasyRacerServerConfig
  { easyRacerHost :: String,
    easyRacerPort :: Int
  }

setupContainer :: TC.TestContainer EasyRacerServerConfig
setupContainer = do
  testServer <-
    TC.run $
      TC.containerRequest (TC.fromTag "ghcr.io/jamesward/easyracer")
        TC.& TC.setExpose [8080]
        TC.& TC.setWaitingFor (TC.waitUntilTimeout 60 (TC.waitForHttp 8080 "/" [200]))

  pure $
    EasyRacerServerConfig
      { easyRacerHost = "0.0.0.0",
        easyRacerPort = TC.containerPort testServer 8080
      }

scenarioUrl :: String -> Int -> Int -> String
scenarioUrl host port scenario = concat ["http://", host, ":", show port, "/", show scenario]

main :: IO ()
main =
  Tasty.defaultMain $
    TC.withContainers setupContainer $ \start ->
      Tasty.testGroup
        "Easy Racer"
        $ zipWith
          ( \scenario num ->
              Tasty.testCase ("Scenario " ++ show num) $
                do
                  EasyRacerServerConfig {..} <- start
                  res <- scenario (scenarioUrl easyRacerHost easyRacerPort num)
                  Tasty.assertEqual "" "right" res
          )
          allScenarios
          [1 ..]

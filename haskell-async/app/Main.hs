{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Lazy.Char8 qualified as L8
import Control.Monad (forM_)
import EasyRacerClient

main :: IO ()
main = do
  let runs = zipWith (\f n -> f $ "http://localhost:8080/" ++ show n) allScenarios [(1 :: Int) ..]
  forM_ runs $ \r -> r >>= L8.putStrLn

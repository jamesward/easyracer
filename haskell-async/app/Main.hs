{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Exception (finally, SomeException)
import Control.Monad (forM_)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as L8
import Network.HTTP.Client
-- import System.Random (mkStdGen, uniformByteString)

-- Race 2 concurrent requests
scenario1 :: Manager -> String -> IO ByteString
scenario1 manager url = do
  req <- parseRequest url
  res <- race (httpLbs req manager) (httpLbs req manager)
  pure $ either responseBody responseBody res

-- Race 2 concurrent requests, where one produces a connection error
scenario2 :: Manager -> String -> IO ByteString
scenario2 manager url = do
  req <- parseRequest url
  responseBody <$> raceSuccess (httpLbs req manager) (httpLbs req manager)

-- Race 10,000 concurrent requests
scenario3 :: Manager -> String -> IO ByteString
scenario3 manager url = do
  req <- parseRequest url
  let tasks = replicate 10000 $ httpLbs req manager
  resp <- raceAnySuccess tasks
  pure $ responseBody resp

-- Race 2 concurrent requests but 1 of them should have a 1 second timeout
scenario4 :: Manager -> String -> IO ByteString
scenario4 manager url = do
  timeoutManager <- newManager $ defaultManagerSettings {managerResponseTimeout = responseTimeoutMicro 1000000}
  req <- parseRequest url
  responseBody <$> raceSuccess (httpLbs req timeoutManager) (httpLbs req manager)

-- Race 2 concurrent requests where a non-200 response is a loser
scenario5 :: Manager -> String -> IO ByteString
scenario5 manager url = do
  req <- parseUrlThrow url
  responseBody <$> raceSuccess (httpLbs req manager) (httpLbs req manager)

-- Race 3 concurrent requests where a non-200 response is a loser
scenario6 :: Manager -> String -> IO ByteString
scenario6 manager url = do
  req <- parseUrlThrow url
  let tasks = replicate 3 $ httpLbs req manager
  resp <- raceAnySuccess tasks
  pure $ responseBody resp

-- Start a request, wait at least 3 seconds then start a second request (hedging)
scenario7 :: Manager -> String -> IO ByteString
scenario7 manager url = do
  req <- parseRequest url
  withAsync (httpLbs req manager) $ \a1 -> do
    _ <- threadDelay 3000000
    withAsync (httpLbs req manager) $ \a2 -> do
      res <- waitEither a1 a2
      pure $ either responseBody responseBody res

-- Race 2 concurrent requests that "use" a resource which is obtained and released through other requests.
-- The "use" request can return a non-20x request, in which case it is not a winner.
scenario8 :: Manager -> String -> IO ByteString
scenario8 manager url =
  let doRequest = do
        openReq <- parseRequest (url ++ "?open")
        withAsync (httpLbs openReq manager) $ \idAsync -> do
          resId <- show . responseBody <$> wait idAsync
          useReq <- parseUrlThrow $ concat [url, "?use=", resId]
          closeReq <- parseRequest $ concat [url, "?close=", resId]
          httpLbs useReq manager `finally` httpNoBody closeReq manager
   in responseBody <$> raceSuccess doRequest doRequest

-- Make 10 concurrent requests where 5 return a 200 response with a letter
scenario9 :: Manager -> String -> IO ByteString
scenario9 manager url = do
  req <- parseUrlThrow url
  let tasks = replicate 10 $ async (httpLbs req manager)
  resps <- waitAll tasks
  pure $ L8.concat $ map responseBody resps
  where
    waitAll :: [IO (Async (Response ByteString))] -> IO [Response ByteString]
    waitAll tasks = do
      asyncs <- sequence tasks
      waitAllSuccess asyncs
      where
        waitAllSuccess [] = pure []
        waitAllSuccess as = do
          (completed, res) <- waitAnyCatch as
          case res of
            Left _ -> waitAllSuccess $ filter (/= completed) as
            Right val -> (val :) <$> waitAllSuccess (filter (/= completed) as)

-- This scenario validates that a computationally heavy task can be run in parallel to another task,
-- and then cancelled.
-- scenario10 :: Manager -> String -> IO ByteString
-- scenario10 manager url = do
--     let rndGen = mkStdGen 42
--     let rstr = uniformByteString 10 rndGen
--     undefined

-- This scenario validates that a race where all racers fail, is handled correctly.
-- Race a request with another race of 2 requests.
scenario11 :: Manager -> String -> IO ByteString
scenario11 manager url = do
    req <- parseUrlThrow url
    res <- raceSuccess (Right <$> httpLbs req manager) (raceSuccessFail (httpLbs req manager) (httpLbs req manager))
    case res of
      Left _ -> error "Failure"
      Right resp -> pure $ responseBody resp

ignore :: Manager -> String -> IO ByteString
ignore _ _ = pure "ignored"

-- raceSuccess :: IO a -> IO a -> IO a
-- raceSuccess left right =
--   withAsync left $ \leftAct -> do
--     withAsync right $ \rightAct -> do
--       res <- waitEitherCatch leftAct rightAct
--       case res of
--         Left (Right val) -> return val
--         Left (Left _) -> wait rightAct
--         Right (Right val) -> return val
--         Right (Left _) -> wait leftAct

-- Race two actions and return the first one that finishes
-- successfully, or fail if both fail.
raceSuccess :: IO a -> IO a -> IO a
raceSuccess !left !right =
  either (error "Both tasks failed!") id
    <$> raceSuccessFail left right

-- Race two actions and return the first one that finishes
-- successfully, or return the exception if both fail.
raceSuccessFail :: IO a -> IO a -> IO (Either SomeException a)
raceSuccessFail left right =
  withAsync left $ \leftAct -> do
    withAsync right $ \rightAct -> do
      res <- waitEitherCatch leftAct rightAct
      case res of
        Left (Right val) -> return $ Right val
        Left (Left _) -> waitCatch rightAct
        Right (Right val) -> return $ Right val
        Right (Left _) -> waitCatch leftAct

raceAnySuccess :: [IO a] -> IO a
raceAnySuccess tasks = do
  asyncs <- mapM async tasks
  waitForSuccess asyncs `finally` cancelMany asyncs
  where
    waitForSuccess :: [Async a] -> IO a
    waitForSuccess [] = error "All tasks failed!"
    waitForSuccess asyncs = do
      (completed, result) <- waitAnyCatch asyncs
      case result of
        Right val -> return val
        Left _ ->
          let remaining = filter (/= completed) asyncs
           in waitForSuccess remaining

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings { managerConnCount = 10000 }
  let scenarios =
        [ scenario1,
          scenario2,
          ignore, -- scenario3,
          scenario4,
          scenario5,
          scenario6,
          scenario7,
          scenario8,
          scenario9,
          ignore, {- scenario10 -}
          scenario11
        ]
  let runs = zipWith (\f n -> f manager $ "http://localhost:8080/" ++ show n) scenarios [(1 :: Int) ..]
  forM_ runs $ \r -> r >>= L8.putStrLn

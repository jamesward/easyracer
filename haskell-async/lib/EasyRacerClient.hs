{-# LANGUAGE BlockArguments #-}

module EasyRacerClient
  ( scenario1,
    scenario2,
    scenario3,
    scenario4,
    scenario5,
    scenario6,
    scenario7,
    scenario8,
    scenario9,
    scenario10,
    scenario11,
    allScenarios
  )
where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception (Exception, SomeException, bracket, catch, evaluate, throwIO)
import Control.Monad (when)
import Crypto.Hash.SHA512 qualified as SHA512
import Data.ByteString qualified as SBS
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.Foldable (traverse_)
import Data.Int (Int16)
import Data.Maybe (fromJust)
import Data.Traversable (for)
import Network.HTTP
  ( RequestMethod (GET),
    Response,
    getResponseBody,
    getResponseCode,
    mkRequest,
    simpleHTTP,
  )
import Network.HTTP.Stream (Result)
import Network.URI (parseURI)
import System.CPUTime (getCPUTime)
import System.Random (mkStdGen, uniform, uniformByteString)
import System.Random.Stateful (StdGen)
import System.Timeout (timeout)

data RequestException = RequestException
  deriving (Show)

instance Exception RequestException

-- Race 2 concurrent requests
scenario1 :: String -> IO ByteString
scenario1 url = raceSuccess (getRequestLBS url) (getRequestLBS url)

-- Race 2 concurrent requests, where one produces a connection error
scenario2 :: String -> IO ByteString
scenario2 url = raceSuccess (getRequestLBS url) (getRequestLBS url)

-- -- Race 10,000 concurrent requests
scenario3 :: String -> IO ByteString
scenario3 url = raceAnySuccess $ replicate 10000 $ getRequestLBS url

-- -- Race 2 concurrent requests but 1 of them should have a 1 second timeout
scenario4 :: String -> IO ByteString
scenario4 url =
  let timeoutEx t io = do
        res <- timeout t io
        case res of
          Just r -> pure r
          Nothing -> throwIO RequestException
   in raceSuccess (timeoutEx 1000000 (getRequestLBS url)) (getRequestLBS url)

-- -- Race 2 concurrent requests where a non-200 response is a loser
scenario5 :: String -> IO ByteString
scenario5 url = raceSuccess (getRequestLBS url) (getRequestLBS url)

-- -- Race 3 concurrent requests where a non-200 response is a loser
scenario6 :: String -> IO ByteString
scenario6 url = raceAnySuccess $ replicate 3 (getRequestLBS url)

-- -- Start a request, wait at least 3 seconds then start a second request (hedging)
scenario7 :: String -> IO ByteString
scenario7 url = do
  withAsync (getRequestLBS url) $ \a1 -> do
    _ <- threadDelay 3000000
    withAsync (getRequestLBS url) $ \a2 -> do
      res <- waitEither a1 a2
      pure $ either id id res

-- -- Race 2 concurrent requests that "use" a resource which is obtained and released through other requests.
-- -- The "use" request can return a non-20x request, in which case it is not a winner.
scenario8 :: String -> IO ByteString
scenario8 url =
  let doRequest = do
        let openUrl = url ++ "?open"
        withAsync (getRequestLBS openUrl) $ \idAsync -> do
          bracket
            (L8.unpack <$> wait idAsync)
            (\resId -> getRequestLBS $ concat [url, "?close=", resId])
            (\resId -> getRequestLBS $ concat [url, "?use=", resId])
   in either (L8.pack . show) id <$> raceSuccessFail doRequest doRequest

-- Make 10 concurrent requests where 5 return a 200 response with a letter
scenario9 :: String -> IO ByteString
scenario9 url = do
  let tasks = replicate 10 $ async (getRequest url)
  resps <- waitAll tasks
  L8.concat <$> traverse getResponseBody resps
  where
    waitAll tasks = do
      asyncs <- sequence tasks
      waitAllSuccess asyncs
      where
        waitAllSuccess [] = pure []
        waitAllSuccess as = do
          (completed, res) <- waitAny as
          (s, _, _) <- getResponseCode res
          if s == 2
            then (res :) <$> waitAllSuccess (filter (/= completed) as)
            else waitAllSuccess $ filter (/= completed) as

-- This scenario validates that a computationally heavy task can be run in parallel to another task,
-- and then cancelled.
scenario10 :: String -> IO ByteString
scenario10 url = do
  let initialRndG = mkStdGen 42
  let (reqId, rndGen) = uniform initialRndG :: (Int16, StdGen)
  let (seed, _) = uniformByteString 512 rndGen
  let taskUrl = concat [url, "?", show $ abs reqId]
  withAsync (getRequestLBS taskUrl) \blockerTask ->
    withAsync (busyWork seed) \busyTask ->
      withAsync (reporter taskUrl) $ \reporterTask ->
        wait blockerTask
          >> cancel busyTask
          >> wait reporterTask
  where
    busyWork :: SBS.ByteString -> IO SBS.ByteString
    busyWork bs =
      let bs' = SHA512.hash bs
       in evaluate bs' >>= busyWork

    reporter reqUrl = do
      load <- currentLoad
      resp <- getRequest $ concat [reqUrl, "=", show load]
      respCode <- getResponseCode resp
      case respCode of
        (2, _, _) -> getResponseBody resp
        (3, _, _) -> reporter reqUrl
        _ -> error . L8.unpack <$> getResponseBody resp

    currentLoad = do
      t0 <- getCPUTime
      threadDelay 1_000_000
      t1 <- getCPUTime
      let delta = (fromIntegral $ t1 - t0) :: Double
      let load = delta / 10 ^ (12 :: Int)
      pure load

-- This scenario validates that a race where all racers fail, is handled correctly.
-- Race a request with another race of 2 requests.
scenario11 :: String -> IO ByteString
scenario11 url = do
  let req = getRequestLBS url
  res <- raceSuccess (Right <$> req) (raceSuccessFail req req)
  case res of
    Left _ -> error "Failure"
    Right resp -> pure resp

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

-- Race a list of actions and return the first one to
-- _successfully_ finish. Fails if all actions fail.
--
-- Thanks to u/jaror and u/leary for this one:
-- https://discourse.haskell.org/t/help-haskell-async-behavior-i-dont-understand/12061/8
raceAnySuccess :: [IO a] -> IO a
raceAnySuccess jobs =
  bracket
    ( do
        result <- newEmptyMVar
        threads <- for jobs \job ->
          forkIO $
            (job >>= putMVar result) `catch` \(_ :: SomeException) -> pure ()
        pure (result, threads)
    )
    (forkIO . traverse_ killThread . snd)
    (takeMVar . fst)

getRequestLBS :: String -> IO ByteString
getRequestLBS url = do
  response <- simpleHTTP $ mkRequest GET (fromJust (parseURI url))
  (s, _, _) <- getResponseCode response
  when (s /= 2) $ throwIO RequestException
  getResponseBody response

getRequest :: String -> IO (Result (Response ByteString))
getRequest url = simpleHTTP $ mkRequest GET (fromJust (parseURI url))

allScenarios :: [String -> IO ByteString]
allScenarios =
        [ scenario1,
          scenario2,
          scenario3,
          scenario4,
          scenario5,
          scenario6,
          scenario7,
          scenario8,
          scenario9,
          scenario10,
          scenario11
        ]

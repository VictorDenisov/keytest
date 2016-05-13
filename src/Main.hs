{-# language OverloadedStrings #-}

module Main where

import Data.Text.Format (fixed)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text (Text)
import Control.Monad (forM_, replicateM_)
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.Chan (newChan, Chan, writeChan, readChan)
import System.TimeIt
import Network.HTTP
import Network.HTTP.Base
import Network.HTTP.Headers
import Data.Aeson
import Data.Vector (fromList)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

requestJson = object [ "auth" .=
                              (object ["identity" .=
                                          (object [ "methods" .= (Array $ fromList [String "password"])
                                                  , "password" .= (object ["user" .=
                                                                              (object [ "id" .= (String "57356bf5329d022aa4000011")
                                                                                      , "password" .= (String "admin")
                                                                                      ]
                                                                              )
                                                                          ]
                                                                  )
                                                  ]
                                          )
                                      ]
                              )
                         ]

requestBody = unpack $ encode requestJson

request = postRequestWithBody
                    "http://127.0.0.1:35357/v3/auth/tokens"
                    "application/json"
                    requestBody

validateRequest = (getRequest "http://127.0.0.1:35357/v3/auth/tokens") {rqHeaders = [ (mkHeader (HdrCustom "X-Auth-Token") "571b344f57c584345d0002ff")
                                                                                    , (mkHeader (HdrCustom "X-Subject-Token") "571b344f57c584345d0002ff")
                                                                                    ]}

--request = getRequest "http://localhost:35357/"

measureTime i c = do
    putStrLn $ "Sending request " ++ (show i)
    before <- getCurrentTime
    --(execTime, res) <- timeItT $ simpleHTTP request
    --res <- simpleHTTP validateRequest
    res <- simpleHTTP request
    after <- getCurrentTime
    let execTime = diffUTCTime after before
    case res of
      Left e -> writeChan c "-1"
      Right r -> writeChan c $ (show i) ++ " - " ++ (show $ rspCode r) ++ " - " ++ (show execTime)
      --(show $ toLazyText $ fixed 5 execTime)

requestCount = 1000

main :: IO ()
main = do
  c <- newChan
  totalStart <- getCurrentTime
  forM_ [1..requestCount] $ \i -> do
    forkIO $ measureTime i c
    threadDelay 10000
  totalFinish <- getCurrentTime
  let totalExecTime = diffUTCTime totalFinish totalStart
  putStrLn $ " Total exec time " ++ (show totalExecTime)
  replicateM_ requestCount $ do
    v <- readChan c
    putStrLn $ show v

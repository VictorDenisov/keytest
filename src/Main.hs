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
import Data.Aeson
import Data.Vector (fromList)
import Data.ByteString.Lazy.Char8 (unpack)

requestJson = object [ "auth" .=
                              (object ["identity" .=
                                          (object [ "methods" .= (Array $ fromList [String "password"])
                                                  , "password" .= (object ["user" .=
                                                                              (object [ "id" .= (String "5589884c2300191ce3000004")
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
                    "http://localhost:35357/v3/auth/tokens"
                    "application/json"
                    requestBody

measureTime c = do
    (execTime, res) <- timeItT $ simpleHTTP request
    case res of
      Left e -> writeChan c "-1"
      Right r -> writeChan c $ (show $ rspCode r) ++ " - " ++ (show $ toLazyText $ fixed 5 execTime)

requestCount = 2000

main :: IO ()
main = do
  c <- newChan
  replicateM_ requestCount $ do
    forkIO $ measureTime c
    threadDelay 20000
  replicateM_ requestCount $ do
    v <- readChan c
    putStrLn $ show v

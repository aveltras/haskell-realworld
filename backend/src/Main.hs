module Main where

import Lib
import RealWorld.Handler

import Network.Wai
import Servant
import Network.Wai.Handler.Warp


main :: IO ()
main = do
  run 3000 app

api :: Proxy RootAPI
api = Proxy

app :: Application
app = serve api server

server :: Server RootAPI
server = undefined

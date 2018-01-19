{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import           Data.Aeson                     (Value(..), object, (.=))
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Network.HTTP.Types.Header
import           App                            (app)

main :: IO ()
main = hspec spec

spec :: Spec
spec = with app $ do

  describe "GET /" $ do
    it "responds with 200" $ do
      get "/" `shouldRespondWith` 200

    it "get on / responds with '{endpoints:{user_timeline:/user/{userName}/timeline}}' and 200 and headers" $ do
      get "/" `shouldRespondWith` expectRootJsonResponse {matchStatus = 200} {matchHeaders = ["Content-Type" <:> "application/json; charset=utf-8"]}

  -- describe "GET /user/{userName}/timeline" $ do
  --   it "responds with tweeter timeline JSON" $ do
  --     get "/user/someuser/timeline" `shouldRespondWith` expectTimeLineJsonResponse {matchStatus = 200}


expectRootJsonResponse =
  let ResponseMatcher status headers body = [json|{endpoints: {user_timeline: "/user/{userName}/timeline"}}|]
  in ResponseMatcher status [hContentType <:> "application/json; charset=utf-8"] body

-- expectTimeLineJsonResponse =
--   let ResponseMatcher status headers body = [json|[{text: "My tweet", userName: "someuser", retweetCount: 10, favoriteCount: 1}]|]
--   in ResponseMatcher status [hContentType <:> "application/json; charset=utf-8"] body

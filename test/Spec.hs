{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main (main) where

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Network.HTTP.Types.Header
import           Data.Aeson (Value(..), object, (.=))

import           App (app)

main :: IO ()
main = hspec spec

spec :: Spec
spec = with app $ do
  describe "GET /" $ do
    it "responds with 200" $ do
      get "/" `shouldRespondWith` 200

    it "responds with '{endpoints:{user_timeline:/user/{userName}/timeline}}" $ do
      get "/" `shouldRespondWith` expectRootJsonResponse

    it "responds with 200 / 'hello'" $ do
      get "/" `shouldRespondWith` expectRootJsonResponse {matchStatus = 200}

    it "has 'Content-Type: application/json; charset=utf-8'" $ do
      get "/" `shouldRespondWith` 200 {matchHeaders = ["Content-Type" <:> "application/json; charset=utf-8"]}

  describe "GET /user/{userName}/timeline" $ do
    it "responds with tweeter timeline JSON" $ do
      get "/user/barcelona_cat/timeline" `shouldRespondWith` 200

expectRootJsonResponse =
  let ResponseMatcher status headers body = [json|{endpoints: {user_timeline: "/user/{userName}/timeline"}}|]
  in ResponseMatcher status [hContentType <:> "application/json; charset=utf-8"] body

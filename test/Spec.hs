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
      get "/user/barcelona_cat/timeline" `shouldRespondWith` expectedJsonResponse

expectRootJsonResponse =
  let ResponseMatcher status headers body = [json|{endpoints: {user_timeline: "/user/{userName}/timeline"}}|]
  in ResponseMatcher status [hContentType <:> "application/json; charset=utf-8"] body

expectedJsonResponse =
  let ResponseMatcher status headers body = [json|[{createdAt:"2018-01-12T15:50:13Z",text:"Els 69 patis escolars oberts al barri recuperen l’horari habitual després de les vacances de Nadal i Reis. Troba el… https://t.co/PfjfvPsgbX",userName:"barcelona_cat",favoriteCount:0,retweetCount:0},{createdAt:"2018-01-12T15:30:36Z","text":"Us mou la solidaritat? Fixeu-vos el repte de participar en la #MagicLine 2018. Les inscripcions estan obertes fins… https://t.co/kZDKw7Zkav",userName:"barcelona_cat",favoriteCount:1,retweetCount:0},{createdAt:"2018-01-12T14:40:13Z","text":"Vols recordar què va donar de sí l'any passat en cinema? La \n@filmotecacat projecta alguns dels films més destacats… https://t.co/MafXIF0c7N",userName:"barcelona_cat",favoriteCount:2,retweetCount:0},{createdAt:"2018-01-12T13:50:16Z","text":"Sigues sostenible i recicla els arbres de Nadal. Del 7 al 17 de gener, porta els avets de Nadal a un dels 223 punts… https://t.co/JcCUtV4q71",userName:"barcelona_cat",favoriteCount:1,retweetCount:1},{createdAt:"2018-01-12T13:01:17Z","text":"Els Tres Tombs tornen aquest gener a Sant Andreu amb la benedicció tradicional dels animals i la cavalcada. Diumeng… https://t.co/OmJ0IZSRyV",userName:"barcelona_cat",favoriteCount:7,retweetCount:2},{createdAt:"2018-01-12T12:10:11Z","text":"'Un món convuls' El fotoperiodista Andoni Lubaki presenta a Can Basté una exposició que recorre alguns dels conflic… https://t.co/aqdPYLq3Mh",userName:"barcelona_cat",favoriteCount:0,retweetCount:1},{createdAt:"2018-01-12T11:38:55Z","text":"@donarsang @cececebe @vallhebron @hospitalclinic @HospitalSantPau Donar sang és #MoltMésdelQueDones! https://t.co/rglSn4KkNw",userName:"barcelona_cat",favoriteCount:2,retweetCount:1},{createdAt:"2018-01-12T11:37:58Z","text":"@donarsang @cececebe I fins al 19 de gener també pots @donarsang als hospitals Vall d'Hebron, Clínic i Sant Pau. Re… https://t.co/sMuA43Y8CH",userName:"barcelona_cat",favoriteCount:5,retweetCount:1},{createdAt:"2018-01-12T11:32:28Z","text":"@donarsang @cececebe Es necessiten 8.000 donacions per recuperar les reserves de sang. Avui i demà pots donar-ne al… https://t.co/BEU3yuGRLi",userName:"barcelona_cat",favoriteCount:2,retweetCount:5},{createdAt:"2018-01-12T11:24:40Z","text":"T’apuntes a viure la superexperiència de @donarsang a la #MaratóDonantsCAT? Després de Nadal les reserves han dismi… https://t.co/CpSL4c0Oxm",userName:"barcelona_cat",favoriteCount:7,retweetCount:10}]|]
  in ResponseMatcher status [hContentType <:> "application/json; charset=utf-8"] body

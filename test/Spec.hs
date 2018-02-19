{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main (main) where

import           App                       (app)
import           Data.Aeson                (Value (..), object, (.=))
import           Network.HTTP.Types.Header
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

spec :: Spec
spec = with app $
  describe "GET /" $ do

    it "responds with 200" $
      get "/" `shouldRespondWith` 200

    it "get on / responds with '{endpoints:{user_timeline:/user/{userName}/timeline}}' and 200 and headers" $
      get "/" `shouldRespondWith` expectRootJsonResponse {matchStatus = 200} {matchHeaders = ["Content-Type" <:> "application/json; charset=utf-8"]}


expectRootJsonResponse =
  let ResponseMatcher status headers body = [json|{endpoints: {user_timeline: "/user/{userName}/timeline"}}|]
  in ResponseMatcher status [hContentType <:> "application/json; charset=utf-8"] body

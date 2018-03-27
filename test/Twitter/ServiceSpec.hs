{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Twitter.ServiceSpec (spec) where

import           Control.Monad.Reader
import           Data.Text
import           Test.Hspec
import           Twitter.Adapter
import           Twitter.Context
import           Twitter.Model
import           Twitter.Service


spec :: Spec
spec = describe "getTimeLine using mocking" $ do
        it "should responds from cache if exist there first" $ do
                cache <- getFrom (mockFrom $ expected "cache" "user") mockNothing mockStoreCache
                cache `shouldBe` expected "cache" "user"
        it "should responds from twitter if it doesnt exist in cache" $ do
                twitter <- getFrom mockNothing (mockFrom $ expected "twitter" "user") mockStoreCache
                twitter `shouldBe` expected "twitter" "user"


getFrom :: MonadIO m =>
        GetUserTimeLine (ReaderT Context IO) ->
        GetUserTimeLine (ReaderT Context IO) ->
        StoreUserTimeLine (ReaderT Context IO) ->
        m TimeLineResponse
getFrom mockCache mockTwitter mockStore = do
        ctx <- liftIO buildCtx
        liftIO $ runReaderT (getTimeLine request mockCache mockTwitter mockStore) ctx


request :: TimeLineRequest
request = createTimeLineRequest "someuser" (Just 10)

mockNothing :: MonadReader Context m => GetUserTimeLine m
mockNothing _ = return Nothing

mockFrom :: MonadReader Context m => TimeLineResponse -> GetUserTimeLine m
mockFrom tweets _ = return (Just tweets)

mockStoreCache :: MonadReader Context m => StoreUserTimeLine m
mockStoreCache _ res = return res

expected :: Text -> Text -> TimeLineResponse
expected textTweet user = Right
        [ createTweet textTweet user Nothing 10 4 ]

{-# LANGUAGE ScopedTypeVariables #-}

module Core.UtilsSpec (spec) where

import           Control.Monad.Identity
import           Control.Monad.Trans.Maybe
import           Core.Utils
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = do
    describe "maybeToLeft" $ do
        prop "maybeToLeft x Just y == Left y"  $
            \(x :: Int) -> maybeToLeft x (Just x) == Left x
        prop "maybeToLeft x Nothing == Right y" $
            \(x :: Int) -> maybeToLeft x (Nothing::Maybe Int) == Right x
    describe "fromMaybeT" $ do
        prop "fromMaybeT (m a) (MaybeT (Just (Just m b))) == Just m b"  $
            \(x :: Int) ->
                fromMaybeT (Just $ Identity x) (MaybeT (Just $ Just (Identity x))) == Just (Identity x)
        prop "fromMaybeT (m a) (MaybeT (Just Nothing)) == Just m a"  $
            \(x :: Int) ->
                fromMaybeT (Just $ Identity x) (MaybeT (Just Nothing)) == Just (Identity x)

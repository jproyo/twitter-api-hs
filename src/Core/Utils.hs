module Core.Utils(
fromMaybeT,
maybeToLeft
) where

import           Control.Monad             ((<=<))
import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)

fromMaybeT :: (Monad m) => m a -> MaybeT m a -> m a
fromMaybeT onFail = maybe onFail return <=< runMaybeT

maybeToLeft :: b -> Maybe a -> Either a b
maybeToLeft _ (Just x) = Left x
maybeToLeft y Nothing  = Right y

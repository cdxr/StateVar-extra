module Data.StateVar.Reader where


import Control.Monad.Reader
import Data.StateVar as StateVar


-- | Read the value of a state variable.
askStateVar :: (MonadReader e m, HasGetter g, MonadIO m) => (e -> g a) -> m a
askStateVar v = liftIO . StateVar.get =<< asks v

-- | Write a new value into a state variable.
(@=) :: (MonadIO m, MonadReader e m, HasSetter s) => (e -> s a) -> a -> m ()
v @= x = liftIO . ($= x) =<< asks v

-- | A variant of `?=` which is strict in the value to be set.
(@=!) :: (MonadIO m, MonadReader e m, HasSetter s) => (e -> s a) -> a -> m ()
v @=! x = x `seq` v @= x

-- | Transform the contents of a state variable with the given function.
(@~) :: (MonadIO m, MonadReader e m, HasSetter v, HasGetter v)
     => (e -> v a) -> (a -> a) -> m ()
v @~ f = liftIO . ($~ f) =<< asks v

-- | A variant of `?~` which is strict in the value to be set.
(@~!) :: (MonadIO m, MonadReader e m, HasSetter v, HasGetter v)
     => (e -> v a) -> (a -> a) -> m ()
v @~! f = do
    a <- askStateVar v
    v @=! f a


mapStateVar :: (a -> b) -> (b -> a) -> StateVar a -> StateVar b
mapStateVar f g v = makeStateVar (fmap f $ readStateVar v) ((v $=) . g)
{-# INLINE mapStateVar #-}

-- | A synonym for `Data.StateVar.get`
readStateVar :: StateVar a -> IO a
readStateVar = StateVar.get
{-# INLINE readStateVar #-}

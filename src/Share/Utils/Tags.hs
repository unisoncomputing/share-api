module Share.Utils.Tags
  ( MonadTags (..),
    Tags,
    TagT (..),
    runTagT,
    HasTags (..),
  )
where

import Share.Prelude

type Tags = Map Text Text

newtype TagT m a = TagT
  { unTagT :: ReaderT Tags m a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadTrans)

runTagT :: (HasTags t, MonadIO m) => t -> TagT m a -> m a
runTagT t (TagT action) = do
  tags <- getTags t
  runReaderT action tags

-- | A class for monads which contain tags, e.g. for logging, spans, etc.
class (Monad m) => MonadTags m where
  askTags :: m (Map Text Text)
  withTags :: Map Text Text -> m a -> m a

instance (Monad m) => MonadTags (TagT m) where
  askTags = TagT ask
  withTags newTags (TagT m) = TagT $ local (newTags <>) m

instance (MonadTags m) => MonadTags (ReaderT e m) where
  askTags = lift askTags
  withTags newTags = mapReaderT (withTags newTags)

class HasTags ctx where
  getTags :: (MonadIO m) => ctx -> m Tags
  addTags :: Tags -> ctx -> ctx

instance HasTags () where
  getTags _ = pure mempty
  addTags _ ctx = ctx

instance HasTags Tags where
  getTags = pure
  addTags newTags ctx = ctx <> newTags

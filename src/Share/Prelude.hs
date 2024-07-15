module Share.Prelude
  ( module X,
    Text,
    ByteString,
    Set,
    Map,
    todo,
    tShow,
    readMaybe,
    eitherToMaybe,
    maybeToEither,
    getFirst,
    unifyEither,
    fromMaybeT,
    guardM,
    guardMaybe,
    guardMaybeM,
    altSum,
    altMap,
    foldMapM,
    onNothing,
    onNothingM,
    whenNothing,
    whenNothingM,
    whenJust,
    whenJustM,
    onLeft,
    onLeftM,
    whenLeft,
    whenLeftM,
    decodeBinary,
    coerce,
    contramap,
    HasCallStack,
    Generic,
    NonEmpty (..),
    Typeable,
    mapLeft,
    mapRight,
    bothMap,
    partitionMap,
    (>>>),
    (<<<),
    (&&&),
    wundefined,
    Exception (..),
    MaybeT (..),
    hoistMaybe,
  )
where

import Control.Applicative as X
import Control.Arrow ((&&&))
import Control.Category hiding (id, (.))
import Control.Monad as X
import Control.Monad.Reader as X
import Control.Monad.State as X
import Control.Monad.Trans.Maybe
import Data.Bifunctor as X
import Data.Binary (Binary)
import Data.Binary qualified as Binary
import Data.Bitraversable as X
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Coerce (coerce)
import Data.Either.Combinators (mapLeft, mapRight)
import Data.Foldable as X
import Data.Function as X
import Data.Functor as X
import Data.Functor.Contravariant (contramap)
import Data.Functor.Identity as X
import Data.Int as X
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Maybe as X (fromMaybe, isJust, isNothing, listToMaybe, maybeToList)
import Data.Proxy as X
import Data.Set (Set)
import Data.Text (Text)
import Data.These as X
import Data.Traversable as X
import Data.Typeable (Typeable)
import Data.Void as X
import Data.Word as X (Word64)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Share.Prelude.Orphans ()
import Share.Utils.Show (tShow)
import Text.Read (readMaybe)
import Unison.Util.Monoid (foldMapM)
import UnliftIO as X (Exception (..), MonadUnliftIO, bracket, bracket_, throwIO, try)
import Witch as X (From (..), TryFrom (..), into, tryInto)
import Witch.Utility as X (as)
import Witherable as X hiding (filter)
import Prelude as X hiding (log)

getFirst :: (Foldable f) => f a -> Maybe a
getFirst = listToMaybe . toList

-- | Throws an error with the provided message and applicable callstack.
todo :: (HasCallStack) => String -> a
todo = error

eitherToMaybe :: Either x a -> Maybe a
eitherToMaybe = either (const Nothing) Just

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither a = maybe (Left a) Right

-- | Run a maybeT, running the default action if the method retunds Nothing.
fromMaybeT :: (Monad m) => m a -> MaybeT m a -> m a
fromMaybeT m = maybe m pure <=< runMaybeT

-- | Unpack a maybe, or fail using Alternative.
guardM :: (Alternative m, Monad m) => m Bool -> m ()
guardM m = m >>= guard

-- | Unpack a maybe, or fail using Alternative.
guardMaybe :: (Alternative m) => Maybe a -> m a
guardMaybe Nothing = empty
guardMaybe (Just a) = pure a

guardMaybeM :: (Monad m, Alternative m) => m (Maybe b) -> m b
guardMaybeM m = m >>= guardMaybe

-- | Like 'fold' but for Alternative.
altSum :: (Alternative f, Foldable t) => t (f a) -> f a
altSum = foldl' (<|>) empty

-- | Like 'foldMap' but for Alternative.
altMap :: (Alternative f, Foldable t) => (a -> f b) -> t a -> f b
altMap f = altSum . fmap f . toList

-- | E.g.
--
-- @@
-- onNothing (throwIO MissingPerson) $ mayThing
-- @@
onNothing :: (Applicative m) => m a -> Maybe a -> m a
onNothing m may = maybe m pure may

onNothingM :: (Monad m) => m a -> m (Maybe a) -> m a
onNothingM =
  flip whenNothingM

-- | E.g. @maybePerson `whenNothing` throwIO MissingPerson@
whenNothing :: (Applicative m) => Maybe a -> m a -> m a
whenNothing may m = maybe m pure may

-- | E.g. @maybePerson `whenNothing` throwIO MissingPerson@
whenNothingM :: (Monad m) => m (Maybe a) -> m a -> m a
whenNothingM action recover =
  action >>= \case
    Nothing -> recover
    Just a -> pure a

-- | Conditionally run an action on a maybe.
-- This is just a more explicit 'for_'.
whenJust :: (Applicative m) => Maybe a -> (a -> m ()) -> m ()
whenJust may f = case may of
  Just a -> f a
  Nothing -> pure ()

whenJustM :: (Monad m) => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM mx f = do
  mx >>= maybe (pure ()) f

onLeft :: (Applicative m) => (a -> m b) -> Either a b -> m b
onLeft =
  flip whenLeft

onLeftM :: (Monad m) => (a -> m b) -> m (Either a b) -> m b
onLeftM =
  flip whenLeftM

whenLeft :: (Applicative m) => Either a b -> (a -> m b) -> m b
whenLeft = \case
  Left a -> \f -> f a
  Right b -> \_ -> pure b

whenLeftM :: (Monad m) => m (Either a b) -> (a -> m b) -> m b
whenLeftM m f =
  m >>= \case
    Left x -> f x
    Right y -> pure y

decodeBinary :: forall a. (Binary a) => BL.ByteString -> Either String a
decodeBinary bs =
  case Binary.decodeOrFail bs of
    Left (_, _, errMsg) -> Left errMsg
    Right (_, _, a) -> Right a

-- | a version of `undefined` that gives a warning, because you just left it there as a placeholder.
{-# WARNING wundefined "You left this wundefined." #-}
wundefined :: (HasCallStack) => a
wundefined = undefined

-- | Map both sides of a bifunctor.
bothMap :: (Bifunctor f) => (a -> b) -> f a a -> f b b
bothMap f = bimap f f

-- | Partition a list into two lists, based on a function that returns either a Left or a Right.
partitionMap :: (a -> Either b c) -> [a] -> ([b], [c])
partitionMap f xs =
  xs & foldMap go
  where
    go a = case f a of
      Left b -> ([b], mempty)
      Right c -> (mempty, [c])

unifyEither :: Either a a -> a
unifyEither = either id id

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Share.IDs
  ( SessionId (..),
    PendingSessionId (..),
    UserId (..),
    RequestId (..),
    PrefixedID (..),
    PrefixedHash (..),
    UserHandle (..),
    TourId (..),
    ProjectSlug (..),
    ProjectId (..),
    ContributionId (..),
    ContributionNumber (..),
    CommentId (..),
    TicketId (..),
    TicketNumber (..),
    BranchId (..),
    BranchName (..),
    BranchShortHand (..),
    BranchOrReleaseShortHand (..),
    ReleaseId (..),
    ReleaseVersion (..),
    ReleaseShortHand (..),
    ProjectReleaseShortHand (..),
    ProjectShortHand (..),
    ProjectBranchShortHand (..),
    SubjectId (..),
    ResourceId (..),
    projectBranchShortHandToBranchShortHand,
    JTI (..),
    CategoryName (..),
    CategoryID (..),
    IsID (toText, fromText),
    idFrom,
    fromId,
    fromUUID,
  )
where

import Control.Monad.Random (Random)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Binary (Binary)
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.Char qualified as Char
import Data.List (intercalate)
import Data.Text qualified as Text
import Data.UUID (UUID)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Hasql.Interpolate qualified as Hasql
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Share.OAuth.Types
  ( JTI (..),
    PendingSessionId (..),
    SessionId (..),
    UserId (..),
  )
import Share.Prelude
import Share.Utils.IDs (CaseInsensitiveID (..), IsID (..), PrefixedID (..), UsingID (..), fromId, fromUUID, idFrom)
import Text.Megaparsec
  ( ErrorFancy (..),
    MonadParsec (eof, takeWhileP),
    ParseError (..),
    ParseErrorBundle (..),
    Parsec,
    ShowErrorComponent (..),
    TraversableStream,
    VisualStream,
    chunk,
    customFailure,
    errorBundlePretty,
    parseMaybe,
    runParser,
    satisfy,
    single,
  )
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char.Lexer (decimal)

newtype RequestId = RequestId Text
  deriving stock (Eq, Ord)
  deriving (IsID, Show, FromHttpApiData, ToHttpApiData, ToJSON, FromJSON) via (Text)

-- | Represents a tour a user may or may not have completed,
-- E.g. a welcome tour, or EULA tour.
newtype TourId = TourId Text
  deriving stock (Eq, Ord)
  deriving (IsID, Show, FromHttpApiData, ToHttpApiData, Hasql.EncodeValue, Hasql.DecodeValue, ToJSON, FromJSON) via Text

-- | A User's Handle
newtype UserHandle = UserHandle Text
  deriving stock (Show, Eq, Ord)
  deriving (Binary, Hasql.EncodeValue, Hasql.DecodeValue) via Text
  deriving (FromHttpApiData, ToHttpApiData, ToJSON, FromJSON) via (UsingID UserHandle)

-- | User Handles must be alphanumeric and must not be empty
--
-- >>> fromText @UserHandle "unison"
-- Right (UserHandle "unison")
--
-- Must not start with an @ (see 'PrefixedUserHandle')
-- >>> fromText @UserHandle "@unison"
-- Left "User handle must contain only letters, numbers, and hyphens"
--
-- May start with a number
-- >>> fromText @UserHandle "123hello"
-- Right (UserHandle "123hello")
--
-- Must not start with a hyphen
-- >>> fromText @UserHandle "-hello"
-- Left "User handle must not start with a hyphen"
--
-- Must not exceed 50 chars
-- >>> fromText @UserHandle (Text.replicate 51 "a")
-- Left "User handle must not be longer than 50 characters"
--
-- Must not contain spaces
-- >>> fromText @UserHandle "hello world"
-- Left "User handle must contain only letters, numbers, and hyphens"
--
-- Must not contain special characters
-- >>> fromText @UserHandle "hello_world"
-- Left "User handle must contain only letters, numbers, and hyphens"
--
-- Must not be empty
-- >>> fromText @UserHandle ""
-- Left "User handle must not be empty"
--
-- Must be lowercase
-- >>> fromText @UserHandle "HELLO"
-- Left "User handle must be lowercase"
instance IsID UserHandle where
  toText (UserHandle txt) = txt
  fromText handleTxt
    | Text.null handleTxt = Left "User handle must not be empty"
    -- A slightly relaxed version of Github's Handle requirements.
    | not (Text.all (\c -> Char.isAlphaNum c || c == '-') handleTxt) = Left "User handle must contain only letters, numbers, and hyphens"
    | (Text.any Char.isUpper handleTxt) = Left "User handle must be lowercase"
    | Text.length handleTxt > 50 = Left "User handle must not be longer than 50 characters"
    | Just ('-', _) <- Text.uncons handleTxt = Left "User handle must not start with a hyphen"
    | otherwise = Right $ UserHandle handleTxt

-- | The name of a project, used in URLs, when paired with a user can be resolved to a project ID
newtype ProjectSlug = ProjectSlug (CI Text)
  deriving stock (Eq, Ord)
  deriving (Hasql.EncodeValue, Hasql.DecodeValue) via CaseInsensitiveID
  deriving (FromHttpApiData, ToHttpApiData, ToJSON, FromJSON, Show) via UsingID ProjectSlug

instance IsID ProjectSlug where
  toText (ProjectSlug s) = CI.original s
  fromText txt = case Text.uncons txt of
    Nothing -> Left "Project slug must not be empty"
    Just (startChar, rest)
      | not (Char.isAlpha startChar) && not (startChar == '_') -> Left "Project slug must start with a letter or underscore"
      | not (Text.all (\c -> (Char.isAlphaNum c || c == '_' || c == '-')) rest) -> Left "Project slug must contain only letters, numbers, underscores, and hyphens"
      | Text.length txt > 50 -> Left "Project slug must not be longer than 50 characters"
      | otherwise -> Right . ProjectSlug $ CI.mk txt

-- | A project identifier of the form '@user/project' usually used by UCM.
-- UCM prefers not to know about share users at all, so it rolls it into the 'project
-- identifier', but Share cares about them, so we split it up when we parse the identifier.
data ProjectShortHand = ProjectShortHand {userHandle :: UserHandle, projectSlug :: ProjectSlug}
  deriving stock (Eq, Ord)
  deriving (Show, FromHttpApiData, ToHttpApiData, ToJSON, FromJSON) via UsingID ProjectShortHand

-- | A project identifier of the form '@user/project' usually used by UCM.
--
-- >>> fromText @ProjectShortHand "@unison/base"
-- Right @unison/base
--
-- Must start with an @
-- >>> fromText @ProjectShortHand "unison/base"
-- Left "User handle must start with '@'"
--
-- Must contain a slash
-- >>> fromText @ProjectShortHand "@unison"
-- Left "Project shorthand must be of the form @user/project"
instance IsID ProjectShortHand where
  toText (ProjectShortHand handle slug) = toText (PrefixedID @"@" handle) <> "/" <> toText slug
  fromText txt = do
    case Text.splitOn "/" txt of
      [handleTxt, slugTxt] -> do
        PrefixedID handle <- fromText @(PrefixedID "@" UserHandle) handleTxt
        projectSlug <- fromText slugTxt
        pure $ ProjectShortHand handle projectSlug
      _ -> Left "Project shorthand must be of the form @user/project"

newtype CategoryName = CategoryName (CI Text)
  deriving stock (Eq, Ord, Show)
  deriving (Hasql.EncodeValue, Hasql.DecodeValue, IsID) via CaseInsensitiveID
  deriving (FromHttpApiData, ToHttpApiData, ToJSON, FromJSON) via UsingID CategoryName

newtype CategoryID = CategoryID UUID
  deriving stock (Eq, Ord)
  deriving (Hasql.EncodeValue, Hasql.DecodeValue) via UUID
  deriving (FromHttpApiData, ToHttpApiData, ToJSON, FromJSON, Show) via (PrefixedID "CAT-" UUID)

-- | Unique ID for a project
newtype ProjectId = ProjectId UUID
  deriving stock (Eq, Ord)
  deriving (Binary, Random, Hasql.EncodeValue, Hasql.DecodeValue) via UUID
  deriving (IsID, Show, FromHttpApiData, ToHttpApiData, ToJSON, FromJSON) via (PrefixedID "P-" UUID)

newtype ContributionId = ContributionId UUID
  deriving stock (Eq, Ord)
  deriving (Binary, Random, Hasql.EncodeValue, Hasql.DecodeValue) via UUID
  deriving (IsID, Show, FromHttpApiData, ToHttpApiData, ToJSON, FromJSON) via (PrefixedID "C-" UUID)

newtype ContributionNumber = ContributionNumber Int32
  deriving stock (Eq, Ord, Show)
  deriving (Hasql.EncodeValue, Hasql.DecodeValue) via Int32
  deriving (IsID, FromHttpApiData, ToHttpApiData, ToJSON, FromJSON) via Int32

data ContributionShortHand = ContributionShortHand
  { projectShortHand :: ProjectShortHand,
    contributionNumber :: ContributionNumber
  }
  deriving stock (Eq, Ord, Show)

newtype TicketId = TicketId UUID
  deriving stock (Eq, Ord)
  deriving (Binary, Random, Hasql.EncodeValue, Hasql.DecodeValue) via UUID
  deriving (IsID, Show, FromHttpApiData, ToHttpApiData, ToJSON, FromJSON) via (PrefixedID "T-" UUID)

newtype TicketNumber = TicketNumber Int32
  deriving stock (Eq, Ord, Show)
  deriving (Hasql.EncodeValue, Hasql.DecodeValue) via Int32
  deriving (IsID, FromHttpApiData, ToHttpApiData, ToJSON, FromJSON) via Int32

-- | Unique ID for a feature branch
newtype BranchId = BranchId UUID
  deriving stock (Eq, Ord)
  deriving (Binary, Random, Hasql.EncodeValue, Hasql.DecodeValue) via UUID
  deriving (IsID, Show, FromHttpApiData, ToHttpApiData, ToJSON, FromJSON) via (PrefixedID "B-" UUID)

-- | Human readable name for a branch. See also BranchShortHand which may include a
-- contributor.
newtype BranchName = BranchName (CI Text)
  deriving stock (Eq, Ord, Show)
  deriving (Hasql.EncodeValue, Hasql.DecodeValue) via CaseInsensitiveID
  deriving (FromHttpApiData, ToHttpApiData, ToJSON, FromJSON) via UsingID BranchName

-- |
--
-- Happy path
-- >>> fromText @BranchName "main"
-- Right (BranchName "main")
--
-- Must start with a letter or underscore
-- >>> fromText @BranchName "_main"
-- Right (BranchName "_main")
--
-- >>> fromText @BranchName "1main"
-- Left "Branch name must start with a letter or underscore"
--
-- Must contain only letters, numbers, underscores, and hyphens
-- >>> fromText @BranchName "main-1"
-- Right (BranchName "main-1")
--
-- >>> fromText @BranchName "main 1"
-- Left "Branch name must contain only letters, numbers, underscores, and hyphens"
--
-- Must not be longer than 50 characters
-- >>> fromText @BranchName $ Text.replicate 51 "a"
-- Left "Branch name must not be longer than 50 characters"
--
-- Must not be empty
-- >>> fromText @BranchName ""
-- Left "Branch name must not be empty"
--
-- Is case insensitive
-- >>> fromText @BranchName "MAIN" == fromText @BranchName "main"
-- True
--
-- But maintains provided case
-- >>> toText <$> fromText @BranchName "MAIN"
-- Right "MAIN"
--
-- Accepts draft release branches
-- >>> toText <$> fromText @BranchName "releases/drafts/0.1.0"
-- Right "releases/drafts/0.1.0"

-- Rejects invalid draft release branches
-- >>> toText <$> fromText @BranchName "releases/drafts/1"
-- Left "Release version must match the form '1.2.3'"
--
-- >>> toText <$> fromText @BranchName "releases/drafts/1.0.0.1"
-- Left "Release version must match the form '1.2.3'"
instance IsID BranchName where
  toText (BranchName s) = CI.original s
  fromText txt =
    let parser = do
          (tag, bn) <- branchNameParser
          let e = case tag of
                BranchNameTag'Plain -> BranchNameParseFailure'InvalidChar
                BranchNameTag'ReleaseDraft -> BranchNameParseFailure'InvalidSemVer
          eof <|> customFailure e
          pure bn
     in case runParser parser "" txt of
          Right x -> Right x
          Left peb -> Left (Text.pack $ simpleErrorBundlePretty peb)

simpleErrorBundlePretty :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => ParseErrorBundle s e -> String
simpleErrorBundlePretty peb@ParseErrorBundle {bundleErrors}
  | es@(_ : _) <- getCustoms bundleErrors = intercalate ", " $ map showErrorComponent es
  | otherwise = errorBundlePretty peb
  where
    getCustoms =
      let f a b = case a of
            FancyError _ es -> onlyCustoms es ++ b
            _ -> b
          onlyCustoms = foldr onlyCustomsFunc []
          onlyCustomsFunc a b = case a of
            ErrorCustom e -> e : b
            _ -> b
       in foldr f []

data BranchShortHand = BranchShortHand
  { contributorHandle :: Maybe UserHandle,
    branchName :: BranchName
  }
  deriving stock (Eq, Ord, Show)
  deriving (FromHttpApiData, ToHttpApiData, ToJSON, FromJSON) via UsingID BranchShortHand

data BranchNameParseFailure
  = BranchNameParseFailure'Empty
  | BranchNameParseFailure'InvalidStartChar
  | BranchNameParseFailure'InvalidChar
  | BranchNameParseFailure'TooLong
  | BranchNameParseFailure'InvalidSemVer
  deriving stock (Eq, Ord)

instance ShowErrorComponent BranchNameParseFailure where
  showErrorComponent = \case
    BranchNameParseFailure'Empty -> "Branch name must not be empty"
    BranchNameParseFailure'InvalidStartChar -> "Branch name must start with a letter or underscore"
    BranchNameParseFailure'InvalidChar -> "Branch name must contain only letters, numbers, underscores, and hyphens"
    BranchNameParseFailure'TooLong -> "Branch name must not be longer than 50 characters"
    BranchNameParseFailure'InvalidSemVer -> "Release version must match the form '1.2.3'"

data BranchNameTag
  = BranchNameTag'ReleaseDraft
  | BranchNameTag'Plain
  deriving stock (Show, Eq)

branchNameParser ::
  Parsec BranchNameParseFailure Text (BranchNameTag, BranchName)
branchNameParser = do
  Megaparsec.observing eof >>= \case
    Left _ -> pure ()
    Right () -> customFailure BranchNameParseFailure'Empty
  (BranchNameTag'ReleaseDraft,) . releaseDraftFixup
    <$> releaseDraftParser
      <|> (BranchNameTag'Plain,)
    <$> plainBranchParser
  where
    releaseDraftParser :: Parsec BranchNameParseFailure Text ReleaseVersion
    releaseDraftParser = do
      _ <- chunk "releases/drafts/"
      Megaparsec.observing semVerParser >>= \case
        Left _ -> customFailure BranchNameParseFailure'InvalidSemVer
        Right x -> pure x

    releaseDraftFixup relVer =
      BranchName
        ( CI.mk $
            "releases/drafts/" <> toText relVer
        )
    plainBranchParser = do
      firstChar <-
        satisfy
          (\startChar -> Char.isAlpha startChar || startChar == '_')
          <|> customFailure BranchNameParseFailure'InvalidStartChar
      rest <-
        takeWhileP
          (Just "letters, numbers, underscores, or hyphens")
          (\c -> (Char.isAlphaNum c || c == '_' || c == '-'))
      let branchName = Text.cons firstChar rest
      case Text.length branchName > 50 of
        True -> customFailure BranchNameParseFailure'TooLong
        False -> pure ()
      pure (BranchName $ CI.mk branchName)

semVerParser :: (Ord e) => Parsec e Text ReleaseVersion
semVerParser = do
  major <- decimal
  _ <- single '.'
  minor <- decimal
  _ <- single '.'
  patch <- decimal
  pure ReleaseVersion {major, minor, patch}

-- | A branch identifier of the form '@user/branch' or 'branch' usually used by UCM.
--
-- With contributor
-- >>> fromText @BranchShortHand "@unison/base"
-- Right (BranchShortHand {contributorHandle = Just (UserHandle "unison"), branchName = BranchName "base"})
--
-- Without contributor
-- >>> fromText @BranchShortHand "base"
-- Right (BranchShortHand {contributorHandle = Nothing, branchName = BranchName "base"})
--
-- Must contain an @
-- >>> fromText @BranchShortHand "unison/base"
-- Left "Branch Shorthand must be of the form '@user/branch' or 'branch'"
--
-- contain a branch name portion
-- >>> fromText @BranchShortHand "@unison"
-- Left "Branch name must start with a letter or underscore"
--
-- Must not contain more than one /
-- >>> fromText @BranchShortHand "@unison/base/other"
-- Left "Branch Shorthand must be of the form '@user/branch' or 'branch'"
--
-- Rejects user handle with draft release
-- >>> fromText @BranchShortHand "@unison/releases/drafts/0.1.0"
-- Left "Branch Shorthand must be of the form '@user/branch' or 'branch'"
--
-- Accepts draft release
-- >>> fromText @BranchShortHand "releases/drafts/0.1.0"
-- Right (BranchShortHand {contributorHandle = Nothing, branchName = BranchName "releases/drafts/0.1.0"})
instance IsID BranchShortHand where
  toText (BranchShortHand {contributorHandle, branchName}) =
    case contributorHandle of
      Just h -> "@" <> toText h <> "/" <> toText @BranchName branchName
      Nothing -> toText @BranchName branchName
  fromText txt = do
    case Text.splitOn "/" txt of
      [Text.uncons -> (Just ('@', h)), n] -> do
        contributorHandle <- Just <$> fromText @UserHandle h
        branchName <- fromText @BranchName n
        pure $ BranchShortHand {contributorHandle, branchName}
      [_] -> BranchShortHand Nothing <$> fromText txt
      ["releases", "drafts", _] -> BranchShortHand Nothing <$> fromText txt
      _ -> Left "Branch Shorthand must be of the form '@user/branch' or 'branch'"

data ProjectBranchShortHand = ProjectBranchShortHand
  { userHandle :: UserHandle,
    projectSlug :: ProjectSlug,
    contributorHandle :: Maybe UserHandle,
    branchName :: BranchName
  }
  deriving stock (Eq, Ord, Show)
  deriving (FromHttpApiData, ToHttpApiData, ToJSON, FromJSON) via UsingID ProjectBranchShortHand

projectBranchShortHandToBranchShortHand :: ProjectBranchShortHand -> BranchShortHand
projectBranchShortHandToBranchShortHand ProjectBranchShortHand {contributorHandle, branchName} =
  BranchShortHand {contributorHandle, branchName}

-- | A fully specified branch identifier of the form '@user/project/@contributor/branch' or
--
-- With contributor
-- >>> fromText @ProjectBranchShortHand "@unison/base/@runarorama/main"
-- Right (ProjectBranchShortHand {userHandle = UserHandle "unison", projectSlug = base, contributorHandle = Just (UserHandle "runarorama"), branchName = BranchName "main"})
--
-- Without contributor
-- >>> fromText @ProjectBranchShortHand "@unison/base/main"
-- Right (ProjectBranchShortHand {userHandle = UserHandle "unison", projectSlug = base, contributorHandle = Nothing, branchName = BranchName "main"})
--
-- Must contain an @
-- >>> fromText @ProjectBranchShortHand "unison/base"
-- Left "User handle must start with '@'"
--
-- Must contain a project slug
-- >>> fromText @ProjectBranchShortHand "@unison/@runarorama/main"
-- Left "Project slug must start with a letter or underscore"
--
-- Must contain a branch name portion
-- >>> fromText @ProjectBranchShortHand "@unison/base/@runarorama"
-- Left "Branch name must start with a letter or underscore"
instance IsID ProjectBranchShortHand where
  toText (ProjectBranchShortHand {userHandle, projectSlug, contributorHandle, branchName}) =
    toText (ProjectShortHand {userHandle, projectSlug}) <> "/" <> toText (BranchShortHand {contributorHandle, branchName})
  fromText txt = do
    case Text.splitOn "/" txt of
      (userHandleTxt : projectSlugTxt : branchPortion) -> do
        PrefixedID userHandle <- fromText @(PrefixedID "@" UserHandle) userHandleTxt
        projectSlug <- fromText projectSlugTxt
        BranchShortHand {contributorHandle, branchName} <- fromText @BranchShortHand (Text.intercalate "/" branchPortion)
        pure $ ProjectBranchShortHand {userHandle, projectSlug, contributorHandle, branchName}
      _ -> Left "Project Branch Shorthand must be of the form '@user/project/@contributor/branch' or '@user/project/branch'"

-- | Unique ID for a project release
newtype ReleaseId = ReleaseId UUID
  deriving stock (Eq, Ord)
  deriving (Binary, Random, Hasql.EncodeValue, Hasql.DecodeValue) via UUID
  deriving (IsID, Show, FromHttpApiData, ToHttpApiData, ToJSON, FromJSON) via (PrefixedID "R-" UUID)

-- | Version for a release, e.g. "1.2.3". See also ReleaseShortHand which includes the
-- 'releases/' prefix.
data ReleaseVersion = ReleaseVersion
  { major :: !Int64,
    minor :: !Int64,
    patch :: !Int64
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (FromHttpApiData, ToHttpApiData, ToJSON, FromJSON) via UsingID ReleaseVersion

instance Hasql.DecodeRow ReleaseVersion where
  decodeRow = Hasql.decodeRow <&> \(major, minor, patch) -> ReleaseVersion {major, minor, patch}

-- |
--
-- Happy path
-- >>> fromText @ReleaseVersion "1.2.3"
-- Right (ReleaseVersion {major = 1, minor = 2, patch = 3})
--
-- Normalizing
-- >>> fromText @ReleaseVersion "010.02.03" == fromText "10.2.3"
-- True
--
-- Must not be empty
-- >>> fromText @ReleaseVersion ""
-- Left "Release version must match the form '1.2.3'"
--
-- Doesn't currently support 'pre-release' versions, we may add this later
-- >>> fromText @ReleaseVersion "1.2.3-alpha"
-- Left "Release version must match the form '1.2.3'"
--
-- Must be a version number
-- >>> fromText @ReleaseVersion "not-a-version"
-- Left "Release version must match the form '1.2.3'"
instance IsID ReleaseVersion where
  toText (ReleaseVersion {major, minor, patch}) = Text.pack (show major <> "." <> show minor <> "." <> show patch)
  fromText txt
    | Just relVer <- parseMaybe (semVerParser @Void) txt = Right relVer
    | otherwise = Left "Release version must match the form '1.2.3'"

-- | A shorthand for a release identifier of the form 'releases/version
newtype ReleaseShortHand = ReleaseShortHand
  { releaseVersion :: ReleaseVersion
  }
  deriving stock (Eq, Ord, Show)
  deriving (FromHttpApiData, ToHttpApiData, ToJSON, FromJSON) via UsingID ReleaseShortHand

-- | A fully specified release identifier of the form 'releases/version
-- This type is isomorphic to ReleaseVersion, but is helpful for parsing branch shorthands.
--
-- >>> toText (ReleaseShortHand ReleaseVersion { major = 1, minor = 2, patch = 3})
-- "releases/1.2.3"
--
-- >>> fromText @ReleaseShortHand "releases/1.2.3"
-- Right (ReleaseShortHand {releaseVersion = ReleaseVersion {major = 1, minor = 2, patch = 3}})
--
-- Must start with 'releases/'
-- >>> fromText @ReleaseShortHand "1.2.3"
-- Left "Release Shorthand must be of the form 'releases/version'"
--
-- Must contain a version number
-- >>> fromText @ReleaseShortHand "releases/not-a-version"
-- Left "Release version must match the form '1.2.3'"
instance IsID ReleaseShortHand where
  toText (ReleaseShortHand {releaseVersion}) = "releases/" <> toText releaseVersion
  fromText txt = do
    case Text.splitOn "/" txt of
      ["releases", n] -> ReleaseShortHand <$> fromText @ReleaseVersion n
      _ -> Left "Release Shorthand must be of the form 'releases/version'"

-- | A fully specified release identifier of the form '@user/project/releases/version
data ProjectReleaseShortHand = ProjectReleaseShortHand
  { userHandle :: UserHandle,
    projectSlug :: ProjectSlug,
    releaseVersion :: ReleaseVersion
  }
  deriving stock (Eq, Ord, Show)
  deriving (FromHttpApiData, ToHttpApiData, ToJSON, FromJSON) via UsingID ProjectReleaseShortHand

-- | A fully specified release identifier of the form '@user/project/releases/version
--
-- Happy path
-- >>> fromText @ProjectReleaseShortHand "@unison/base/releases/1.2.3"
-- Right (ProjectReleaseShortHand {userHandle = UserHandle "unison", projectSlug = base, releaseVersion = ReleaseVersion {major = 1, minor = 2, patch = 3}})
--
-- Must contain an @
-- >>> fromText @ProjectReleaseShortHand "unison/base"
-- Left "Project Release Shorthand must be of the form '@user/project/releases/version'"
--
-- Must contain a project slug
-- >>> fromText @ProjectReleaseShortHand "@unison/@runarorama/main"
-- Left "Project Release Shorthand must be of the form '@user/project/releases/version'"
--
-- Must contain a release version portion
-- >>> fromText @ProjectReleaseShortHand "@unison/base/@runarorama"
-- Left "Project Release Shorthand must be of the form '@user/project/releases/version'"
--
-- Must have a 'releases' segment
-- >>> fromText @ProjectReleaseShortHand "@unison/base/other/1.2.3"
-- Left "Project Release Shorthand must be of the form '@user/project/releases/version'"
instance IsID ProjectReleaseShortHand where
  toText (ProjectReleaseShortHand {userHandle, projectSlug, releaseVersion}) =
    toText (ProjectShortHand {userHandle, projectSlug}) <> "/" <> toText (ReleaseShortHand releaseVersion)
  fromText txt = do
    mapLeft (const errMsg) $
      case Text.splitOn "/" txt of
        (userHandleTxt : projectSlugTxt : releaseShortHandTxt) -> do
          PrefixedID userHandle <- fromText @(PrefixedID "@" UserHandle) userHandleTxt
          projectSlug <- fromText projectSlugTxt
          ReleaseShortHand releaseVersion <- fromText (Text.intercalate "/" releaseShortHandTxt)
          pure $ ProjectReleaseShortHand {userHandle, projectSlug, releaseVersion}
        _ -> Left errMsg
    where
      errMsg = "Project Release Shorthand must be of the form '@user/project/releases/version'"

-- | For parsing a parameter that's either a branch or a release shorthand
data BranchOrReleaseShortHand
  = IsBranchShortHand BranchShortHand
  | IsReleaseShortHand ReleaseShortHand
  deriving stock (Eq, Ord)
  deriving (Show, FromHttpApiData, ToHttpApiData, ToJSON, FromJSON) via (UsingID BranchOrReleaseShortHand)

-- | For parsing a parameter that's either a branch or a release shorthand
--
-- >>> fromText @BranchOrReleaseShortHand "releases/1.2.3"
-- Right releases/1.2.3
--
-- >>> fromText @BranchOrReleaseShortHand "@runarorama/main"
-- Right @runarorama/main
--
-- >>> fromText @BranchOrReleaseShortHand "invalid/main"
-- Left "Release or Branch Shorthand must be of the form 'releases/x.y.z or 'branch-name' or '@contributor/branch-name'"
instance IsID BranchOrReleaseShortHand where
  toText (IsReleaseShortHand release) = toText release
  toText (IsBranchShortHand branch) = toText branch
  fromText txt = do
    case fromText @(ReleaseShortHand) txt of
      Right release -> Right $ IsReleaseShortHand release
      Left _ -> do
        case fromText @(BranchShortHand) txt of
          Right branch -> Right $ IsBranchShortHand branch
          Left _ -> Left "Release or Branch Shorthand must be of the form 'releases/x.y.z or 'branch-name' or '@contributor/branch-name'"

newtype CausalHash = CausalHash Text
  deriving stock (Eq, Ord)
  deriving (Hasql.EncodeValue, Hasql.DecodeValue) via Text
  deriving (Show, FromHttpApiData, ToHttpApiData, ToJSON, FromJSON, IsID) via Text

newtype CommentId = CommentId UUID
  deriving stock (Eq, Ord)
  deriving (Hasql.EncodeValue, Hasql.DecodeValue) via UUID
  deriving (Show, FromHttpApiData, ToHttpApiData, ToJSON, FromJSON, IsID) via (PrefixedID "CMT-" UUID)

-- | Helper newtype for serializing/deserializing prefixed hashes.
newtype PrefixedHash (prefix :: Symbol) h = PrefixedHash h
  deriving newtype (Eq, Ord)
  deriving (Show)

instance (From h Text, KnownSymbol prefix) => From (PrefixedHash prefix h) Text where
  from (PrefixedHash h) =
    let prefix = Text.pack $ symbolVal (Proxy @prefix)
     in prefix <> into @Text h

instance (KnownSymbol prefix, ToHttpApiData h) => ToHttpApiData (PrefixedHash prefix h) where
  toUrlPiece (PrefixedHash h) =
    let prefix = Text.pack $ symbolVal (Proxy @prefix)
     in prefix <> toUrlPiece h

instance (KnownSymbol prefix, FromHttpApiData h) => FromHttpApiData (PrefixedHash prefix h) where
  parseUrlPiece txt =
    let prefix = Text.pack $ symbolVal (Proxy @prefix)
     in maybe (Left $ "Could not parse '" <> txt <> "', expected " <> prefix <> "<hash>") Right $ do
          bareHash <- Text.stripPrefix prefix txt
          h <- eitherToMaybe $ parseUrlPiece bareHash
          pure (PrefixedHash h)

instance (ToHttpApiData h, KnownSymbol prefix) => ToJSON (PrefixedHash prefix h) where
  toJSON h = toJSON (toUrlPiece h)

instance (FromHttpApiData h, KnownSymbol prefix) => FromJSON (PrefixedHash prefix h) where
  parseJSON = Aeson.withText "PrefixedHash" \txt ->
    let prefix = Text.pack $ symbolVal (Proxy @prefix)
     in maybe (fail $ "Could not parse '" <> Text.unpack txt <> "', expected " <> Text.unpack prefix <> "<hash>") pure $ do
          bareHash <- Text.stripPrefix prefix txt
          h <- eitherToMaybe $ parseUrlPiece bareHash
          pure (PrefixedHash h)

newtype SubjectId = SubjectId UUID
  deriving stock (Eq, Ord)
  deriving (Hasql.EncodeValue, Hasql.DecodeValue) via UUID
  deriving (Show, FromHttpApiData, ToHttpApiData, ToJSON, FromJSON, IsID) via (PrefixedID "SUB-" UUID)

newtype ResourceId = ResourceId UUID
  deriving stock (Eq, Ord)
  deriving (Hasql.EncodeValue, Hasql.DecodeValue) via UUID
  deriving (Show, FromHttpApiData, ToHttpApiData, ToJSON, FromJSON, IsID) via (PrefixedID "RES-" UUID)

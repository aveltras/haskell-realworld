{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import           Data.Aeson
import           Data.Text
import           Servant.API
import GHC.Generics (Generic)


type RootAPI = "users" :> AuthAPI
              :<|> "user" :> UserAPI
              :<|> "profiles" :> ProfileAPI
              :<|> "articles" :> ArticleAPI
              :<|> "tags" :> TagAPI

type AuthAPI = ReqBody '[JSON] RegisterPayload :> Post '[JSON] UserPayload
          :<|> "login" :> ReqBody '[JSON] LoginPayload :> Post '[JSON] UserPayload

type UserAPI = Get '[JSON] UserPayload
              :<|> Put '[JSON] UserPayload

type ProfileAPI = Capture "username" Text :> (
  Get '[JSON] ProfilePayload
  :<|> "follow" :> (Post '[JSON] ProfilePayload :<|> Delete '[JSON] ProfilePayload)
  )

type ArticleAPI = Get '[JSON] [ArticlePayload]
                :<|> "feed" :> Get '[JSON] [ArticlePayload]
                :<|> ReqBody '[JSON] CreateArticlePayload :> Post '[JSON] ArticlePayload
                :<|> Capture "slug" Text :> (
                  Get '[JSON] ArticlePayload
                  :<|> ReqBody '[JSON] UpdateArticlePayload :> Post '[JSON] ArticlePayload
                  :<|> Delete '[JSON] NoContent
                  :<|> "comments" :> (
                    ReqBody '[JSON] AddCommentPayload :> Post '[JSON] CommentPayload
                    :<|> Capture "id" Int :> Delete '[JSON] NoContent
                  )
                  :<|> "favorite" :> (
                    Post '[JSON] ArticlePayload
                    :<|> Delete '[JSON] ArticlePayload
                  )
                )

type TagAPI = Get '[JSON] TagListPayload

data UserPayload = UserPayload
  { email    :: Text,
    token    :: Text,
    username :: Text,
    bio      :: Text,
    image    :: Text
  } deriving (Generic, ToJSON, FromJSON)

data ProfilePayload = ProfilePayload
  { profileUsername  :: Text
  , profileBio       :: Text
  , profileImage     :: Text
  , profileFollowing :: Bool
  } deriving (Generic, ToJSON, FromJSON)

type TagListPayload = [Text]

data ArticlePayload = ArticlePayload
  { articleSlug           :: Text
  , articleTitle          :: Text
  , articleDescription    :: Text
  , articleBody           :: Text
  , articleTags           :: TagListPayload
  , articleCreatedAt      :: Text
  , articleUpdatedAt      :: Text
  , articleFavorited      :: Bool
  , articleFavoritesCount :: Int
  , articleAuthor         :: ProfilePayload
  } deriving (Generic, ToJSON, FromJSON)

data CommentPayload = CommentPayload
  { commentId        :: Int
  , commentCreatedAt :: Text
  , commentUpdatedAt :: Text
  , commentBody      :: Text
  , commentAuthor    :: ProfilePayload
  } deriving (Generic, ToJSON, FromJSON)

newtype ErrorPayload = ErrorPayload
  { errorBody :: Text
  } deriving (Generic, ToJSON, FromJSON)

data LoginPayload = LoginPayload
  { loginEmail    :: Text
  , loginPassword :: Text
  } deriving (Generic, ToJSON, FromJSON)

data RegisterPayload = RegisterPayload
  { registerUsername :: Text
  , registerEmail    :: Text
  , registerPassword :: Text
  } deriving (Generic, ToJSON, FromJSON)

data CreateArticlePayload = CreateArticlePayload
  { createArticleTitle       :: Text
  , createArticleDescription :: Text
  , createArticleBody        :: Text
  , createArticleTagList     :: TagListPayload
  } deriving (Generic, ToJSON, FromJSON)

newtype UpdateArticlePayload = UpdateArticlePayload
  { updateArticleTitle :: Text
  } deriving (Generic, ToJSON, FromJSON)

newtype AddCommentPayload = AddCommentPayload
  { addCommentBody :: Text
  } deriving (Generic, ToJSON, FromJSON)

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import           Data.Text
import           Servant.API


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
  }

data ProfilePayload = ProfilePayload
  { profileUsername  :: Text
  , profileBio       :: Text
  , profileImage     :: Text
  , profileFollowing :: Bool
  }

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
  }

data CommentPayload = CommentPayload
  { commentId        :: Int
  , commentCreatedAt :: Text
  , commentUpdatedAt :: Text
  , commentBody      :: Text
  , commentAuthor    :: ProfilePayload
  }

newtype ErrorPayload = ErrorPayload
  { errorBody :: Text
  }

data LoginPayload = LoginPayload
  { loginEmail    :: Text
  , loginPassword :: Text
  }

data RegisterPayload = RegisterPayload
  { registerUsername :: Text
  , registerEmail    :: Text
  , registerPassword :: Text
  }

data CreateArticlePayload = CreateArticlePayload
  { createArticleTitle       :: Text
  , createArticleDescription :: Text
  , createArticleBody        :: Text
  , createArticleTagList     :: TagListPayload
  }

newtype UpdateArticlePayload = UpdateArticlePayload
  { updateArticleTitle :: Text
  }

newtype AddCommentPayload = AddCommentPayload
  { addCommentBody :: Text
  }

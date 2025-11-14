{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.Share.Webhooks.API
  ( API,
    Routes (..),
    WebhookPayloadExamples,
  )
where

import Servant
import Share.BackgroundJobs.Webhooks.Types
import Share.JWT
import Share.Prelude

type API = NamedRoutes Routes

data Routes mode
  = Routes
  { payloadExamples :: mode :- "examples" :> WebhookExamplesEndpoint
  }
  deriving stock (Generic)

type WebhookPayloadExamples = [WebhookEventPayload JWTParam]

type WebhookExamplesEndpoint =
  Get '[JSON] WebhookPayloadExamples

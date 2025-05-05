{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.Support.Zendesk where

import Control.Monad.Reader
import Data.Aeson
import Data.Either (fromRight)
import Servant
import Servant.Client
import Share.Env qualified as Env
import Share.IDs
import Share.Prelude
import Share.Utils.Servant.Client (runClient)
import Share.Web.App
import Share.Web.Support.Types

-- | Field Id for the Share Handle custom ticket field. See https://unison-computing.zendesk.com/admin/objects-rules/tickets/ticket-fields
zendeskShareHandleFieldId :: Int
zendeskShareHandleFieldId = 19863263042459

-- | Field Id for the UserId custom ticket field. See https://unison-computing.zendesk.com/admin/objects-rules/tickets/ticket-fields
zendeskShareUserIdFieldId :: Int
zendeskShareUserIdFieldId = 19863242162331

zendeskAPIBaseURL :: BaseUrl
zendeskAPIBaseURL =
  fromRight (error "Invalid zendesk baseURL") $ parseBaseUrl "https://unison-computing.zendesk.com/api/v2/"

-- https://developer.zendesk.com/api-reference
type ZendeskAPI = BasicAuth "" BasicAuthData :> "tickets" :> ZendeskCreateTicketEndpoint

type ZendeskCreateTicketEndpoint =
  ReqBody '[JSON] ZendeskTicket
    :> PostCreated '[JSON] Value

data ZendeskTicket = ZendeskTicket
  { subject :: Text,
    body :: Text,
    priority :: Priority,
    requesterName :: Text,
    requesterEmail :: Maybe Email,
    shareHandle :: UserHandle,
    shareUserId :: UserId
  }
  deriving stock (Show)

instance ToJSON ZendeskTicket where
  toJSON (ZendeskTicket {subject, body, priority, requesterName, requesterEmail, shareHandle, shareUserId}) =
    object
      [ "ticket"
          .= object
            [ "comment" .= object ["body" .= body],
              "priority" .= priority,
              "subject" .= subject,
              "requester" .= object ["name" .= requesterName, "email" .= requesterEmail],
              "custom_fields"
                .= [ object ["id" .= zendeskShareHandleFieldId, "value" .= shareHandle],
                     object ["id" .= zendeskShareUserIdFieldId, "value" .= shareUserId]
                   ]
            ]
      ]

createTicket :: ZendeskTicket -> WebApp Value
createTicket ticket = do
  authData <- asks Env.zendeskAuth
  let mkRequest = hoistClient (Proxy @ZendeskAPI) (runClient zendeskAPIBaseURL) $ client (Proxy @ZendeskAPI)
  mkRequest authData ticket

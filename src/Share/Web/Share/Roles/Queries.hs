{-# LANGUAGE TypeOperators #-}

module Share.Web.Share.Roles.Queries (displaySubjectsOf) where

import Control.Lens
import Share.Postgres
import Share.Postgres.Users.Queries (userDisplayInfoOf)
import Share.Web.Authorization.Types (DisplayAuthSubject, ResolvedAuthSubject, _OrgSubject, _TeamSubject, _UserSubject)
import Share.Web.Share.Orgs.Queries (orgDisplayInfoOf)
import Share.Web.Share.Teams.Queries (teamDisplayInfoOf)

displaySubjectsOf :: (Traversal s t ResolvedAuthSubject DisplayAuthSubject) -> s -> Transaction e t
displaySubjectsOf trav s = pipelined do
  s & unsafePartsOf trav \authSubjectList -> do
    let userSubjects = (authSubjectList ^. unsafePartsOf (traversed . _UserSubject))
    newUsers <- userDisplayInfoOf traversed userSubjects
    let teamSubjects = (authSubjectList ^. unsafePartsOf (traversed . _TeamSubject))
    newTeams <- teamDisplayInfoOf traversed teamSubjects
    let orgSubjects = (authSubjectList ^. unsafePartsOf (traversed . _OrgSubject))
    newOrgs <- orgDisplayInfoOf traversed orgSubjects
    pure
      ( authSubjectList
          & unsafePartsOf (traversed . _UserSubject) .~ newUsers
          & unsafePartsOf (traversed . _TeamSubject) .~ newTeams
          & unsafePartsOf (traversed . _OrgSubject) .~ newOrgs
      )

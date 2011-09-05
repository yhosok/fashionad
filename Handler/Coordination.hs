{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Coordination where

import Control.Applicative

import Foundation
import Handler.Item

coordForm :: UserId -> Maybe Coordination -> Html -> Form FashionAd FashionAd (FormResult Coordination, Widget)
coordForm uid mc = renderTable $ Coordination
    <$> pure uid
    <*> areq textField "title" (fmap coordinationTitle mc)
    <*> aopt textField "description" (fmap coordinationDesc mc)

getCoordinationsR :: Handler RepHtml
getCoordinationsR = do
  mu <- requireAuth
  cos <- runDB $ selectList [] []
  defaultLayout $ do
    setTitle "fashionad homepage"
    addWidget $(widgetFile "coordinations")

getCoordinationR :: CoordinationId -> Handler RepHtml
getCoordinationR cid = do
  (uid,u) <- requireAuth
  mc <- runDB $ get cid
  items <- runDB $ selectList [ItemCoordination ==. cid] []
  ((res, form), enc) <- runFormPost $ coordForm uid mc
  ((_, itemForm), _) <- runFormPost $ itemForm cid Nothing
  case res of
    FormSuccess c -> do
      runDB $ replace cid c
      setMessage "Updated Coordination"
      redirect RedirectTemporary CoordinationsR
    _ -> return ()
  defaultLayout $ do
    addWidget $(widgetFile "coordination")

postCoordinationR :: CoordinationId -> Handler RepHtml
postCoordinationR = getCoordinationR

getAddCoordinationR ::Handler RepHtml
getAddCoordinationR = do
  (uid, u) <- requireAuth
  let items = []
  ((res,form),enc) <- runFormPost $ coordForm uid Nothing
  case res of
    FormSuccess c -> do
      cid <- runDB $ insert c
      setMessage "Added new Coordination"
      redirect RedirectTemporary CoordinationsR
    _ -> return ()
  defaultLayout $ do
    addWidget $(widgetFile "item")

postAddCoordinationR :: Handler RepHtml
postAddCoordinationR = getAddCoordinationR

postDelCoordinationR :: CoordinationId -> Handler RepHtml
postDelCoordinationR = undefined

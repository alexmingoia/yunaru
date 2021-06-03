module App.Controller.Discover where

import App.Controller.Page
import App.Controller.Session as Session
import App.Model.Database as DB
import App.Model.Env
import App.Model.FeedDetailed as FeedDetailed
import App.View.Discover
import Web.Twain

list :: RouteM AppEnv ()
list = do
  appEnv <- env
  userM <- Session.getUser
  categoryM <- paramMaybe "category"
  feedsDtld <- DB.exec $ FeedDetailed.findByCategory categoryM userM
  sendHtmlPage status200 (appName appEnv) $
    discoverPageHtml appEnv categoryM feedsDtld

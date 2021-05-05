{-# LANGUAGE DeriveGeneric #-}

module App.Model.PaymentProvider where

import App.Model.Selda

data PaymentProvider = Stripe
  deriving (Show, Read, Bounded, Enum, Generic)

instance SqlType PaymentProvider

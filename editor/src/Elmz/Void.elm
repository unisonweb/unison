module Elmz.Void (Void, absurd) where

data Void

absurd : Void -> a
absurd v = absurd v

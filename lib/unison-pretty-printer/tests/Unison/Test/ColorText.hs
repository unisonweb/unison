{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Unison.Test.ColorText where

import EasyTest

test :: Test ()
test =
  scope "colortext" . tests $
    []

module Messages where

import Native.Messages

import Signal (Signal, Message)

send : Signal Message -> Signal ()
send = Native.Messages.send

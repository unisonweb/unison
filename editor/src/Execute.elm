module Execute where

{-| Execute scheduling and delivery of `Signal.Message` values. -}

import Native.Execute
import Signal (Signal, Message)

{-| The message which does nothing. -}
noop : Message
noop = Native.Execute.noop

{-| Combine two messages into one. The first argument will
be delivered before the second. -}
combine : Message -> Message -> Message
combine = Native.Execute.combine

{-| Schedule the input `Message` values for delivery.
The output signal has same event occurrences as the input signal,
and there is no guarantee that effects of message delivery will
be visible when the output `Signal ()` updates.

Implementation uses `setTimeout(.., 0)` in Javascript to schedule
delivery of the `Message`.
-}
schedule : Signal Message -> Signal ()
schedule = Native.Execute.schedule

{-| Schedule the input `Message` values for delivery and wait for
completion of each delivery. Unlike `schedule`, the output `Signal`
will refresh once per `Message` _after_ all effects of message
delivery have propagated through the signal graph. There will be
exactly one refresh of the output signal for each input `Message`,
but the implementation uses `setTimeout(.., 0)` in Javascript so
there aren't any guarantees about exact ordering of updates,
especially with regard to other events in the signal graph.
-}
complete : Signal Message -> Signal ()
complete = Native.Execute.complete

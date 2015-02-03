Elm.Native.Execute = {};
Elm.Native.Execute.make = function(localRuntime) {

    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.Execute = localRuntime.Native.Execute || {};
    if (localRuntime.Native.Execute.values) {
        return localRuntime.Native.Execute.values;
    }
    var Signal = Elm.Signal.make(localRuntime);
    
    // schedule : Signal Message -> Signal ()
    function schedule(msgs) {
        var tuple0 = { ctor: "_Tuple0" }; 
        function scheduleForce(thunk) {
          setTimeout(thunk, 0);
          return tuple0;
        }
        return A2( Signal.map, scheduleForce, msgs );
    }

    // complete : Signal Message -> Signal ()
    function complete(msgs) {
        var tuple0 = { ctor: "_Tuple0" }; 
        var output = Signal.constant(tuple0);
        // need to nest the calls to `setTimeout` to ensure the output signal
        // is refreshed *after* the message has been delivered
        function scheduleForce(thunk) {
          setTimeout(
            function() { 
              thunk(); 
              setTimeout(function() { localRuntime.notify(output.id, tuple0); }, 0)
            }, 0);
          return tuple0;
        }
        var forced = A2( Signal.map, scheduleForce, msgs );
        function k(x) { return function(y) { return x; } }
        // the sampleOn output is important, since the `map2` would otherwise 
        // emit an event when either a message comes in OR a message is delivered 
        return A2(Signal.sampleOn, output, A3( Signal.map2, k, output, forced));
    }

    return localRuntime.Native.Execute.values = {
        schedule: schedule,
        complete : complete
    };
};

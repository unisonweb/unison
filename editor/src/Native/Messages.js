Elm.Native.Messages = {};
Elm.Native.Messages.make = function(localRuntime) {

    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.Messages = localRuntime.Native.Messages || {};
    if (localRuntime.Native.Messages.values) {
        return localRuntime.Native.Messages.values;
    }
    var Signal = Elm.Signal.make(localRuntime);
    
    // send : Signal Message -> Signal ()
    function send(msgs) {
        var tuple0 = { ctor: "_Tuple0" }; 
        function scheduleForce(thunk) {
          setTimeout(thunk, 0);
          return tuple0;
        }
        return A2( Signal.map, scheduleForce, msgs );
    }

    return localRuntime.Native.Messages.values = {
        send: send
    };
};

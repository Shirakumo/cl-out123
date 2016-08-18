## About cl-out123
This is a bindings library to [libout123](https://www.mpg123.de/api/) which allows easy cross-platform audio playback.

## How To
Precompiled versions of the underlying library are included in this. If you want to build it manually however, refer to the [mpg123](https://www.mpg123.de/) page.

Load the system through ASDF or Quicklisp:

    (ql:quickload :cl-out123)

Create a new output object:

    (defvar *out* (make-instance 'cl-out123:output))

This will initialise a standard output handler object for you, based on some hopefully sane defaults. We can now look at a list of possible drivers:

    (cl-out123:drivers *out*)

If we're fine with the automatic default or now have a backend we know we want to use, we can connect to it:

    (cl-out123:connect *out* :driver "pulse")

Now that we have a stable connection to the sound system, we can query it for possible output formats:

    (cl-out123:formats *out* '(44100) 1 2)

Finally once we have figured out a proper format to use, or again are fine with the default, we can start playback to the device:

    (cl-out123:start *out* :rate 44100 :channels 2 :encoding :signed-16)

Now buffered audio data that conforms to the format we picked can be sent to be played back using `play` or `play-directly`:

    (cl-out123:play *out* #(...))

It will return you the amount of bytes it actually managed to play back. If need be, playback can also be `pause`d, `resume`d, and `stop`ped. Currently buffered data can be `drop`ped, or `drain`ed too to allow you to synchronise things.

Once we're done, simply `stop` the output object:

    (cl-out123:stop *out*)

If for some reason you find yourself needing to change output format or device after having initialised your output already, you can reconfigure it using `reinitialize-instance`. This will cause your current playback to end if it is currently running, but should otherwise phase over smoothly.

    (reinitialize-instance *out* :driver "alsa")

There is no need to explicitly deallocate or clean up data. As soon as the output object is garbage collected, the cleanup will be handled for you automatically. If you would like to explicitly close the connection anyway:

    (cl-out123:disconnect *out*)

And that's pretty much all there is to it. The heavy burden falls onto you to get the audio data ready in the proper format for it to be played back. The actual transfer process to the sound system should be easy with this library.

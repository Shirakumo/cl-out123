(in-package #:org.shirakumo.fraf.out123)

;; low-level.lisp
(docs:define-docs
  (variable cl-out123-cffi::*here*
    "Variable containing the path to this very file, hopefully.")

  (variable cl-out123-cffi:*static*
    "Variable containing a pathname to the static directory.")
  )

;; conditions.lisp
(docs:define-docs
  (type output-error
    "Error condition superclass for all errors related to this library.

See OUTPUT")

  (function output
    "Returns the output object associated with the condition.

See OUTPUT-ERROR")

  (type creation-failure
    "Condition signalled in the case the allocation of the output handler fails.

See OUTPUT-ERROR")

  (type not-connected
    "Condition signalled if an operation is attempted that requires the output to be connected, but it isn't.

See OUTPUT-ERROR")

  (type failed-driver-listing
    "Condition signalled if the listing of available drivers fails for some reason.

See OUTPUT-ERROR")

  (type failed-format-listing
    "Condition signalled if the listing of available formats fails for some reason.

See OUTPUT-ERROR")

  (type already-connected
    "Condition signalled if an attempt is made to connect again while the output is still connected.

See OUTPUT-ERROR")

  (type error-string-error
    "Error condition superclass for errors that have an error-string from the out123 library.

See ERROR-STRING
See OUTPUT-ERROR")

  (function error-string
    "Returns the error string from the out123 library for the error we encountered.

See ERROR-STRING-ERROR")

  (type connection-failed
    "Condition signalled if the connection of an output to its device fails.

See ERROR-STRING-ERROR
See DRIVER
See DEVICE")

  (type start-failed
    "Condition signalled if starting the device fails.

See ERROR-STRING-ERROR
See RATE
See CHANNELS
See ENCODING")

  (type playback-failed
    "Condition signalled if the playback of a buffer fails.

See ERROR-STRING-ERROR
See BYTES")

  (type buffer-set-failed
    "Condition signalled if the setting of the background buffer fails.

See ERROR-STRING-ERROR
See BYTES"))

;; toolkit.lisp
(docs:define-docs
  (function with-foreign-values
    "Same as CFFI:WITH-FOREIGN-OBJECTS, but resolves the bindings at the end and returns them as values.")

  (function with-error
    "Checks the result of FORM against an error and if found, signals a condition according to the spec.")

  (function with-generic-error
    "Signals a simple-error if the result of FORM is an error.

See WITH-ERROR")

  (function device-default-name
    "Attempts to return a somewhat sensible name to use for our application."))

;; wrapper.lisp
(docs:define-docs
  (function dispose-handle
    "Properly disposes of the handle as quickly as possible.

Performs three steps if the handle is not a null-pointer:
Output is drained by DRAIN, the handle is CLOSEd, and finally DELeted.

See CL-OUT123-CFFI:DRAIN
See CL-OUT123-CFFI:CLOSE
See CL-OUT123-CFFI:DEL")

  (type output
    "Class representing an output to a sound playback device.

This holds all the necessary state and information in order to connect and
play back audio data to a device.

Some options can only be changed by closing and opening a new connection to
the backend. In order to do this, you can use REINITIALIZE-INSTANCE. It will
attempt to preserve state across reinitialisation, but pending sound data
will be disposed off immediately as to not block.

See HANDLE
See PLAYING
See CONNECTED
See DRIVER
See DEVICE
See RATE
See CHANNELS
See ENCODING
See OUTPUT-TO
See PRELOAD
See GAIN
See DEVICE-BUFFER
See NAME
See OUTPUT-LOCK")

  (function output-lock
    "Reader for the lock used to mutually exclude access to playback.

You should not need this unless you are working with the internals of the library.

See OUTPUT")

  (function handle
    "Returns the pointer to the actual handle object that the output object encapsulates.

You should not need this unless you are working with the internals of the library.

See OUTPUT")

  (function playing
    "Returns T if the output is currently playing audio.")

  (function connected
    "Returns T if the output is connected to a device.")

  (function driver
    "Returns the string naming the driver used to play back sound or NIL if unknown.

Can be set as an initarg on the output.

In order to get a listing of all possible drivers, see DRIVERS.

See OUTPUT
See DRIVERS
See CONNECT")

  (function device
    "Returns the string naming the device used to play back sound or NIL if unknown.

Can be set as an initarg on the output.

See OUTPUT
See CONNECT")

  (function rate
    "Returns the sampling rate the backend uses to process the buffer data.

Can be set as an initarg on the output.

See OUTPUT
See START")

  (function channels
    "Returns the number of audio channels the backend uses to process the buffer data.

Can be set as an initarg on the output.

See OUTPUT
See START")

  (function encoding
    "Returns the encoding the backend uses to decode the buffer data.

Can be set as an initarg on the output.

See ENCODINGS for a list of possible encodings.

See OUTPUT
See START
See ENCODINGS")

  (function framesize
    "Returns the framesize the backend uses to decode the buffer data.

This is determined automatically by the backend once the output
has STARTed.

See OUTPUT
See START")

  (function output-to
    "Returns a list of special flags that tell the backend (if possible) where to output to.

Can be set as an initarg on the output.

The flags in the list can be one of :HEADPHONES :INTERNAL-SPEAKER :LINE-OUT

See OUTPUT")

  (function preload
    "Returns the percentage of data that is preloaded into the device buffer.

Can be set as an initarg on the output.

Allowed values are:
T    for the default that the backend will choose for itself.
NIL  for no preloading, aka 0.0.
REAL for the approximate fraction that should be preloaded [0,1].

See OUTPUT")

  (function gain
    "Returns an integer representing the output device gain. This is driver-specific.

Can be set as an initarg on the output.

See OUTPUT")

  (function device-buffer
    "Returns the number of seconds that the device buffer should hold.

Can be set as an initarg on the output.

Allowed values are:
T    for the default that the backend will choose for itself. 
REAL for the approximate amount of seconds of buffering. Should
     not be more than 0.5.

See OUTPUT")

  (function name
    "Returns the name used to identify the output in the audio playback device, if permitted.

Can be set as an initarg on the otuput.

The default value is calculated by DEVICE-DEFAULT-NAME

See OUTPUT
See DEVICE-DEFAULT-NAME")

  (function drivers
    "Returns a list of drivers with their name and description.

You can use this to determine the available drivers and pick a suitable backend based on that.

See OUTPUT")

  (function driver-info
    "Returns the current driver and device the output uses, if any.

This might differ from DRIVER and DEVICE if the handle was not yet
CONNECTed, was connected by non-standard means, or something else
went wrong.

See OUTPUT
See CONNECT")

  (function check-connected
    "Signals an error if the output is not connected.

See OUTPUT
See CONNECTED
See NOT-CONNECTED")

  (function decode-encodings
    "Decodes the ORed together encodings into a list of keywords.")

  (function encodings
    "Returns a list of the possible encodings for the output rate and channel count you specified.

The output must be connected and will be stopped if it is playing.

See OTUPUT")

  (function formats
    "Returns a list of possible formats for the requested rates and channels.

RATES should be a list of integers.

The output must be connected and will be stopped if it is playing.

Each item in the list is a plist with :RATE, :CHANNELS, and :ENCODINGS as keys.")

  (function playback-format
    "Returns the current rate, channels, encoding, and framesize used by the output, if any.

This might differ from RATE, CHANNELS, ENCODING, and FRAMESIZE
if the handle has not yet been STARTed, was started by
non-standard means, or something else went wrong.

See OUTPUT
See START")

  (function connect
    "Connects the output to the requested driver and device.

The output must not be connected already.

If the connection was successful, the output's DRIVER and DEVICE
are updated to reflect the ones that were actually chosen by the
backend. This might differ from what you requested.

This function is safe to be called from multiple threads,
as it will mutually exclude them through a lock.

See OUTPUT
See DRIVER
See DEVICE
See CONNECTED")

  (function disconnect
    "Disconnects the output from its driver and device.

If the output is already disconnected, this does nothing.

If there is still audio data to be played on the buffer, then this
will block until it is finished. If you wish to abort immediately,

This function is safe to be called from multiple threads,
as it will mutually exclude them through a lock.
call DROP first.

See OUTPUT
See DROP
See CONNECTED")

  (function start
    "Starts playback to the connected output.

The output must be connected and must not have been started before.

If the start was successful, the output's RATE, CHANNELS, ENCODING,
and FRAMESIZE are updated to reflect the values that were actually
chosen by the backend. This might differ from what you requested.

This function is safe to be called from multiple threads,
as it will mutually exclude them through a lock.

See OUTPUT
See RATE
See CHANNELS
See ENCODING
See FRAMESIZE
See PLAYING")

  (function pause
    "Pauses playback.

This function is safe to be called from multiple threads,
as it will mutually exclude them through a lock.

See OUTPUT
See PLAYING")

  (function resume
    "Resumes playback.

This function is safe to be called from multiple threads,
as it will mutually exclude them through a lock.

See OUTPUT
See PLAYING")

  (function stop
    "Stops playback.

If there is still audio data to be played on the buffer, then this
will block until it is finished. If you wish to abort immediately,
call DROP first.

This function is safe to be called from multiple threads,
as it will mutually exclude them through a lock.

See OUTPUT
See PLAYING")

  (function with-playback
    "Ensures a clean playback environment for the body.

First calls START with the given options, then evaluates the body
forms in an unwind-protect that calls STOP on exit.

See START
See STOP")

  (function play-directly
    "Directly sends the given buffer to out123 to be played back on the output.

BUFFER must be a pointer to a foreign byte array of at least
BYTES size. Returns the number of bytes that were actually
played back.

This does not catch errors. If you need to check for errors,
see CL-OUT123-CFFI:ERRCODE.

This does not care for synchronisation or mutual exclusion.
If you call this simultaneously from multiple threads, things
/will/ crash and burn horribly.

See CL-OUT123-CFFI:ERRCODE
See HANDLE
See OUTPUT")

  (function play
    "Send the octet-vector or array-pointer to out123 to be played back on the output.

If you need low latency, then this is definitely not the
function for you. The buffer is converted into a foreign
byte array before being sent off.

Returns the number of bytes that were actually played back.

This function is safe to be called from multiple threads,
as it will mutually exclude them through a lock.

See PLAY-DIRECTLY
See OUTPUT")

  (function drop
    "Drops all remaining output from the internal buffers.

This function is safe to be called from multiple threads,
as it will mutually exclude them through a lock.

See OUTPUT")

  (function drain
    "Blocks until all output in the buffer has been played back.

Implicitly resumes playback if it is paused first and then
pauses it again once it finishes.

This function is safe to be called from multiple threads,
as it will mutually exclude them through a lock.

See PLAYING
See OUTPUT
See NDRAIN")

  (function ndrain
    "Blocks until either all or bytes number of bytes have been played back.

Implictly resumes playback if it is paused first and then
pauses it again if there are no remaining bytes to be played
back.

This function is safe to be called from multiple threads,
as it will mutually exclude them through a lock.

See PLAYING
See OUTPUT
See DRAIN")

  (function buffered
    "Returns the number of bytes that currently reside in the internal buffer.

This number changes constantly as audio is played back.

This is setfable. Once set, a fork occurs to spawn a
\"thread\" to process the buffer in in the background.
See the <out123.h> file for more information.

See OUTPUT"))

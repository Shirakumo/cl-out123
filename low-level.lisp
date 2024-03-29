(in-package #:org.shirakumo.fraf.out123.cffi)

(defvar *here* #.(or *compile-file-pathname* *load-pathname* *default-pathname-defaults*))
(defvar *static* (make-pathname :name NIL :type NIL :defaults (merge-pathnames "static/" *here*)))
(pushnew *static* cffi:*foreign-library-directories*)

#+windows
(let ((path (format NIL "~a;~a"
                    (uiop:native-namestring
                     (merge-pathnames #+x86 "win32" #+x86-64 "win64" *static*))
                    (uiop:getenv "PATH"))))
  #+sbcl (sb-posix:setenv "PATH" path 1)
  #+ccl (ccl:setenv "PATH" path T)
  #+ecl (ext:setenv "PATH" path))

(define-foreign-library libout123
  (:darwin (:or "libout123.dylib" "libout123.so"
                #+X86 "mac32-libout123.dylib"
                #+X86-64 "mac64-libout123.dylib"))
  (:unix (:or "libout123.so"
              #+X86 "lin32-libout123.so"
              #+X86-64 "lin64-libout123.so"))
  (:windows (:or "out123.dll"
                 #+X86 "win32-libout123.dll"
                 #+X86-64 "win64-libout123.dll"))
  (t (:default "out123")))

(use-foreign-library libout123)

;;; fmt123.h
(defcenum enc
  (:8           #x000F)
  (:16          #x0040)
  (:24          #x4000)
  (:32          #x0100)
  (:signed      #x0080)
  (:float-type  #x0E00)
  (:int16       #x00D0)                 ; 16 | signed | 0x10
  (:uint16      #x0060)                 ; 16 | 0x20
  (:uint8       #x0001)
  (:int8        #x0082)                 ; signed | 0x02
  (:ulaw8       #x0004)
  (:alaw8       #x0008)
  (:int32       #x1180)                 ; 32 | signed | 0x1000
  (:uint32      #x2100)                 ; 32 | 0x2000
  (:int24       #x5080)                 ; 24 | signed | 0x1000
  (:uint24      #x6000)                 ; 24 | 0x2000
  (:float       #x0200)
  (:double      #x0400)
  (:any         #x77FF))                ;  int16  | uint16 | uint8 | int8
                                        ;| ulaw-8 | alaw-8 | int32 | uint32
                                        ;| int24  | uint24 | float | double

(declaim (inline samplesize))
(defun samplesize (enc)
  (cond ((/= 0 (logand enc (foreign-enum-value 'enc :8)))
         1)
        ((/= 0 (logand enc (foreign-enum-value 'enc :16)))
         2)
        ((/= 0 (logand enc (foreign-enum-value 'enc :24)))
         3)
        ((or (/= 0 (logand enc (foreign-enum-value 'enc :32)))
             (= enc (foreign-enum-value 'enc :float)))
         4)
        ((= enc (foreign-enum-value 'enc :double))
         8)
        (T
         0)))

(defcstruct (fmt :class fmt :conc-name fmt-)
  (rate :long)
  (channels :int)
  (encoding :int))

;;; out123.h
(defctype size_t #+x86-64 :unsigned-long
                 #-x86-64 :unsigned-int)

(defcenum parms
  (:flags 1)
  :preload
  :gain
  :verbose
  :devicebuffer
  :propflags
  :name)

(defcenum flags
  (:headphones #x01)
  (:internal-speaker #x02)
  (:line-out #x04)
  (:quiet #x08)
  (:keep-playing #x10))

(defcenum propflags
  (:live #x01)
  (:persistent #x02))

(defcenum error
  (:err -1)
  (:ok 0)
  :doom
  :bad-driver-name
  :bad-driver
  :no-driver
  :not-live
  :dev-play
  :dev-open
  :buffer-error
  :module-error
  :arg-error
  :bad-param
  :set-no-param
  :bad-handle
  :errcount)

(defcstruct (handle :class handle))

(defcfun (new "out123_new") :pointer)

(defcfun (del "out123_del") :void
  (handle :pointer))

(defcfun (strerror "out123_strerror") :string
  (handle :pointer))

(defcfun (errcode "out123_errcode") error
  (handle :pointer))

(defcfun (plain-strerror "out123_plain_strerror") :string
  (errcode error))

(defcfun (set-buffer "out123_set_buffer") error
  (handle :pointer)
  (buffer-bytes size_t))

(defcfun (param "out123_param") error
  (handle :pointer)
  (code parms)
  (value :long)
  (fvalue :double)
  (svalue :string))

(declaim (inline param-int param-float param-string))
(defun param-int (handle code value)
  (param handle code value 0.0d0 (null-pointer)))

(defun param-float (handle code value)
  (param handle code 0 value (null-pointer)))

(defun param-string (handle code value)
  (param handle code 0 0.0d0 value))

(defcfun (getparam "out123_getparam") error
  (handle :pointer)
  (code parms)
  (ret-value (:pointer :long))
  (ret-fvalue (:pointer :double))
  (ret-svalue (:pointer :string)))

(declaim (inline getparam-int getparam-float getparam-string))
(defun getparam-int (handle code value)
  (getparam handle code value 0.0d0 (null-pointer)))

(defun getparam-float (handle code value)
  (getparam handle code 0 value (null-pointer)))

(defun getparam-string (handle code value)
  (getparam handle code 0 0.0d0 value))

(defcfun (param-from "out123_param_from") error
  (handle :pointer)
  (handle-from :pointer))

(defcfun (drivers "out123_drivers") :int
  (handle :pointer)
  (names (:pointer :string))
  (descr (:pointer :string)))

(defcfun (open "out123_open") error
  (handle :pointer)
  (driver :string)
  (device :string))

(defcfun (driver-info "out123_driver_info") error
  (handle :pointer)
  (driver (:pointer :string))
  (device (:pointer :string)))

(defcfun (close "out123_close") :void
  (handle :pointer))

(defcfun (encodings "out123_encodings") :int
  (handle :pointer)
  (rate :long)
  (channels :int))

(defcfun (encsize "out123_encsize") :int
  (encoding enc))

(defcfun (formats "out123_formats") :int
  (handle :pointer)
  (rates (:pointer :long))
  (ratecount :int)
  (minchannels :int)
  (maxchannels :int)
  (fmtlist :pointer))

(defcfun (enc-list "out123_enc_list") :int
  (enclist (:pointer (:pointer enc))))

(defcfun (enc-byname "out123_enc_byname") enc
  (name :string))

(defcfun (enc-name "out123_enc_name") :string
  (encoding enc))

(defcfun (enc-longname "out123_enc_longname") :string
  (encoding enc))

(defcfun (start "out123_start") error
  (handle :pointer)
  (rate :long)
  (channels :int)
  (encoding enc))

(defcfun (pause "out123_pause") :void
  (handle :pointer))

(defcfun (continue "out123_continue") :void
  (handle :pointer))

(defcfun (stop "out123_stop") :void
  (handle :pointer))

(defcfun (play "out123_play") size_t
  (handle :pointer)
  (buffer :pointer)
  (bytes size_t))

(defcfun (drop "out123_drop") :void
  (handle :pointer))

(defcfun (drain "out123_drain") :void
  (handle :pointer))

(defcfun (ndrain "out123_ndrain") :void
  (handle :pointer)
  (bytes size_t))

(defcfun (buffered "out123_buffered") size_t
  (handle :pointer))

(defcfun (getformat "out123_getformat") error
  (handle :pointer)
  (rate (:pointer :long))
  (channels (:pointer :int))
  (encoding (:pointer enc))
  (framesize (:pointer :int)))

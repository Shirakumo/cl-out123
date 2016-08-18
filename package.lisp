#|
 This file is a part of cl-out123
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:cl-out123-cffi
  (:nicknames #:org.shirakumo.fraf.out123.cffi)
  (:use #:cl #:cffi)
  ;; low-level.lisp
  (:shadow :open :close :continue)
  (:export
   #:*static*
   #:libout123
   #:enc
   #:smaplesize
   #:fmt
   #:fmt-rate
   #:fmt-channels
   #:fmt-encoding
   #:size_t
   #:handle
   #:parms
   #:flags
   #:propflags
   #:error
   #:new
   #:del
   #:strerror
   #:errcode
   #:plain-strerror
   #:set-buffer
   #:param
   #:param-int
   #:param-float
   #:param-string
   #:param-from
   #:getparam
   #:getparam-int
   #:getparam-float
   #:getparam-string
   #:getparam-from
   #:drivers
   #:open
   #:driver-info
   #:close
   #:encodings
   #:encsize
   #:formats
   #:enc-list
   #:enc-byname
   #:enc-name
   #:enc-longname
   #:start
   #:pause
   #:continue
   #:stop
   #:play
   #:drop
   #:drain
   #:ndrain
   #:buffered
   #:getformat))

(defpackage #:cl-out123
  (:nicknames #:org.shirakumo.fraf.out123)
  (:use #:cl)
  ;; wrapper.lisp
  (:export
   ))

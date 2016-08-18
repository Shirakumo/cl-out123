#|
 This file is a part of cl-out123
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.out123)

(defmacro with-foreign-values (bindings &body body)
  `(with-foreign-objects ,bindings
     ,@body
     (values ,@(loop for (name type) in bindings
                     collect `(mem-ref ,name ,type)))))

(defmacro with-error ((err datum &rest datum-args) &body form)
  `(let ((,err (progn ,@form)))
     (unless (eql ,err :ok)
       (let ((,err (cl-out123-cffi:plain-strerror ,err)))
         (error ,datum ,@datum-args)))))

(defmacro with-generic-error (form)
  (let ((err (gensym "ERR")))
    `(with-error (,err "~s failed: ~a" ',form ,err)
       ,form)))

(defun device-default-name ()
  (format NIL "~a (cl-out123)" (lisp-implementation-type)))

(defun dispose-handle (handle)
  (unless (null-pointer-p handle)
    (cl-out123-cffi:drain handle)
    (cl-out123-cffi:close handle)
    (cl-out123-cffi:del handle)))

(defclass output ()
  ((handle :initform NIL :reader handle)
   (playing :initform NIL :reader playing :writer set-playing)
   (connected :initform NIL :reader connected :writer set-connected)
   (driver :initarg :driver :reader driver)
   (device :initarg :device :reader device)
   (rate :initarg :rate :reader rate)
   (channels :initarg :channels :reader channels)
   (encoding :initarg :encoding :reader encoding)
   (output-to :initarg :output-to :reader output-to)
   (preload :initarg :preload :reader preload)
   (gain :initarg :gain :reader gain)
   (device-buffer :initarg :device-buffer :reader device-buffer)
   (name :initarg :name :reader name))
  (:default-initargs
   :driver NIL
   :device NIL
   :rate 44100
   :channels 2
   :encoding :unsigned-16
   :output-to ()
   :preload T
   :gain NIL
   :device-buffer T
   :name (device-default-name)))

(defmethod shared-initialize :after ((output output) slots &key output-to preload gain device-buffer name)
  (let ((handle (cl-out123-cffi:new)))
    (when (or (not handle) (null-pointer-p handle))
      (error "Failed to create output handle."))
    (tg:finalize output (lambda () (dispose-handle handle)))
    (setf (slot-value output 'handle) handle)
    (when output-to
      (let ((code 0))
        (dolist (flag output-to)
          (setf code (logior code (foreign-enum-value 'cl-out123-cffi:flags flag))))
        (cl-out123-cffi:param-int handle :flags code)))
    (etypecase preload
      (real (cl-out123-cffi:param-float handle :preload (float preload 0.0d0)))
      ((eql T))
      ((eql NIL) (cl-out123-cffi:param-float handle :preload 0.0d0)))
    (when gain (cl-out123-cffi:param-int handle :gain gain))
    (etypecase device-buffer
      (real (cl-out123-cffi:param-float handle :devicebuffer (float device-buffer 0.0d0)))
      ((eql T) (cl-out123-cffi:param-float handle :devicebuffer 0.0d0)))
    (when name
      (cl-out123-cffi:param-string handle :name name))))

(defmethod reinitialize-instance :around ((output output) &key)
  (dispose-handle (handle output))
  (call-next-method)
  (when (connected output)
    (connect output))
  (when (playing output)
    (start output)))

(defun drivers (output)
  (with-foreign-objects ((drivers :pointer) (descriptions :pointer))
    (let ((driverc (cl-out123-cffi:drivers (handle output) drivers descriptions)))
      ;; FIXME
      (when (<= driverc 0) (error "Failed to retrieve driver listing."))
      (let ((drivers (mem-ref drivers :pointer))
            (descriptions (mem-ref descriptions :pointer)))
        (unwind-protect
             (loop for i from 0 below driverc
                   collect (list (mem-aref drivers :string i)
                                 (mem-aref descriptions :string i))
                   do (foreign-free (mem-aref drivers :pointer i))
                      (foreign-free (mem-aref descriptions :pointer i)))
          (foreign-free drivers)
          (foreign-free descriptions))))))

(defun driver-info (output)
  (with-foreign-values ((driver :string) (device :string))
    (with-generic-error (cl-out123-cffi:driver-info (handle output) driver device))))

(defun check-connected (output)
  ;; FIXME
  (unless (connected output) (error "~a has not yet been connected!" output)))

(defun decode-encodings (encs)
  (loop for enc in (foreign-enum-keyword-list 'cl-out123-cffi:enc)
        when (/= 0 (logand (foreign-enum-value 'cl-out123-cffi:enc enc) encs))
        collect (list enc (cl-out123-cffi:encsize enc))))

(defun encodings (output rate channels)
  (check-connected output)
  (set-playing NIL output)
  (let ((encs (cl-out123-cffi:encodings (handle output) rate channels)))
    (when (= -1 encs) (error "Failed to retrieve encoding listing."))
    (decode-encodings encs)))

(defun formats (output rates min-channels max-channels)
  (check-connected output)
  (set-playing NIL output)
  (with-foreign-objects ((frates :long (length rates)) (formats :pointer))
    (loop for i from 0 for rate in rates
          do (setf (mem-aref frates :long) rate))
    (let* ((formatc (cl-out123-cffi:formats (handle output) frates (length rates) min-channels max-channels formats))
           (formats (mem-ref formats :pointer)))
      (when (= -1 formatc) (error "Failed to retrieve format listing."))
      (unwind-protect
           (loop for i from 0 below formatc
                 for fmt = (mem-aref formats :pointer i)
                 when (< 0 (cl-out123-cffi:fmt-encoding fmt))
                 collect (list :rate (cl-out123-cffi:fmt-rate fmt)
                               :channels (cl-out123-cffi:fmt-channels fmt)
                               :encoding (decode-encodings (cl-out123-cffi:fmt-encoding fmt)))
                 do (foreign-free fmt))
        (foreign-free formats)))))

(defun playback-format (output)
  (with-foreign-values ((rate :long) (channels :int) (encoding 'cl-out123-cffi:enc) (framesize :int))
    (with-generic-error (cl-out123-cffi:getformat (handle output) rate channels encoding framesize))))

(defun connect (output &key (driver (driver output))
                            (device (device output)))
  (with-generic-error (cl-out123-cffi:open (handle output) driver device))
  (set-connected T output)
  output)

(defun disconnect (output)
  (cl-out123-cffi:close (handle output))
  (set-connected NIL output)
  output)

(defun start (output &key (rate (rate output))
                          (channels (channels output))
                          (encoding (encoding output)))
  (check-connected output)
  (with-generic-error (cl-out123-cffi:start (handle output) rate channels encoding))
  (set-playing T output)
  (multiple-value-bind (rate channels encoding framesize) (playback-format output)
    (setf (slot-value output 'rate) rate)
    (setf (slot-value output 'channels) channels)
    (setf (slot-value output 'encoding) encoding)
    (setf (slot-value output 'framesize) framesize))
  output)

(defun pause (output)
  (cl-out123-cffi:pause (handle output))
  (set-playing NIL output)
  output)

(defun resume (output)
  (cl-out123-cffi:continue (handle output))
  (set-playing T output)
  output)

(defun stop (output)
  (cl-out123-cffi:stop (handle output))
  (set-playing NIL output)
  output)

(declaim (inline play-directly))
(defun play-directly (output buffer bytes)
  (cl-out123-cffi:play (handle output) buffer bytes))

(defun play (output bytes)
  (prog1
      (with-foreign-array (arr bytes :char)
        (play-directly output arr bytes))
    (with-generic-error (cl-out123-cffi:errcode (handle output)))))

(defun drop (output)
  (cl-out123-cffi:drop (handle output))
  output)

(defun drain (output)
  (set-playing T output)
  (cl-out123-cffi:drain (handle output))
  (set-playing NIL output)
  output)

(defun ndrain (output bytes)
  (set-playing T output)
  (cl-out123-cffi:ndrain (handle output) bytes)
  (when (= 0 (buffered output))
    (set-playing NIL output))
  output)

(defun buffered (output)
  (cl-out123-cffi:buffered (handle output)))

(defun (setf buffered) (bytes output)
  (with-generic-error (cl-out123-cffi:set-buffer (handle output) bytes)))

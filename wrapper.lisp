(in-package #:org.shirakumo.fraf.out123)

(defun dispose-handle (handle thread &key (delete T))
  (unless (null-pointer-p handle)
    (flet ((close-properly ()
             (cl-out123-cffi:drop handle)
             (cl-out123-cffi:close handle)
             (when delete
               (cl-out123-cffi:del handle))))
      (if (eql (bt:current-thread) thread)
          (close-properly)
          (bt:interrupt-thread thread #'close-properly)))))

(defun make-output (driver &rest args &key &allow-other-keys)
  (apply #'make-instance 'output :driver driver args))

(defclass output ()
  ((handle :initform NIL :reader handle)
   (playing :initform NIL :reader playing :writer set-playing)
   (connected :initform NIL :reader connected :writer set-connected)
   (driver :initarg :driver :reader driver)
   (device :initarg :device :reader device)
   (rate :initarg :rate :reader rate)
   (channels :initarg :channels :reader channels)
   (encoding :initarg :encoding :reader encoding)
   (framesize :initform NIL :reader framesize)
   (output-to :initarg :output-to :reader output-to)
   (preload :initarg :preload :reader preload)
   (gain :initarg :gain :reader gain)
   (device-buffer :initarg :device-buffer :reader device-buffer)
   (name :initarg :name :reader name)
   (cthread :initform (cons (bt:current-thread) NIL) :reader cthread)
   (playback-lock :initform (bt:make-lock "output lock") :reader playback-lock))
  (:default-initargs
   :driver NIL
   :device NIL
   :rate 44100
   :channels 2
   :encoding :int16
   :output-to ()
   :preload T
   :gain NIL
   :device-buffer T
   :name (device-default-name)))

(defmethod print-object ((output output) stream)
  (print-unreadable-object (output stream :type T)
    (format stream "~s~:[~*~*~; ~a/~a~]~:[~; :PLAYING~]"
            (name output) (connected output) (driver output) (device output) (playing output))))

(defmethod shared-initialize :after ((output output) slots &key)
  (with-slots (output-to preload gain device-buffer name) output
    (let ((handle (cl-out123-cffi:new)))
      (when (or (not handle) (null-pointer-p handle))
        (error 'creation-failure :output output))
      (let ((cthread (cthread output)))
        (tg:finalize output (lambda () (dispose-handle handle (car cthread)))))
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
        ((eql T) (cl-out123-cffi:param-float handle :devicebuffer 0.0d0))
        ((eql NIL)))
      (when name
        (cl-out123-cffi:param-string handle :name name)))))

(defmethod reinitialize-instance :around ((output output) &key)
  ;; Make sure that our finalizer cannot accidentally try to
  ;; dispose of an already disposed handle later...
  (tg:cancel-finalization output)
  (dispose-handle (handle output) (car (cthread output)))
  (call-next-method)
  (when (connected output)
    (set-connected NIL output)
    (connect output))
  (when (playing output)
    (start output))
  output)

(defun drivers (output)
  (with-foreign-objects ((drivers :pointer) (descriptions :pointer))
    (let ((driverc (cl-out123-cffi:drivers (handle output) drivers descriptions)))
      (when (<= driverc 0)
        (error 'failed-driver-listing :output output))
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
    (with-error (err 'failed-driver-info :output output :error err)
      (cl-out123-cffi:driver-info (handle output) driver device))))

(defun check-connected (output)
  (unless (connected output)
    (error 'not-connected :output output)))

(defun decode-encodings (encs)
  (loop for enc in '(:int32 :int24 :int16 :int8 :uint32 :uint24 :uint16 :uint8 :ulaw8 :alaw8 :float :double)
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
  (assert (<= 0 min-channels max-channels) ())
  (set-playing NIL output)
  (with-foreign-objects ((frates :long (length rates)) (formats :pointer))
    (loop for i from 0 for rate in rates
          do (setf (mem-aref frates :long) rate))
    (let* ((formatc (cl-out123-cffi:formats (handle output) frates (length rates) min-channels max-channels formats))
           (formats (mem-ref formats :pointer)))
      (when (= -1 formatc)
        (error 'failed-format-listing :output output))
      (unwind-protect
           (loop for i from 0 below formatc
                 for fmt = (mem-aptr formats :pointer i)
                 when (< 0 (cl-out123-cffi:fmt-encoding fmt))
                 collect (list :rate (cl-out123-cffi:fmt-rate fmt)
                               :channels (cl-out123-cffi:fmt-channels fmt)
                               :encodings (decode-encodings (cl-out123-cffi:fmt-encoding fmt))))
        (foreign-free formats)))))

(defun playback-format (output)
  (with-foreign-values ((rate :long) (channels :int) (encoding 'cl-out123-cffi:enc) (framesize :int))
    (with-error (err 'failed-playback-format :output output :error err)
      (cl-out123-cffi:getformat (handle output) rate channels encoding framesize))))

(defun connect (output &key (driver (driver output))
                            (device (device output)))
  (bt:with-lock-held ((playback-lock output))
    (when (connected output)
      (error 'already-connected :output output))
    (with-error (err 'connection-failed :output output :error err :driver driver :device device)
      (cl-out123-cffi:open (handle output)
                           (or driver (null-pointer))
                           (or device (null-pointer))))
    (setf (car (cthread output)) (bt:current-thread))
    (set-connected T output)
    (multiple-value-bind (driver device) (driver-info output)
      (setf (slot-value output 'driver) driver)
      (setf (slot-value output 'device) device)))
  output)

(defun disconnect (output)
  (bt:with-lock-held ((playback-lock output))
    (when (connected output)
      (dispose-handle (handle output) (car (cthread output)) :delete NIL)
      (set-connected NIL output)
      (set-playing NIL output)))
  output)

(defun start (output &key (rate (rate output))
                          (channels (channels output))
                          (encoding (encoding output)))
  (bt:with-lock-held ((playback-lock output))
    (check-connected output)
    (with-error (err 'start-failed :output output :error err :rate rate :channels channels :encoding encoding)
      (cl-out123-cffi:start (handle output) rate channels encoding))
    (set-playing T output)
    (multiple-value-bind (rate channels encoding framesize) (playback-format output)
      (setf (slot-value output 'rate) rate)
      (setf (slot-value output 'channels) channels)
      (setf (slot-value output 'encoding) encoding)
      (setf (slot-value output 'framesize) framesize)))
  output)

(defun pause (output)
  (bt:with-lock-held ((playback-lock output))
    (when (playing output)
      (cl-out123-cffi:pause (handle output))
      (set-playing NIL output)))
  output)

(defun resume (output)
  (bt:with-lock-held ((playback-lock output))
    (unless (playing output)
      (cl-out123-cffi:continue (handle output))
      (set-playing T output)))
  output)

(defun stop (output)
  (bt:with-lock-held ((playback-lock output))
    (cl-out123-cffi:stop (handle output))
    (set-playing NIL output))
  output)

(defmacro with-playback ((output &key (rate `(rate ,output))
                                              (channels `(channels ,output))
                                              (encoding `(encoding ,output))) &body body)
  `(progn (start ,output :rate ,rate :channels ,channels :encoding ,encoding)
          (unwind-protect
               (progn ,@body)
            (stop ,output))))

(declaim (inline play-directly))
(defun play-directly (output buffer bytes)
  (#+sbcl sb-sys:without-interrupts
   #+ccl ccl:without-interrupts
   #+ecl mp:without-interrupts
   #+cmucl sys:without-interrupts
   #+lispworks lw:without-interrupts
   #+allegro excl:without-interrupts
   #-(or sbcl ccl ecl cmucl lispworks allegro) progn
    (cl-out123-cffi:play (handle output) buffer bytes)))

(defun play (output bytes &optional (count (length bytes)))
  (bt:with-lock-held ((playback-lock output))
    (prog1
        (etypecase bytes
          (foreign-pointer
           (play-directly output bytes count))
          ((vector (unsigned-byte 8))
           (with-foreign-array (arr bytes `(:array :unsigned-char ,count))
             (play-directly output arr count)))
          ((vector single-float)
           (with-foreign-array (arr bytes `(:array :float ,count))
             (play-directly output arr (* count (cffi:foreign-type-size :float)))))
          ((vector double-float)
           (with-foreign-array (arr bytes `(:array :double ,count))
             (play-directly output arr (* count (cffi:foreign-type-size :double))))))
      (with-error (err 'playback-failed :output output :error err :bytes bytes)
        (cl-out123-cffi:errcode (handle output))))))

(defun drop (output)
  (bt:with-lock-held ((playback-lock output))
    (cl-out123-cffi:drop (handle output)))
  output)

(defun drain (output)
  (bt:with-lock-held ((playback-lock output))
    (set-playing T output)
    (cl-out123-cffi:drain (handle output))
    (set-playing NIL output))
  output)

(defun ndrain (output bytes)
  (bt:with-lock-held ((playback-lock output))
    (set-playing T output)
    (cl-out123-cffi:ndrain (handle output) bytes)
    (when (= 0 (buffered output))
      (set-playing NIL output)))
  output)

(defun buffered (output)
  (cl-out123-cffi:buffered (handle output)))

(defun (setf buffered) (bytes output)
  (with-error (err 'buffer-set-failed :output output :error err :bytes bytes)
    (cl-out123-cffi:set-buffer (handle output) bytes)))

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
                     collect `(unless (null-pointer-p ,name) (mem-ref ,name ,type))))))

(defmacro with-error ((err datum &rest datum-args) &body form)
  `(let ((,err (progn ,@form)))
     (unless (eql ,err :ok)
       (let ((,err (if (eql ,err :err)
                       "Unknown error."
                       (cl-out123-cffi:plain-strerror ,err))))
         (error ,datum ,@datum-args)))))

(defmacro with-generic-error (form)
  (let ((err (gensym "ERR")))
    `(with-error (,err "~s failed: ~a" ',form ,err)
       ,form)))

(defun device-default-name (&optional (ident (package-name *package*)))
  (format NIL "~a ~a" (lisp-implementation-type) ident))

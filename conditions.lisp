#|
 This file is a part of cl-out123
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.out123)

(define-condition output-error (error)
  ((output :initarg :output :reader output)))

(define-condition creation-failure (output-error)
  ()
  (:report (lambda (c s) (format s "Failed to create output handle for ~a." (output c)))))

(define-condition not-connected (output-error)
  ()
  (:report (lambda (c s) (format s "Attempted to perform an operation that requires a connected output on ~a." (output c)))))

(define-condition failed-driver-listing (output-error)
  ()
  (:report (lambda (c s) (format s "Failed to list available drivers for ~a." (output c)))))

(define-condition failed-format-listing (output-error)
  ()
  (:report (lambda (c s) (format s "Failed to list available fromats for ~a." (output c)))))

(define-condition already-connected (output-error)
  ()
  (:report (lambda (c s) (format s "The output ~a is already connected." (output c)))))

(define-condition error-string-error (output-error)
  ((error :initarg :error :reader error-string)))

(define-condition connection-failed (error-string-error)
  ((driver :initarg :driver :reader driver)
   (device :initarg :device :reader device))
  (:report (lambda (c s) (format s "~a failed to connect to ~a/~a: ~a"
                                 (output c) (driver c) (device c) (error-string c)))))

(define-condition start-failed (error-string-error)
  ((rate :initarg :rate :reader rate)
   (channels :initarg :channels :reader channels)
   (encoding :initarg :encoding :reader encoding))
  (:report (lambda (c s) (format s "~a failed to start playback with ~a Hz, ~a c, ~a: ~a"
                                 (output c) (rate c) (channels c) (encoding c) (error-string c)))))

(define-condition playback-failed (error-string-error)
  ((bytes :initarg :bytes :reader bytes))
  (:report (lambda (c s) (format s "~a failed to play back: ~a"
                                 (output c) (error-string c)))))

(define-condition buffer-set-failed (error-string-error)
  ((bytes :initarg :bytes :reader bytes))
  (:report (lambda (c s) (format s "~a failed to set buffer to ~a bytes: ~a"
                                 (output c) (bytes c) (error-string c)))))

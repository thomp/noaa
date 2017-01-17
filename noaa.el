;;; noaa.el --- Get NOAA weather data -*- lexical-binding: t -*-

;; Copyright (C) 2017 David Thompson
;; Author: David Thompson
;; Version: 0.1
;; Keywords: NOAA, weather, HTTP
;; URL: https://github.com/thomp/************************
;; Package-Requires: ((request "0.2.0") (cl-lib "0.5") (emacs "24")) 

;;; Commentary:

;; This package provides a way to view an NOAA weather
;; forecast for a specific geographic location.

;;; Code:

(require 'cl)
(require 'json)
(require 'request)

;; edit latitude and longitude to correspond to point of interest
(defvar noaa-latitude 36.7478)

(defvar noaa-longitude -119.771)

(defvar noaa-buffer-spec "*noaa.el*"
  "Buffer or buffer name.")

(defun noaa ()
  (interactive)
  (noaa-url-retrieve (noaa-url noaa-latitude noaa-longitude)))

(defun noaa-aval (alist key)
  (let ((pair (assoc key alist)))
    (if pair
	(cdr pair)
      nil)))

(defun noaa-handle-noaa-result (result)
  (switch-to-buffer noaa-buffer-spec)
  ;; retrieve-fn accepts two arguments: a key-value store and a key
  ;; retireve-fn returns the corresponding value
  (let ((retrieve-fn 'noaa-aval))
    (let ((properties (funcall retrieve-fn result 'properties)))
      (if (not properties)
	  (message "Couldn't find properties. The NOAA API spec may have changed.")
	(let ((periods (funcall retrieve-fn properties 'periods))
	      ;; LAST-DAY-NUMBER is used for aesthetics --> separate data by day
	      (last-day-number -1)
	      (day-field-width 16)
	      (temp-field-width 5)
	      (forecast-field-width 40))
	 (erase-buffer)
	 (dotimes (i (length periods))
	   (let ((period (elt periods i)))
	     (let ((start-time (funcall retrieve-fn period 'startTime)))
	       (let ((day-number (noaa-iso8601-to-day start-time))
		     (name (funcall retrieve-fn period 'name))
		     (temp (funcall retrieve-fn period 'temperature))
		     (short-forecast (funcall retrieve-fn period 'shortForecast)))
		 ;; simple output w/some alignment
		 (progn (if (not (= last-day-number day-number))
			    (newline))
			(insert (format "%s" name))
			(move-to-column day-field-width t)
			(insert (format "%s" temp))
			(move-to-column (+ day-field-width temp-field-width) t)
			(insert (format "%s" short-forecast))
			(newline)) 
		 (setq last-day-number day-number)))))
	 (beginning-of-buffer))))))

;; utility function to handle ISO8601 times (emacs built-ins aren't there yet -- leaning on date is non-portable but works nicely for linux systems)
(defun noaa-iso8601-to-day (iso8601-string)
  (elt (parse-time-string (shell-command-to-string (format "date -d %s --iso-8601=date" iso8601-string))) 3))

(defun noaa-quit ()
  (interactive)
  (kill-buffer noaa-buffer-spec))

(defun noaa-url (&optional latitude longitude)
  "Return a string representing a URL."
  (format "https://api.weather.gov/points/%s,%s/forecast" (or latitude noaa-latitude) (or longitude noaa-longitude)))

(defun noaa-url-retrieve (url)
  "Return the buffer containing only the 'raw' body of the HTTP response. Call CALLBACK with the buffer as a single argument."
  (noaa-url-retrieve-tkf-emacs-request url))

;; async version relying on tfk emacs-request library
(defun noaa-url-retrieve-tkf-emacs-request (&optional url)
  (interactive)
  (request (noaa-url noaa-latitude noaa-longitude)
	   :parser 'buffer-string ;'json-read
	   :error (function*
		   (lambda (&key data error-thrown response symbol-status &allow-other-keys)
		     (message "data: %S " data) 
		     (message "symbol-status: %S " symbol-status)
		     (message "E Error response: %S " error-thrown)
		     (message "response: %S " response)))
	   :status-code '((500 . (lambda (&rest _) (message "Got 500 -- the NOAA server seems to be unhappy"))))
	   :success 'http-callback))

(cl-defun http-callback (&key data response error-thrown &allow-other-keys)
  (let ((noaa-buffer (get-buffer-create noaa-buffer-spec)))
    (switch-to-buffer noaa-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (and error-thrown (message (error-message-string error-thrown)))
      (let* ((ctype-header (request-response-header response "content-type"))
	     (ctype-list (and ctype-header (rfc2231-parse-string ctype-header)))
	     (charset (cdr (assq 'charset (cdr ctype-list))))
	     (coding-system (and charset (intern (downcase charset))))
	     (ctype-name (car ctype-list))
	     ;(guessed-mode (assoc-default ctype-name http-content-type-mode-alist))
	     ;(pretty-callback (assoc-default ctype-name http-pretty-callback-alist))
	     )
	;(message "Setting noaa-result")
	;(setq noaa-result data)
	;(message "Done setting noaa-result")
	;(noaa-insert data)
	;(message "1234")
	
	;; (when (stringp data)
	;;   (setq data (decode-coding-string data (or coding-system 'utf-8)))
	;;   (let ((text data)
	;; 	 ;(fontified (http-fontify-text text guessed-mode))
	;; 	 )
	;;     (insert text)))
	)
      ;; (when http-show-response-headers
      ;; 	(goto-char (if http-show-response-headers-top (point-min) (point-max)))
      ;; 	(or http-show-response-headers-top (insert "\n"))
      ;; 	(let ((hstart (point))
      ;; 	      (raw-header (request-response--raw-header response)))
      ;; 	  (unless (string-empty-p raw-header)
      ;; 	    (insert raw-header)
      ;; 	    (let ((comment-start (or comment-start http-fallback-comment-start)))
      ;; 	      (comment-region hstart (point)))
      ;; 	    (put-text-property hstart (point) 'face 'font-lock-comment-face))))

      ;(http-response-mode)
      )
    (goto-char (point-min))
    (let ((result (json-read)))
      ;(message "Got result %S " result)
      ;(setq noaa-result result)
      ;(message "Done setting result")
      (noaa-handle-noaa-result result)
      (noaa-mode)
      )
    ;(display-buffer noaa-buffer)
    ))

(defun noaa-parse-json-in-buffer ()
  "Parse and return the JSON object present in the noaa.el buffer."
  (switch-to-buffer noaa-buffer-spec)
  (json-read))

(defun noaa-insert (x)
  (switch-to-buffer noaa-buffer-spec)
  (insert x))


;;
;; noaa mode
;;

;;;###autoload
(define-derived-mode noaa-mode text-mode "noaa"
  "Major mode for displaying NOAA weather data
\\{noaa-mode-map}
"
  )

(defvar noaa-mode-map (make-sparse-keymap)
  "Keymap for `noaa-mode'.")

(define-key noaa-mode-map (kbd "q") 'noaa-quit)

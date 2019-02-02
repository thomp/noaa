;;; noaa.el --- Get NOAA weather data -*- lexical-binding: t -*-

;; Copyright (C) 2017,2018 David Thompson
;; Author: David Thompson
;; Version: 0.1
;; Keywords:
;; Homepage: https://github.com/thomp/noaa
;; URL: https://github.com/thomp/noaa
;; Package-Requires: ((request "0.2.0") (cl-lib "0.5") (emacs "24") (dash "2.14.1"))

;;; Commentary:

;; This package provides a way to view an NOAA weather
;; forecast for a specific geographic location.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'json)
(require 'request)

(defgroup noaa ()
  "View an NOAA weather forecast for a specific geographic location."
  :group 'external)

(defcustom noaa-latitude 36.7478
  "The latitude corresponding to the location of interest."
  :group 'noaa
  :type '(number))

(defcustom noaa-longitude -119.771
  "The latitude corresponding to the location of interest."
  :group 'noaa
  :type '(number))

(defvar noaa-buffer-spec "*noaa.el*"
  "Buffer or buffer name.")

(defvar noaa-display-styles '(default extended terse)
  "List of symbols indicating the various manners in which forecast data can be presented. The first member of the list is the currently active style.")

(defface noaa-face-date '((t (:foreground "#30c2ba")))
  "Face used for date.")

(defface noaa-face-short-forecast '((t (:foreground "grey")))
  "Face used for short forecast text.")

(defface noaa-face-temp '((t (:foreground "#cfd400")))
  "Face used for temperature.")

;; Forecast data for a specified time range
(defstruct noaa-forecast
  start-time
  end-time
  day-number
  detailed-forecast
  short-forecast
  name
  temp
  temp-trend
  temp-unit
  wind-speed
  wind-direction)

;; FORECASTS is a list of noaa-forecast structs. TYPE is a keyword (e.g., :daily, :hourly) providing guidance on how to treat the forecasts in the FORECAST-SET slot.
(defstruct noaa-forecast-set
  forecasts
  type)

(defvar noaa-last-forecast-set
  nil
  "A NOAA-FORECAST-SET struct describing the last forecast retrieved.")

(defvar noaa-last-forecast-raw
  nil
  "The server response associated with the last forecast request.")

;;;###autoload
(defun noaa ()
  "Request weather forecast data. Display the data in the buffer specified by ‘noaa-buffer-spec’."
  (interactive)
  ;; Honor CALENDAR- values if NOAA-LATITUDE and NOAA-LONGITUDE have
  ;; not been specified
  (when (not (and (numberp noaa-latitude)
		  (numberp noaa-longitude)))
    (when (and (numberp calendar-latitude)
	       (numberp calendar-longitude))
      (message "Using CALENDAR-LATITUDE and CALENDAR-LATITUDE values")
      (setf noaa-latitude calendar-latitude
	    noaa-longitude calendar-longitude)))
  (cond ((and (numberp noaa-latitude)
	      (numberp noaa-longitude))
	 (noaa-url-retrieve (noaa-url noaa-latitude noaa-longitude nil)
			    (function noaa-http-callback-daily)))
	(t
	 (message "To use NOAA, first set NOAA-LATITUDE and NOAA-LONGITUDE."))))

(defun noaa-aval (alist key)
  "Utility function to retrieve value associated with key KEY in alist ALIST."
  (let ((pair (assoc key alist)))
    (if pair
	(cdr pair)
      nil)))

(defun noaa-display-last-forecast ()
  "Clear the buffer specified by NOAA-BUFFER-SPEC and display the
forecast described by the value of NOAA-LAST-FORECAST-SET."
  (interactive)
  (erase-buffer)
  (cond ((eq (noaa-forecast-set-type noaa-last-forecast-set) :hourly)	      ;(noaa-forecast-hourly-p noaa-last-forecast-set)
	 (noaa-display-as-hourly noaa-last-forecast-set))
	(t
	 ;; if not hourly, assume daily
	 (noaa-display-last-forecast-as-daily)))
  (beginning-of-buffer))

(defun noaa-display-last-forecast-as-daily ()
  "A helper function for NOAA-DISPLAY-LAST-FORECAST."
  (let (
	;; LAST-DAY-NUMBER is used for aesthetics --> separate data by day
	(last-day-number -1)
	(day-field-width 16)
	(temp-field-width 5)
	(forecast-length (length (noaa-forecast-set-forecasts noaa-last-forecast-set))))
    (dotimes (index forecast-length)
      (let ((day-forecast (elt (noaa-forecast-set-forecasts noaa-last-forecast-set) index)))
	(noaa-insert-day-forecast
	 day-forecast
	 (and (noaa-forecast-day-number day-forecast)
	      last-day-number
	      (= (noaa-forecast-day-number day-forecast)
		 last-day-number)))
	(setq last-day-number (noaa-forecast-day-number day-forecast))))))

(defun noaa-display-as-hourly (forecast-set)
  "Insert the hourly forecast described by FORECAST-SET into the current buffer. A helper function for NOAA-DISPLAY-LAST-FORECAST."
  (let ((forecast-length (length (noaa-forecast-set-forecasts forecast-set)))
	;; Desirable to identify days
	;; - but name may not be defined for points in an hourly forecast (name field may be "")
	(name nil)
	;; - can try tracking using calendar day
	(day-of-month )
	)
    (dotimes (index forecast-length)
      (let ((forecast (elt (noaa-forecast-set-forecasts forecast-set) index)))
	(let ((this-forecast-name (noaa-forecast-name forecast)))
	  ;; dtk-empty-sequence-p
	  (when (or (not this-forecast-name)
		    (= 0 (length this-forecast-name)))
	    (setf this-forecast-name (noaa-iso8601-to-day-name (noaa-forecast-start-time forecast))))
	  (noaa-insert-hour-forecast
	   forecast
	   (cond ((not (equalp name this-forecast-name))
		  ;; day changed
		  (setf name this-forecast-name)
		  ;; ensure name is set in the forecast struct
		  (setf (noaa-forecast-name (elt (noaa-forecast-set-forecasts forecast-set) index))
			this-forecast-name)
		  t)
		 (t nil))
	   ))))))

(defun noaa-forecast-hourly-p (forecast)
  "Return T if the set of forecast structs described by FORECAST seems to represent an hourly forecast."
  (< (noaa-forecast-range forecast)
     (* 2 24 60 60) 			;172800
     ))
(defun noaa-forecast-range (forecast)
  "Return difference, in sec, between earliest start time and latest end time in the set of forecast structs described by FORECAST."
  (- (apply 'max (noaa-forecast-ends forecast))
     (apply 'min (noaa-forecast-starts forecast))))
(defun noaa-forecast-ends (forecast)
  "Return a list of end times corresponding to points in FORECAST."
  (mapcar #'(lambda (forecast-point)
	      (noaa-iso8601-to-seconds (noaa-forecast-end-time forecast-point)))
	  forecast))
(defun noaa-forecast-starts (forecast)
  "Return a list of start times corresponding to points in FORECAST."
  (mapcar #'(lambda (forecast-point)
	      (noaa-iso8601-to-seconds (noaa-forecast-start-time forecast-point)))
	  forecast))

(defun noaa-hourly ()
  "Retrieve and display the hourly forecast."
  (interactive)
  (noaa-url-retrieve (noaa-url noaa-latitude noaa-longitude t)
		     (function noaa-http-callback-hourly))
  (noaa-display-last-forecast))

(defun noaa-insert-day-forecast (noaa-forecast last-day-p)
  "Insert the forecast text for the forecast described by NOAA-FORECAST into the current buffer. A helper function for NOAA-DISPLAY-LAST-FORECAST-AS-DAILY."
  (let ((style (first noaa-display-styles)))
    (cond ((eq style 'terse)
	   (unless last-day-p
	     (insert (propertize (format "%s "
					 (s-truncate 3
						     (noaa-forecast-name noaa-forecast)
						     ""))
				 'face 'noaa-face-date)))
	   (insert (propertize (format "%s " (noaa-forecast-temp noaa-forecast))
			       'face 'noaa-face-temp)))
	  ((eq style 'default)
	   (let ((day-field-width 16)
		 (temp-field-width 5))
	     ;; simple output w/some alignment
	     (unless last-day-p
	       (newline))
	     (insert (propertize (format "%s" (noaa-forecast-name noaa-forecast)) 'face 'noaa-face-date))
	     (move-to-column day-field-width t)
	     (insert (propertize (format "% s" (noaa-forecast-temp noaa-forecast)) 'face 'noaa-face-temp))
	     (move-to-column (+ day-field-width temp-field-width) t)
	     (insert (propertize (format "%s" (noaa-forecast-short-forecast noaa-forecast)) 'face 'noaa-face-short-forecast))
	     (newline)))
	  ((eq style 'extended)
	   (let ((day-field-width 16)
		 (temp-field-width 5))
	     (insert (propertize (format "%s" (noaa-forecast-name noaa-forecast)) 'face 'noaa-face-date))
	     (move-to-column day-field-width t)
	     (insert (propertize (format "% s" (noaa-forecast-temp noaa-forecast)) 'face 'noaa-face-temp))
	     (newline) (newline)
	     (insert (propertize (format "%s" (noaa-forecast-detailed-forecast noaa-forecast)) 'face 'noaa-face-short-forecast))
	     (newline) (newline)))
	  (t
	   (error "Unrecognized style")))))

;; If WITH-DAY-P is true, depending on style, provide an indication of the day (e.g., name of day or calendar date).
(defun noaa-insert-hour-forecast (noaa-forecast with-day-p)
  "Insert the forecast described by NOAA-FORECAST into the current buffer. A helper function for NOAA-DISPLAY-AS-HOURLY."
  (when with-day-p
    (newline)
    (insert
     (propertize (noaa-forecast-name noaa-forecast)
		 'face 'noaa-face-date))
    (newline))
  (let ((style (first noaa-display-styles)))
    (cond ((eq style 'terse)
	   ;; s-truncate is convenient since it accomodates string of length less than truncate value
	   (insert (propertize (noaa-iso8601-to-hour-min (noaa-forecast-start-time noaa-forecast)) 'face 'noaa-face-date))
	   (insert " -" ?\x0020)
	   (insert (propertize (format "%s " (noaa-forecast-temp noaa-forecast))
			       'face 'noaa-face-temp)))
	  ((eq style 'default)
	   (noaa-insert-hour-forecast-default noaa-forecast with-day-p))
	  ((eq style 'extended)
	   ;; until a different "extended" style is developed...
	   (noaa-insert-hour-forecast-default noaa-forecast with-day-p))
	  (t
	   (error "Unrecognized style")))))

(defun noaa-insert-hour-forecast-default (noaa-forecast with-day-p)
  (let ((hour-field-end-col 7)
	(temp-field-end-col 12))
    (insert ?\x0020)
    (insert (propertize (noaa-iso8601-to-hour-min (noaa-forecast-start-time noaa-forecast)) 'face 'noaa-face-date))
    (move-to-column hour-field-end-col t)
    (insert (propertize (format "% s" (noaa-forecast-temp noaa-forecast)) 'face 'noaa-face-temp))
    (move-to-column temp-field-end-col t)
    (insert (propertize (format "%s" (noaa-forecast-short-forecast noaa-forecast)) 'face 'noaa-face-short-forecast))
    (newline)))

(defun noaa-handle-noaa-result (result)
  "Handle the data described by RESULT (presumably the result of an HTTP request for NOAA forecast data). Return a list of periods."
  (switch-to-buffer noaa-buffer-spec)
  ;; retrieve-fn accepts two arguments: a key-value store and a key
  ;; retrieve-fn returns the corresponding value
  (let ((retrieve-fn 'noaa-aval))
    (let ((properties (funcall retrieve-fn result 'properties)))
      (if (not properties)
	  (message "Couldn't find properties. The NOAA API spec may have changed.")
	(funcall retrieve-fn properties 'periods)))))

(defun noaa-iso8601-to-hour-min (iso8601-string)
  "Return a string representing the time as HH:MM as specified by ISO8601-STRING. For example, invocation with 2018-12-24T18:00:00-08:00 should return 18:00."
  (format-time-string "%H:%M" (parse-iso8601-time-string iso8601-string)))

(defun noaa-iso8601-to-day (iso8601-string)
  "Return an integer representing the day as specified by ISO8601-STRING. For example, invocation with 2018-12-24T18:00:00-08:00 should return 24."
  (string-to-number (format-time-string "%d" (parse-iso8601-time-string iso8601-string))))

(defun noaa-iso8601-to-day-name (iso8601-string)
  "Return a string representing the name of a day of the week where the value is that specified by ISO8601-STRING. For example, invocation with `2018-12-12T01:00:00-08:00' should return `Wednesday'."
  (format-time-string "%A" (parse-iso8601-time-string iso8601-string)))

(defun noaa-iso8601-to-seconds (iso8601-string)
  "Return an integer representing the number of seconds since since 1970-01-01 00:00:00 UTC as indicated by the ISO 8601 time indicated by ISO8601-STRING. For example, invocation with `2018-12-24T18:00:00-08:00' should return 1545703200."
  (string-to-number (format-time-string "%s" (parse-iso8601-time-string iso8601-string))))

;;;###autoload
(defun noaa-quit ()
  "Leave the buffer specified by ‘noaa-buffer-spec’."
  (interactive)
  (kill-buffer noaa-buffer-spec))

(defun noaa-clear-forecast-set (forecast-set)
  "Set the slots in the forecast-set struct FORECAST-SET to NIL."
  (setf (noaa-forecast-set-forecasts forecast-set)
	nil)
  (setf (noaa-forecast-set-type forecast-set)
	nil))

(defun noaa-populate-forecasts (periods forecast-set)
  "Populate the forecasts slot of the forecast-set struct FORECAST-SET using PERIODS."
  ;; retrieve-fn accepts two arguments: a key-value store and a key
  ;; retrieve-fn returns the corresponding value
  (let ((retrieve-fn 'noaa-aval)
	(number-of-periods (length periods)))
    (setf (noaa-forecast-set-forecasts forecast-set)
	  (make-list (length periods) nil))
    (dotimes (i number-of-periods)
      (let ((period (elt periods i)))
	(let ((start-time (funcall retrieve-fn period 'startTime)))
	  (let ((day-number (noaa-iso8601-to-day start-time))
		(detailed-forecast (funcall retrieve-fn period 'detailedForecast))
		(end-time (funcall retrieve-fn period 'endTime))
		;; NAME is descriptive. It is not always the name of a week day. Exaples of valid values include "This Afternoon", "Thanksgiving Day", or "Wednesday Night". For an hourly forecast, it may simply be the empty string.
		(name (funcall retrieve-fn period 'name))
		(temp (funcall retrieve-fn period 'temperature))
		(short-forecast (funcall retrieve-fn period 'shortForecast)))
	    (setf (elt (noaa-forecast-set-forecasts forecast-set) i)
		  (make-noaa-forecast :start-time start-time :end-time end-time :day-number day-number :name name :temp temp :detailed-forecast detailed-forecast :short-forecast short-forecast))))))))

(defun noaa-url (&optional latitude longitude hourlyp)
  "Return a string representing a URL. LATITUDE and LONGITUDE should be numbers. HOURLYP should be true if the forecast requested is an hourly forecast."
  (let ((url-string (format "https://api.weather.gov/points/%s,%s/forecast" (or latitude noaa-latitude) (or longitude noaa-longitude))))
    (when hourlyp
      (setf url-string
	    (concatenate 'string url-string "/hourly")))
    url-string))

(defun noaa-url-retrieve (url &optional http-callback)
  "Return the buffer containing only the 'raw' body of the HTTP response associated with a GET request to URL. If URL is NIL, the GET request is made to the URL described by `(noaa-url noaa-latitude noaa-longitude)'. Call HTTP-CALLBACK with the buffer as a single argument."
  (noaa-url-retrieve-tkf-emacs-request url http-callback))

;; async version relying on tfk emacs-request library
(defun noaa-url-retrieve-tkf-emacs-request (&optional url http-callback)
  (request (or url (noaa-url noaa-latitude noaa-longitude))
	   :parser 'buffer-string ;'json-read
	   :error (function*
		   (lambda (&key data error-thrown response symbol-status &allow-other-keys)
		     (message "data: %S " data)
		     (message "symbol-status: %S " symbol-status)
		     (message "E Error response: %S " error-thrown)
		     (message "response: %S " response)))
	   :status-code '((500 . (lambda (&rest _) (message "Got 500 -- the NOAA server seems to be unhappy"))))
	   :success http-callback))

(cl-defun noaa-http-callback-daily (&key data response error-thrown &allow-other-keys)
  ;; currently: callback populates noaa-last-forecast-set and then calls (noaa-display-last-forecast) and (noaa-mode)
  (noaa-http-callback :data data :response response :error-thrown error-thrown)
  (setf (noaa-forecast-set-type noaa-last-forecast-set) :daily)
  (noaa-display-last-forecast)
  (noaa-mode))

(cl-defun noaa-http-callback-hourly (&key data response error-thrown &allow-other-keys)
  (noaa-http-callback :data data :response response :error-thrown error-thrown)
  (setf (noaa-forecast-set-type noaa-last-forecast-set) :hourly)
  (noaa-display-last-forecast)
  (noaa-mode))

(cl-defun noaa-http-callback (&key data response error-thrown &allow-other-keys)
  (let ((noaa-buffer (get-buffer-create noaa-buffer-spec)))
    (switch-to-buffer noaa-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (and error-thrown (message (error-message-string error-thrown))))
    (goto-char (point-min))
    (let ((result (json-read-from-string data)))
      (setf noaa-last-forecast-raw result)
      (let ((periods (noaa-handle-noaa-result result)))
	(unless (noaa-forecast-set-p noaa-last-forecast-set)
	  (setf noaa-last-forecast-set (make-noaa-forecast-set :forecasts nil :type nil)))
 	(noaa-populate-forecasts periods noaa-last-forecast-set)))))

(cl-defun noaa-http-callback--simple (&key data response error-thrown &allow-other-keys)
  (let ((noaa-buffer (get-buffer-create noaa-buffer-spec)))
    (switch-to-buffer noaa-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (and error-thrown (message (error-message-string error-thrown)))
      (noaa-insert data))))

(defun noaa-parse-json-in-buffer ()
  "Parse and return the JSON object present in the buffer specified by ‘noaa-buffer-spec’."
  (switch-to-buffer noaa-buffer-spec)
  (json-read))

(defun noaa-insert (x)
  "Insert X into the buffer specified by ‘noaa-buffer-spec’."
  (switch-to-buffer noaa-buffer-spec)
  (insert x))

(defun noaa-next-style ()
  "Transition to the next style described by NOAA-DISPLAY-STYLES."
  (interactive)
  ;; wouldn't hurt to add this to other (interactive) fns that should only operate within noaa-mode
  (unless (eq (current-buffer) (get-buffer noaa-buffer-spec))
    (display-warning :warning (format "Not in %s buffer" noaa-buffer-spec)))
  (setf noaa-display-styles (-rotate 1 noaa-display-styles)) ; dash provides -rotate
  (noaa-display-last-forecast))

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

(define-key noaa-mode-map (kbd "h") 'noaa-hourly)
(define-key noaa-mode-map (kbd "q") 'noaa-quit)
(define-key noaa-mode-map (kbd "n") 'noaa-next-style)

(provide 'noaa)
;;; noaa.el ends here

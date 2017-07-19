# noaa.el

*View a simple summary of an NOAA weather forecast*

The [NOAA](http://www.noaa.gov) exposes a number of services which provide weather-related data. **noaa.el** provides an interface for viewing forecast data at api.weather.gov.

---

### Getting started without the Emacs package manager

1. Download `noaa.el`.

2. Load `noaa.el`. For example, you might add the following line to `~/.emacs`:

    `(load "/path/to/noaa.el")`

### Configure noaa.el

Ensure `noaa-latitude` and `noaa-longitude` are set to the desired values. For example, one might set them via `ielm` (<kbd>M-x</kbd> `ielm`):

    ;; set latitude and longitude for noaa.el
	(setq noaa-latitude 45)
	(setq noaa-longitude 120)

## Use

1. Use <kbd>M-x</kbd> `noaa` to invoke `noaa`.

2. When done, use <kbd>q</kbd> to invoke `noaa-quit`.

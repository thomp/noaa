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

## License

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

If you did not receive a copy of the GNU General Public License along with this program, see http://www.gnu.org/licenses/.

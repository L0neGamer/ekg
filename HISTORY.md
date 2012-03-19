## 0.3.0.3 (2012-03-19)

 * Support Snap 0.8

## 0.3.0.2 (2012-03-07)

 * Don't require an internet connection, by serving Bootstrap CSS and
   jQuery from the monitoring server.

## 0.3.0.1 (2012-01-26)

 * Switch from Blueprint to Bootstrap CSS

 * Overhaul look-and-feel

## 0.3.0.0 (2012-01-01)

 * Add gauges and change counters to always be monotonically increasing

 * Add web interface column headers

 * Reorganize the web interface to show counters and gauges in separate sections

 * Change REST API to allow separate access to counters and gauges

 * Return the server time in the JSON response it and use server time
   instead of client time when graphing

 * Make it possible to graph user-defined counters and gauges

 * Format numbers using comma separators

 * Show a message box when the server can't be reached

## 0.2.0.0 (2011-12-30)

 * Add user-defined counters

 * Suppress Snap logging to stdio

 * Add REST-style API for accessing single counters

## 0.1.0.0 (2011-12-27)

 * First EKG release

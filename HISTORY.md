## 0.3.1.2 (2012-09-18)

 * Support GHC 7.6

## 0.3.1.1 (2012-06-25)

 * Bump dependencies.

## 0.3.1.0 (2012-04-17)

 * Add labels, which are free-form string values exported by the
   monitoring server.  Labels allow you to export e.g. the command
   line arguments used to start the executable or the host name it's
   running on.

## 0.3.0.4 (2012-04-03)

 * Add original JavaScript files to tarball to ease distribution
   packaging.

## 0.3.0.4 (2012-04-03)

 * Change icons to Creative Commons Attribution 3.0 licensed one

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

## 0.4.0.1 (2014-07-12)

 * Fix JS bug which led to type errors for distribution metrics.

## 0.4.0.0 (2014-05-01)

 * Lots of the internals were split off into a new package, ekg-core.

 * The `Gauge.modify` function was removed, as it can't be supported
   by the new, more efficient implementation of gauges.

 * The JSON API was significantly overhauled. The the Haddock
   documentation for details.

 * The metric store used internally by the server is now exposed and
   can be used to share the same metric store between ekg and e.g.
   ekg-statsd.

 * It's now possible to provide a custom metric store to the server.

 * The getDistribution function was added.

 * The UI now has less special treatment for built-in metrics.

## 0.3.1.3 (2013-02-22)

 * Fixed security issue where ekg would always listen to all incoming
   requests, even if "localhost" was specified.

 * Always export par_tot_bytes_copied. Previously it was only exported
   if using base-4.6 and later.

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

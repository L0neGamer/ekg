$(function () {
    // Number formatters
    function formatSuffix(val, prec) {
        if (val == null)
            return "N/A"

        var prec = prec || 1;
        if (val >= 1000000000)
            return (val / 1000000000).toFixed(prec) + " GB";
        else if (val >= 1000000)
            return (val / 1000000).toFixed(prec) + " MB";
        else if (val >= 1000)
            return (val / 1000).toFixed(prec) + " kB";
        else
            return val.toFixed(prec) + " B";
    }

    function formatRate(val, prec) {
        if (val == null)
            return "N/A"

        return formatSuffix(val, prec) + "/s";
    }

    function formatPercent(val, prec) {
        if (val == null)
            return "N/A"

        var prec = prec || 1;
        return val.toFixed(prec) + " %"
    }

    // Set up polling interval control
    var updateInterval = 1000;  // ms
    $("#updateInterval").val(updateInterval).change(function () {
        updateInterval = $(this).val();
    });

    // Allow the UI to be paused
    var paused = false;
    $('#pause-ui').click(function() {
        if (paused) {
            $(this).text("Pause UI");
            paused = false;
        } else {
            $(this).text("Unpause UI");
            paused = true;
        }
    });

    // Plot formatters
    function suffixFormatter(val, axis) {
        return formatSuffix(val, axis.tickDecimals);
    }

    function rateFormatter(val, axis) {
        return formatRate(val, axis.tickDecimals);
    }

    function percentFormatter(val, axis) {
        return formatPercent(val, axis.tickDecimals);
    }

    // Replaces every instance of the underscore character ("_") by a
    // dash ("-").
    function dasherize(s) {
        return s.replace(/_/g, "-");
    };

    // Fetch data periodically and notify interested parties.
    var listeners = [];
    var fetchData = function() {
        function onDataReceived(stats) {
            var now = new Date().getTime();
            for(var i = 0; i < listeners.length; i++)
                listeners[i](stats, now);
        }
        
        $.get("http://localhost:8000/", onDataReceived, "json");
        
        setTimeout(fetchData, updateInterval);
    };
    fetchData();

    var emptyStats = {
        bytes_allocated          : 0,
        num_gcs                  : 0,
        max_bytes_used           : 0,
        num_bytes_usage_samples  : 0,
        cumulative_bytes_used    : 0,
        bytes_copied             : 0,
        current_bytes_used       : 0,
        current_bytes_slop       : 0,
        max_bytes_slop           : 0,
        peak_megabytes_allocated : 0,
        mutator_cpu_seconds      : 0,
        mutator_wall_seconds     : 0,
        gc_cpu_seconds           : 0,
        gc_wall_seconds          : 0,
        cpu_seconds              : 0,
        wall_seconds             : 0,
        par_avg_bytes_copied     : 0,
        par_max_bytes_copied     : 0
    };

    function addPlot(elem, series, opts) {
        var defaultOptions = {
            series: { shadowSize: 0 },  // drawing is faster without shadows
            xaxis: { mode: "time", tickSize: [10, "second"] }
        };
        var options = $.extend(true, {}, defaultOptions, opts)
        var data = new Array(series.length), maxPoints = 60;
        for(var i = 0; i < series.length; i++)
            data[i] = [];

        var plot = $.plot(elem, [], options);

        var prev_stats, prev_time;
        function onDataReceived(stats, time) {
            for(var i = 0; i < series.length; i++) {
                if (data[i].length >= maxPoints)
                    data[i] = data[i].slice(1);
                
                data[i].push([time, series[i].fn(stats, time,
                                                 prev_stats, prev_time)]);
            }
            
            // zip lengends with data
            res = []
            for(var i = 0; i < series.length; i++)
                res.push({ label: series[i].label, data: data[i] });
            
            if (!paused) {
                plot.setData(res);
                plot.setupGrid();
                plot.draw();
            }
            prev_stats = stats;
            prev_time = time;
        }
        
        listeners.push(onDataReceived);
    }

    function addCounter(elem, fn, formatter) {
        var prev_stats, prev_time;
        function onDataReceived(stats, time) {
            if (!paused)
                elem.text(formatter(fn(stats, time, prev_stats, prev_time)));
            prev_stats = stats;
            prev_time = time;
        }
        
        listeners.push(onDataReceived);
    }

    $(document).ready(function() {
        // Metrics
        var current_bytes_used = function(stats) { return stats.current_bytes_used };
        var max_bytes_used = function(stats) { return stats.max_bytes_used };
        var max_bytes_slop = function(stats) { return stats.max_bytes_slop };
        var current_bytes_slop = function(stats) { return stats.current_bytes_slop };
        var productivity_wall_percent = function(stats, time, prev_stats, prev_time) {
            if (prev_stats == undefined)
                return null;
            var mutator_seconds = stats.mutator_wall_seconds -
                prev_stats.mutator_wall_seconds;
            var gc_seconds = stats.gc_wall_seconds - prev_stats.gc_wall_seconds;
            return 100 * mutator_seconds / (mutator_seconds + gc_seconds);
        }
        var productivity_cpu_percent = function(stats, time, prev_stats, prev_time) {
            if (prev_stats == undefined)
                return null;
            var mutator_seconds = stats.mutator_cpu_seconds -
                prev_stats.mutator_cpu_seconds;
            var gc_seconds = stats.gc_cpu_seconds - prev_stats.gc_cpu_seconds;
            return 100 * mutator_seconds / (mutator_seconds + gc_seconds);
        }
        var allocation_rate = function(stats, time, prev_stats, prev_time) {
            if (prev_stats == undefined)
                return null;
            return 1000 * (stats.bytes_allocated -  prev_stats.bytes_allocated) /
                (time - prev_time);
        }

        // Plots
        addPlot($("#current-bytes-used-plot"),
                [{ label: "residency", fn: current_bytes_used }],
                { yaxis: { tickFormatter: suffixFormatter } });
        addPlot($("#allocation-rate-plot"),
                [{ label: "rate", fn: allocation_rate }],
                { yaxis: { tickFormatter: rateFormatter } });
        addPlot($("#productivity-plot"),
                [{ label: "wall clock time", fn: productivity_wall_percent },
                 { label: "cpu time", fn: productivity_cpu_percent }],
                { yaxis: { tickDecimals: 1, tickFormatter: percentFormatter } });

        // Counters
        addCounter($("#max-bytes-used"), max_bytes_used, formatSuffix)
        addCounter($("#current-bytes-used"), current_bytes_used, formatSuffix)
        addCounter($("#max-bytes-slop"), max_bytes_slop, formatSuffix)
        addCounter($("#current-bytes-slop"), current_bytes_slop, formatSuffix)
        addCounter($("#productivity-wall"), productivity_wall_percent, formatPercent)
        addCounter($("#productivity-cpu"), productivity_cpu_percent, formatPercent)
        addCounter($("#allocation-rate"), allocation_rate, formatRate)
    });
});

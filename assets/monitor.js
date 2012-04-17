$(document).ready(function () {
    "use strict";

    // Number formatters
    function commaify(n)
    {
        var nStr = n.toString();
        var x = nStr.split('.');
        var x1 = x[0];
        var x2 = x.length > 1 ? '.' + x[1] : '';
        var rgx = /(\d+)(\d{3})/;
        while (rgx.test(x1)) {
            x1 = x1.replace(rgx, '$1' + ',' + '$2');
        }
        return x1 + x2;
    }

    function formatSuffix(val, opt_prec) {
        if (val === null) {
            return "N/A";
        }

        var prec = opt_prec || 1;
        if (val >= 1000000000) {
            return (val / 1000000000).toFixed(prec) + " GB";
        } else if (val >= 1000000) {
            return (val / 1000000).toFixed(prec) + " MB";
        } else if (val >= 1000) {
            return (val / 1000).toFixed(prec) + " kB";
        } else {
            return val.toFixed(prec) + " B";
        }
    }

    function formatRate(val, prec) {
        if (val === null) {
            return "N/A";
        }

        return formatSuffix(val, prec) + "/s";
    }

    function formatPercent(val, opt_prec) {
        if (val === null) {
            return "N/A";
        }

        var prec = opt_prec || 1;
        return val.toFixed(prec) + " %";
    }

    // Set up polling interval control
    var updateInterval = 1000;  // ms
    $("#updateInterval").val(updateInterval).change(function () {
        updateInterval = $(this).val();
    });

    // Allow the UI to be paused
    var paused = false;
    $('#pause-ui').click(function () {
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

    function suffixFormatterGeneric(val, axis) {
        if (val >= 1000000000) {
            return (val / 1000000000).toFixed(axis.tickDecimals) + " G";
        } else if (val >= 1000000) {
            return (val / 1000000).toFixed(axis.tickDecimals) + " M";
        } else if (val >= 1000) {
            return (val / 1000).toFixed(axis.tickDecimals) + " k";
        } else {
            return val.toFixed(axis.tickDecimals);
        }
    }

    function rateFormatter(val, axis) {
        return formatRate(val, axis.tickDecimals);
    }

    function percentFormatter(val, axis) {
        return formatPercent(val, axis.tickDecimals);
    }

    // Fetch data periodically and notify interested parties.
    var listeners = [];

    function subscribe(fn) {
        listeners.push(fn);
    }

    function unsubscribe(fn) {
        listeners = listeners.filter(function (el) {
            if (el !== fn) {
                return el;
            }
        });
    }

    var alertVisible = false;
    function fetchData() {
        function onDataReceived(stats) {
            if (alertVisible) {
                $(".alert-message").hide();
            }
            alertVisible = false;
            for (var i = 0; i < listeners.length; i++) {
                listeners[i](stats, stats.server_timestamp_millis);
            }
        }

        function onError() {
            $(".alert-message").show();
            alertVisible = true;
        }

        $.ajax({
            dataType: 'json',
            success: onDataReceived,
            error: onError,
            cache: false
        });

        setTimeout(fetchData, updateInterval);
    }
    fetchData();

    function addPlot(elem, series, opts) {
        var defaultOptions = {
            series: { shadowSize: 0 },  // drawing is faster without shadows
            xaxis: { mode: "time", tickSize: [10, "second"] }
        };
        var options = $.extend(true, {}, defaultOptions, opts);
        var data = new Array(series.length);
        var maxPoints = 60;
        for(var i = 0; i < series.length; i++) {
            data[i] = [];
        }

        var plot = $.plot(elem, [], options);

        var prev_stats, prev_time;
        function onDataReceived(stats, time) {
            for(var i = 0; i < series.length; i++) {
                if (data[i].length >= maxPoints) {
                    data[i] = data[i].slice(1);
                }

                data[i].push([time, series[i].fn(stats, time,
                                                 prev_stats, prev_time)]);
            }

            // zip lengends with data
            var res = [];
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

        subscribe(onDataReceived);
        return onDataReceived;
    }

    function addCounter(elem, fn, formatter) {
        var prev_stats, prev_time;
        function onDataReceived(stats, time) {
            if (!paused)
                elem.text(formatter(fn(stats, time, prev_stats, prev_time)));
            prev_stats = stats;
            prev_time = time;
        }

        subscribe(onDataReceived);
    }

    function addDynamicPlot(key, button, graph_fn, label_fn) {
        function getStats(stats, time, prev_stats, prev_time) {
            return graph_fn(key, stats, time, prev_stats, prev_time);
        }

        $("#plots:last").append(
            '<div id="' + key + '-plot" class="plot-container">' +
                '<img src="cross.png" class="close-button"><h3>' + key +
                '</h3><div class="plot"></div></div>');
        var plot = $("#plots > .plot-container:last > div");
        var observer = addPlot(plot,
                [{ label: label_fn(key), fn: getStats }],
                { yaxis: { tickFormatter: suffixFormatterGeneric } });

        var plotContainer = $("#" + key + "-plot");
        var closeButton = plotContainer.find("img");
        closeButton.hide();
        closeButton.click(function () {
            plotContainer.remove();
            button.show();
            unsubscribe(observer);
        });

        plotContainer.hover(
            function () {
                closeButton.show();
            },
            function () {
                closeButton.hide();
            }
        );
    }

    function addDynamicCounters(table, group_fn, graph_fn, label_fn) {
        var counters = {};
        function onDataReceived(stats, time) {
            $.each(group_fn(stats), function (key, value) {
                var elem;
                if (key in counters) {
                    elem = counters[key];
                } else {
                    // Add UI element
                    table.find("tbody:last").append(
                        '<tr><td>' + key +
                            ' <img src="chart_line_add.png" class="graph-button"' +
                            ' width="16" height="16"' +
                            ' alt="Add graph" title="Add graph"></td>' +
                            '<td class="value">N/A</td></tr>');
                    elem = table.find("tbody > tr > td:last");
                    counters[key] = elem;

                    var button = table.find("tbody > tr:last > td:first > img");
                    button.click(function () {
                        addDynamicPlot(key, button, graph_fn, label_fn);
                        $(this).hide();
                    });
                }
                if (!paused)
                    elem.text(commaify(value));
            });
        }

        subscribe(onDataReceived);
    }

    function addDynamicLabels(table, group_fn) {
        var labels = {};
        function onDataReceived(stats, time) {
            $.each(group_fn(stats), function (key, value) {
                var elem;
                if (key in labels) {
                    elem = labels[key];
                } else {
                    // Add UI element
                    table.find("tbody:last").append(
                        '<tr><td>' + key + '</td>' +
                            '<td class="string">N/A</td></tr>');
                    elem = table.find("tbody > tr > td:last");
                    labels[key] = elem;
                }
                if (!paused)
                    elem.text(value);
            });
        }

        subscribe(onDataReceived);
    }

    function initAll() {
        // Metrics
        var current_bytes_used = function (stats) {
            return stats.gauges.current_bytes_used;
        };
        var max_bytes_used = function (stats) {
            return stats.gauges.max_bytes_used;
        };
        var max_bytes_slop = function (stats) {
            return stats.gauges.max_bytes_slop;
        };
        var current_bytes_slop = function (stats) {
            return stats.gauges.current_bytes_slop;
        };
        var productivity_wall_percent = function (stats, time, prev_stats, prev_time) {
            if (prev_stats == undefined)
                return null;
            var mutator_seconds = stats.counters.mutator_wall_seconds -
                prev_stats.counters.mutator_wall_seconds;
            var gc_seconds = stats.counters.gc_wall_seconds -
                prev_stats.counters.gc_wall_seconds;
            return 100 * mutator_seconds / (mutator_seconds + gc_seconds);
        };
        var productivity_cpu_percent = function (stats, time, prev_stats, prev_time) {
            if (prev_stats == undefined)
                return null;
            var mutator_seconds = stats.counters.mutator_cpu_seconds -
                prev_stats.counters.mutator_cpu_seconds;
            var gc_seconds = stats.counters.gc_cpu_seconds -
                prev_stats.counters.gc_cpu_seconds;
            return 100 * mutator_seconds / (mutator_seconds + gc_seconds);
        };
        var allocation_rate = function (stats, time, prev_stats, prev_time) {
            if (prev_stats == undefined)
                return null;
            return 1000 * (stats.counters.bytes_allocated -
                           prev_stats.counters.bytes_allocated) /
                (time - prev_time);
        };

        // Plots
        addPlot($("#current-bytes-used-plot > div"),
                [{ label: "residency", fn: current_bytes_used }],
                { yaxis: { tickFormatter: suffixFormatter } });
        addPlot($("#allocation-rate-plot > div"),
                [{ label: "rate", fn: allocation_rate }],
                { yaxis: { tickFormatter: rateFormatter } });
        addPlot($("#productivity-plot > div"),
                [{ label: "wall clock time", fn: productivity_wall_percent },
                 { label: "cpu time", fn: productivity_cpu_percent }],
                { yaxis: { tickDecimals: 1, tickFormatter: percentFormatter } });

        // Counters
        addCounter($("#max-bytes-used"), max_bytes_used, formatSuffix);
        addCounter($("#current-bytes-used"), current_bytes_used, formatSuffix);
        addCounter($("#max-bytes-slop"), max_bytes_slop, formatSuffix);
        addCounter($("#current-bytes-slop"), current_bytes_slop, formatSuffix);
        addCounter($("#productivity-wall"), productivity_wall_percent, formatPercent);
        addCounter($("#productivity-cpu"), productivity_cpu_percent, formatPercent);
        addCounter($("#allocation-rate"), allocation_rate, formatRate);

        addDynamicCounters($("#counter-table"), function (stats) {
            return stats.counters;
        }, function (key, stats, time, prev_stats, prev_time) {
            if (prev_stats == undefined)
                return null;
            return 1000 * (stats.counters[key] - prev_stats.counters[key]) /
                (time - prev_time);
        }, function (label) {
            return label + "/s";
        });

        addDynamicCounters($("#gauge-table"), function (stats) {
            return stats.gauges;
        }, function (key, stats, time) {
            return stats.gauges[key];
        }, function (label) {
            return label;
        });

        addDynamicLabels($("#label-table"), function (stats) {
            return stats.labels;
        });
    }

    initAll();
});

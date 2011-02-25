\name{Metrics}
\alias{Metrics}
\title{Sets the metrics of interest (clicks, pageviews, etc)...}
\usage{Metrics(metrics.param=NA)}
\description{Sets the metrics of interest (clicks, pageviews, etc)
Optional.
The aggregated statistics for user activity in a profile, such as
clicks or pageviews. When queried by alone, metrics provide aggregate
values for the requested date range, such as overall pageviews or
total bounces. However, when requested with dimensions, values are
segmented by the dimension. For example, ga:pageviews requested with
ga:country returns the total pageviews per country rather than the
total pageviews for the entire profile. When requesting metrics, keep
in mind:
Any request must supply at least one metric because a request cannot
consist only of dimensions.
You can supply a maximum of 10 metrics for any query.
Most combinations of metrics from multiple categories can be used
together, provided no dimensions are specified.
The exception to the above is the ga:visitors metric, which can only
be used in combination with a subset of metrics.
Any given metric can be used in combination with other dimensions or
metrics, but only where Valid Combinations apply for that metric.
Metric values are always reported as an aggregate because the Data
Export API does not provide calculated metrics. For a list of common
calculations based on aggregate metrics.
NOTE: We do check for valid metrics.}
\value{The metrics value if metrics.param is not set.}
\arguments{\item{metrics.param}{A vector of up to 10 dimensions, either as
a single string or a vector or strings. E.g.
"ga:visits" or c("ga:visits", "ga:bounces")
If NULL is used, the metrics parameter will be
unset. If no parameter is specified, the current
metrics value is returned.}}

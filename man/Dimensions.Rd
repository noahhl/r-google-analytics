\name{Dimensions}
\alias{Dimensions}
\title{Sets the dimensions.}
\usage{Dimensions(dimensions.param=NA)}
\description{Sets the dimensions.
Optional.
The dimensions parameter defines the primary data keys for your
Analytics report, such as ga:browser or ga:city. Use dimensions to
segment your web property metrics. For example, while you can ask for
the total number of pageviews to your site, it might be more
interesting to ask for the number of pageviews segmented by browser.
In this case, you'll see the number of pageviews from Firefox,
Internet Explorer, Chrome, and so forth.
When using dimensions in a feed request, be aware of the following
constraints:
You can supply a maximum of 7 dimensions for any query.
You can not send a query comprised only of dimensions:
You must combine any requested dimension with at least one metric.
Any given dimension can be used with other dimensions or metrics,
but only where Valid Combinations apply for that dimension.
More information on valid combinations can be found here:
http://code.google.com/apis/analytics/docs/gdata/
gdataReferenceDimensionsMetrics.html#validCombinations
NOTE: This method does not check for invalid dimensions or combinations.}
\value{The dimensions value if dimensions.param is not set.}
\arguments{\item{dimensions.param}{A vector of up to 7 dimensions, either as
a single string or a vector or strings, E.g.
"ga:source,ga:medium" or c("ga:source", "ga:medium")
If NULL is used, the dimensions parameter will be
unset. If no parameter is specified, the current
dimension value is returned.}}

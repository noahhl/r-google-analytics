\name{Sort}
\alias{Sort}
\title{Sets the sorting criteria.}
\usage{Sort(sort.param=NA)}
\description{Sets the sorting criteria.
Optional.
Indicates the sorting order and direction for the returned data.
For example, the following parameter would first sort by ga:browser
and then by ga:pageviews in ascending order.
If you do not indicate a sorting order in your query, the data is
sorted by dimension from left to right in the order listed.
When using the sort parameter, keep in mind the following:
Sort only by dimensions or metrics value that you have used in the
dimensions or metrics parameter. If your request sorts on a field that
is not indicated in either the dimensions or metrics parameter, you
will receive a request error.
Google Analytics treats dimensions as strings, so all dimensions are
sorted in ascending alphabetical order in an en-US locale.
Google Analytics treats all metrics as numbers, so all metrics are
sorted in ascending numeric order.
The sort direction can be changed from ascending to descending by
using a minus sign (-) prefix on the requested field.
Note: We do not check that the sort parameters are also defined in
the dimensions or metrics parameters.}
\value{The sort value if sort.param is not set.}
\arguments{\item{sort}{The sorting order for the data to be returned.
e.g. "ga:visits" or c("ga:visits", "-ga:browser")
If NULL is used, the sort parameter will be
unset. If no parameter is specified, the current sort value
is returned.}}

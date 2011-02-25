\name{EndDate}
\alias{EndDate}
\title{Sets the end date.}
\usage{EndDate(end.date.param=NA)}
\description{Sets the end date.
Optional.
All Analytics feed requests must specify a beginning and ending date
range. If you do not indicate start- and end-date values for the
request, the server returns a request error.
Date values are in the form YYYY-MM-DD.
The earliest valid start-date is 2005-01-01. There is no upper limit
restriction for a start-date. However, setting a start-date that is
too far in the future will most likely return empty results.}
\value{The end.date value if end.date.param is not set.}
\arguments{\item{end.date.param}{An end date of the form 'YYYY-MM-DD'
as a string. If NULL is used, the end.date.param
parameter will be unset. If no parameter is specified,
the current end.date value is returned.}}

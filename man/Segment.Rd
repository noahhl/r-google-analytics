\name{Segment}
\alias{Segment}
\title{Sets the segments, see dxp:segment in the Account Feed Response section...}
\usage{Segment(segment.param=NA)}
\description{Sets the segments, see dxp:segment in the Account Feed Response section
in the GA literature online.
http://code.google.com/apis/analytics/docs/gdata/gdataDeveloperGuide.html
Optional.
For general information on advanced segments, see Advanced
Segmentation in the Help Center. You can request an advanced segment
in the data feed in two ways:
(1) The numeric ID of a default or custom advanced segment.
The account feed returns all default advanced segments and their
IDs, as well as any custom segments defined for the account.
For more information on segment and their IDs, see dxp:segment in
the Account Feed Response section.
(2) The dynamic parameter in the query.
Use this method to segment your data request by one or more
dimensions and/or metrics. You can also use regular expressions
for segments just as you would for the filters parameter.
Dynamic segments use the same Expressions and Operators used for
the filters parameter. When using OR boolean logic or AND boolean
logic, dynamic segment expressions follow the same rules as for
the filters parameter, except that you may use OR boolean logic
with both dimensions or metrics.
Dimensions/metrics combinations in the advanced segment expression
have fewer restrictions. Except where noted in the table, you can use
any dimension or metric in combination with another in your filter.
The segment parameter is once again difficult to write checks for,
as this is a handler we rely on the GA API to report errors with the
request.}
\value{The segment value if segment.param is not set.}
\arguments{\item{segment}{An advanced segment definition to slice and dice your
Analytics data. If NULL is used, the segment parameter will be
unset. If no parameter is specified, the current segment value
is returned.}}
\examples{gaid::10
dynamic::ga:medium==referral}

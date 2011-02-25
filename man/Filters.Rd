\name{Filters}
\alias{Filters}
\title{Sets the filters used.}
\usage{Filters(filters.param=NA)}
\description{Sets the filters used.
Optional.
The filters query string parameter restricts the data returned from
your request to the Analytics servers. When you use the filters
parameter, you supply a dimension or metric you want to filter,
followed by the filter expression. For example, the following feed
query requests ga:pageviews and ga:browser from profile 12134, where
the ga:browser dimension starts with the string Firefox:}
\value{The filters value if filters.param is not set.}
\arguments{\item{filters}{The filter string for the GA request.
e.g. "ga:medium==referral".
If NULL is used, the filters parameter will be unset.
If no parameter is specified, the current filters value
is returned.}}

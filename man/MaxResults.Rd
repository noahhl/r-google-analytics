\name{MaxResults}
\alias{MaxResults}
\title{Sets the maximum number of results to return.}
\usage{MaxResults(max.results.param=NA)}
\description{Sets the maximum number of results to return.
Optional.
Maximum number of entries to include in this feed. You can use this in
combination with start-index to retrieve a subset of elements, or use
it alone to restrict the number of returned elements, starting with
the first.
If you do not use the max-results parameter in your query, your feed
returns the default maximum of 1000 entries.
The Analytics Data Export API returns a maximum of 10,000 entries per
request, no matter how many you ask for. It can also return fewer
entries than requested, if there aren't as many dimension segments as
you expect. For instance, there are fewer than 300 possible values for
ga:country, so when segmenting only by country, you can't get more
than 300 entries, even if you set max-results to a higher value.}
\value{The max.results value if max.results.param is not set.}
\arguments{\item{max.results}{Maximum number of entries to include in the data feed.
If not specified we return the default of 1000.}}

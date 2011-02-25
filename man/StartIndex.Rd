\name{StartIndex}
\alias{StartIndex}
\title{Sets the starting index from where to return results from.}
\usage{StartIndex(start.index.param=NA)}
\description{Sets the starting index from where to return results from.
Optional.
If not supplied, the starting index is 1. (Feed indexes are 1-based.
That is, the first entry is entry 1, not entry 0.) Use this parameter
as a pagination mechanism along with the max-results parameter for
situations when totalResults exceeds 10,000 and you want to retrieve
entries indexed at 10,001 and beyond.}
\value{The start.index value if start.index.param is not set.}
\arguments{\item{start.index.param}{The starting point of pagination for results to be
returned. If NULL is used, the start.index parameter
will be unset. If no parameter is specified, the
current start.index value is returned.}}

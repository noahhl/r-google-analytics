\name{GetReportData}
\alias{GetReportData}
\title{Returns the data specified by the query, auto-paginating and combining...}
\usage{GetReportData(query.builder, start.index=1, max.rows=1000)}
\description{Returns the data specified by the query, auto-paginating and combining
rows if needed. This also validates the query to ensure the minimum
required parameters are set.}
\value{A R data.frame of all the rows of data available up to the first
1,000,000.}
\arguments{\item{query.builder}{An instance of the QueryBuilder() function.}
\item{start.index}{The starting point for where GA retrieves data.}
\item{max.rows}{The total number of results to return and join together in
the data. This is set to 10,000 at the start so the user can
see the total number, and then choose. We have an upper
limit of 1,000,000 rows in place.}}

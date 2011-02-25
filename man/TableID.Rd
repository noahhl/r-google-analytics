\name{TableID}
\alias{TableID}
\title{Sets the table id for a user.}
\usage{TableID(table.id.param=NA)}
\description{Sets the table id for a user.
Optional.
The unique table ID used to retrieve the Analytics Report data. This
ID is provided by the <ga:table.id> element for each entry in the
account feed. We run a series of checks that the form of the data is
being correctly entered.
NOTE: This function does not test the table.id is valid from the account
profile.}
\value{The table.id value if table.id.param is not set.}
\arguments{\item{table.id.param}{This value is the table ID of the profile,
e.g "ga:1234".
If NULL is used, the table.id parameter will
be unset. If no parameter is specified, the
current table.id value is returned.}}

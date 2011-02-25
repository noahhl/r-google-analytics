\name{GetXMLDataFeed}
\alias{GetXMLDataFeed}
\title{Returns the data XML for a requested uri.}
\usage{GetXMLDataFeed(query)}
\description{Returns the data XML for a requested uri.
This function uses the RCurl packages to pass the built uri
query and authenticate the Google Account being used.}
\value{An XML string of the data returned from GA of the specified query.}
\arguments{\item{query}{This is the uri query.}}

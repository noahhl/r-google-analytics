\name{GetRDataFromXML}
\alias{GetRDataFromXML}
\title{A function to convert the XML data into an R data...}
\usage{GetRDataFromXML(xml.string)}
\description{A function to convert the XML data into an R data.frame.
This function takes an XML string (GA schema) and processes the
XML to return an R data.frame.}
\value{A list of information extracted from the XML;
data: A data.frame of metric and/or dimension attributes and values, this
will include C.I's if over 500,000 entries.
aggr.totals: GA might match millions of rows of data, but the API will
only return a max of 10k rows at a time. Along with every
response, the API will return the aggregates of each metric
that was matched (not returned).
total.results: The total number of rows of data that GA matched.}
\arguments{\item{xml.string}{An XML string from matching the GA API schema.}}

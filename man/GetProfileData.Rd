\name{GetProfileData}
\alias{GetProfileData}
\title{A function to convert the profile XML into an R data...}
\usage{GetProfileData(profile=GetProfileXML())}
\description{A function to convert the profile XML into an R data.frame.
We have the raw XML for a user profile from the GetProfileXML() function,
we clean this information and place into a human readable table in R
for consumption. We also allow the user to pass in a string XML if
required for offline inspection.}
\value{A list of the processed XML string comprising of the profile
as a data.frame and the number of accounts the profile has access to.}
\arguments{\item{profile}{The XML string to be processed.}}

\name{RGoogleAnalytics}
\alias{RGoogleAnalytics}
\title{Creates a skeleton shell for accessing the Google Analytics API.}
\description{Creates a skeleton shell for accessing the Google Analytics API.}
\value{Returns a list of methods, for accessing the Google Analytics API.
GetXmlDataFeed(),
GetRDataFromXML(),
SetCredentials(),
GetProfileXML(),
GetProfileData(),
GetReportData(),
For more information please look at the help pages for each function.}
\examples{ga <- RGoogleAnalytics()
ga$SetCredentials("INSERT_USER_NAME", "INSERT_PASSWORD")
# Get the list of different profiles, to help build the query.
prof <- ga$GetProfileList()
# Build the query.
query.builder <- QueryBuilder()
query.builder$Init(start.date = "2010-05-01",
end.date = "2010-08-20",
dimensions = "ga:date",
metrics = "ga:visits",
sort = "ga:date",
table.id = "ga:30661272")
ga.data <- ga$GetRDataFromQuery(query.builder)
# Look at the data returned.
head(ga.data$data)}

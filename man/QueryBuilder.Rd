\name{QueryBuilder}
\alias{QueryBuilder}
\title{The main builder class for constructing URI requests.}
\description{The main builder class for constructing URI requests.
This function lists all the elements and parameters that make up a data
feed request. In general, you provide the table ID corresponding to the
profile you want to retrieve data from, choose the combination of
dimensions and metrics, and provide a date range along with other
parameters in a query string.
More detailed information on each parameter can be found on the below:
http://code.google.com/apis/analytics/docs/
gdata/gdataReferenceDataFeed.html#dataRequest
We use a builder method to construct the final query for data checking and
exceptions, rather than raw strings.
This also enables us to scale the parameters map in later versions.
All the parameter values that are accepted for a profile can be found
by looking at the metrics available in the GetProfileData() function.}
\value{builder: The builder method function to process the parameters.}
\examples{# An example of using this construction pattern is shown below.
# Set the relevant variable information, if the variable is optional, you
# do not need to specify it in the construction or simply place it as NULL.
# query <- QueryBuilder()
# query$Init(start.date = "2010-05-01",
# end.date = "2010-08-20",
# dimensions = "ga:date",
# metrics = "ga:visits",
# sort = "ga:date",
# table.id = "ga:30661272")
# ga.data <- ga$GetRDataFromQuery(query)}

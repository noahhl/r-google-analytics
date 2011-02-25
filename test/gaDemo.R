# Copyright 2010 Google Inc. All Rights Reserved.
# Author: mpearmain@ (Mike Pearmain).
# Author: api.nickm@ (Nick Mihailovski).
# Author: nicolasremy@ (Nicolas Remy).

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

source("./RGoogleAnalytics.R")
source("./QueryBuilder.R")

ga <- RGoogleAnalytics()

# Set the user name and password for a GA account.
ga$SetCredentials("INSERT_USER_NAME", "INSERT_PASSWORD")

# Get the list of different profiles and table IDs to help build the query.
profile.data <- ga$GetProfileData()

# my.table.id is set to a constant for the example, as the Table ID
# is user specific. Use the GetProfileData method to see all the Table IDs
# the user has access to.
my.table.id <- "ga:123456"

# Build the query.
query <- QueryBuilder()

# Set the parameters.
query$Init(start.date = "2010-05-01",
           end.date   = "2010-08-20",
           dimensions = "ga:date",
           metrics    = "ga:visits",
           sort       = "ga:date",
           table.id   = my.table.id)

# Return the data specified in the query builder.
ga.data <- ga$GetReportData(query)

# Look at the data returned.
head(ga.data)


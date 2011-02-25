# Copyright 2010 Google Inc. All Rights Reserved.
# Author: mpearmain@ (Mike Pearmain)
# Author: api.nickm@ (Nick Mihailovski)

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

# Run unit tests in R, for QueryBuilder.R
# The driver file to execute is ./Runit_driver.R
source("../R/QueryBuilder.R")

# Tests for QueryBuilder().
TestQueryBuilder <- function() {
  test.list <- list(start.date  = function(start.date = NULL){},
                    end.date    = function(end.date = NULL) {},
                    dimensions  = function(dimensions = NULL) {},
                    metrics     = function(metrics = NULL) {},
                    segment     = function(segment = NULL) {},
                    sort        = function(sort = NULL) {},
                    filters     = function(filters = NULL) {},
                    max.results = function(max.results = NULL) {},
                    start.index = function(start.index = NULL) {},
                    table.id    = function(table.id = NULL) {},
                    build       = function(build = NULL) {})
  test.QueryBuilder <- QueryBuilder()
  # Test the structure of the object returned by QueryBuilder() match.
  checkEquals(str(test.list), str((test.QueryBuilder)))
}

# Tests for the StartDate() function within the QueryBuilder() class.
TestStartDate <- function() {
  query <- QueryBuilder()

  # Test setting the start.date.
  query$start.date("2005-11-01")
  checkEquals("2005-11-01", query$start.date())

  # Test setting to NULL unsets the parameter.
  query$start.date(NULL)
  checkEquals(NULL, query$start.date())

  # Test to check that a date is the correct type.
  checkException(query$start.date(19810626), silent = TRUE)

  # Test to check that a date must be in the YYYY-MM-DD format.
  checkException(query$start.date("June 26th 1981"), silent = TRUE)
}

# Tests for the EndDate() function within the QueryBuilder() class.
TestEndDate <- function() {
  query <- QueryBuilder()

  # Test setting the end.date.
  query$end.date("2010-09-12")
  checkEquals("2010-09-12", query$end.date())

  # Test setting to NULL unsets the parameter.
  query$end.date(NULL)
  checkEquals(NULL, query$end.date())

  # Test to check that a date is the correct type.
  checkException(query$end.date(19810626), silent = TRUE)

  # Test to check that a date must be in the YYYY-MM-DD format.
  checkException(query$end.date("June 26th 1981"), silent = TRUE)
}

# Tests for the Dimensions() function within the QueryBuilder() class.
# Dimensions is an optional parameter.
TestDimensions <- function() {
  query <- QueryBuilder()

  # Test passing a string as a parameter.
  query$dimensions("ga:source,ga:medium")
  checkEquals("ga:source,ga:medium", query$dimensions())


  # Test passing a vector as a parameter.
  query$dimensions(c("ga:date", "ga:landingPagePath"))
  checkEquals("ga:date,ga:landingPagePath", query$dimensions())

  # Test value is unset if input is NULL.
  query$dimensions(NULL)
  checkEquals(NULL, query$dimensions())

  # Test vector format.
  dimensions.not.vector <- as.Date("1981-06-26", "%Y-%m-%d")
  checkException(test.QueryBuilder$dimensions(dimensions.not.vector),
                 silent = TRUE)

  # Test that vector length cannot exceed 7 dimensions.
  dimensions.high <- c("ga:source",
                       "ga:medium",
                       "ga:source",
                       "ga:medium",
                       "ga:source",
                       "ga:medium",
                       "ga:source",
                       "ga:medium")
  checkException(test.QueryBuilder$dimensions(dimensions.high), silent = TRUE)

  # Test to check vector is character.
  dimensions.numeric <- c(1, 2, 3, 4, 5)
  checkException(test.QueryBuilder$dimensions(dimensions.numeric),
                 silent = TRUE)
}

# Tests for the metrics() function within the QueryBuilder() class.
TestMetrics <- function() {
  query <- QueryBuilder()

  # Test a pass inputs, we are testing for an invisible NULL return.
  #checkEquals(test.QueryBuilder$metrics(metrics.pass.string), NULL)
  #checkEquals(test.QueryBuilder$metrics(metrics.pass.vector), NULL)

  # Test passing a string as a parameter.
  query$metrics("ga:pageviews,ga:visits")
  checkEquals("ga:pageviews,ga:visits", query$metrics())

  # Test passing a vector as a parameter.
  query$metrics(c("ga:timeOnSite", "ga:transactions"))
  checkEquals("ga:timeOnSite,ga:transactions", query$metrics())

  # Test value is unset if input is NULL.
  query$metrics(NULL)
  checkEquals(NULL, query$metrics())

  # Test vector format.
  metrics.not.vector <- as.Date("1981-06-26", "%Y-%m-%d")
  checkException(query$metrics(metrics.not.vector), silent = TRUE)

  # Test that vector length cannot exceed 10 metrics.
  metrics.high <- c("ga:pageviews", "ga:visits",
                    "ga:pageviews", "ga:visits",
                    "ga:pageviews", "ga:visits",
                    "ga:pageviews", "ga:visits",
                    "ga:pageviews", "ga:visits",
                    "ga:pageviews")
  checkException(query$metrics(metrics.high), silent = TRUE)

  # Test to check vector is character.
  metrics.numeric <- c(1, 2, 3, 4, 5)
  checkException(query$metrics(metrics.numeric), silent = TRUE)
}

# This is a trivial test as we do no error checking, and let the
# GA API return the segment error.
TestSegment <- function() {
  query <- QueryBuilder()
  segment.param <- "dynamic::ga:medium==referral"

  # Test passing a string as a parameter.
  query$segment(segment.param)
  checkEquals(segment.param, query$segment())

  # Test value is unset if input is NULL.
  query$segment(NULL)
  checkEquals(NULL, query$segment())
}

# Tests for the Sort() function within the QueryBuilder() class.
# Sort() is an optional parameter.
TestSort <- function() {
  query <- QueryBuilder()

  # Test passing a string as a parameter.
  query$sort("ga:source,ga:medium")
  checkEquals("ga:source,ga:medium", query$sort())

  # Test passing a vector as a parameter.
  query$sort(c("ga:pageviews", "ga:visits"))
  checkEquals("ga:pageviews,ga:visits", query$sort())


  # Test value is unset if input is NULL.
  query$sort(NULL)
  checkEquals(NULL, query$sort())

  # Test vector format.
  sort.not.vector <- as.Date("1981-06-26", "%Y-%m-%d")
  checkException(query$sort(sort.not.vector), silent = TRUE)

  # Test to check vector is character.
  sort.numeric <- c(1, 2, 3, 4, 5)
  checkException(query$sort(sort.numeric), silent = TRUE)
}

# This is a trivial test as we do no error checking, and let the
# GA API return the filters error.
TestFilters <- function() {
  query <- QueryBuilder()

  # Test passing a valid string as a parameter.
  filter <- "ga:medium==referral;ga:source==google"
  query$filters(filter)
  checkEquals(filter, query$filters())

  # Test value is unset if input is NULL.
  query$filters(NULL)
  checkEquals(NULL, query$filters())
}

# Tests for the MaxResults() function within the QueryBuilder() class.
# MaxResults() is an optional parameter.
TestMaxResults <- function() {
  query <- QueryBuilder()

  # Test passing a valid number as a parameter.
  query$max.results(5000)
  checkEquals(5000, query$max.results())

  # Test value is unset if input is NULL.
  query$max.results(NULL)
  checkEquals(NULL, query$max.results())

  # Test that vectors are not allowed.
  checkException(query$max.results(c(1, 2, 3)), silent = TRUE)

  # Test that strings are not allowed.
  checkException(query$max.results("3000"), silent = TRUE)
}

# Tests for the StartIndex() function within the QueryBuilder() class.
# StartIndex() is an optional parameter.
TestStartIndex <- function() {
  query <- QueryBuilder()

  # Test passing a valid number as a parameter.
  query$start.index(1000)
  checkEquals(1000, query$start.index())

  # Test value is unset if input is NULL.
  query$start.index(NULL)
  checkEquals(NULL, query$start.index())

  # Test that vectors are not allowed.
  checkException(query$start.index(c(1, 2, 3)), silent = TRUE)

  # Test that strings are not allowed.
  checkException(query$start.index("1000"), silent = TRUE)
}

# Tests for the TableID() function within the QueryBuilder() class.
TestTableID <- function() {
  query <- QueryBuilder()

  # Test passing a valid string as a parameter.
  query$table.id("ga:1174")
  checkEquals("ga:1174", query$table.id())

  # Test value is unset if input is NULL.
  query$table.id(NULL)
  checkEquals(NULL, query$table.id())

  # Test that vectors are not allowed.
  checkException(query$table.id(c("ga:1234", "ga:567")), silent = TRUE)


  # Test vector format.
  table.id.not.vector <- as.Date("1981-06-26", "%Y-%m-%d")
  checkException(query$table.id(table.id.not.vector), silent = TRUE)

  # Test to check numerics are not allowed.
  checkException(query$table.id(1174), silent = TRUE)
}

# Tests the ToUri() function within the QueryBuilder() class.
# This assumes the parameters pass the parameter tests.
TestToUri <- function() {
  expected.uri <- paste("https://www.google.com/analytics/feeds/data",
                        "?start-date=2010%2D05%2D01",
                        "&end-date=2010%2D05%2D31",
                        "&dimensions=ga%3Adate",
                        "&metrics=ga%3Avisits",
                        "&segment=dynamic%3A%3Aga%3Amedium%3D%3Dorganic",
                        "&sort=ga%3Adate",
                        "&filters=ga%3Asource%3D%3Dgoogle",
                        "&max-results=10000",
                        "&start-index=25",
                        "&ids=ga%3A30661272",
                        sep = "")

  # Build the query.
  query <- QueryBuilder()
  query$start.date("2010-05-01")
  query$end.date("2010-05-31")
  query$dimensions("ga:date")
  query$metrics("ga:visits")
  query$segment("dynamic::ga:medium==organic")
  query$sort("ga:date")
  query$filters("ga:source==google")
  query$max.results(10000)
  query$start.index(25)
  query$table.id("ga:30661272")

  checkEquals(expected.uri, query$to.uri())

  # Test setting parameters to NULL do not end up in the final URL.

  query$start.date(NULL)
  query$end.date(NULL)
  query$dimensions(NULL)
  query$metrics(NULL)
  query$segment(NULL)
  query$sort(NULL)
  query$filters(NULL)
  query$max.results(NULL)
  query$start.index(NULL)
  query$table.id(NULL)

  checkEquals("https://www.google.com/analytics/feeds/data?", query$to.uri())
}

# Tests for the Validate() function within the QueryBuilder() class.
# NOTE: This is not the same as the parameters being incorrect.
TestValidate <- function() {

  # Missing start.date.
  builder <- QueryBuilder()
  builder$end.date("2010-05-31")
  builder$metrics("ga:visitors")
  builder$table.id("ga:30661272")
  checkException(builder$validate(), silent = TRUE)

  # Missing end.date.
  builder <- QueryBuilder()
  builder$start.date("2010-05-01")
  builder$metrics("ga:visitors")
  builder$table.id("ga:30661272")
  checkException(builder$validate(), silent = TRUE)

  # Missing metrics.
  builder <- QueryBuilder()
  builder$start.date("2010-05-01")
  builder$end.date("2010-05-31")
  builder$table.id("ga:30661272")
  checkException(builder$validate(), silent = TRUE)

  # Missing table.id.
  builder <- QueryBuilder()
  builder$start.date("2010-05-01")
  builder$end.date("2010-05-31")
  builder$metrics("ga:visitors")
  checkException(builder$validate(), silent = TRUE)
}


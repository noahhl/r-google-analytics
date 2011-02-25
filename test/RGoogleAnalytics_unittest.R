# Copyright 2010 Google Inc. All Rights Reserved.
# Author: mpearmain@google.com (Mike Pearmain)

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

# Run unit tests in R, for RGoogleAnalytics.R
# The driver file to execute is ./Runit_driver.R


# Unit tests for the RGoogleAnalytics() class.
# The SetCredentials() function is trivial and only returns an Auth.Token as a
# string.  This is non-trivial to mock so no unit test is applied.

# Test the parsing of XML feed after it has been returned for profile
# information.
TestParseAccountFeedXML <- function() {
  # We create mock data of the XML feed that would be returned from the
  # GetProfileXML() function.
  ga <- RGoogleAnalytics()
  # Create a data.frame with the same properties as the mock XML.
  AccountName <- c("白雪公主")
  ProfileName <- c("www.googlestore.com ")
  TableId <- c("ga:30661272")
  test.profile <- data.frame(AccountName, ProfileName, TableId)

  data <- ga$ParseAccountFeedXML(AccountFeedXMLString())

  checkEquals(test.profile, data$profile)

  # Check the "totalResults" matched to 1, as in the XML.
  checkEqualsNumeric(1, as.numeric(data$total.results))
}

# Test the parsing of XML data feed after it has been returned for a uri
# containing the data.
TestParseDataFeedXML <- function() {
  # We create mock data of the XML feed that would be returned from the
  # GetDataFeedXML() function.
  ga <- RGoogleAnalytics()

  # Testing the $data structure is the same.
  ga.source  <- c("blogger.com",
                  "google.com",
                  "stumbleupon.com",
                  "google.co.uk",
                  "google.co.in")
  ga.medium  <- rep("referral", 5)
  ga.visits  <- c(68140, 29666, 4012, 2968, 2793)
  ga.bounces <- c(61095, 14979, 848, 2084, 1891)
  test.data  <- data.frame(ga.source, ga.medium, ga.visits, ga.bounces,
                           stringsAsFactors = FALSE)
  names(test.data) <- c("ga:source", "ga:medium", "ga:visits", "ga:bounces")
  xml.data   <- ga$ParseDataFeedXML(DataFeedXMLString())
  checkEquals(test.data, xml.data$data)

  # Testing the $total.results is as expected.
  test.total.results <- 6451
  checkEquals(test.total.results, xml.data$total.results)

  # Testing aggr.totals match
  ga.visits <- 136540
  ga.bounces <- 101535
  test.aggr.totals <- as.data.frame(rbind(ga.visits, ga.bounces))
  names(test.aggr.totals) <- "aggregate.totals"
  rownames(test.aggr.totals) <- c("ga:visits", "ga:bounces")
  checkEquals(test.aggr.totals, xml.data$aggr.totals)
}

# Test parsing the XML data returned from a Google Analytics API (Google Data)
# error.
TestParseApiErrorMessage <- function() {
  ga <- RGoogleAnalytics()
  error.message = ga$ParseApiErrorMessage(SampleErrorXMLString())

  checkEquals('InvalidRequestUriException', error.message$code)
  checkEquals('Sort key ga:foo is not a dimension or metric in this query.',
              error.message$reason)
}

# Sample XML structures.

AccountFeedXMLString <- function() {
  return('<?xml version="1.0" encoding="UTF-8"?>
     <feed xmlns="http://www.w3.org/2005/Atom"
           xmlns:dxp="http://schemas.google.com/analytics/2009"
           xmlns:ga="http://schemas.google.com/ga/2009"
           xmlns:openSearch="http://a9.com/-/spec/opensearch/1.1/"
           xmlns:gd="http://schemas.google.com/g/2005"
  gd:etag="W/&quot;CEINQ344fSp7I2A9Wx5QF0s.&quot;" gd:kind="analytics#accounts">
  <id>
    http://www.google.com/analytics/feeds/accounts/analytics-support@google.com
  </id>
  <updated>2010-09-06T01:43:12.035-07:00</updated>
  <title>Profile list for analytics-support@google.com</title>
  <link rel="self" type="application/atom+xml"
        href="https://www.google.com/analytics/feeds/accounts/default"/>
  <author>
    <name>Google Analytics</name>
  </author>
  <generator version="1.0">Google Analytics</generator>
  <openSearch:totalResults>1</openSearch:totalResults>
  <openSearch:startIndex>1</openSearch:startIndex>
  <openSearch:itemsPerPage>47</openSearch:itemsPerPage>
  <entry gd:etag="W/&quot;CEINQ344fSp7I2A9Wx5QF0s.&quot;"
         gd:kind="analytics#account">
    <id>http://www.google.com/analytics/feeds/accounts/ga:30661272</id>
    <updated>2010-09-06T01:43:12.035-07:00</updated>
    <title>www.googlestore.com </title>
    <link rel="alternate" type="text/html"
          href="http://www.google.com/analytics"/>
    <dxp:property name="ga:accountId" value="30481"/>
    <dxp:property name="ga:accountName" value="白雪公主"/>
    <dxp:property name="ga:profileId" value="30661272"/>
    <dxp:property name="ga:webPropertyId" value="UA-30481-1"/>
    <dxp:property name="ga:currency" value="USD"/>
    <dxp:property name="ga:timezone" value="America/Los_Angeles"/>
    <dxp:tableId>ga:30661272</dxp:tableId>
  </entry>
  </feed>')
}

# test xml code from
# http://ga-api-http-samples.googlecode.com/svn/trunk/src/v2/dataFeedResponse.xml
DataFeedXMLString <- function() {
  return("<?xml version='1.0' encoding='UTF-8'?>
<feed xmlns='http://www.w3.org/2005/Atom' xmlns:dxp='http://schemas.google.com/analytics/2009' xmlns:openSearch='http://a9.com/-/spec/opensearch/1.1/' xmlns:gd='http://schemas.google.com/g/2005' gd:etag='W/&quot;DUINSHcycSp7I2A9WxRWFEQ.&quot;' gd:kind='analytics#data'>
  <id>http://www.google.com/analytics/feeds/data?ids=ga:1174&amp;dimensions=ga:medium,ga:source&amp;metrics=ga:bounces,ga:visits&amp;filters=ga:medium%3D%3Dreferral&amp;start-date=2008-10-01&amp;end-date=2008-10-31</id>
  <updated>2008-10-31T16:59:59.999-07:00</updated>
  <title>Google Analytics Data for Profile 1174</title>
  <link rel='self' type='application/atom+xml' href='http://www.google.com/analytics/feeds/data?max-results=5&amp;sort=-ga%3Avisits&amp;end-date=2008-10-31&amp;start-date=2008-10-01&amp;metrics=ga%3Avisits%2Cga%3Abounces&amp;ids=ga%3A1174&amp;dimensions=ga%3Asource%2Cga%3Amedium&amp;filters=ga%3Amedium%3D%3Dreferral'/>
  <link rel='next' type='application/atom+xml' href='http://www.google.com/analytics/feeds/data?start-index=6&amp;max-results=5&amp;sort=-ga%3Avisits&amp;end-date=2008-10-31&amp;start-date=2008-10-01&amp;metrics=ga%3Avisits%2Cga%3Abounces&amp;ids=ga%3A1174&amp;dimensions=ga%3Asource%2Cga%3Amedium&amp;filters=ga%3Amedium%3D%3Dreferral'/>
  <author>
    <name>Google Analytics</name>
  </author>
  <generator version='1.0'>Google Analytics</generator>
  <openSearch:totalResults>6451</openSearch:totalResults>
  <openSearch:startIndex>1</openSearch:startIndex>
  <openSearch:itemsPerPage>5</openSearch:itemsPerPage>
  <dxp:aggregates>
    <dxp:metric confidenceInterval='0.0' name='ga:visits' type='integer' value='136540'/>
    <dxp:metric confidenceInterval='0.0' name='ga:bounces' type='integer' value='101535'/>
  </dxp:aggregates>
  <dxp:dataSource>
    <dxp:property name='ga:profileId' value='1174'/>
    <dxp:property name='ga:webPropertyId' value='UA-30481-1'/>
    <dxp:property name='ga:accountName' value='Google Store'/>
    <dxp:tableId>ga:1174</dxp:tableId>
    <dxp:tableName>www.googlestore.com</dxp:tableName>
  </dxp:dataSource>
  <dxp:endDate>2008-10-31</dxp:endDate>
  <dxp:startDate>2008-10-01</dxp:startDate>
  <entry gd:etag='W/&quot;C0UEQX47eSp7I2A9WxRWFEw.&quot;' gd:kind='analytics#datarow'>
    <id>http://www.google.com/analytics/feeds/data?ids=ga:1174&amp;ga:medium=referral&amp;ga:source=blogger.com&amp;filters=ga:medium%3D%3Dreferral&amp;start-date=2008-10-01&amp;end-date=2008-10-31</id>
    <updated>2008-10-30T17:00:00.001-07:00</updated>
    <title>ga:source=blogger.com | ga:medium=referral</title>
    <link rel='alternate' type='text/html' href='http://www.google.com/analytics'/>
    <dxp:dimension name='ga:source' value='blogger.com'/>
    <dxp:dimension name='ga:medium' value='referral'/>
    <dxp:metric confidenceInterval='0.0' name='ga:visits' type='integer' value='68140'/>
    <dxp:metric confidenceInterval='0.0' name='ga:bounces' type='integer' value='61095'/>
  </entry>
  <entry gd:etag='W/&quot;C0UEQX47eSp7I2A9WxRWFEw.&quot;' gd:kind='analytics#datarow'>
    <id>http://www.google.com/analytics/feeds/data?ids=ga:1174&amp;ga:medium=referral&amp;ga:source=google.com&amp;filters=ga:medium%3D%3Dreferral&amp;start-date=2008-10-01&amp;end-date=2008-10-31</id>
    <updated>2008-10-30T17:00:00.001-07:00</updated>
    <title>ga:source=google.com | ga:medium=referral</title>
    <link rel='alternate' type='text/html' href='http://www.google.com/analytics'/>
    <dxp:dimension name='ga:source' value='google.com'/>
    <dxp:dimension name='ga:medium' value='referral'/>
    <dxp:metric confidenceInterval='0.0' name='ga:visits' type='integer' value='29666'/>
    <dxp:metric confidenceInterval='0.0' name='ga:bounces' type='integer' value='14979'/>
  </entry>
  <entry gd:etag='W/&quot;C0UEQX47eSp7I2A9WxRWFEw.&quot;' gd:kind='analytics#datarow'>
    <id>http://www.google.com/analytics/feeds/data?ids=ga:1174&amp;ga:medium=referral&amp;ga:source=stumbleupon.com&amp;filters=ga:medium%3D%3Dreferral&amp;start-date=2008-10-01&amp;end-date=2008-10-31</id>
    <updated>2008-10-30T17:00:00.001-07:00</updated>
    <title>ga:source=stumbleupon.com | ga:medium=referral</title>
    <link rel='alternate' type='text/html' href='http://www.google.com/analytics'/>
    <dxp:dimension name='ga:source' value='stumbleupon.com'/>
    <dxp:dimension name='ga:medium' value='referral'/>
    <dxp:metric confidenceInterval='0.0' name='ga:visits' type='integer' value='4012'/>
    <dxp:metric confidenceInterval='0.0' name='ga:bounces' type='integer' value='848'/>
  </entry>
  <entry gd:etag='W/&quot;C0UEQX47eSp7I2A9WxRWFEw.&quot;' gd:kind='analytics#datarow'>
    <id>http://www.google.com/analytics/feeds/data?ids=ga:1174&amp;ga:medium=referral&amp;ga:source=google.co.uk&amp;filters=ga:medium%3D%3Dreferral&amp;start-date=2008-10-01&amp;end-date=2008-10-31</id>
    <updated>2008-10-30T17:00:00.001-07:00</updated>
    <title>ga:source=google.co.uk | ga:medium=referral</title>
    <link rel='alternate' type='text/html' href='http://www.google.com/analytics'/>
    <dxp:dimension name='ga:source' value='google.co.uk'/>
    <dxp:dimension name='ga:medium' value='referral'/>
    <dxp:metric confidenceInterval='0.0' name='ga:visits' type='integer' value='2968'/>
    <dxp:metric confidenceInterval='0.0' name='ga:bounces' type='integer' value='2084'/>
  </entry>
  <entry gd:etag='W/&quot;C0UEQX47eSp7I2A9WxRWFEw.&quot;' gd:kind='analytics#datarow'>
    <id>http://www.google.com/analytics/feeds/data?ids=ga:1174&amp;ga:medium=referral&amp;ga:source=google.co.in&amp;filters=ga:medium%3D%3Dreferral&amp;start-date=2008-10-01&amp;end-date=2008-10-31</id>
    <updated>2008-10-30T17:00:00.001-07:00</updated>
    <title>ga:source=google.co.in | ga:medium=referral</title>
    <link rel='alternate' type='text/html' href='http://www.google.com/analytics'/>
    <dxp:dimension name='ga:source' value='google.co.in'/>
    <dxp:dimension name='ga:medium' value='referral'/>
    <dxp:metric confidenceInterval='0.0' name='ga:visits' type='integer' value='2793'/>
    <dxp:metric confidenceInterval='0.0' name='ga:bounces' type='integer' value='1891'/>
  </entry>
</feed>")
}

SampleErrorXMLString <- function() {
return("<errors xmlns='http://schemas.google.com/g/2005'>
  <error>
  <domain>GData</domain>
  <code>InvalidRequestUriException</code>
  <internalReason>Sort key ga:foo is not a dimension or metric in this query.</internalReason>
  </error>
</errors>
")
}

# Copyright 2010 Google Inc. All Rights Reserved.
# Author: Mike Pearmain.
# Author: Nick Mihailovski.
# Author: Nicolas Remy.

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

# This script allows to use the Google Analytics (GA) API to read data
# directly from GA into R as a data.frame.
# As of now this script allows you to:
#   - Create a new export object.
#   - Authenticate with your Google Account.
#   - Return an account profile for an authorised Google Account.
#   - Create a new API query
#   - Use the query to return a data.frame populated with metrics.
# It requires the RCurl and XML packages.
# These packages can be downloaded from http://www.omegahat.org/R
# using the following command,
# 'install.packages('RCurl', repos = "http://www.omegahat.org/R")'
# 'install.packages('XML', repos = "http://www.omegahat.org/R")'
#
# If errors occur when downloading 'Rcurl' or XML' packages ensure the libcurl
# and libxml libraries are up to date on the Linux machine.
# 'sudo apt install libxml2-dev'
# 'sudo apt install libcurl4-gnutls-dev'
#
# A QueryBuilder.R script is also available as set of helper functions for
# constructing GA uri queries.

library(RCurl)
library(XML)

# R functions to load data.

RGoogleAnalytics <- function() {
  # Creates a skeleton shell for accessing the Google Analytics API.
  #
  # Returns:
  #   Returns a list of methods, for accessing the Google Analytics API.
  #   GetProfileData(),
  #   GetAccountFeedXML(),
  #   ParseAccountFeedXML(),
  #   GetReportData(),
  #   GetDataFeed(),
  #   GetDataFeedXML(),
  #   ParseDataFeedXML(),
  #   ParseApiErrorMessage(),
  #   SetCredentials(),
  #   For more information please look at the help pages for each function.
  #
  # Examples:
  #   ga <- RGoogleAnalytics()
  #   ga$SetCredentials("INSERT_USER_NAME", "INSERT_PASSWORD")
  #
  #   # Get the list of different profiles, to help build the query.
  #   prof <- ga$GetProfileData()
  #
  #   # Build the query.
  #   query.builder <- QueryBuilder()
  #   query.builder$Init(start.date = "2010-05-01",
  #                      end.date   = "2010-08-20",
  #                      dimensions = "ga:date",
  #                      metrics    = "ga:visits",
  #                      sort       = "ga:date",
  #                      table.id   = "ga:30661272")
  #   ga.data <- ga$GetReportData(query.builder)
  #
  #   # Look at the data returned.
  #   head(ga.data$data)

  # Constants.
  kMaxDefaultRows <- 10000
  kMaxPages <- 100

  # Private members.
  # We set the authorization token to ensure the user can access the profile
  # information, and retrieve data.
  # We explicitly avoiding storing the user name and password for security
  # reasons. The Auth token is valid for 14 days.
  auth.token <- NULL

  StoreCredentials <- function(user, pass) {
    # This is crappy security, but let's at least not save our username and password in scripts or in plaintext on the file system. Someone's still going to hack this, but let's not make it SO easy.
      user <- serialize(user, NULL)
      pass <- serialize(pass, NULL)
      creds <- list(user=user, pass=pass)
      save(creds, file=paste(ifelse(.Platform$OS.type=="unix", "~", Sys.getenv("R_USER")), "/.rGoogleAnalytics_creds", sep=""))
      SetCredentials(unserialize(user), unserialize(pass))
    }

  SetCredentials <- function(username, pass) {
    # Authorizes this script to access the users Google Analytics Account.
    # Passes the username and password to the Google Accounts API ClientLogin
    # routine to get an authorization token which can be used to access
    # the user's Google Analytics data. This token is stored in the auth.token
    # private member and stays valid for 14 days. The token can later be used
    # with queries to the Google Analytics API. Note: We do not store the
    # username or password for security reasons.
    #
    # Args:
    #   username: A valid user name for a Google Analytics account.
    #   pass: A valid password for the Google Analytics account.
    # Returns:
    #   Returns the authorization token of the users account.

    # Error handling.
    # Ensure that both user name and password have been entered.
    if (is.null(username) || is.null(pass)) {
      stop("Please supply a user name and password")
    }

    auth.resp <- postForm("https://www.google.com/accounts/ClientLogin",
                          Email = username,
                          Passwd = pass,
                          accountType = "GOOGLE",
                          source = "r-google-analytics",
                          service = "analytics")
    gtoken <- unlist(strsplit(auth.resp, "\n"))
    parsed.gtoken <- unlist(strsplit(gtoken[3], "Auth="))
    if (length(parsed.gtoken) >= 2) {
      auth.token <<- unlist(strsplit(gtoken[3], "Auth="))[[2]]
    } else {
      stop("Authentication failed.")
    }

    return(invisible())
  }

  CheckAuthToken <- function() {
    # Checks whether a valid authorization token exists.
    # We test for the presence of the authorization token at various points in
    # the code. The SetCredentials function sets the global auth.token on
    # assignment, so this checks the presence of the variable.
    #
    # Returns:
    #   A stop call if the auth.token has not been retrieved.
    if (is.null(auth.token))
      stop("Please enter the user name and password in SetCredentials")
  }

  GetAnyXMLAttribute <- function(vNode, attr.name) {
    # Function to return the value attribute of the nodes of the parsed XML.
    #
    # Args:
    #   vNode: The XML node to be inspected.
    #   attr.name: The attribute to be returned.
    # Returns:
    #   The value contained with the XML node.
    kNumericAttributes = c("value", "confidenceInterval")
    if (xmlName(vNode) == "metric" && attr.name %in% kNumericAttributes)
      return(as.numeric(xmlGetAttr(vNode, attr.name)))
    else
      return(xmlGetAttr(vNode, attr.name))
  }

  GetProfileData <- function() {
    # Returns a list of account names, profile names and table ids for the
    # currently authorized user. You can use the table ids to specify which
    # profile to access data for using the GetReportData() function. If any
    # error occurs, this function will stop and print the error. You must
    # use the SetCredentials function to authorize access to your data
    # before using this function.

    api.response <- GetAccountFeedXML()
    status <- api.response$status

    if (status$code != '200') {
      error.message = ParseApiErrorMessage(api.response$body)
      stop(paste(status$code, status$message, "\n",
                 error.message$code, error.message$reason))
    }

    return(ParseAccountFeedXML(api.response$body))
  }

  GetAccountFeedXML <- function() {
    # Makes a request to the Google Analytics API Account Feed and returns
    # the XML response as well as the status codes of the HTTP request.
    #
    # Returns:
    #   A list containing the XML response from the API as well as the status.
    #   body: An XML string with the body of the API response.
    #   status: A list containing the status code and message returned from
    #           the API.
    CheckAuthToken()

    # Get GA Account Feed.
    header = basicTextGatherer()

    google.auth <- paste("GoogleLogin auth=", auth.token)
    response <-
      getURL("https://www.google.com/analytics/feeds/accounts/default",
             .encoding = 'UTF-8',
             httpheader = c("Authorization" = google.auth,
               "GData-Version" = 2),
             headerfunction=header$update)

    header.value <- parseHTTPHeader(header$value())
    header$reset()

    return(list(body = response,
                status = list(code = header.value[['status']],
                              message = header.value[['statusMessage']])))
  }

  ParseAccountFeedXML <- function(xml.string) {
    # Parses and returns the Account Feed XML response.
    # This function extracts the Account Name, Profile Name and
    # Table Id properties from the XML returned by the Account Feed, and
    # add them to a data.frame. It then returns the data.frame and the
    # total number of profiles returned from the API as a list.
    #
    # Args:
    #   xml.string: The Account Feed XML string to be processed.
    # Returns:
    #   A list of the processed Account Feed.
    #   profile: A data.frame containing the Account Name, Profile Name,
    #            and Table IDs for all profiles fetched by the API.
    #   total.results: The total number of profiles returned by the API
    #                  request.

    # Get XML data into an R object.
    feed.xml <- xmlTreeParse(xml.string,
                             asText = TRUE,
                             useInternalNode = TRUE)

    # get namespaces of XML doc.
    feed.name.space <- sapply(xmlNamespaceDefinitions(feed.xml),
                              function(ns) ns$uri)
    names(feed.name.space)[1] <- "ns"

    # Return the total number of results available.
    total.results <-
      data.frame(unlist(xpathApply(feed.xml,
                                   path = "//openSearch:totalResults",
                                   xmlValue,
                                   namespaces = feed.name.space)))
    names(total.results) <- "total.results"

    # Return the Table ID's from the account feed.
    table.id.list <-
      data.frame(unlist(xpathApply(feed.xml,
                                   path = "//ns:entry/dxp:tableId",
                                   xmlValue,
                                   namespaces = feed.name.space)))

    ns.path <- "//ns:entry/dxp:property[@name = 'ga:accountName']"
    # Return the account name from the account feed.
    table.account.list <-
      data.frame(unlist(xpathApply(feed.xml,
                                   path = ns.path,
                                   GetAnyXMLAttribute,
                                   "value",
                                   namespaces = feed.name.space)))

    # Return the profile name from the account feed.
    table.profile.list <-
      data.frame(unlist(xpathApply(feed.xml,
                                   path = "//ns:entry/ns:title",
                                   xmlValue,
                                   namespaces = feed.name.space)))

    # Join the data.frames and rename the variables.
    profile <- cbind(table.account.list,
                     table.profile.list,
                     table.id.list)

    names(profile) <- c("AccountName", "ProfileName", "TableId")

    # We return the profiles and the results.
    return(list(profile = profile,
                total.results = total.results))
  }

  GetDataFeed <- function(query.uri) {
    # Makes a request to the Google Analytics API Data Feed.
    # This methods first prints the request URI specified by the user,
    # then attempts to make a request to the Google Analytics Data Feed.
    # If everything is successful, it will return the parsed response.
    # If an error occurred, the program will stop and the user will be
    # alerted with as much information about the error as we can easily
    # provide.

    # Args:
    #   query.uri: The request URI as a string to send to the Google Analytics
    #     API.
    # Returns:
    #   If successful, a data frame with the response. If an error occurred,
    #   an error message is printed and the program will terminate.

    # Error checking.
    # Ensure the input is of character format.
    if (!is.character(query.uri)) {
      stop("the query.uri parameter must be a character string")
    }

    print(paste("Executing query:", query.uri))

    api.response <- GetDataFeedXML(query.uri)
    status <- api.response$status

    if (status$code != "200") {
      error.message = ParseApiErrorMessage(api.response$body)
      stop(paste(status$code, status$message, "\n",
                 error.message$code, error.message$reason))
    }

    return(ParseDataFeedXML(api.response$body))
  }

  GetDataFeedXML <- function(query.uri) {
    # Makes a request to the Google Analytics API Data Feed and returns
    # the XML response as well as the status codes of the HTTP request.
    #
    # Args:
    #   query.uri: The Data Feed query URI string.
    # Returns:
    #   A list containing the XML response from the API as well as the status.
    #   body: An XML string with the body of the API response.
    #   status: A list containing the status code and message returned from
    #           the API.
    CheckAuthToken()

    # Get GA Data Feed.
    header = basicTextGatherer()
    google.auth <- paste("GoogleLogin auth=", auth.token)

    response <- getURL(query.uri,
                       .encoding = 'UTF-8',
                       httpheader = c("Authorization" = google.auth,
                         "GData-Version" = 2),
                       headerfunction=header$update)

    header.value <- parseHTTPHeader(header$value())
    header$reset()

    return(list(body = response,
                status = list(code = header.value[['status']],
                              message = header.value[['statusMessage']])))
  }

  ParseDataFeedXML <- function(xml.string) {
    # Parses and returns the Data Feed XML response.
    # This function converts the XML data into an R data.frame.
    #
    # Args:
    #   xml.string: An XML string from matching the GA API schema.
    # Returns:
    #  A list of information extracted from the XML;
    #  data: A data.frame of metric and/or dimension attributes and values.
    #        Includes Confidence Intervals if any of the data has been sampled.
    #  aggr.totals: GA might match millions of rows of data, but the API will
    #               only return a max of 10k rows at a time. Along with every
    #               response, the API will return the aggregates of each metric
    #               that was matched (not returned).
    #  total.results: The total number of rows of data that GA matched.

    # Error checking.
    # Ensure the input is of character format.
    if (!is.character(xml.string)) {
      stop("xml.string must be a character string")
    }

    feed.data = xml.string
    # get XML data into an R object.
    feed.xml <- xmlTreeParse(feed.data,
                             asText = TRUE,
                             useInternalNode = TRUE)

    # get namespaces of XML doc.
    feed.name.space <- sapply(xmlNamespaceDefinitions(feed.xml),
                              function(ns) ns$uri)
    names(feed.name.space)[1] <- "ns"

    # get the titles of the dimensions and metrics.
    titles <- unlist(xpathApply(feed.xml,
                                "//ns:entry[1]/*[@name]",
                                GetAnyXMLAttribute, "name",
                                namespaces = feed.name.space))

    # return the values of the titles as a data frame.
    values.list <- lapply(titles, function(name) {
      unlist(xpathApply(feed.xml,
                        paste("//ns:entry/dxp:*[@name='", name, "']",
                              sep = ""),
                        GetAnyXMLAttribute, "value",
                        namespaces = feed.name.space))
    })
    df.value <- data.frame(values.list, stringsAsFactors = FALSE)
    names(df.value) = titles

    # get the names of the confidence interval on the metrics.
    ci.check <- unlist(xpathApply(feed.xml,
                                  "//ns:entry[1]/*[@confidenceInterval]",
                                  GetAnyXMLAttribute, "name",
                                  namespaces = feed.name.space))

    # return the C.I of the metrics as a data frame.
    df.ci <- data.frame(lapply(ci.check, function(name) {
      unlist(xpathApply(feed.xml,
                        paste("//ns:entry/dxp:*[@name='", name, "']",
                              sep = ""),
                        GetAnyXMLAttribute, "confidenceInterval",
                        namespaces = feed.name.space))
    }),
                        stringsAsFactors = FALSE)
    # Rename the variables to reflect C.I status.
    # If the sum of all values is 0, remove
    names(df.ci) <- sub("$", ".C.I", ci.check)
    # join the C.I with the data frames if sum of col != 0
    df.ci <- df.ci[colSums(df.ci) != 0]
    df <- df.value
    if (ncol(df.ci) > 0 && nrow(df.ci) > 0) {
      df <- cbind(df.value, df.ci[colSums(df.ci) != 0])
    }
    
    # get the names of the Aggregates on the metrics
    aggr.check <- unlist(xpathApply(feed.xml,
                                    "//dxp:aggregates/dxp:metric",
                                    GetAnyXMLAttribute, "name",
                                    namespaces = feed.name.space))

    # return the values of the aggr as a data frame.
    df.aggr <-
      data.frame("aggregate.totals" = sapply(aggr.check, function(name) {
        unlist(xpathApply(feed.xml,
                          paste("//dxp:aggregates/dxp:*[@name='", name, "']",
                                sep = ""),
                          GetAnyXMLAttribute, "value",
                          namespaces = feed.name.space))
      }))
    # return the total results.
    # The total_results element of the feed, gives you the total numbers of
    # entries (rows) of data that GA matched.
    total.results <- unlist(xpathApply(feed.xml,
                                       path = "//openSearch:totalResults",
                                       xmlValue,
                                       namespaces = feed.name.space))
    names(total.results) <- "total.results"
    total.results <- as.numeric(total.results)
    return(list(data = df,
                aggr.totals = df.aggr,
                total.results = total.results))

  }

  ParseApiErrorMessage <- function(xml.string) {
    # Parses and returns the XML error message as a list.
    # This function takes the XML returned when there is a Google Analytics API
    # error and returns the error codes and message. This does not handle errors
    # returned from the Google Accounts API -that uses a different error syntax.
    #
    # Args:
    #   xml.string: An XML string returned when there is an API error.
    # Returns:
    #  A list of information extracted from the XML.
    #  code: The high level type of error that occurred.
    #  internal.reason: A more detailed description of what caused the error.

    # Error checking.
    # Ensure the input is of character format.
    if (!is.character(xml.string)) {
      stop("xml.string must be a character string")
    }

    code = ""
    reason = ""

    # Check to only parse error messages that come from GData.
    if (length(grep("<domain>GData</domain>", xml.string))) {

      # Get XML data into an R object.
      error.xml <- xmlTreeParse(xml.string,
                                asText = TRUE,
                                useInternalNode = TRUE)

      # Get namespaces of XML doc.
      xml.name.space <- sapply(xmlNamespaceDefinitions(error.xml),
                                function(ns) ns$uri)
      names(xml.name.space)[1] <- "ns"

      # Get the error code.
      code <- unlist(xpathApply(error.xml,
                                path = "//ns:error/ns:code",
                                xmlValue,
                                namespaces = xml.name.space))

      # Get the error internalReason.
      reason <- unlist(xpathApply(error.xml,
                                  path = "//ns:error/ns:internalReason",
                                  xmlValue,
                                  namespaces = xml.name.space))
    }

    return(list(code = code,
                reason = reason))
  }

  GetReportData <- function(query.builder,
                            start.index = 1,
                            max.rows = NULL) {
    # Returns the data specified by the query, auto-paginating and combining
    # rows if needed. This also validates the query to ensure the minimum
    # required parameters are set. The value of max.rows will override
    # the max.results parameter in the query and can be set up to 1,000,000.
    # If any errors occur, they will be printed out and the program will stop.
    # You must use the SetCredentials function to authorize access to your data
    # before you can use the GetReportData.
    #
    # Args:
    #   query.builder: An instance of the QueryBuilder() function.
    #   start.index: The starting point for where GA retrieves data.
    #   max.rows: The total number of results to return and join together in
    #             the data. This will overwrite the query value in max.results
    #             and can be set up to 1,000,000. By default the Google
    #             Analytics API only supports returning 10,000 rows per
    #             request. If max.rows is greater than 10,000 multiple
    #             requests will be made to the API and the results will be
    #             merged.
    # Returns:
    #   A R data.frame of all the rows of data available up to the first
    #   1,000,000.

    query.builder$validate()

    # Ensure the starting index is set per the user request
    # We can only return 10,000 rows in a single query
    kMaxDefaultRows <- 10000

    if (!is.null(max.rows)) {
      query.builder$max.results(min(kMaxDefaultRows, max.rows))
    }

    df <- GetDataFeed(query.builder$to.uri())

    if (is.null(max.rows) || max.rows <= kMaxDefaultRows) {
      # No extra pagination is needed.
      return(df)

    } else {
      # Handle pagination. First get the number of pages needed. Then
      # update the start index for each page and request the data.

      pagination <- min(kMaxPages, ceiling(df$total.results /
                                     kMaxDefaultRows))
      for (i in seq_along(2:pagination)) {
        start.index <- (i * kMaxDefaultRows) + 1
        query.builder$start.index(start.index)
        ga.data <- GetDataFeed(query.builder$to.uri())
        df$data <- rbind(df$data, ga.data$data)
        rm(ga.data)
      }
      return(df)
    }
  }


  ##############################################################################
  #Read saved user/pass
  tryCatch({
      suppressWarnings(load(file=paste(ifelse(.Platform$OS.type=="unix", "~", Sys.getenv("R_USER")), "/.rGoogleAnalytics_creds", sep="")))
      SetCredentials(unserialize(creds$user), unserialize(creds$pass))
      cat("Succesfully loaded saved credentials.\n")
    }, error = function(e) {
      cat("Didn't find any credentials. Store some with StoreCredentials() or set them for just this session with SetCredentials().\n")
    })


  return(list(GetProfileData       = GetProfileData,
              GetAccountFeedXML    = GetAccountFeedXML,
              ParseAccountFeedXML  = ParseAccountFeedXML,
              GetReportData        = GetReportData,
              GetDataFeed          = GetDataFeed,
              GetDataFeedXML       = GetDataFeedXML,
              ParseDataFeedXML     = ParseDataFeedXML,
              ParseApiErrorMessage = ParseApiErrorMessage,
              StoreCredentials     = StoreCredentials,
              SetCredentials       = SetCredentials))
}

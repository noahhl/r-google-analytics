\name{CheckAuthToken}
\alias{CheckAuthToken}
\title{A Check to see if there is a valid authorization token.}
\description{A Check to see if there is a valid authorization token.
We test for the presence of the authorization token at various points in
the code. The SetCredentials function sets the global auth.token on
assignment, so this checks the presence of the variable.}
\value{A stop call if the auth.token has not been retrieved.}

\name{SetCredentials}
\alias{SetCredentials}
\title{Fetches the GA Auth Token for a user with a password.}
\usage{SetCredentials(user, pass)}
\description{Fetches the GA Auth Token for a user with a password.
Fetches GA authentication token, which can then be used to fetch data,
Technically works by submitting the information to the API and storing the
account information in gtoken, we then return the 'Auth' entry token.
As mentioned above we don't keep the user name or passwords for security
reasons.}
\value{Returns the authentication token of the users account.}
\arguments{\item{user}{A valid user name for a GA account.}
\item{pass}{A valid password for the GA account.}}

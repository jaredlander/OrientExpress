% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/dbConnection.r
\name{buildDBURL}
\alias{buildDBURL}
\title{buildDBURL}
\usage{
buildDBURL(db, type = "query")
}
\arguments{
\item{db}{An \code{OrientDB} object}

\item{type}{Type of command to execute}
}
\value{
A full URL for the OrientDB curl API
}
\description{
Builds URL for OrientDB API
}
\details{
Builds URL for OrientDB API
}
\examples{
db <- dbInfo(host='127.0.0.1', database='GratefulDeadConcerts',
             username='admin', password='admin', port='2480')
buildDBURL(db)
buildDBURL(db, 'sql')
}
\author{
Jared P. Lander
}


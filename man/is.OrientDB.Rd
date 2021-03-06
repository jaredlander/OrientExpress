% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/dbConnection.r
\name{is.OrientDB}
\alias{is.OrientDB}
\title{is.OrientDB}
\usage{
is.OrientDB(x)
}
\arguments{
\item{x}{An object to be tested}
}
\value{
A logical indicating if the object is an OrientDB object
}
\description{
Check if object is an OrientDB object
}
\details{
Check if object is an OrientDB object
}
\examples{
db <- dbInfo(host='127.0.0.1', database='GratefulDeadConcerts',
             username='admin', password='admin', port='2480')
is.OrientDB(db)
}
\author{
Jared P. Lander
}


#' @title dbInfo
#' @description Builds connection information for a database
#' @details Essentially builds the URL for a GET call
#' @author Jared P. Lander
#' @export dbInfo
#' @rdname dbInfo
#' @param host Hostname either as an IP address or a URL without the 'http'
#' @param database Name of the database
#' @param username username
#' @param password password
#' @param port Port number for connecting over http
#' @return An object of class \code{OrientDB} to be used as a connection string
#' @examples 
#' dbInfo(host='127.0.0.1', database='GratefulDeadConcerts', username='admin', password='admin', port='2480')
#' 
dbInfo <- function(host='localhost', database='GratefulDeadConcerts', username='admin', password='admin', port='2480')
{
    db <- list(host=host, port=port, database=database, username=username, password=password)
    class(db) <- 'OrientDB'
    return(db)
}

#' @title print.OrientDB
#' @description Print method for OrientDB connection string
#' @details S3 method for printing conenction string info
#' @author Jared P. Lander
#' @export print.OrientDB
#' @export
#' @rdname print.OrientDB
#' @param x \code{OrientDB} object
#' @param \dots Further arguments
#' @return Prints the connection string
#' @examples 
#' dbInfo(host='127.0.0.1', database='GratefulDead', username='admin', password='admin', port='2480')
#' print(dbInfo(host='127.0.0.1', database='GratefulDead', username='admin', password='admin', port='2480'))
#' 
print.OrientDB <- function(x, ...)
{
    writeLines(sprintf('host:     %s\nport:     %s\ndatabase: %s\nusername: %s\npassword: %s', 
                       x$host, x$port, x$database, '*****', '*****'))
}

#' @title buildDBUrlGeneric
#' @description Builds URL for OrientDB API
#' @details Builds URL for OrientDB API
#' @author Jared P. Lander
#' @export buildDBUrlGeneric
#' @rdname buildDBUrlGeneric
#' @param db An \code{OrientDB} object
#' @return A string with a place holder for the query type
#' @examples 
#' db <- dbInfo(host='127.0.0.1', database='GratefulDead', username='admin', password='admin', port='2480')
#' buildDBUrlGeneric(db)
#' 
buildDBUrlGeneric <- function(db)
{
    sprintf('http://%s:%s@%s:%s/%%s/%s', db$username, db$password, db$host, db$port, db$database)
}

#' @title buildDBURL
#' @description Builds URL for OrientDB API
#' @details Builds URL for OrientDB API
#' @author Jared P. Lander
#' @rdname buildDBURL
#' @param db An \code{OrientDB} object
#' @param type Type of command to execute
#' @return A full URL for the OrientDB curl API
#' @examples 
#' db <- dbInfo(host='127.0.0.1', database='GratefulDead', username='admin', password='admin', port='2480')
#' buildDBURL(db)
#' buildDBURL(db, 'sql')
#' 
buildDBURL <- function(db, type='query')
{
    sprintf(buildDBUrlGeneric(db), type)
}

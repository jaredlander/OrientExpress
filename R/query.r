#' @param limit The limit for the number of records; defualt is -1 for all records

content(mine)
buildQuery <- function(db, query, limit=-1)
{
    sprintf('%s/sql/%s/%s', buildDBURL(db=db, type='query'), URLencode(query), limit)
}

query <- function(db, query, limit=-1)
{
    GET(buildQuery(db, query, limit))
}

format.logical <- function(x, ...)
{
    x
}
#' @param limit The limit for the number of records; defualt is -1 for all records
myDB <- dbInfo('162.223.15.109', database='GratefulDeadConcerts', username='admin', password='admin', port='2480')
buildQuery(myDB, 'select * from V', 20)
mine <- query(myDB, 'select * from V', 20)
content(mine)
buildQuery <- function(db, query, limit=-1)
{
    sprintf('%s/sql/%s/%s', buildDBURL(db=db, type='query'), URLencode(query), limit)
}

query <- function(db, query, limit=-1)
{
    GET(buildQuery(db, query, limit))
}

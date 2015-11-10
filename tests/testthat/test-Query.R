context("Query functions behave")

host <- '127.0.0.1'
database <- 'GratefulDeadConcerts'
username <- 'admin'
password <- 'admin'
port <- '2480'
dbLine <- dbInfo(host=host, database=database, username=username, password=password, port=port)
selectStatement <- 'Select * from V'

query1 <- buildQuery(db=dbLine, query=selectStatement)
query2 <- buildQuery(db=dbLine, query=selectStatement, limit=20)
query3 <- buildQuery(db=dbLine, query=selectStatement, limit=-1)

# generic proper result for build query
properResult <- sprintf('http://%s:%s@%s:%s/query/%s/sql/%s/%%s', 
                        username, password, host, port, database, selectStatement)

test_that('buildQuery results in a string of length one', {
    expect_is(query1, 'character')
    expect_is(query2, 'character')
    expect_is(query3, 'character')
    
    expect_equal(length(query1), 1)
    expect_equal(length(query2), 1)
    expect_equal(length(query3), 1)
})

test_that('buildQuery errors out if not provided a proper db', {
    expect_error(buildQuery(3, 'SELECT * FROM V'))
})

test_that('buildQuery builds the proper string', {
    #str_detect(query1, 'http://.*?:.*?@\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}:\\d{1,4}/query/')
    expect_equal(query1, URLencode(sprintf(properResult, '-1')))
    expect_equal(query2, URLencode(sprintf(properResult, '20')))
    expect_equal(query3, URLencode(sprintf(properResult, '-1')))
})

# can't test query without an internet connection and a working DB
test_that('query errors if not provided a db', {
    expect_error(query(3, 'select * from V'))
})

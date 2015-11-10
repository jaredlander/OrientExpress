context("Database connection is generated")

host <- '127.0.0.1'
database <- 'GratefulDeadConcerts'
username <- 'admin'
password <- 'admin'
port <- '2480'
dbLine <- dbInfo(host=host, database=database, username=username, password=password, port=port)

test_that('A proper DB object is created', {
    expect_is(dbLine, 'OrientDB')
    expect_equal(length(dbLine), 5)
})

test_that('Slots are set', {
    expect_match(dbLine$host, host)
    expect_match(dbLine$database, database)
    expect_match(dbLine$username, username)
    expect_match(dbLine$password, password)
    expect_match(dbLine$port, port)
})

test_that('The DB URL Generic is built properly', {
    expect_equal(buildDBUrlGeneric(dbLine), sprintf('http://%s:%s@%s:%s/%%s/%s/%%s', username, password, host, port, database))
})

test_that('The DB URL is built properly', {
    expect_equal(buildDBURL(dbLine), sprintf('http://%s:%s@%s:%s/%s/%s', username, password, host, port, 'query', database))
    expect_equal(buildDBURL(dbLine, 'sql'), sprintf('http://%s:%s@%s:%s/%s/%s', username, password, host, port, 'sql', database))
})

# don't know how to test if print works properly
# test_that('DB connection prints', {
#     expect_equal(print(dbLine), sprintf('host:     %s\nport:     %s\ndatabase: %s\nusername: %s\npassword: %s', 
#                                         host, port, database, '*****', '*****'))
# })

context("insert behaves")

st <- '127.0.0.1'
database <- 'GratefulDeadConcerts'
username <- 'admin'
password <- 'admin'
port <- '2480'
dbLine <- dbInfo(host=host, database=database, username=username, password=password, port=port)

goodObject <- list('@class'='V', type='song', song_type='cover', name='Super Cool Song')
badObject1 <- c('@class'='V', type='song', song_type='cover', name='Super Cool Song')
badObject2 <- list('@class'='V', 'song', song_type='cover', name='Super Cool Song')
badObject3 <- list('V', 'song', 'cover', 'Super Cool Song')

test_that('insert errors out without the appropriate database object and list', {
    expect_error(insert(3, goodObject))
    expect_error(insert(3, badObject1))
    expect_error(insert(3, badObject2))
    expect_error(insert(3, badObject3))
    
    #expect_error(insert(dbLine, goodObject))
    expect_error(insert(dbLine, badObject1))
    #expect_error(insert(dbLine, badObject2))
    expect_error(insert(dbLine, badObject3))
}
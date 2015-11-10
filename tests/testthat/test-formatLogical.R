context("format.logical behaves as expected")

test_that('format.logical returns a logical', {
    expect_is(format(TRUE), 'logical')
    expect_is(format(FALSE), 'logical')
    expect_is(format(NA), 'logical')
    
    expect_is(format(TRUE, NA, FALSE), 'logical')
})

test_that('format.logical returns the proper lengths', {
    expect_equal(length(format(TRUE)), 1)
    expect_equal(length(format(FALSE)), 1)
    expect_equal(length(format(NA)), 1)
    
    expect_equal(length(format(c(TRUE, FALSE, NA))), 3)
})

test_that('format.logical gives proper results', {
    expect_identical(format(TRUE), TRUE)
    expect_identical(format(FALSE), FALSE)
    expect_identical(format(NA), NA)
    
    expect_identical(format(c(TRUE, FALSE, NA)), c(TRUE, FALSE, NA))
})

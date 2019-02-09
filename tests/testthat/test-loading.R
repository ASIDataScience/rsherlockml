# Copyright 2018-2019 Faculty Science Limited

context("test-loading.R")

test_that(
  "onLoad sets the right options",
  {
    mock_getenv <- mock('test-protocol', 'test-domain')

    mockery::stub(.onLoad, 'Sys.getenv', mock_getenv)
    # prevent global assignment
    mockery::stub(.onLoad, '<<-', `<-`)

    expect_null(.onLoad())

    expect_equal(
      getOption('faculty.hudson_url'),
      'test-protocol://hudson.test-domain'
    )

    expect_args(mock_getenv, 1, 'FACULTY_PROTOCOL')
    expect_args(mock_getenv, 2, 'FACULTY_DOMAIN')
  }
)

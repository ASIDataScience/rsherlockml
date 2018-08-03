context("test-loading.R")

test_that(
  "onLoad sets the right options",
  {
    mock_getenv <- mock('test-protocol', 'test-domain')
    
    stub(.onLoad, 'Sys.getenv', mock_getenv)
    # prevent global assignment
    stub(.onLoad, '<<-', `<-`)
    
    expect_null(.onLoad())
    
    expect_equal(
      getOption('sherlockml.hudson_url'), 
      'test-protocol://hudson.test-domain'
    )
    
    expect_args(mock_getenv, 1, 'SHERLOCKML_PROTOCOL')
    expect_args(mock_getenv, 2, 'SHERLOCKML_DOMAIN')
  }
)

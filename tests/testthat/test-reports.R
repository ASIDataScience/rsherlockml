# Copyright 2018-2019 Faculty Science Limited

context("test-reports.R")

dummy_report_path <- '/project/report-path'
dummy_report_name <- 'dummy-report-name'
dummy_report_description <- 'Dummy report description.'

dummy_remote_report_path <- '/s3-path'
dummy_report_object <- list(active_version=list(report_path=dummy_remote_report_path),
                            report_id='dummy-report-id',
                            report_name=dummy_report_name)
dummy_datasets_credentials <- list(access_key='dummy-key',
                                   secret_key='dummy-secret',
                                   bucket='dummy-bucket',
                                   region='dummy-region')

inp_file <- file(test_path('fixtures/test_tavern_report_list.json'))
json_text <- readLines(inp_file)
close(inp_file)

dummy_tavern_response <- jsonlite::fromJSON(json_text)

dummy_report <- test_path("fixtures/test_report.Rmd")

test_that(
  "wait and check make the correct s3 calls",
  {

    mockery::stub(wait_and_check, 'Sys.sleep', NULL)

    mock_s3_head <- mock(TRUE)

    mockery::stub(wait_and_check, 'datasets_list', list(dummy_remote_report_path))

    mockery::stub(
      wait_and_check, 'get_report_list',
      mock(
        tibble::tibble(report_id=c('dummy-report-id'), status=c('success')),
        tibble::tibble(report_id=c('dummy-report-id'), status=c('failure'))
      )
    )

    expect_null(wait_and_check(dummy_report_object))

    expect_error(wait_and_check(dummy_report_object))
  }
)

test_that(
  "publish new version makes a POST request",
  httptest::with_mock_api({

    options(list(faculty.user_id='jan'))

    mockery::stub(
      publish_new_version, 'get_report_list',
      mock(
        tibble::tibble(report_id=c('dummy-report-id'), status=c('success'),
                       report_name=c('dummy-report-name')),
        cycle=TRUE
      )
    )

    mockery::stub(publish_new_version, 'make_tmp_notebook', '/project/test-path.ipynb')
    mockery::stub(publish_new_version, 'on.exit', NULL)
    mockery::stub(publish_new_version, 'add_hudson_header', NULL)

    mock_post <- mock('mock-post-response')

    mockery::stub(publish_new_version, 'httr::POST', mock_post)
    mockery::stub(publish_new_version, 'httr::stop_for_status', NULL)
    mockery::stub(publish_new_version, 'httr::content', list(report_id='dummy-report-id'))

    mockery::stub(publish_new_version, 'wait_and_check', NULL)

    mock_datasets_put <- mock(NULL)
    mockery::stub(publish_new_version, 'datasets_put', mock_datasets_put)

    publish_new_version(dummy_report_name, dummy_report_path)

    expect_args(
      mock_post, 1,
      'test-protocol://tavern.test-domain/report/dummy-report-id/version',
      body=list(
        notebook_path='test-path.ipynb',
        author_id='jan',
        draft=FALSE
      ),
      NULL,
      encode='json'
    )
  })
)

test_that(
  "publish new report makes a POST request",
  httptest::with_mock_api({

    options(list(faculty.user_id='jan'))

    mockery::stub(
      publish_new_report, 'get_report_list',
      mock(
        tibble::tibble(report_id=c('dummy-report-id'), status=c('success'),
                       report_name=c('dummy-report-name')),
        cycle=TRUE
      )
    )

    mockery::stub(publish_new_report, 'make_tmp_notebook', '/project/test-path.ipynb')
    mockery::stub(publish_new_report, 'on.exit', NULL)
    mockery::stub(publish_new_report, 'add_hudson_header', NULL)

    mock_post <- mock('mock-post-response')

    mockery::stub(publish_new_report, 'httr::POST', mock_post)
    mockery::stub(publish_new_report, 'httr::stop_for_status', NULL)
    mockery::stub(publish_new_report, 'httr::content', list(report_id='dummy-report-id'))

    mockery::stub(publish_new_report, 'wait_and_check', NULL)

    mock_datasets_put <- mock(NULL)
    mockery::stub(publish_new_report, 'datasets_put', mock_datasets_put)

    publish_new_report(dummy_report_name, dummy_report_path, dummy_report_description)

    expect_args(
      mock_post, 1,
      paste(
        'test-protocol://tavern.test-domain/project',
        Sys.getenv("FACULTY_PROJECT_ID"),
        sep="/"
      ),
      body=list(
        report_name=dummy_report_name,
        notebook_path='test-path.ipynb',
        description=dummy_report_description,
        author_id='jan'
      ),
      NULL,
      encode='json'
    )
  })
)

test_that(
  "make_tmp_notebook copies an empty notebook correctly",
  {

    mockery::stub(make_tmp_notebook, 'tempfile', '/tmp/test.ipynb')

    nb_path <- make_tmp_notebook()

    expect_true(file.exists(nb_path))

    file.remove(nb_path)
  }
)

test_that(
  "publish_report distinguishes between new report or new version correctly",
  {

    mockery::stub(publish_report, 'set_hudson_token', NULL)
    mockery::stub(publish_report, 'set_user_id', NULL)

    mockery::stub(
      publish_report,
      'get_report_list',
      mock(
        list(report_name=c('dummy-report-name')),
        list(report_name=c('different_dummy_report_name'))
      )
    )

    mock_new_version <- mock(NULL)
    mockery::stub(
      publish_report,
      'publish_new_version',
      mock_new_version
    )

    mock_new_report <- mock(NULL)
    mockery::stub(
      publish_report,
      'publish_new_report',
      mock_new_report
    )

    expect_null(
      publish_report(
        dummy_report_name,
        dummy_report_path,
        dummy_report_description
      )
    )

    expect_args(
      mock_new_version, 1,
      dummy_report_name,
      dummy_report_path
    )

    expect_null(
      publish_report(
        dummy_report_name,
        dummy_report_path,
        dummy_report_description
      )
    )

    expect_args(
      mock_new_report, 1,
      dummy_report_name,
      dummy_report_path,
      dummy_report_description
    )
  }
)

test_that(
  "the report format runs",
  {
    expect_type(report(), typeof(rmarkdown::html_document()))
  }
)

# test_that(
#   "the report format works", {
#
#     publish_report <- function(...) NULL
#
#     expect_null(rmarkdown::render(dummy_report))
#
#     publish_report <- rfaculty:::publish_report
#   }
# )

test_that(
  "get_report_list parses tavern responses correctly",
  {
    mockery::stub(get_report_list, 'set_hudson_token', NULL)
    mockery::stub(get_report_list, 'set_user_id', NULL)

    mockery::stub(get_report_list, 'httr::stop_for_status', NULL)
    mockery::stub(get_report_list, 'add_hudson_header', NULL)

    mock_get <- mock(json_text)
    mockery::stub(get_report_list, 'httr::GET', mock_get)

    mockery::stub(
      get_report_list, 'httr::content',
      list(dummy_tavern_response)
    )

    get_report_list()
    # expect_equal(
    #   get_report_list(),
    #   list(dummy_tavern_response)
    # )
  }
)

test_that(
  "extract_report_info extracts the right info",
  {
    expect_type(
      extract_report_info(list(dummy_tavern_response)),
      "list"
    )
  }
)

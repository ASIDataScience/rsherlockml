# Copyright 2018-2019 Faculty Science Limited

context("test-authentication.R")

library(assertthat)
library(lubridate)

hudson_url <- getOption("faculty.hudson_url")

Sys.setenv(FACULTY_CLIENT_ID = "test-client-id")
Sys.setenv(FACULTY_CLIENT_SECRET = "test-client-secret")

httptest::with_mock_api({
  test_that(
    "the correct hudson endpoint is called with the right payload", {
      options(list(
        faculty.hudson.expiry = NULL,
        faculty.hudson.token = NULL
      ))

      httptest::expect_POST(
        set_hudson_token(),
        url = paste(hudson_url, "access_token", sep = "/"),
        jsonlite::toJSON(list(
          client_id =
            jsonlite::unbox(Sys.getenv("FACULTY_CLIENT_ID")),
          client_secret =
            jsonlite::unbox(Sys.getenv("FACULTY_CLIENT_SECRET")),
          grant_type =
            jsonlite::unbox("client_credentials")
        ))
      )
    }
  )
})

test_that(
  "the hudson credentials are set correctly", {
    options(list(
      faculty.hudson.expiry = NULL,
      faculty.hudson.token = NULL
    ))

    test_date <- lubridate::now()
    test_token <- do.call(paste0, as.list(sample(LETTERS, size = 20)))

    mock_post <- mock(NULL)
    mockery::stub(set_hudson_token, "httr::POST", mock_post)

    mock_parse_content <- mock(
      list(expires_in = 500, token_type = "Bearer", access_token = test_token)
    )
    mockery::stub(set_hudson_token, "httr::content", mock_parse_content)

    expect_null(set_hudson_token())

    expect_true(
      getOption("faculty.hudson.expiry") %within%
        interval(now() + seconds(490), now() + seconds(510))
    )

    expect_equal(
      getOption("faculty.hudson.token"),
      paste("Bearer", test_token)
    )

  }
)

test_that(
  "the token is not refreshed if it has not expired yet", {
    options(list(
      faculty.hudson.expiry = now() + seconds(500),
      faculty.hudson.token = "test-token"
    ))

    expect_null(set_hudson_token())
  }
)

test_that(
  "the user ID gets set correctly", {
    options(list(faculty.user_id = NULL))

    mock_get <- mock(NULL)
    mockery::stub(set_user_id, "httr::GET", mock_get)

    mock_parse_content <- mock(
      list(account = list(userId = "test-id"))
    )
    mockery::stub(set_user_id, "httr::content", mock_parse_content)

    expect_null(set_user_id())

    expect_equal(getOption("faculty.user_id"), "test-id")
  }
)

test_that(
  "datasets credentials are retrieved from the right place", {
    httptest::with_mock_api({
      mockery::stub(get_datasets_credentials, "set_hudson_token", mock(NULL))
      httptest::expect_GET(
        get_datasets_credentials(),
        url = paste(getOption("faculty.secret_url"),
                    "sfs", Sys.getenv("FACULTY_PROJECT_ID"),
                    sep = "/")
      )
    })
  }
)

test_that(
  "auth headers are actual headers", {
    mockery::stub(add_hudson_header, "httr::add_headers", mock("dummy-header"))
    expect_equal(add_hudson_header(), "dummy-header")
  }
)

test_that(
  "datasets credentials are retrieved from the right place", {
    mockery::stub(
      get_datasets_credentials,
      "set_hudson_token",
      mock(NULL, cycle = TRUE)
    )

    mock_get <- mock(NULL, cycle = TRUE)
    mockery::stub(get_datasets_credentials, "httr::GET", mock_get)

    mock_parse_content <- mock(list(verified = FALSE))
    mockery::stub(get_datasets_credentials, "httr::content", mock_parse_content)

    # this is a recursive lad, so we can mock it inside itself
    mockery::stub(
      get_datasets_credentials,
      "get_datasets_credentials",
      mock(list(verified = TRUE))
    )
    # mock sleeping so we don"t fall asleep ourselves:
    mockery::stub(get_datasets_credentials, "Sys.sleep", mock(NULL))

    expect_equal(get_datasets_credentials(), list(verified = TRUE))

    mock_parse_content <- mock(list(verified = TRUE))
    mockery::stub(get_datasets_credentials, "httr::content", mock_parse_content)
    expect_equal(get_datasets_credentials(), list(verified = TRUE))
  }
)

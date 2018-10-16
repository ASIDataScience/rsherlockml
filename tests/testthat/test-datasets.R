context("test-datasets.R")

test_remote_content <- c("/input", "/input/test.file")

test_that(
  "get calls datasets.get correctly",
  {
    m <- mock(NULL, cycle=TRUE)
    stub(datasets_get, "pydatasets$get", m)

    expect_null(datasets_get("test-remote", "test-local"))
    expect_null(datasets_get("test-remote", "test-local", "test-project-id"))

    expect_args(m, 1, "test-remote", "test-local", NULL)
    expect_args(m, 2, "test-remote", "test-local", "test-project-id")
  }
)

test_that(
  "put calls datasets.put correctly",
  {
    m <- mock(NULL, cycle=TRUE)
    stub(datasets_put, "pydatasets$put", m)

    expect_null(datasets_put("test-remote", "test-local"))
    expect_null(datasets_put("test-remote", "test-local", "test-project-id"))

    expect_args(m, 1, "test-remote", "test-local", NULL)
    expect_args(m, 2, "test-remote", "test-local", "test-project-id")
  }
)

test_that(
  "list calls datasets.ls correctly",
  {
    m <- mock(test_remote_content, cycle=TRUE)
    stub(datasets_list, "pydatasets$ls", m)

    expect_equal(
      datasets_list(),
      test_remote_content
    )
    expect_args(m, 1, "/", NULL, FALSE)

    expect_equal(
      datasets_list("/input"),
      test_remote_content
    )
    expect_args(m, 2, "/input", NULL, FALSE)

    expect_equal(
      datasets_list("/input", "test-project-id"),
      test_remote_content
    )
    expect_args(m, 3, "/input", "test-project-id", FALSE)

    expect_equal(
      datasets_list("/input", "test-project-id", TRUE),
      test_remote_content
    )
    expect_args(m, 4, "/input", "test-project-id", TRUE)
  }
)

test_that(
  "copy calls datasets.cp correctly",
  {
    m <- mock(NULL, cycle=TRUE)
    stub(datasets_copy, "pydatasets$cp", m)

    # arguments: source_path, destination_path, project_id=NULL

    expect_null(datasets_copy("test-source", "test-destination"))
    expect_args(m, 1, "test-source", "test-destination", NULL)

    expect_null(datasets_copy("test-source", "test-destination", "test-project-id"))
    expect_args(m, 2, "test-source", "test-destination", "test-project-id")
  }
)

test_that(
  "move calls datasets.mv correctly",
  {
    m <- mock(NULL, cycle=TRUE)
    stub(datasets_move, "pydatasets$mv", m)

    # arguments: source_path, destination_path, project_id=NULL

    expect_null(datasets_move("test-source", "test-destination"))
    expect_args(m, 1, "test-source", "test-destination", NULL)

    expect_null(datasets_move("test-source", "test-destination", "test-project-id"))
    expect_args(m, 2, "test-source", "test-destination", "test-project-id")
  }
)

test_that(
  "delete calls datasets.rm correctly",
  {
    m <- mock(NULL, cycle=TRUE)
    stub(datasets_delete, "pydatasets$rm", m)

    # arguments: source_path, destination_path, project_id=NULL

    expect_null(datasets_delete("test-source"))
    expect_args(m, 1, "test-source", NULL)

    expect_null(datasets_delete("test-source", "test-project-id"))
    expect_args(m, 2, "test-source", "test-project-id")
  }
)

test_that(
  "etag calls datasets.etag correctly",
  {
    m <- mock("etag-1", "etag-2")
    stub(datasets_etag, "pydatasets$etag", m)

    expect_equal(datasets_etag("test-file"), "etag-1")
    expect_args(m, 1, "test-file", NULL)

    expect_equal(datasets_etag("test-file", "test-project-id"), "etag-2")
    expect_args(m, 2, "test-file", "test-project-id")
  }
)

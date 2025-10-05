test_that(".run_worker_task successfully executes a task", {
  # Mock the global functions that the worker will call
  mock_file <- function(...) { "file_mock" }
  mock_readRDS <- function(...) { quote(2 + 2) }
  mock_close <- function(...) { }
  
  saved_object <- NULL
  mock_saveRDS <- function(object, file) {
    saved_object <<- object
  }
  
  # Use with_mocked_bindings to temporarily replace these functions
  testthat::with_mocked_bindings(
    .run_worker_task(),
    file = mock_file,
    readRDS = mock_readRDS,
    close = mock_close,
    saveRDS = mock_saveRDS,
    eval = base::eval, # Make sure we use the real eval
    .package = "base"
  )
  
  # The task was quote(2 + 2), so the result should be 4
  expect_equal(saved_object$value, 4)
})

test_that(".run_worker_task captures and returns an error", {
  mock_readRDS <- function(...) { quote(stop("test error")) }
  saved_object <- NULL
  mock_saveRDS <- function(object, file) { saved_object <<- object }
  
  # We don't expect an error here, the worker should catch it
  expect_no_error(
    testthat::with_mocked_bindings(
      .run_worker_task(),
      file = function(...) "file_mock",
      readRDS = mock_readRDS,
      close = function(...) { },
      saveRDS = mock_saveRDS,
      .package = "base"
    )
  )
  
  # The saved object should be the error itself
  expect_s3_class(saved_object, "error")
  expect_equal(saved_object$message, "test error")
})


# --- .handle_proxy_task tests ---

# Mock processx::process object
MockProcess <- R6::R6Class("MockProcess",
  public = list(
    initialize = function(...) { invisible(self) },
    write_input = function(...) { invisible(self) },
    close_input = function(...) { invisible(self) },
    wait = function(...) { invisible(self) },
    get_exit_status = function() { self$.exit_status },
    read_output_raw = function() { self$.output },
    read_all_stderr_lines = function() { self$.stderr },
    .exit_status = 0,
    .output = raw(),
    .stderr = character()
  )
)

test_that(".handle_proxy_task successfully processes a task", {
  # Create a mock process that will succeed
  mock_px <- MockProcess$new()
  mock_px$.exit_status <- 0
  # The output should be a serialized list(value = ...)
  con <- rawConnection(raw(0), "w")
  saveRDS(list(value = 123), con)
  mock_px$.output <- rawConnectionValue(con)
  close(con)

  # Mock processx::process$new to return our mock object
  mock_process_new <- function(...) { mock_px }

  result <- testthat::with_mocked_bindings(
    gpumux:::.handle_proxy_task(task = quote(1), worker_script_path = "worker.R"),
    process = list(new = mock_process_new),
    .package = "processx"
  )

  expect_equal(result, 123)
})
test_that(".handle_proxy_task handles worker execution error", {
  mock_px <- MockProcess$new()
  mock_px$.exit_status <- 1
  mock_px$.stderr <- "Worker crashed"
  
  mock_process_new <- function(...) { mock_px }
  
  expect_error(
    testthat::with_mocked_bindings(
      gpumux:::.handle_proxy_task(task = quote(1), worker_script_path = "worker.R"),
      process = list(new = mock_process_new),
      .package = "processx"
    ),
    "Ephemeral worker failed. Stderr: Worker crashed"
  )
})

test_that(".handle_proxy_task handles worker producing no output", {
  mock_px <- MockProcess$new()
  mock_px$.exit_status <- 0
  mock_px$.output <- raw() # Empty output
  mock_px$.stderr <- "Something weird happened"
  
  mock_process_new <- function(...) { mock_px }
  
  expect_error(
    testthat::with_mocked_bindings(
      gpumux:::.handle_proxy_task(task = quote(1), worker_script_path = "worker.R"),
      process = list(new = mock_process_new),
      .package = "processx"
    ),
    "Ephemeral worker produced no output. Stderr: Something weird happened"
  )
})

test_that(".handle_proxy_task propagates error from worker", {
  mock_px <- MockProcess$new()
  mock_px$.exit_status <- 0
  # The output is a serialized error object
  con <- rawConnection(raw(0), "w")
  saveRDS(simpleError("worker error"), con)
  mock_px$.output <- rawConnectionValue(con)
  close(con)

  mock_process_new <- function(...) { mock_px }

  expect_error(
    testthat::with_mocked_bindings(
      gpumux:::.handle_proxy_task(task = quote(1), worker_script_path = "worker.R"),
      process = list(new = mock_process_new),
      .package = "processx"
    ),
    "worker error"
  )
})

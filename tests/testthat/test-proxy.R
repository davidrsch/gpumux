test_that(".launch_proxy_daemons generates correct mirai config", {
  # Mock dependencies
  mock_writeLines <- function(...) { }
  mock_tempfile <- function(...) "temp_file.R"
  mock_reg.finalizer <- function(...) { }
  daemons_list_arg <- NULL
  mock_mirai_daemons <- function(n, .list) {
    daemons_list_arg <<- .list
    # Return dummy daemons
    lapply(1:n, function(i) structure(list(), class = "daemon"))
  }

  # Sample gpu_status
  gpu_status <- data.frame(
    gpu_id = c(0, 1),
    vendor = "nvidia",
    name = "TEST GPU",
    workers_allocated = c(2, 1),
    memory_total_mb = 8192
  )

  # Use with_mocked_bindings to replace the real functions
  testthat::with_mocked_bindings(
    writeLines = mock_writeLines,
    tempfile = mock_tempfile,
    reg.finalizer = mock_reg.finalizer,
    .package = "base",
    code = {
      testthat::with_mocked_bindings(
        daemons = mock_mirai_daemons,
        .package = "mirai",
        code = {
          gpumux:::.launch_proxy_daemons(gpu_status)

          # --- Assertions ---
          # 3 daemons in total (2 for GPU 0, 1 for GPU 1)
          expect_length(daemons_list_arg, 3)

          # Check config for GPU 0
          expect_equal(daemons_list_arg[[1]]$env$CUDA_VISIBLE_DEVICES, "0")
          expect_equal(daemons_list_arg[[2]]$env$CUDA_VISIBLE_DEVICES, "0")
          expect_equal(
            daemons_list_arg[[1]]$Rscript_args,
            c("--vanilla", "temp_file.R", "temp_file.R")
          )

          # Check config for GPU 1
          expect_equal(daemons_list_arg[[3]]$env$CUDA_VISIBLE_DEVICES, "1")
        }
      )
    }
  )
})

test_that(".launch_proxy_daemons handles no workers allocated", {
  mock_writeLines <- function(...) { }
  mock_tempfile <- function(...) "temp_file.R"
  mock_reg.finalizer <- function(...) { }
  daemons_list_arg <- NULL
  mock_mirai_daemons <- function(n, .list) {
    daemons_list_arg <<- .list
    lapply(1:n, function(i) structure(list(), class = "daemon"))
  }

  gpu_status <- data.frame(
    gpu_id = c(0, 1),
    vendor = "nvidia",
    name = "TEST GPU",
    workers_allocated = c(0, 0),
    memory_total_mb = 8192
  )

  testthat::with_mocked_bindings(
    writeLines = mock_writeLines,
    tempfile = mock_tempfile,
    reg.finalizer = mock_reg.finalizer,
    .package = "base",
    code = {
      testthat::with_mocked_bindings(
        daemons = mock_mirai_daemons,
        .package = "mirai",
        code = {
          gpumux:::.launch_proxy_daemons(gpu_status)
          expect_length(daemons_list_arg, 0)
        }
      )
    }
  )
})

test_that(".launch_proxy_daemons stops for unsupported vendor", {
  gpu_status <- data.frame(
    gpu_id = 0,
    vendor = "amd", # unsupported
    name = "TEST GPU",
    workers_allocated = 1,
    memory_total_mb = 8192
  )

  expect_error(
    gpumux:::.launch_proxy_daemons(gpu_status),
    "Unsupported GPU vendor: amd"
  )
})

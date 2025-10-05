library(mirai)
#--- gpu_daemons() input validation ----

test_that("gpu_daemons stops with invalid inputs", {
  # Missing n_workers
  expect_error(
    gpumux::gpu_daemons(gpu_ids = 0, memory_per_worker_mb = 1024),
    "`n_workers` must be a non-negative integer."
  )

  # Non-numeric inputs
  expect_error(
    gpumux::gpu_daemons(n_workers = 1, memory_per_worker_mb = "a"),
    "`memory_per_worker_mb` must be a positive integer when creating workers."
  )
  expect_error(
    gpumux::gpu_daemons(n_workers = 1, gpu_ids = "a", memory_per_worker_mb = 1),
    "`gpu_ids` must be numeric"
  )
})

#--- gpu_daemons() core logic with mocking ----

# A fake list_gpus() function to return predictable test data
mock_list_gpus <- function(gpus) {
  function() {
    gpus
  }
}

# A fake mirai::daemons() to inspect the generated config
mock_mirai_daemons <- function(n, .list) {
  # Return a list of dummy daemon objects for the proactive initialization
  lapply(1:n, function(i) {
    structure(.list[[i]], class = c("daemon", "mirai_cluster"))
  })
}

test_that("correctly allocates workers for a single GPU", {
  mock_gpus <- data.frame(
    gpu_id = 0,
    vendor = "nvidia",
    name = "TEST GPU 0",
    memory_total_mb = 8192,
    memory_free_mb = 7000
  )

  testthat::with_mocked_bindings(
    code = {
      testthat::with_mocked_bindings(
        code = {
          # Capture result
          daemons <- gpumux::gpu_daemons(
            n_workers = 3,
            gpu_ids = 0,
            memory_per_worker_mb = 2000,
            reserve_memory_mb = 1000
          )

          expect_length(daemons, 3)
          expect_equal(daemons[[1]]$env$CUDA_VISIBLE_DEVICES, "0")
          expect_equal(daemons[[3]]$env$CUDA_VISIBLE_DEVICES, "0")
        },
        daemons = mock_mirai_daemons,
        .package = "mirai"
      )
    },
    list_gpus = mock_list_gpus(mock_gpus),
    .package = "gpumux"
  )
})

test_that("correctly allocates workers for multiple GPUs", {
  mock_gpus <- data.frame(
    gpu_id = c(0, 1),
    vendor = c("nvidia", "nvidia"),
    name = c("TEST GPU 0", "TEST GPU 1"),
    memory_total_mb = c(8192, 4096),
    memory_free_mb = c(7000, 4000)
  )

  testthat::with_mocked_bindings(
    code = {
      testthat::with_mocked_bindings(
        code = {
          # GPU 0: 7000 free, reserve 1000 -> 6000 avail. 3 workers @ 2000MB.
          # GPU 1: 4000 free, reserve 500 -> 3500 avail. 3 workers @ 1000MB.
          daemons <- gpumux::gpu_daemons(
            n_workers = 6,
            gpu_ids = c(0, 1),
            memory_per_worker_mb = c(2000, 1000),
            reserve_memory_mb = c(1000, 500)
          )

          expect_length(daemons, 6) # 3 from GPU 0, 3 from GPU 1
          # Check configs for GPU 0
          expect_equal(daemons[[1]]$env$CUDA_VISIBLE_DEVICES, "0")
          expect_equal(daemons[[3]]$env$CUDA_VISIBLE_DEVICES, "0")
          # Check configs for GPU 1
          expect_equal(daemons[[4]]$env$CUDA_VISIBLE_DEVICES, "1")
          expect_equal(daemons[[6]]$env$CUDA_VISIBLE_DEVICES, "1")
        },
        daemons = mock_mirai_daemons,
        .package = "mirai"
      )
    },
    list_gpus = mock_list_gpus(mock_gpus),
    .package = "gpumux"
  )
})

test_that("handles insufficient memory and missing GPUs", {
  mock_gpus <- data.frame(
    gpu_id = 0,
    vendor = "nvidia",
    name = "TEST GPU 0",
    memory_total_mb = 8192,
    memory_free_mb = 1500
  )

  testthat::with_mocked_bindings(
    code = {
      # Test case: GPU ID doesn't exist
      expect_error(
        gpumux::gpu_daemons(
          n_workers = 1,
          gpu_ids = 99,
          memory_per_worker_mb = 1024
        ),
        "None of the specified `gpu_ids` were found"
      )

      # Test case: GPU exists but has not enough memory
      expect_error(
        gpumux::gpu_daemons(
          n_workers = 2,
          gpu_ids = 0,
          memory_per_worker_mb = 1024,
          reserve_memory_mb = 512 # 1500 - 512 = 988 avail < 2*1024 needed
        ),
        "Not enough free memory"
      )
    },
    list_gpus = mock_list_gpus(mock_gpus),
    .package = "gpumux"
  )
})

test_that("proactive initialization is triggered for frameworks", {
  mock_gpus <- data.frame(
    gpu_id = 0,
    vendor = "nvidia",
    name = "TEST GPU 0",
    memory_total_mb = 8192,
    memory_free_mb = 7000
  )

  # Create a mock for everywhere that sets a flag
  everywhere_called <- FALSE
  mock_everywhere <- function(...) { everywhere_called <<- TRUE; invisible(NULL) }

  testthat::with_mocked_bindings(
    list_gpus = mock_list_gpus(mock_gpus),
    everywhere = mock_everywhere,
    .package = "gpumux",
    code = {
      testthat::with_mocked_bindings(
        daemons = mock_mirai_daemons,
        .package = "mirai",
        code = {
          # Run with framework = "tensorflow"
          gpumux::gpu_daemons(
            n_workers = 1,
            gpu_ids = 0,
            memory_per_worker_mb = 1024,
            framework = "tensorflow"
          )
          # Check that our mock function was called
          expect_true(everywhere_called, info = "mirai::everywhere() should be called for initialization")
        }
      )
    }
  )
})

test_that("terminates daemons when n_workers is 0", {
  daemons_called_with <- NULL
  mock_mirai_daemons_for_termination <- function(n) {
    daemons_called_with <<- n
  }

  testthat::with_mocked_bindings(
    code = {
      gpumux::gpu_daemons(n_workers = 0)
      expect_equal(daemons_called_with, 0)
    },
    daemons = mock_mirai_daemons_for_termination,
    .package = "mirai"
  )
})

test_that("stops if no GPUs are detected", {
  mock_gpus <- data.frame() # No GPUs
  testthat::with_mocked_bindings(
    code = {
      expect_error(
        gpumux::gpu_daemons(n_workers = 1, memory_per_worker_mb = 1024),
        "No GPUs detected by `list_gpus()`.",
        fixed = TRUE
      )
    },
    list_gpus = mock_list_gpus(mock_gpus),
    .package = "gpumux"
  )
})

test_that("correctly allocates remaining workers", {
  mock_gpus <- data.frame(
    gpu_id = c(0, 1),
    vendor = c("nvidia", "nvidia"),
    name = c("TEST GPU 0", "TEST GPU 1"),
    memory_total_mb = c(8192, 8192),
    memory_free_mb = c(7000, 7000)
  )

  # 3 workers on 2 GPUs -> GPU 0 gets 2, GPU 1 gets 1
  testthat::with_mocked_bindings(
    code = {
      testthat::with_mocked_bindings(
        code = {
          daemons <- gpumux::gpu_daemons(
            n_workers = 3,
            gpu_ids = c(0, 1),
            memory_per_worker_mb = 1024
          )
          expect_length(daemons, 3)
          expect_equal(daemons[[1]]$env$CUDA_VISIBLE_DEVICES, "0")
          expect_equal(daemons[[2]]$env$CUDA_VISIBLE_DEVICES, "0")
          expect_equal(daemons[[3]]$env$CUDA_VISIBLE_DEVICES, "1")
        },
        daemons = mock_mirai_daemons,
        .package = "mirai"
      )
    },
    list_gpus = mock_list_gpus(mock_gpus),
    .package = "gpumux"
  )
})

test_that("proxy worker type calls .launch_proxy_daemons", {
  mock_gpus <- data.frame(
    gpu_id = 0,
    vendor = "nvidia",
    name = "TEST GPU 0",
    memory_total_mb = 8192,
    memory_free_mb = 7000
  )

  proxy_launched <- FALSE
  mock_launch_proxy <- function(...) {
    proxy_launched <<- TRUE
  }

  testthat::with_mocked_bindings(
    list_gpus = mock_list_gpus(mock_gpus),
    .launch_proxy_daemons = mock_launch_proxy,
    .package = "gpumux",
    code = {
      gpumux::gpu_daemons(
        n_workers = 1,
        gpu_ids = 0,
        memory_per_worker_mb = 1024,
        worker_type = "proxy"
      )
      expect_true(proxy_launched, info = ".launch_proxy_daemons should be called for worker_type='proxy'")
    }
  )
})

# --- .get_tensorflow_startup_expr() tests ----

test_that(".get_tensorflow_startup_expr warns if tensorflow is not installed", {
  expr <- gpumux:::.get_tensorflow_startup_expr(1024)
  test_env <- new.env(parent = baseenv())
  test_env$requireNamespace <- function(...) FALSE
  test_env$message <- function(...) {}

  warnings <- testthat::capture_warnings(eval(expr, envir = test_env))

  expect_true(any(grepl("`tensorflow` package not found", warnings)))
})

test_that(".get_tensorflow_startup_expr warns if no GPUs are visible", {
  expr <- gpumux:::.get_tensorflow_startup_expr(1024)
  test_env <- new.env(parent = baseenv())
  test_env$requireNamespace <- function(...) TRUE
  test_env$message <- function(...) {}
  test_env$Sys.setenv <- function(...) {}
  test_env$tf <- list(
    config = list(
      list_physical_devices = function(...) list()
    )
  )

  warnings <- testthat::capture_warnings(eval(expr, envir = test_env))

  expect_true(any(grepl("TensorFlow worker sees no GPUs", warnings)))
})

test_that(".get_tensorflow_startup_expr succeeds and sets memory limit", {
  expr <- gpumux:::.get_tensorflow_startup_expr(1024)
  test_env <- new.env(parent = baseenv())
  test_env$requireNamespace <- function(...) TRUE
  test_env$warning <- function(...) {}
  test_env$Sys.setenv <- function(...) {}
  test_env$Sys.getenv <- function(...) "0"

  test_env$set_config_called <- FALSE
  test_env$tf <- list(
    config = list(
      list_physical_devices = function(...) list("GPU_OBJECT"),
      set_logical_device_configuration = function(...) {
        test_env$set_config_called <- TRUE
      },
      LogicalDeviceConfiguration = function(...) "LOGICAL_DEVICE_CONFIG"
    )
  )

  messages <- testthat::capture_messages(eval(expr, envir = test_env))

  expect_true(test_env$set_config_called)
  expect_true(any(grepl("SUCCESS", messages)))
})

test_that(".get_tensorflow_startup_expr handles errors during configuration", {
  expr <- gpumux:::.get_tensorflow_startup_expr(1024)
  test_env <- new.env(parent = baseenv())
  test_env$requireNamespace <- function(...) TRUE
  test_env$message <- function(...) {}
  test_env$Sys.setenv <- function(...) {}
  test_env$tf <- list(
    config = list(
      list_physical_devices = function(...) list("GPU_OBJECT"),
      set_logical_device_configuration = function(...) stop("test error"),
      LogicalDeviceConfiguration = function(...) "LOGICAL_DEVICE_CONFIG"
    )
  )

  warnings <- testthat::capture_warnings(eval(expr, envir = test_env))

  expect_true(any(grepl("Failed to set TensorFlow memory limit", warnings)))
  expect_true(any(grepl("test error", warnings)))
})

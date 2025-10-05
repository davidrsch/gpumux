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
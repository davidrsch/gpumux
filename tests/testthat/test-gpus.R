mock_system2_success <- function(...) {
  # A minimal nvidia-smi -q -x output with two GPUs
  r"(<?xml version="1.0" ?>
<nvidia_smi_log>
  <gpu>
    <product_name>NVIDIA GeForce RTX 3090</product_name>
    <minor_number>0</minor_number>
    <fb_memory_usage>
      <total>24576 MiB</total>
      <free>20000 MiB</free>
    </fb_memory_usage>
  </gpu>
  <gpu>
    <product_name>NVIDIA GeForce RTX 3080</product_name>
    <minor_number>1</minor_number>
    <fb_memory_usage>
      <total>10240 MiB</total>
      <free>8000 MiB</free>
    </fb_memory_usage>
  </gpu>
</nvidia_smi_log>)"
}

mock_system2_fail <- function(...) {
  stop("system2 failed")
}

mock_sys_which_fail <- function(...) {
  ""
}

test_that("list_gpus works with successful nvidia-smi call", {
  testthat::with_mocked_bindings(
    system2 = mock_system2_success,
    Sys.which = function(...) "/usr/bin/nvidia-smi",
    .package = "base",
    code = {
      gpus <- gpumux::list_gpus()
      expect_equal(nrow(gpus), 2)
      expect_equal(gpus$gpu_id, c(0, 1))
      expect_equal(gpus$vendor, c("nvidia", "nvidia"))
      expect_equal(
        gpus$name,
        c("NVIDIA GeForce RTX 3090", "NVIDIA GeForce RTX 3080")
      )
      expect_equal(gpus$memory_total_mb, c(24576, 10240))
      expect_equal(gpus$memory_free_mb, c(20000, 8000))
    }
  )
})

test_that(".list_nvidia_gpus handles nvidia-smi not found", {
  testthat::with_mocked_bindings(
    Sys.which = mock_sys_which_fail,
    .package = "base",
    code = {
      # Should return NULL, which list_gpus turns into an empty data frame
      gpus <- gpumux::list_gpus()
      expect_equal(nrow(gpus), 0)
    }
  )
})

test_that("list_gpus returns empty data.frame when no vendors work", {
  testthat::with_mocked_bindings(
    .list_nvidia_gpus = function() NULL,
    .package = "gpumux",
    code = {
      gpus <- gpumux::list_gpus()
      expect_equal(nrow(gpus), 0)
      expect_s3_class(gpus, "data.frame")
    }
  )
})


test_that(".list_nvidia_gpus handles system2 error", {
  testthat::with_mocked_bindings(
    system2 = mock_system2_fail,
    Sys.which = function(...) "/usr/bin/nvidia-smi",
    .package = "base",
    code = {
      # Should return NULL, which list_gpus turns into an empty data frame
      gpus <- gpumux::list_gpus()
      expect_equal(nrow(gpus), 0)
    }
  )
})

test_that(".list_nvidia_gpus handles empty XML", {
  mock_system2_empty <- function(...) {
    '<?xml version="1.0" ?><nvidia_smi_log></nvidia_smi_log>'
  }
  testthat::with_mocked_bindings(
    system2 = mock_system2_empty,
    Sys.which = function(...) "/usr/bin/nvidia-smi",
    .package = "base",
    code = {
      gpus <- gpumux::list_gpus()
      expect_equal(nrow(gpus), 0)
    }
  )
})

test_that(".list_nvidia_gpus handles XML with no gpu tags", {
  mock_system2_no_gpu <- function(...) {
    '<?xml version="1.0" ?><nvidia_smi_log><attached_gpus>0</attached_gpus></nvidia_smi_log>'
  }
  testthat::with_mocked_bindings(
    system2 = mock_system2_no_gpu,
    Sys.which = function(...) "/usr/bin/nvidia-smi",
    .package = "base",
    code = {
      gpus <- gpumux::list_gpus()
      expect_equal(nrow(gpus), 0)
    }
  )
})

test_that(".list_nvidia_gpus handles empty memory value", {
  mock_system2_empty_mem <- function(...) {
    r"(<?xml version="1.0" ?>
<nvidia_smi_log>
  <gpu>
    <product_name>NVIDIA GeForce RTX 3090</product_name>
    <minor_number>0</minor_number>
    <fb_memory_usage>
      <total></total>
      <free>20000 MiB</free>
    </fb_memory_usage>
  </gpu>
</nvidia_smi_log>)"
  }
  testthat::with_mocked_bindings(
    system2 = mock_system2_empty_mem,
    Sys.which = function(...) "/usr/bin/nvidia-smi",
    .package = "base",
    code = {
      gpus <- gpumux::list_gpus()
      expect_equal(nrow(gpus), 1)
      expect_true(is.na(gpus$memory_total_mb))
      expect_equal(gpus$memory_free_mb, 20000)
    }
  )
})

test_that(".list_nvidia_gpus handles system2 warning", {
  mock_system2_warning <- function(...) {
    warning("test warning")
  }
  testthat::with_mocked_bindings(
    system2 = mock_system2_warning,
    Sys.which = function(...) "/usr/bin/nvidia-smi",
    .package = "base",
    code = {
      gpus <- gpumux::list_gpus()
      expect_equal(nrow(gpus), 0)
    }
  )
})

# --- AMD ROCm tests ---

mock_rocm_success_two_gpus <- function(...) {
  paste(
    "GPU[0] : Radeon PRO W6800",
    "GPU[0] : VRAM Total Memory (B): 17179869184",
    "GPU[0] : VRAM Total Used (B): 2147483648",
    "GPU[1] : Radeon PRO W6600",
    "GPU[1] : VRAM Total Memory (MiB): 8192",
    "GPU[1] : VRAM Total Used (MiB): 1024",
    sep = "\n"
  )
}

test_that("list_gpus works with successful rocm-smi call", {
  testthat::with_mocked_bindings(
    .list_nvidia_gpus = function() data.frame(),
    .package = "gpumux",
    code = {
      testthat::with_mocked_bindings(
        system2 = mock_rocm_success_two_gpus,
        Sys.which = function(cmd, ...) {
          if (cmd == "rocm-smi") "/opt/rocm/bin/rocm-smi" else ""
        },
        .package = "base",
        code = {
          gpus <- gpumux::list_gpus()
          expect_equal(nrow(gpus), 2)
          expect_equal(gpus$gpu_id, c(0, 1))
          expect_equal(gpus$vendor, c("amd", "amd"))
          expect_equal(gpus$name, c("Radeon PRO W6800", "Radeon PRO W6600"))
          # First GPU: bytes -> MiB: 17179869184 B = 16384 MiB, used 2048 MiB -> free 14336
          expect_equal(round(gpus$memory_total_mb[1]), 16384)
          expect_equal(round(gpus$memory_free_mb[1]), 14336)
          # Second GPU: MiB direct: total 8192, used 1024 -> free 7171? actually 8192-1024=7168
          expect_equal(gpus$memory_total_mb[2], 8192)
          expect_equal(gpus$memory_free_mb[2], 7168)
        }
      )
    }
  )
})

test_that(".list_amd_gpus handles rocm-smi not found", {
  testthat::with_mocked_bindings(
    .list_nvidia_gpus = function() data.frame(),
    .package = "gpumux",
    code = {
      testthat::with_mocked_bindings(
        Sys.which = function(cmd, ...) if (cmd == "rocm-smi") "" else "",
        .package = "base",
        code = {
          gpus <- gpumux::list_gpus()
          expect_equal(nrow(gpus), 0)
        }
      )
    }
  )
})

test_that(".list_amd_gpus handles system2 error and warning", {
  testthat::with_mocked_bindings(
    .list_nvidia_gpus = function() data.frame(),
    .package = "gpumux",
    code = {
      # Error case
      testthat::with_mocked_bindings(
        system2 = function(...) stop("fail"),
        Sys.which = function(cmd, ...) {
          if (cmd == "rocm-smi") "/opt/rocm/bin/rocm-smi" else ""
        },
        .package = "base",
        code = {
          gpus <- gpumux::list_gpus()
          expect_equal(nrow(gpus), 0)
        }
      )
      # Warning case
      testthat::with_mocked_bindings(
        system2 = function(...) warning("warn"),
        Sys.which = function(cmd, ...) {
          if (cmd == "rocm-smi") "/opt/rocm/bin/rocm-smi" else ""
        },
        .package = "base",
        code = {
          gpus <- gpumux::list_gpus()
          expect_equal(nrow(gpus), 0)
        }
      )
    }
  )
})

test_that(".list_amd_gpus handles missing memory value", {
  mock_rocm_missing <- function(...) {
    paste(
      "GPU[0] : Radeon PRO W6800",
      # Missing total line
      "GPU[0] : VRAM Total Used (B): 2147483648",
      sep = "\n"
    )
  }
  testthat::with_mocked_bindings(
    .list_nvidia_gpus = function() data.frame(),
    .package = "gpumux",
    code = {
      testthat::with_mocked_bindings(
        system2 = mock_rocm_missing,
        Sys.which = function(cmd, ...) {
          if (cmd == "rocm-smi") "/opt/rocm/bin/rocm-smi" else ""
        },
        .package = "base",
        code = {
          gpus <- gpumux::list_gpus()
          expect_equal(nrow(gpus), 1)
          expect_true(is.na(gpus$memory_total_mb))
          expect_true(is.na(gpus$memory_free_mb))
        }
      )
    }
  )
})

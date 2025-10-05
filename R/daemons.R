#' Create a mirai daemons object for GPU workers
#'
#' This function intelligently creates a `mirai` daemons object by allocating
#' workers to specified GPUs based on available VRAM.
#'
#' @param gpu_ids A numeric vector of GPU IDs to use (e.g., `0`, `c(0, 1)`).
#' @param n_workers The total number of workers to create across all specified GPUs.
#'   To terminate all daemons, set `n_workers = 0`.
#' @param memory_per_worker_mb The amount of VRAM in MB to allocate for each worker.
#'   This is used for both capacity planning and, if a supported `framework` is
#'   chosen, for setting a memory limit within the worker.
#' @param reserve_memory_mb A numeric value for the VRAM to reserve on each GPU in MB.
#' @param framework A character string specifying the ML/AI framework to be used
#'   by the workers. Currently supported: `"none"` (default), `"tensorflow"`.
#'   If a supported framework is specified, `gpumux` will automatically configure
#'   the worker to respect the `memory_per_worker_mb` limit.

#' @return A `mirai` daemons object, ready to be used with `mirai::mirai()`.
#' @importFrom mirai collect_mirai everywhere
#' @export
#' @param worker_type A character string specifying the daemon strategy.
#'   `"persistent"` (the default) creates long-lived daemons that execute many
#'   tasks, offering high performance. `"proxy"` creates daemons that spawn a
#'   new, clean worker process for each task, offering maximum stability and
#'   guaranteed memory cleanup at the cost of performance overhead.
gpu_daemons <- function(
  gpu_ids = 0,
  n_workers = NULL,
  memory_per_worker_mb = NULL,
  reserve_memory_mb = 1024,
  framework = "none",
  worker_type = "persistent"
) {
  # --- Input Validation ---
  worker_type <- match.arg(worker_type, c("persistent", "proxy"))

  if (is.null(n_workers) || !is.numeric(n_workers) || n_workers < 0) {
    stop("`n_workers` must be a non-negative integer.", call. = FALSE)
  }
  if (!is.numeric(gpu_ids)) {
    stop("`gpu_ids` must be numeric", call. = FALSE)
  }

  # Handle daemon termination first
  if (n_workers == 0) {
    message("Terminating all mirai daemons.")
    mirai::daemons(0)
    return(invisible(NULL))
  }

  # Validation for worker creation
  if (
    is.null(memory_per_worker_mb) ||
      !is.numeric(memory_per_worker_mb) ||
      any(memory_per_worker_mb <= 0)
  ) {
    stop(
      "`memory_per_worker_mb` must be a positive integer when creating workers.",
      call. = FALSE
    )
  }

  # --- Core Logic ---
  gpu_status <- list_gpus()
  if (nrow(gpu_status) == 0) {
    stop("No GPUs detected by `list_gpus()`.", call. = FALSE)
  }

  # Filter for the gpus that we want to use
  gpu_status <- gpu_status[gpu_status$gpu_id %in% gpu_ids, ]
  if (nrow(gpu_status) == 0) {
    stop("None of the specified `gpu_ids` were found.", call. = FALSE)
  }

  # Distribute workers
  base_workers <- floor(n_workers / nrow(gpu_status))
  remaining_workers <- n_workers %% nrow(gpu_status)
  workers_per_gpu <- rep(base_workers, nrow(gpu_status))
  if (remaining_workers > 0) {
    workers_per_gpu[1:remaining_workers] <- workers_per_gpu[
      1:remaining_workers
    ] +
      1
  }
  gpu_status$workers_allocated <- workers_per_gpu

  # Memory check
  total_memory_needed <- gpu_status$workers_allocated * memory_per_worker_mb
  if (
    any(total_memory_needed > gpu_status$memory_free_mb - reserve_memory_mb)
  ) {
    stop(
      "Not enough free memory on one or more GPUs for the requested number of workers.",
      call. = FALSE
    )
  }

  # --- Launch Daemons ---
  if (worker_type == "persistent") {
    # --- Daemon Configuration (Persistent) ---
    all_daemon_configs <- list()
    for (i in 1:nrow(gpu_status)) {
      gpu_info <- gpu_status[i, ]
      num_workers_on_gpu <- gpu_info$workers_allocated

      if (num_workers_on_gpu > 0) {
        # 1. Get vendor-specific device isolation variable
        device_env_var <- switch(
          gpu_info$vendor,
          nvidia = "CUDA_VISIBLE_DEVICES",
          stop("Unsupported GPU vendor: ", gpu_info$vendor)
        )

        # 2. Create config for each worker on this GPU
        gpu_daemon_configs <- lapply(1:num_workers_on_gpu, function(x) {
          config <- list()
          config$env <- list()
          config$env[[device_env_var]] <- as.character(gpu_info$gpu_id)
          config
        })
        all_daemon_configs <- c(all_daemon_configs, gpu_daemon_configs)
      }
    }

    # --- Launch Daemons & Proactively Initialize ---
    message("GPU Allocation Summary (Persistent Workers):")
    print(tibble::as_tibble(gpu_status[, c(
      "gpu_id",
      "vendor",
      "name",
      "workers_allocated",
      "memory_total_mb"
    )]))

    message("Total workers to be created: ", length(all_daemon_configs))
    daemons <- mirai::daemons(
      n = length(all_daemon_configs),
      .list = all_daemon_configs
    )

    # Proactive initialization if a framework is specified
    if (framework != "none") {
      message(paste(
        "Proactively initializing",
        length(all_daemon_configs),
        "workers for framework:",
        framework
      ))

      startup_expr <- switch(
        framework,
        tensorflow = .get_tensorflow_startup_expr(memory_per_worker_mb),
        stop("Unsupported framework: ", framework)
      )

      # Dispatch the same setup task to all workers
      everywhere(startup_expr)
      message("All workers initialized.")
    }

    return(daemons)

  } else if (worker_type == "proxy") {
    # --- Launch Daemons (Proxy) ---
    .launch_proxy_daemons(gpu_status = gpu_status)
  }
}


# --- Framework-specific helper functions ---

.get_tensorflow_startup_expr <- function(memory_limit) {
  substitute(
    expr = {
      message("gpumux: STARTUP SCRIPT EXECUTING...")
      Sys.setenv(TF_FORCE_GPU_ALLOW_GROWTH = "true")
      if (!requireNamespace("tensorflow", quietly = TRUE)) {
        warning(
          "gpumux: `tensorflow` package not found in worker, cannot set memory limit."
        )
      } else {
        gpus <- tensorflow::tf$config$list_physical_devices("GPU")
        if (length(gpus) > 0) {
          tryCatch(
            {
              tensorflow::tf$config$set_logical_device_configuration(
                gpus[[1]],
                list(tensorflow::tf$config$LogicalDeviceConfiguration(
                  memory_limit = mem_limit
                ))
              )
              message(paste(
                "gpumux: SUCCESS - TensorFlow worker on GPU",
                Sys.getenv("CUDA_VISIBLE_DEVICES"),
                "memory limit set to",
                mem_limit,
                "MB"
              ))
            },
            error = function(e) {
              warning(paste(
                "gpumux: ERROR - Failed to set TensorFlow memory limit on worker:",
                e$message
              ))
            }
          )
        } else {
          warning("gpumux: ERROR - TensorFlow worker sees no GPUs.")
        }
      }
    },
    env = list(mem_limit = memory_limit)
  )
}

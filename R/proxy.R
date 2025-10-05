.launch_proxy_daemons <- function(gpu_status, ...) {
  # All logic is now self-contained in this function, using rlang to build
  # the scripts dynamically. This avoids file path issues and is more robust.

  # --- 1. Define the Ephemeral Worker Logic as an Expression ---
  # This now simply calls the testable helper function.
  worker_expr <- rlang::expr({
    gpumux:::.run_worker_task()
  })

  # --- 2. Define the Proxy Loop Logic as an Expression ---
  # This now simply calls the testable helper function.
  proxy_loop_expr <- rlang::expr({
    # The worker script is passed in as a command-line argument.
    worker_script_path <- commandArgs(trailingOnly = TRUE)[1]

    # The main task handler that mirai will call.
    .proxy_task_handler <- function(task) {
      gpumux:::.handle_proxy_task(task, worker_script_path)
    }

    # Return the handler for mirai to use.
    .proxy_task_handler
  })

  # --- 3. Create and Launch Daemons ---

  # Create temporary, session-specific files for the scripts.
  worker_script_file <- tempfile(fileext = ".R")
  proxy_script_file <- tempfile(fileext = ".R")

  # Write the expressions into the temp files.
  # The worker script needs to load the package to find the helper.
  worker_script_content <- paste(
    "library(gpumux)",
    rlang::expr_deparse(worker_expr),
    sep = "\n"
  )
  proxy_script_content <- paste(
    "library(gpumux)",
    rlang::expr_deparse(proxy_loop_expr),
    sep = "\n"
  )
  writeLines(worker_script_content, worker_script_file)
  writeLines(proxy_script_content, proxy_script_file)


  # Ensure temp files are cleaned up when the R session ends.
  reg.finalizer(globalenv(), function(e) {
    unlink(c(worker_script_file, proxy_script_file))
  }, onexit = TRUE)

  # Create daemon configurations.
  all_daemon_configs <- list()
  for (i in 1:nrow(gpu_status)) {
    gpu_info <- gpu_status[i, ]
    num_workers_on_gpu <- gpu_info$workers_allocated

    if (num_workers_on_gpu > 0) {
      device_env_var <- switch(
        gpu_info$vendor,
        nvidia = "CUDA_VISIBLE_DEVICES",
        stop("Unsupported GPU vendor: ", gpu_info$vendor)
      )

      gpu_daemon_configs <- lapply(1:num_workers_on_gpu, function(x) {
        config <- list()
        config$env <- list()
        config$env[[device_env_var]] <- as.character(gpu_info$gpu_id)
        # Tell mirai to run the proxy script, passing the worker script's path.
        config$Rscript_args <- c("--vanilla", proxy_script_file, worker_script_file)
        config
      })
      all_daemon_configs <- c(all_daemon_configs, gpu_daemon_configs)
    }
  }

  # Launch the daemons.
  message("GPU Allocation Summary (Proxy Workers):")
  print(tibble::as_tibble(gpu_status[, c(
    "gpu_id", "vendor", "name", "workers_allocated", "memory_total_mb"
  )]))

  message(
    "Launching ", length(all_daemon_configs),
    " proxy daemons. Each will spawn a new worker per task."
  )

  daemons <- mirai::daemons(
    n = length(all_daemon_configs),
    .list = all_daemon_configs
  )

  return(daemons)
}


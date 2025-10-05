.launch_proxy_daemons <- function(gpu_status, ...) {
  # All logic is now self-contained in this function, using rlang to build
  # the scripts dynamically. This avoids file path issues and is more robust.

  # --- 1. Define the Ephemeral Worker Logic as an Expression ---
  # This worker reads a task from stdin and writes the result to stdout.
  worker_expr <- rlang::expr({
    tryCatch(
      {
        # Use binary-mode connections for robustness.
        con_in <- file("stdin", "rb")
        task <- readRDS(con_in)
        close(con_in)

        result <- list(value = eval(task))

        # Write the result object to stdout.
        con_out <- stdout()
        saveRDS(result, file = con_out)
      },
      error = function(e) {
        # If an error occurs, serialize the error object to stdout so the
        # proxy can catch it and propagate it.
        tryCatch(
          {
            saveRDS(e, file = stdout())
          },
          error = function(e2) {
            # Final fallback if we can't even save the error.
          }
        )
      }
    )
  })

  # --- 2. Define the Proxy Loop Logic as an Expression ---
  # This expression takes the `worker_expr` and injects it.
  proxy_loop_expr <- rlang::expr({
    # This code runs in the long-lived proxy daemon.

    # The worker script is passed in as a command-line argument.
    worker_script_path <- commandArgs(trailingOnly = TRUE)[1]

    # The main task handler that mirai will call.
    .proxy_task_handler <- function(task) {
      # Launch the ephemeral worker, piping stdin/stdout for communication.
      px <- processx::process$new(
        command = R.home("bin/Rscript"),
        args = c("--vanilla", worker_script_path),
        stdin = "|",
        stdout = "|",
        stderr = "|"
      )

      # Write the serialized task to the worker's stdin pipe.
      px$write_input(saveRDS(task, NULL))
      px$close_input()

      # Wait for the process to finish.
      px$wait()

      # Check for execution errors.
      if (px$get_exit_status() != 0) {
        stop(
          "Ephemeral worker failed. Stderr: ",
          paste(px$read_all_stderr_lines(), collapse = "\n")
        )
      }

      # Read the raw binary output from stdout.
      output_bytes <- px$read_output_raw()
      if (length(output_bytes) == 0) {
        # Handle cases where the worker produces no output unexpectedly.
        stop(
          "Ephemeral worker produced no output. Stderr: ",
          paste(px$read_all_stderr_lines(), collapse = "\n")
        )
      }

      # Unserialize the result from the raw byte vector.
      result <- readRDS(rawConnection(output_bytes))

      # Propagate errors that occurred within the worker's R code.
      if (inherits(result, "error")) {
        stop(result)
      } else {
        return(result$value)
      }
    }

    # Return the handler for mirai to use.
    .proxy_task_handler
  })

  # --- 3. Create and Launch Daemons ---

  # Create temporary, session-specific files for the scripts.
  worker_script_file <- tempfile(fileext = ".R")
  proxy_script_file <- tempfile(fileext = ".R")

  # Write the expressions into the temp files.
  writeLines(rlang::expr_deparse(worker_expr), worker_script_file)
  writeLines(rlang::expr_deparse(proxy_loop_expr), proxy_script_file)

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

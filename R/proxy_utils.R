# This function contains the logic for the ephemeral worker process.
# It is designed to be called from a script and is not exported.
.run_worker_task <- function() {
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
}

# This function contains the logic for the proxy daemon's task handler.
# It is designed to be called from the proxy script and is not exported.
.handle_proxy_task <- function(task, worker_script_path) {
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

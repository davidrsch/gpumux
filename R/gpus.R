#' List available GPUs and their memory status from all supported vendors.
#'
#' This function queries the system for GPUs from supported vendors (currently NVIDIA)
#' and reports their identity and current memory allocation.
#'
#' @return A `data.frame` with one row per GPU, containing the following columns:
#'   * `gpu_id` (integer): The 0-indexed ID of the GPU.
#'   * `vendor` (character): The GPU vendor (e.g., "nvidia").
#'   * `name` (character): The product name of the GPU (e.g., "NVIDIA GeForce RTX 3090").
#'   * `memory_total_mb` (numeric): The total installed memory of the GPU in megabytes.
#'   * `memory_free_mb` (numeric): The currently available memory of the GPU in megabytes.
#' @export
#' @examples
#' \dontrun{
#'   list_gpus()
#' }
list_gpus <- function() {
  all_gpus <- data.frame(
    gpu_id = integer(),
    vendor = character(),
    name = character(),
    memory_total_mb = numeric(),
    memory_free_mb = numeric()
  )

  # --- NVIDIA GPU Detection ---
  nvidia_gpus <- .list_nvidia_gpus()
  if (!is.null(nvidia_gpus) && nrow(nvidia_gpus) > 0) {
    all_gpus <- rbind(all_gpus, nvidia_gpus)
  }

  # --- AMD GPU Detection (Future) ---
  # amd_gpus <- .list_amd_gpus()
  # if (nrow(amd_gpus) > 0) {
  #   all_gpus <- rbind(all_gpus, amd_gpus)
  # }

  return(all_gpus)
}

# Internal function to get NVIDIA GPU details
.list_nvidia_gpus <- function() {
  # Helper function to parse memory strings like "12288 MiB"
  parse_memory_mb <- function(mem_string) {
    if (is.na(mem_string) || mem_string == "") {
      return(NA_real_)
    }
    as.numeric(gsub("\\s*MiB", "", mem_string))
  }

  xml_output <- tryCatch(
    {
      suppressWarnings({
        nvidia_smi_path <- Sys.which("nvidia-smi")
      })
      if (nvidia_smi_path == "") {
        return(NULL)
      }
      system2(nvidia_smi_path, args = c("-q", "-x"), stdout = TRUE)
    },
    warning = function(w) {
      return(NULL)
    },
    error = function(e) {
      return(NULL)
    }
  )

  if (is.null(xml_output) || length(xml_output) == 0) {
    return(data.frame())
  }

  doc <- xml2::read_xml(paste(xml_output, collapse = "\n"))
  gpus <- xml2::xml_find_all(doc, "//gpu")

  if (length(gpus) == 0) {
    return(data.frame())
  }

  gpu_data <- lapply(seq_along(gpus), function(i) {
    gpu_node <- gpus[[i]]

    # Default to 0-indexed position in the XML output as the ID
    id <- i - 1

    # Try to get the official minor number, which is the correct ID.
    # On some systems this can be "N/A".
    minor_num_node <- xml2::xml_find_first(gpu_node, ".//minor_number")
    if (length(minor_num_node) > 0) {
      minor_num_text <- xml2::xml_text(minor_num_node)
      # Check for "N/A" and convert to integer if it's a valid number string
      id_val <- suppressWarnings(as.integer(minor_num_text))
      if (!is.na(id_val)) {
        id <- id_val
      }
    }

    name <- xml2::xml_text(xml2::xml_find_first(gpu_node, ".//product_name"))
    mem_info <- xml2::xml_find_first(gpu_node, ".//fb_memory_usage")
    total_mem <- parse_memory_mb(xml2::xml_text(xml2::xml_find_first(
      mem_info,
      ".//total"
    )))
    free_mem <- parse_memory_mb(xml2::xml_text(xml2::xml_find_first(
      mem_info,
      ".//free"
    )))

    data.frame(
      gpu_id = id,
      vendor = "nvidia",
      name = name,
      memory_total_mb = total_mem,
      memory_free_mb = free_mem
    )
  })

  do.call(rbind, gpu_data)
}

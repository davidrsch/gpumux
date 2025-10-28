#' List available GPUs and their memory status from all supported vendors.
#'
#' This function queries the system for GPUs from supported vendors (currently NVIDIA
#' and AMD ROCm)
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

  # --- AMD GPU Detection ---
  amd_gpus <- .list_amd_gpus()
  if (!is.null(amd_gpus) && nrow(amd_gpus) > 0) {
    all_gpus <- rbind(all_gpus, amd_gpus)
  }

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

# Internal function to get AMD ROCm GPU details
.list_amd_gpus <- function() {
  # Convert memory numeric and units to MiB
  to_mib <- function(value, unit) {
    if (is.na(value) || is.na(unit) || value == "") {
      return(NA_real_)
    }
    unit <- tolower(trimws(unit))
    val <- suppressWarnings(as.numeric(value))
    if (is.na(val)) {
      return(NA_real_)
    }
    if (unit %in% c("mib", "mb")) {
      return(val)
    } else if (unit %in% c("b", "bytes")) {
      return(val / (1024^2))
    } else if (unit %in% c("gib", "gb")) {
      return(val * 1024)
    } else if (unit %in% c("kib", "kb")) {
      return(val / 1024)
    } else {
      # Unknown unit
      return(NA_real_)
    }
  }

  rocm_output <- tryCatch(
    {
      suppressWarnings({
        rocm_smi_path <- Sys.which("rocm-smi")
      })
      if (rocm_smi_path == "") {
        return(NULL)
      }
      # Prefer a single command with product name and VRAM info
      system2(
        rocm_smi_path,
        args = c("--showproductname", "--showmeminfo", "vram"),
        stdout = TRUE
      )
    },
    warning = function(w) {
      return(NULL)
    },
    error = function(e) {
      return(NULL)
    }
  )

  # If command couldn't be executed or returned nothing
  if (is.null(rocm_output) || length(rocm_output) == 0) {
    return(data.frame())
  }

  # Normalize to vector of lines
  txt <- paste(rocm_output, collapse = "\n")
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]

  # Accumulators per GPU index
  names_map <- list()
  total_map <- list()
  used_map <- list()

  for (ln in lines) {
    # Parse memory lines like:
    # "GPU[0] : VRAM Total Memory (B): 17179869184"
    # "GPU[0] : VRAM Total Used (B): 2147483648"
    m_mem <- regexec(
      "GPU\\[(\\d+)\\].*VRAM\\s+Total\\s+(Memory|Used)\\s*\\(([^)]+)\\)\\s*:\\s*([0-9]+)",
      ln
    )
    r_mem <- regmatches(ln, m_mem)[[1]]
    if (length(r_mem) >= 5) {
      idx <- as.integer(r_mem[2])
      kind <- tolower(r_mem[3]) # "Memory" or "Used"
      unit <- r_mem[4]
      val_raw <- r_mem[5]
      val_mib <- to_mib(val_raw, unit)
      key <- as.character(idx)
      if (!is.na(idx)) {
        if (kind == "memory") {
          total_map[[key]] <- val_mib
        } else if (kind == "used") {
          used_map[[key]] <- val_mib
        }
      }
    }

    # Parse name lines like: "GPU[0] : Radeon PRO W6800" (exclude VRAM lines)
    if (!grepl("VRAM", ln, ignore.case = TRUE)) {
      m_name <- regexec("GPU\\[(\\d+)\\].*:\\s*(.+)$", ln)
      r_name <- regmatches(ln, m_name)[[1]]
      if (length(r_name) >= 3) {
        idx <- as.integer(r_name[2])
        if (!is.na(idx)) {
          names_map[[as.character(idx)]] <- r_name[3]
        }
      }
    }
  }

  # Build rows for GPUs seen in either names or memory maps; use 0-based ordering fallback
  gpu_indices <- unique(c(
    as.integer(names(names_map)),
    as.integer(names(total_map)),
    as.integer(names(used_map))
  ))

  if (length(gpu_indices) == 0 || all(is.na(gpu_indices))) {
    return(data.frame())
  }

  gpu_indices <- sort(gpu_indices)

  rows <- lapply(gpu_indices, function(i) {
    key <- as.character(i)
    name <- if (!is.null(names_map[[key]])) names_map[[key]] else NA_character_
    total <- if (!is.null(total_map[[key]])) total_map[[key]] else NA_real_
    used <- if (!is.null(used_map[[key]])) used_map[[key]] else NA_real_
    free <- if (!is.na(total) && !is.na(used)) total - used else NA_real_

    data.frame(
      gpu_id = i,
      vendor = "amd",
      name = name,
      memory_total_mb = total,
      memory_free_mb = free
    )
  })

  do.call(rbind, rows)
}

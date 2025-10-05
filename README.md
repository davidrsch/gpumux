# gpumux

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/davidrsch/gpumux/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/davidrsch/gpumux/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/davidrsch/gpumux/graph/badge.svg)](https://app.codecov.io/gh/davidrsch/gpumux)
<!-- badges: end -->

The goal of `gpumux` is to provide VRAM-aware worker management to safely run multiple `mirai` daemons on one or more GPUs. It supports both high-performance persistent workers and stable proxy workers that isolate tasks in separate processes. The package can also proactively initialize workers for specific deep learning frameworks, configuring GPU memory limits automatically.

## Installation

You can install the development version of `gpumux` from GitHub with:

```r
# install.packages("pak")
pak::pak("davidrsch/gpumux")
```

## Example

This example shows how to use `gpumux` to create `mirai` daemons on available GPUs.

```r
library(gpumux)
library(mirai)

# 1. List available GPUs
# This function is vendor-agnostic and will be improved to support more vendors in the future.
# For now, it supports NVIDIA GPUs.
gpus <- list_gpus()
print(gpus)

# 2. Create daemons on the first GPU
# This will create 2 workers on GPU 0, allocating 1GB of VRAM for each.
# It will also reserve 1GB of VRAM on the GPU.
if (nrow(gpus) > 0) {
  daemons <- gpu_daemons(
    gpu_ids = 0,
    n_workers = 2,
    memory_per_worker_mb = 1024,
    reserve_memory_mb = 1024
  )
}

# 3. Use the daemons with mirai
# ... your mirai code here ...

# 4. Terminate the daemons when you are done
if (exists("daemons")) {
  daemons(0)
}
```
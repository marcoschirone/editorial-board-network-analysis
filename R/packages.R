# R/packages.R
# Purpose: Load all required packages for the symbolic capital network analysis pipeline.

suppressPackageStartupMessages({
  # Core packages for data manipulation, file paths, and configuration
  library(here)
  library(tidyverse)
  library(config)
  
  # Network analysis and visualization
  library(igraph)
  library(ggraph)
  
  # Data I/O
  library(readxl)
  library(openxlsx)
  
  # Parallel processing and targets
  library(future)
  library(furrr)
  library(targets)
  
  # Advanced visualization and themes
  library(viridis)
  library(patchwork) # For combining plots
  library(ggalluvial) # Optional: for alluvial diagrams
  library(circlize) # Optional: for chord diagrams
  
  # Statistics and modeling
  library(ineq)  # For Gini coefficient
})
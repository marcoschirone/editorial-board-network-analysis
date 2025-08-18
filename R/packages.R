# R/packages.R
# Purpose: Load all required packages for the analysis pipeline.

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
  library(igraph)
  library(ggraph)
  library(ggrepel) 
  library(readxl)
  library(openxlsx)
  library(config)
  library(future)
  library(furrr)
})
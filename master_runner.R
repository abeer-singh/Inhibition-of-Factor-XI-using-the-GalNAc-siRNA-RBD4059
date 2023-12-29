#master file to run all codes
library(tidyverse)
analysis_function <- function(file){
  source(file)
  rm(list = ls()) 
}

c("2a.R", "2b.R", "3a.R", "3b.R", "3c.R", "3d.R", "3e.R", "3f.R", "4b.R", "4d.R", "fxi_aptt_summary.R") |>
  map(analysis_function)


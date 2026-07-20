# Install and load packages required for the project

if (!require(pacman)) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse,
  janitor,
  readxl,
  readr,
  stringr,
  scales,
  ggrepel
)

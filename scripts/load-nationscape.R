library(tidyverse)
library(haven)
library(labelled)
library(pollster)

theme_set(theme_minimal())
theme_update(text = element_text(size=13),
             #text = element_text(family="Source Sans Pro")
)

if (!requireNamespace("curl", quietly = TRUE)) {
  install.packages("curl")
}

library(curl)

githubURL <- "https://raw.githubusercontent.com/zilinskyjan/DataViz/temp/data_nationscape2019/Nationscape_first10waves.rds"
download.file(githubURL,"Nationscape_first10waves.rds", method="curl")
a <- readRDS("Nationscape_first10waves.rds")
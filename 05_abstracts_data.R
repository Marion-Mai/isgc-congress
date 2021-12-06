## ISGC NETCONF SCRIPT 7 - SUNBELT 2020 - 07_abstracts_data.R
## 2021-05-11
## M. Maisonobe & F. Briatte

# load libraries

library(dplyr) # for data wrangling
library(purrr) # for map()
library(readr) # for write_tsv
library(stringr) # for str_extract
library(tidyr) # for unite

# load the 2015-2019 abstracts dataset
abstracts <- read_tsv("data/abstracts-2015-2019.tsv")

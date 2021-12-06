# library(tidyverse)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(readr)
library(httr)
library(stringi)
library(jsonlite)

# setwd("~/isgc/isgc-congress")

# extract (likely) the first part of the first names --------------------------------------------

# simple heuristics to get first names from (mostly) Western names
d <- fs::dir_ls("data", regexp = "all") %>% # equivalent to open https://github.com/briatte/stage/blob/main/author-lists/isgc-authors-all.tsv
  map(readr::read_tsv, col_types = cols(.default = "c")) %>% # equivalent to open https://github.com/briatte/stage/blob/main/author-lists/isgc-authors-all.tsv
  map_dfr(select, first_name, family_name) %>%
  distinct() %>%
    mutate(first = str_remove_all(first_name, "^[A-Z]\\s") %>% # i.e. to remove the "M" before "M Virginia"
        word(1, sep = " ") %>% # 1st firstname
      # remove lonely initials in "J M"
        str_replace_all("^[A-Z]$", " ") %>%
      str_squish()
      )  %>%
  filter(! first %in% "")

n <- select(d, first) %>%
  # just to be sure
  filter(!is.na(first)) %>%
  distinct()

# guess names via genderizer.io -------------------------------------------
cat("Querying genderizer.io for", nrow(n), "first names...\n")

f <- "data-net/genderizer-results.tsv"
if (!fs::file_exists(f)) {

  # init
  n %>%
    # name as sent to genderizer.io (sanitized)
    mutate(name = NA_character_) %>%
    mutate(gender = NA_character_) %>%
    mutate(probability = NA_real_) %>%
    mutate(count = NA_integer_) %>%
    write_tsv(f)

}

read_tsv(f, col_types = "cccdi")

repeat {

  # sample 10 names (batch limit)
  g <- readr::read_tsv(f, col_types = "cccdi") %>%
    filter(is.na(gender)) %>%
    slice_sample(n = 10)

  r <- g$first %>%
    # sanitize: remove non-ASCII characters (as a precaution)
    stringi::stri_trans_general(id = "Latin-ASCII") %>%
    str_flatten(collapse = "&name[]=") %>%
    str_c("https://api.genderize.io/?name[]=", .)

  cat("Guessing", str_trunc(str_c(g$first, collapse = ", "), 50))
  if (!nrow(g)) {

    cat("... done\n")
    break

  }

  r <- try(httr::GET(r))

  if("try-error" %in% class(r)) {

    cat(": network error\n")
    break

  }

  # will fail around 1,000 requests/day (API limit)
  if(status_code(r) != 200) {

    cat(":", content(r)$error, "\n")
    break

  }

  Sys.sleep(1.5)

  r <- content(r, as = "text") %>%
    fromJSON(flatten = TRUE) %>%
    # denote unknown genders as missing
    mutate(gender = str_replace_na(gender, "unknown")) %>%
    mutate(first = g$first, .before = 1)
  
  ff <- "data-net/genderizer-results-update.tsv"

  # reload names list
  g <- readr::read_tsv(f, col_types = "cccdi")

  # collate to names list
  g <- bind_rows(
    # new additions
    semi_join(r, g, by = "first"),
    # not yet queried
    anti_join(g, r, by = "first")
  ) %>%
    arrange(first)

  cat("", sum(!is.na(g$gender)), "guessed,", sum(is.na(g$gender)), "left\n")
  write_tsv(g, ff)

}

g <- readr::read_tsv(ff, col_types = "cccdi")

cat(
  "Genders:",
  sum(g$gender %in% "female"), "females,",
  sum(g$gender %in% "male"), "males,",
  sum(is.na(g$gender)), "missing,",
  sum(g$gender %in% "unknown"), "unknown.\n"
)

# few ambiguous results
filter(g, probability > 0, probability < 0.9)

# very few unknowns
filter(g, probability == 0)

# very hacky but works


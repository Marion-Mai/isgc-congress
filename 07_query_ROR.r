# library(tidyverse)
library(readr)
library(dplyr)
library(stringr)
library(stringi)
library(fs)
library(httr)
library(jsonlite)
library(magrittr)
library(purrr)


a <- readr::read_tsv("data-net/edges-2015-2019.tsv", col_types = cols(.default = "c")) %>%
  select(institution) %>%
  filter(!is.na(institution)) %>%
  distinct()

cat("Querying ROR for", nrow(a), "affiliations")

d <- a %>%
  mutate(
    url = "https://api.ror.org/organizations?affiliation=" %>%
      # replace spaces
      str_c(str_replace_all(institution, "\\s", "+")) %>%
      # sanitize: remove non-ASCII characters (otherwise error 400)
      stringi::stri_trans_general(id = "Latin-ASCII"),
    ror = list(NA)
  )

# remove affiliations that have already been parsed
f <- "data-net/ror-results.rds"
if (fs::file_exists(f)) {

  d <- anti_join(d, readr::read_rds(f), by = "institution")
  cat(" (skipping", nrow(a) - nrow(d), "already queried)...\n")

} else {

  cat("...\n")

}

cat("\n")

for (i in nrow(d):1) {

  cat(
    str_pad(i, 4),
    str_pad(str_trunc(d$institution[ i ], 50), 50, side = "right"),
    "..."
  )

  r <- try(httr::GET(d$url[ i ]), silent = TRUE)

  if ("try-error" %in% class(r)) {
    cat("ERROR\n")
    next
  }

  Sys.sleep(0.75)

  if (httr::status_code(r) != 200) {

    cat(" error", httr::status_code(r), "\n")
    next

  } else {

    cat(" OK\n")
    d$ror[ i ] <- httr::content(r, as = "text", encoding = "UTF-8")

  }

}

# subset to valid results
d <- mutate(d, ror_results = map_lgl(ror, is.character)) %>%
  filter(ror_results) %>%
  mutate(
    ror_results = map_chr(ror,
            ~ jsonlite::fromJSON(.x, flatten = TRUE) %>%
              magrittr::extract2("number_of_results")
    ),
    ror_results = as.integer(ror_results)
  )

# export full results -----------------------------------------------------

if (fs::file_exists(f)) {

  # append
  readr::write_rds(bind_rows(d, readr::read_rds(f)), f, compress = "gz")

} else {

  # initialize
  readr::write_rds(d, f, compress = "gz")

}

# export results overview -------------------------------------------------

d <- readr::read_rds(f)

cat("\nExporting overview of", nrow(d), "results on disk...")

r <- map_dfr(d$ror[ d$ror_results > 0 ],
  ~ jsonlite::fromJSON(.x, flatten = TRUE) %>%
    magrittr::extract2("items") %>%
    slice(1) %>%
    select(
      # confidence
      chosen, score, matching_type,
      # information
      organization.name, organization.id, organization.country.country_name
    )
)

# queried, successfully
bind_cols(select(filter(d, ror_results > 0), -ror), r) %>%
  # queried, not successfully
  bind_rows(select(filter(d, ror_results == 0), -ror)) %>%
  # not queried yet
  full_join(a, ., by = "institution") %>%
  # stable order
  arrange(institution) %>%
  readr::write_tsv("data-net/ror-overview.tsv", na = "")

cat(" done.\n")

# kthxbye

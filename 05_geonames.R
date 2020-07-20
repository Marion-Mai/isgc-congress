## ISGC NETCONF SCRIPT 5 - SUNBELT 2020 - 05_geonames.R
## 2020-07-14 Bastille Day
## M. Maisonobe & F. Briatte

library(dplyr) # for select and %>% + loads tibbles
library(purrr) # for map()
library(readr) # for read_csv

library(geonames)
#https://geonames.wordpress.com/page/2/
#http://www.geonames.org/export/codes.html
#https://geocoder.readthedocs.io/providers/GeoNames.html on Python
#http://geonames.r-forge.r-project.org/

options(geonamesUsername = "xxx")

############################################################################################################################

d <- read_tsv("data-net/edges-2015-2019.tsv")

# keep communication included in the programme only # including keynotes and posters for the geographic analysis
d <- d %>%
  filter(is.na(j) == F) # (final_status %in% c("OC","FC") to build co-panel networks)


##############################################################################################################################
# the following code has not been updated: it was valid for 2017 and 2019 only --> we might find other issues with 2015 data #

# load all communications included in the programme + authors' addresses of these com for 2017 and 2019
f <- read_tsv("data/abstracts-addresses-2017-2019.tsv")

country <- tibble(country = f$country) %>%
  distinct()%>%
  purrr::map_df(str_subset, "\\w+") %>% #  |  "\\d+"
  purrr::map_df(str_subset, "\\D+") %>%
  dplyr::arrange()

# search all the countries on geonames (non fuzzy search)

# countrydf <- "data/geonames-countries.rds"
countrydf <- "data/geonames-countries.tsv"

if (!file.exists(countrydf)) {

countrydf <-
  country %>%
  split(.$country) %>%
  map( ~ GNsearch(name = .x$country, featureCode = "PCLI", fuzzy = 1)) %>%
  compact() %>%
  map_dfr(~ .x %>% as_tibble(), .id = "country_src")  %>% # (.)
  full_join(country, by = c("country_src" = "country")) %>% #"country_src"
  rename(ISO2 = countryCode,
         countryname = countryName) %>%
  select(country_src, countryname, ISO2)%>%
  distinct()%>%
  # write_rds(countrydf, "data/geonames-countries.rds")
  write_tsv(countrydf, "data/geonames-countries.tsv")

  }

# countrydf <- read_rds("data/geonames-countries.rds")
countrydf <- read_tsv("data/geonames-countries.tsv") %>%
             mutate(countryname = na_if(countryname, "data/geonames-countries.tsv")) %>% # issue: how can I avoid this?
             mutate(ISO2 = na_if(ISO2, "data/geonames-countries.tsv"))

# focus on the subset of ambiguous cases
ambiguous <- countrydf %>%
  drop_na(countryname) %>%
  group_by(country_src) %>%
  filter(n()>1) %>%
  filter(!country_src %in% "WA") %>%
  group_by(country_src) %>%
  slice(ifelse(!(country_src %in% c("REPUBLIC OF KOREA", "ROC", "UK")), 1,
               ifelse(!(country_src %in% c("UK")), 2,
                      3))) %>%
  distinct() %>%
  #mutate(countryname = replace(countryname, country_src %in% "WA", NA),
  # ISO2 = replace (ISO2, country_src %in% "WA", NA)) %>%
  rename(ISO2_dest = ISO2, country_dest = countryname)

# fix ambiguous cases
countrydfclean <- countrydf %>%
  left_join(ambiguous, by = "country_src") %>%
  mutate(
    ISO2_dest = ifelse(country_src != "WA", coalesce(ISO2_dest, ISO2), NA),
    country_dest = ifelse(country_src != "WA", coalesce(country_dest, countryname), NA)
  )

# select unfound countries
unfoundr <- countrydfclean %>% #countrydf
  filter(is.na(country_dest)) %>%
  distinct(country_src)

# countrydf2 <- "data/geonames-countries-2.rds"
countrydf2 <- "data/geonames-countries-2.tsv"

if (!file.exists(countrydf2)) {

# search for unfound countries with geonames (fuzzy search autorised)
countrydf2 <- unfoundr %>%
  split(.$country_src) %>% #country_src
  map( ~ GNsearch(name = .x$country_src, featureCode = "PCLI", featureCode = "ADM1", fuzzy = 0)) %>% #country_src
  compact() %>%
  map_dfr(~ .x %>% as_tibble(), .id = "country_src")  %>% # (.)
  full_join(unfoundr, by = c("country_src" = "country_src")) %>% #"country_src"
  rename(ISO2 = countryCode,
         countryname = countryName) %>%
  select(country_src, countryname, ISO2)%>%
  distinct() %>%
  # write_rds(countrydf2, "data/geonames-countries-2.rds")
  write_tsv(countrydf2, "data/geonames-countries-2.tsv")

  }

# countrydf2 <- read_rds("data/geonames-countries-2.rds")
countrydf2 <- read_tsv("data/geonames-countries-2.tsv") %>%
  mutate(countryname = na_if(countryname, "data/geonames-countries-2.tsv")) %>% # issue: how to avoid having to do this?
  mutate(ISO2 = na_if(ISO2, "data/geonames-countries-2.tsv"))

# fix ambiguous cases
rclean <- countrydf2 %>% filter(!country_src %in% c("CEDEX", "FR", "MILANO")) %>% # check "GERMANY/FRANCE" - one lines for two addresses -->
                                                                                  # I later include two rows in the recipe table: GERMANY/FRANCE --> GERMANY and GERMANY/FRANCE --> FRANCE
  group_by(country_src) %>%
  slice(ifelse(!(country_src %in% c("MADRID", "N IRELAND, UK", "TAIWAN, R.O.C.", "THE NETHERLAND", "WA")), 1,
               ifelse((country_src %in% c("TAIWAN, R.O.C.", "THE NETHERLAND")), 2,
                      ifelse((country_src %in% c("N IRELAND, UK")), 3,
                             7)))) %>%
  distinct() %>%
  mutate(countryname = replace(countryname, country_src %in% "P. R. CHINA", "China"),
         ISO2 = replace (ISO2, country_src %in% "P. R. CHINA", "CN")) %>%
  rename(ISO2_dest = ISO2, country_dest = countryname)

countrydfclean2 <- countrydfclean %>%
  left_join(rclean, by = "country_src") %>%
  mutate(
    ISO2_dest = coalesce(ISO2_dest.x, ISO2_dest.y),
    country_dest = coalesce(country_dest.x, country_dest.y)
  )%>%
  select(country_src, country_dest, ISO2_dest)%>%
  distinct()

# last fix : manual
leftovers <- countrydfclean2 %>%
  filter(is.na(country_dest))  %>%
  distinct(country_src)

leftovers$country_src
# "WA"     "MILANO" "HONG KONG" "FR"     "CEDEX"
# Check WA, might be in Australia or in the States
leftovers$country_dest <- c("United States", "Italy", "China", "France", "France")
leftovers$ISO2_dest <- c("US", "IT", "CN", "FR", "FR")

# export the table resulting from this country identification process
recipe <- countrydfclean2 %>%
  left_join(leftovers, by = "country_src") %>%
  mutate(
    ISO2_dest = coalesce(ISO2_dest.x, ISO2_dest.y),
    country_dest = coalesce(country_dest.x, country_dest.y)
  )%>%
  select(country_src, country_dest, ISO2_dest)%>%
  distinct() %>%
  bind_rows(
  .,
  tibble::tribble(
    ~ country_src, ~ country_dest, ~ ISO2_dest,
    "GERMANY/FRANCE", "France", "FR"))

write_tsv(recipe, "data/geonames-recipe-countries.tsv")

########################################## back to the main database ####################################################
# following step: CITY SEARCH

f <- f %>%
  left_join(recipe, f, by = c("country" = "country_src"))

cities <- f %>% distinct(city, ISO2_dest)

Unaccent2 <- function(text) {
  text <- iconv(text, from="UTF-8", to="ASCII//TRANSLIT")   #ASCII//TRANSLIT//IGNORE
  return(text)
}

cities$city <- cities$city %>% #  |  "\\d+"
  str_remove_all("CEDEX") %>%
  str_remove_all("CÉDEX") %>%
  str_remove_all("F-") %>%
  str_remove_all("\\d+") %>%
  str_trim() %>%
  str_replace_all("-", " ") %>%
  Unaccent2()

cities <- cities %>% drop_na() %>% distinct()

write_tsv(cities, "data/cities.tsv")

citiesdf <- "data/geonames-cities.rds"
# citiesdf <- "data/geonames-cities.tsv"

if (!file.exists(citiesdf)) {

citiesdf <-
  cities %>%
  split(.$city) %>%
  map( ~ GNsearch(name = .x$city, country = .x$ISO2_dest, featureClass = "P", fuzzy = 1)) %>%
  compact() %>%
  map_dfr(~ .x %>% as_tibble(), .id = "city_src")  %>% # (.)
  full_join(country, by = c("city_src" = "city")) %>% #"country_src"
  rename(cityname = toponymName,
         provincename = adminName,
         countryname = countryName,
         long = lng,
         lat = lat) %>%
  select(cityname, provincename, countryname, long, lat)%>%
  distinct() %>%
  write_rds(citiesdf, "data/geonames-cities.rds")

  }

citiesdf <- read_rds("data/geonames-cities.rds")


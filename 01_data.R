## ISGC NETCONF SCRIPT 1 - SUNBELT 2020 - 01_data.R
## 2020-07-14 Bastille Day
## M. Maisonobe & F. Briatte

# load libraries

library(dplyr) # for data wrangling
library(purrr) # for map()
library(readr) # for write_tsv
library(stringr) # for str_extract
library(tidyr) # for unite

# load the 2015-2019 abstracts dataset
abstracts <- read_tsv("data/abstracts-2015-2019.tsv")

# keep OC (Oral Communications) and FC (Flash Communications) only
ocfc <- abstracts %>%
  filter(final_status %in% c("OC","FC", "FC - PC", "OC - PC"))

# count number of com per topic
ocfc %>%
  count(topic_1)

# count number of com per topic per year
ocfc %>%
  group_by(year, topic_1) %>%
  mutate(n_topic = n()) %>%
  pivot_wider(names_from = year, values_from = n_topic)

# count number of com per topic per year and export this as a table
table <- ocfc %>%
  group_by(year, topic_1) %>%
  count(sort = T) %>%
  pivot_wider(names_from = year, values_from = n, values_fill = list(n = 0))

write_tsv(table, "data/topic_distribution.tsv")

# load the 2015-2019 authors-abstract dataset
authors_abstracts <- read_tsv("data/authors-abstracts-2015-2019.tsv")


# solve a few problematic case (including 'two lines on one' issue)
authors_abstracts <- filter(authors_abstracts, ! firstname %in% c("A. SIMPSON &  P. JESSOP",
                                          "Patrick MAESTRO, SOLVAY, Scientific director / Jean-Luc Moullet CNRS, General Director in charge of valorization / Serge HUBERSON, University of Poitiers, Vice-President in charge of research",
                                          "Stéphane STREIFF, SOLVAY, Directeur E2P2L, Shanghaï, CHINA  / Yannick POUILLOUX, University of Poitiers, Directeur IC2MP, POITIERS, FRANCE",
                                          "A. KOVACS", "F. TOUCHARD"
                            )) %>%
  bind_rows(
    .,
    tibble::tribble(
      ~ firstname, ~ lastname, ~ year, ~ id, ~ Idu, ~ j,  ~ email, ~ institution, ~ city, ~ country,
      "A.", "SIMPSON", 2019, 1138, 1055, "2019_1705_1643", 1, "Royal Society of Chemistry", "CAMBRIDGE", "UNITED-KINGDOM",
      "P.", "JESSOP", 2019, 1138, 1055, "2019_1705_1643", 1, "Queen's University", "KINGSTON", "CANADA",
      "Stéphane", "STREIFF", 2019, 1146, 1063, "2019_1715_1452", 1, "SOLVAY", "SHANGHAI", "CHINA",
      "Yannick", "POUILLOUX", 2019, 1146, 1063, "2019_1715_1452", 1, "University of Poitiers", "POITIERS", "FRANCE",
      "Patrick", "MAESTRO", 2019, 1147, 1064, "2019_1715_1452", 1, "SOLVAY", "PARIS", "FRANCE",
      "Jean-Luc", "Moullet", 2019, 1147, 1064, "2019_1715_1452", 1, "CNRS", "PARIS", "FRANCE",
      "Serge", "Huberson", 2019, 1147, 1064, "2019_1715_1452", 1, "University of Poitiers", "POITIERS", "FRANCE",
      "A.", "KOVACS", 2019, 1188, 1075, "2019_1602_1643", 1331, "CHEMAXON KFT", "BUDAPEST", "HUNGARY",
      "DORA", "BARNA", 2019, 1188, 1075, "2019_1602_1643", 1331, "CHEMAXON KFT", "BUDAPEST", "HUNGARY",
      "F.", "TOUCHARD", 2019, 1189, 1076, "2019_1602_1643", 1331, "UPR 3346 - ENSMA", "FUTUROSCOPE CHASSENEUIL", "FRANCE",
      "L.", "CHOCINSKI", 2019, 1189, 1076, "2019_1602_1643", 1331, "UPR 3346 - ENSMA", "FUTUROSCOPE CHASSENEUIL", "FRANCE"
    )
  ) # --> file with 6485 rows

###### TO DO: remove duplicated participants' names per abstract - still needs to be done 21.06.2021

#> inv <- authors_abstracts %>%
  # filter(firstname %in% lastname)

# db <- authors_abstracts %>%
  # group_by(Idu, lastname) %>%
  # summarise(n = n(), firstname = first(firstname), lastname = first(lastname), email = first(email)) %>%
  # filter(n >= 2)

authors <- authors_abstracts %>%
  distinct(firstname, lastname, email) %>%
  mutate(idind = row_number())%>%
  arrange() # 4947 rows

# isolate authors identity information only
authors <- authors_abstracts %>%
           distinct(firstname, lastname, email) %>%
           mutate(idind = row_number())%>%
           arrange() # 4947 rows

# finalize participant names
n <- authors %>%
     mutate(across(.cols = c(firstname, lastname), .fns = function(x) {
                str_to_upper(x) %>%
                iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
                str_remove_all("[\"^'*`\\\\~\\.]") %>%
                str_replace_all("-|\\s+", " ")%>%
                str_trim }, .names =  "a_{col}")) %>% #
     mutate(a_email = str_to_lower(email))  # file with 4947 rows

ncor <- n %>%
          distinct(a_firstname, a_lastname, a_email) %>%
          group_by(a_firstname, a_lastname)  %>%
          summarise(a_email = first(a_email), nb_email = n()) # file with 4131 rows

# TO DO: keep the longest name instead of the first - done 21.06.2021
# Dont take the organisation team's email into account (email 1342) - done 21.06.2021

ncor <- ncor %>%
       mutate(a_email = ifelse(a_email %in% c(1, 1342), paste0("Tag", "_", a_firstname, "_", a_lastname), a_email)) %>%
       filter(a_email != "Tag_NA_NA")%>%
       group_by(a_lastname, a_email) %>% #
          mutate(b_firstname = a_firstname[which.max(str_length(a_firstname))],
                 b_lastname = first(a_lastname), a_email = first(a_email), nb_email = first(nb_email)) %>%
       ungroup() %>%
       filter(a_email != "Tag_NESSE_NA")%>%
       group_by(b_firstname, a_email) %>% #
          mutate(b_lastname = b_lastname[which.max(str_length(b_lastname))]) %>% #
       ungroup() %>%
          mutate(fname = word(b_firstname, 1, sep=" ")) %>% #1st firstname
          mutate(lname = word(b_lastname, 1, sep=" ")) %>% #lst lastname
          mutate(init = str_extract(b_firstname, "^.{1}")) %>% #1st character of firstnames
       group_by(fname, b_lastname) %>% #
          mutate(b_firstname = b_firstname[which.max(str_length(b_firstname))]) %>% # # #1st character of firstnames
       filter(!is.na(init), !is.na(a_lastname)) %>% # remove na
       group_by(init, b_lastname) %>%
          mutate(
          # this is useful only for the checks below
          b_firstname_copy = b_firstname,
          b_firstname = if_else(
      n() > 1 & str_length(fname) == 1,
      b_firstname[ which.max(str_length(b_firstname)) ],
      b_firstname
    )
  )    %>%
                  # CHECKS: (1) find people for which b_firstname was modified
                  # ncor %>%
                  # mutate(
                  #  changed = (b_firstname_copy != b_firstname),
                  #  id = cur_group_id()
                  # ) %>%
                  # show every group for which at least one b_firstname has changed
                  #   filter(id %in% id[ changed ]) %>%
                  #   group_split()

       ungroup() %>%
       select(-c(b_firstname_copy)) %>%
       unite("name", b_firstname:b_lastname, sep = ", ", remove = F) %>%

                  # (2) names with only 1 or 2 different letters:
                     # library(stringdist)
                  #  for(i in unique(ncor$name)) {
                  #    m <- stringdist::stringdist(i, unique(ncor$name))
                  #    m <- which(m > 0 & m < 3)
                  #    if (length(m) > 0)
                  #      cat(i, ":", str_c("\n ~ ", unique(ncor$name)[ m ]), "\n\n") #
                  #  }

  mutate(b_firstname = if_else(b_lastname %in% "NARDELLO RATAJ", "VERONIQUE", b_firstname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "DA SILVA PEREZ", "DENILSON", b_firstname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "KOELEWIJN", "STEVEN FRISO", b_firstname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "CARMICHAEL", "EUGENE", b_firstname)) %>%
  mutate(b_lastname = if_else(b_lastname %in% "VANBOREKHOVEN", "VANBROEKHOVEN", b_lastname)) %>%
  mutate(b_lastname = if_else(b_lastname %in% "RODENAS OLALLA", "RODENAS OLAYA", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "WAN MOHD ASHRI", "WAN MOHD ASHRI WAN", b_firstname),
         b_lastname =  if_else(b_lastname %in% "WAN MOHD ASHRI", "DAUD", b_lastname), ) %>%
  mutate(b_lastname = if_else(b_lastname %in% "BJARACHARYA", "BAJRACHARYA", b_lastname)) %>%
  mutate(b_lastname = if_else(b_lastname %in% "HESEMAN", "HESEMANN", b_lastname)) %>%
  mutate(b_lastname = if_else(b_firstname %in% "AIGARS", "PAZHE", b_lastname)) %>%
  mutate(b_lastname = if_else(b_lastname %in% "NEMMATI KHARAT", "NEMATI KHARAT", b_lastname)) %>%
  mutate(b_lastname = if_else(b_lastname %in% "M MARRUCHO", "MARRUCHO", b_lastname)) %>%
  mutate(b_lastname = if_else(b_lastname %in% "NEGAHDAR", "NEGAHDAR", b_lastname)) %>%
  mutate(b_lastname = if_else(b_lastname %in% "PERA TITUS EXT", "PERA TITUS", b_lastname)) %>%
  mutate(b_lastname = if_else(b_lastname %in% "GALLUCI", "GALLUCCI", b_lastname)) %>% # file with 4129 rows
  mutate(b_firstname = if_else(b_lastname %in% "CORDOVA", "ARMANDO", b_firstname)) %>% # added 23.07.2021
  mutate(b_firstname = if_else(b_lastname %in% "BURGUETE", "MARIA ISABEL", b_firstname)) %>% # added 23.07.2021
  mutate(b_firstname = if_else(b_lastname %in% "RODE", "CHANDRASHEKHAR", b_firstname))  # added 23.07.2021

ncor %>% distinct(b_firstname, b_lastname) %>%
filter(str_detect(b_lastname, "^(\\w\\s)+"))  # manual action required for A RITA C DUARTE, ANA M MATIAS, L REIS etc.

# (3) check for names included in others names
for (i in unique(ncor$name)){
  j <- str_subset(ncor$name[ ncor$name != i ], i)
  if (length(j)) cat(i, ":", str_c(j, collapse = ","), "\n")
}

ncor <- ncor %>%
  mutate(b_firstname = if_else(b_lastname %in% "C DUARTE", "ANA", b_firstname),
         b_lastname =  if_else(b_lastname %in% "C DUARTE", "DUARTE", b_lastname), ) %>%
  mutate(b_lastname = if_else(b_lastname %in% "M MATIAS", "MATIAS", b_lastname))  %>%
  mutate(b_firstname = if_else(b_lastname %in% "L REIS", "RL", b_firstname),
         b_lastname =  if_else(b_lastname %in% "L REIS", "REIS", b_lastname), ) %>%
  mutate(b_firstname = if_else(name %in% "JOSE, I GARCIA", "JOSE I", b_firstname),
         b_lastname =  if_else(name %in% "JOSE, I GARCIA", "GARCIA", b_lastname), ) %>%
  mutate(b_firstname = if_else(name %in% "SIMAO, P PINHO", "SIMAO P", b_firstname),
       b_lastname =  if_else(name %in% "SIMAO, P PINHO", "PINHO", b_lastname), ) %>%
  mutate(b_firstname = if_else(b_lastname %in% "WASSERCHEID", "PETER", b_firstname),
       b_lastname =  if_else(b_lastname %in% "WASSERSCHEID", "PINHO", b_lastname), ) %>%
  mutate(b_firstname = if_else(b_lastname %in% "SONIA MILENA", "SONIA MILENA", b_firstname),
       b_lastname =  if_else(b_lastname %in% "SONIA MILENA", "AGUILERA SEGURA", b_lastname), )%>%
  mutate(b_firstname = if_else(b_lastname %in% "CONSTABLE", "DAVID J", b_firstname), # added 22.06.2021
         b_lastname =  if_else(b_lastname %in% "CONSTABLE", "CHICHESTER CONSTABLE", b_lastname), )%>%
  mutate(b_firstname = if_else(b_lastname %in% "JEAN MICHEL", "JEAN MICHEL", b_firstname), # added 23.07.2021
         b_lastname =  if_else(b_lastname %in% "JEAN MICHEL", "TATIBOUET", b_lastname), )%>%
  mutate(b_firstname = if_else(b_lastname %in% "MOHD ZAINI", "NURUL AQILAH MOHD", b_firstname), # added 23.07.2021
         b_lastname =  if_else(b_lastname %in% "MOHD ZAINI", "ZAINI", b_lastname), )%>%
  mutate(b_firstname = if_else(b_firstname %in% "PINSOLLE EXTERIEUR", "ALEXANDRE", b_firstname), # added 23.07.2021
         b_lastname =  if_else(b_firstname %in% "PINSOLLE EXTERIEUR", "PINSOLLE", b_lastname), )%>%

  # Remaining questions:
  # are ANA LOPEZ and ANA LOPEZ CONTRERAS the same person? does not seem so, different institution & country
  # are FRANCISCA MANO and MARIA FRANCISCA MANO the same person? yes, same affiliation
  # are FRANCISCO, VILA and FRANCISCO, VILAPLANA the same person? does not seem so, different institution & country
  # are MARIA, FUENTES and MARIA, FUENTES CAMPOS the same person? yes, same affiliation
  # are LUIS, SERRANO and LUIS, SERRANO CANTADOR the same person? does not seem so, different institution & country
  # are JON, SOLAR and JON, SOLAR IRAZABAL the same person? yes, same affiliation

  mutate(b_lastname = if_else(name %in% "HELENE, THOMAS", "THOMAS GUYON", b_lastname))%>%
  mutate(b_firstname = if_else(name %in% "MARIA FRANCISCA, MANO", "FRANCISCA", b_firstname)) %>%
  mutate(b_firstname = if_else(name %in% "T, CANTAT", "THIBAULT", b_firstname)) %>%
  mutate(b_lastname = if_else(name %in% "MARIA, FUENTES", "FUENTES CAMPOS", b_lastname)) %>%
  mutate(b_lastname = if_else(name %in% "JON, SOLAR", "SOLAR IRAZABAL", b_lastname)) # file with 4129 rows

# remaining question: how does LILIANA A, RODRIGUEZ became	LILIANA, RODRIGUEZ and are we sure it is the same person? different city, same country, different year, different email address...

out <- ncor %>%
       distinct(b_firstname, b_lastname) # 3893 unique id # issue: previousely: 3889

# reorder inversed names (from Dimensions) using Aïssa's file "name_isgc_inverse"

nameinv <- read_csv("data-net/name_isgc_inverse.csv")

ncor <- ncor %>%
  left_join(nameinv, by = c("b_lastname" = "family_name", "b_firstname" = "first_name")) %>%
  mutate(c_firstname = ifelse(is.na(id), b_firstname, b_lastname),
         c_lastname = ifelse(is.na(id), b_lastname, b_firstname))


# integrate cleaned names in 'authors' and 'authors_abstracts' tables - n is an intermediary table

n <- n %>%
           left_join(select(ncor, a_firstname, a_lastname, c_firstname, c_lastname)) # 4947 rows # Joining, by = c("a_firstname", "a_lastname")

authors <- authors %>%
  left_join(select(n, idind, c_firstname, c_lastname)) # Joining, by = "idind"

authors_abstracts <- authors_abstracts %>%
  left_join(authors) %>% # 6485 rows
  drop_na(c_lastname) %>% # remove NA (in a later stage try to find them in another table) --> 6459 rows (26 NA have been removed)
  unite("i", c_firstname:c_lastname, sep = ", ", remove = F) %>%
  distinct() # from 6459 rows to 6451


length(unique(authors_abstracts$i)) #3867 unique names

write_tsv(authors_abstracts, "data-net/edges-2015-2019.tsv")
authors_abstracts <- read_tsv("data-net/edges-2015-2019.tsv")

############################## ###################################################################################################

# ==============================================================================
# COUNTS
# ==========================================================================================
# AFSP equivalent: https://github.com/briatte/congres-afsp/blob/master/01_data.r line 267
# ==========================================================================================

# keeping everyone including industrials and keynote speakers

d <- authors_abstracts  %>%
     mutate(year = as.numeric(year))

# how many participations over the 3 conferences?
t <- group_by(d, i) %>%
  summarise(t_c = n_distinct(year)) %>%
  arrange(-t_c)

table(t$t_c) # 150 participants went to all conferences, ~ 3284 went to only 1, 459 went to 2
table(t$t_c > 1) / nrow(t) # ~ 85% were involved only 1 of 3 conferences in 5 years # FALSE: 0.8435654 TRUE: 0.1564346

# limiting ourselves to oral communications and flash communications panels
d <- d %>%
  filter(final_status %in% c("OC","FC", "FC - PC", "OC - PC"))

# how many participations to OC and FC over the 3 conferences?
t <- group_by(d, i) %>%
  summarise(t_c = n_distinct(year)) %>%
  arrange(-t_c)

table(t$t_c) # 94 participants communicated to each conference, ~ 2034 communicated only once, 283 communicated twice
table(t$t_c > 1) / nrow(t) # ~ 85% communicated only 1 of 3 conferences in 5 years # FALSE: 0.8436333 TRUE: 0.1563667

# number of OC and FC panels overall
n_distinct(d$j) # 219

# number of panels in each conference
cat("\nPanels per conference:\n\n")
print(tapply(d$j, d$year, n_distinct))

'2015 2017 2019
  64   78   77 '

# add number of panels intervention per conference
# (useful for edge weighting)
d <- group_by(d, year, i) %>%
  summarise(n_p = n()) %>%
  inner_join(d, ., by = c("year", "i"))

# add total number of panels and total number of conferences with at least one communication
# (useful for vertex subsetting)
d <- group_by(d, i) %>%
  summarise(t_p = n_distinct(j), t_c = n_distinct(year)) %>%
  inner_join(d, ., by = "i")

# nb of authors per paper

nbaut <- d  %>%
  group_by(id, year) %>%
  count(sort = T)

# mean nb of authors per paper

mean(nbaut$n) # 4.130841

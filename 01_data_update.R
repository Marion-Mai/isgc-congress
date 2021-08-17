## ISGC NETCONF SCRIPT 1 - SUNBELT 2020 - 01_data.R
## first draft: 2020-07-14 Bastille Day; last edit: 2021-08-05
## M. Maisonobe & F. Briatte

# myPaths <- .libPaths()
# myPaths <- c(myPaths[2], myPaths[1])  # switch them
# .libPaths(myPaths)  # reassign them

# load libraries

library(dplyr) # for data wrangling
library(purrr) # for map()
library(readr) # for write_tsv
library(stringr) # for str_extract
library(tidyr) # for unite

# setwd("C:/Data_Marion/check")

# load the 2015-2019 abstracts dataset
abstracts <- read_tsv("data/abstracts-2015-2019.tsv")

# load the 2015-2019 authors-abstract dataset
authors_abstracts <- read_tsv("data/authors-abstracts-2015-2019.tsv")

# remove line breaks, tabs etc. in the affiliations' field
authors_abstracts$institution <- str_replace_all(authors_abstracts$institution, "[\\r\\n\\t]+", " ") # %>%
  # str_trim(str_replace_all(authors_abstracts$institution, "\\s+", " ")) ## remove excessive white space if necessary

authors_abstracts$institution[authors_abstracts$lastname %in% "ROBCIUC"] # Check that the tab has been removed from Robciuc's affiliation


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

# isolate authors identity information only
authors <- authors_abstracts %>%
  distinct(firstname, lastname, email) %>%
  mutate(idind = row_number())%>%
  arrange() # 4947 rows

# finalize participant names
authors <- authors %>%
  mutate(across(.cols = c(firstname, lastname), .fns = function(x) {
    str_to_upper(x) %>%
      iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
      str_replace_all("\\.", " ") %>% # to keep track of the initials
      str_remove_all("[\"^'*`\\\\~]") %>%
      str_replace_all("-|\\s+", " ") %>%
      str_trim }, .names =  "a_{col}")) %>% 
  mutate(email = str_to_lower(email))  # file with 4947 rows


# reorder inversed names (from Dimensions) using Aissa's file "name_isgc_inverse"

nameinv <- read_csv("data-net/name_isgc_inverse.csv")  %>%
  rename(inv = id)

authors <- authors %>%
  left_join(nameinv, by = c("a_lastname" = "family_name", "a_firstname" = "first_name")) %>%
  mutate(b_firstname = ifelse(is.na(inv), a_firstname, a_lastname),
         b_lastname = ifelse(is.na(inv), a_lastname, a_firstname))  %>%
  select(-c(inv))

n <- authors %>%
  distinct(b_firstname, b_lastname, email) %>%
  group_by(b_firstname, b_lastname)  %>%
  summarise(email = first(email), nb_email = n()) # file with 4081 rows

n <- n %>%
  mutate(email = ifelse(email %in% c(1, 1342), paste0("Tag", "_", b_firstname, "_", b_lastname), email)) %>% # Dont take the organisation team's emails into account (email 1342 and 1)
  filter(email != "Tag_NA_NA") %>%
  group_by(b_lastname, email) %>%
  mutate(b_firstname = b_firstname[which.max(str_length(b_firstname))], # keep the longest name instead of the first
         b_lastname = first(b_lastname), email = first(email), nb_email = first(nb_email)) %>%
  ungroup() %>%
  filter(email != "Tag_NESSE_NA") %>% # remove the author's line "NESSE" (name of an organisation, not an author)
  group_by(b_firstname, email) %>% #
  mutate(b_lastname = b_lastname[which.max(str_length(b_lastname))]) %>% #
  ungroup() %>%
  mutate(fname = word(b_firstname, 1, sep = " ")) %>% # 1st firstname
  mutate(lname = word(b_lastname, 1, sep = " ")) %>% # lst lastname
  mutate(init = str_extract(b_firstname, "^.{1}")) %>% # 1st character of firstnames
  group_by(fname, b_lastname) %>%
  mutate(b_firstname = b_firstname[which.max(str_length(b_firstname))]) %>% # keep the longest name instead of the first
  filter(!is.na(init), !is.na(b_lastname)) %>% # remove na
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
  # n %>%
  # mutate(
  #  changed = (b_firstname_copy != b_firstname),
  #  id = cur_group_id()
  # ) %>%
  # show every group for which at least one b_firstname has changed
  #   filter(id %in% id[ changed ]) %>%
  #   group_split()
  
  ungroup() %>%
  select(-c(b_firstname_copy)) %>%
  unite("name", b_firstname:b_lastname, sep = ", ", remove = F)

# write_tsv(n, "authors.tsv")
# n <- read_tsv("authors_v3.tsv")

  # (2) names with only 1 or 2 different letters:
   library(stringdist)
    for(i in unique(n$name)) {
     m <- stringdist::stringdist(i, unique(n$name))
      m <- which(m > 0 & m < 3)
      if (length(m) > 0)
       cat(i, ":", str_c("\n ~ ", unique(n$name)[ m ]), "\n\n") 
    }
  
  # Remove titles: Prof, Dr, etc.
  mutate(b_firstname = if_else(b_lastname %in% "CORDOVA", "ARMANDO", b_firstname)) %>% # added 23.07.2021
  
  mutate(b_firstname = if_else(b_lastname %in% "NARDELLO RATAJ", "VERONIQUE", b_firstname)) %>% # correct mispelled firstname "VERONQIUE"
  mutate(b_lastname = if_else(b_firstname %in% "ZHIHONG", "WEI", b_lastname)) %>% # correct mispelled lastname "WIE" # https://orcid.org/0000-0002-9460-7908
  mutate(b_firstname = if_else(b_lastname %in% "KOELEWIJN", "STEVEN FRISO", b_firstname)) %>% # Stef --> STEVEN FRISO
  mutate(b_firstname = if_else(b_lastname %in% "CARMICHAEL", "EUGENE", b_firstname)) %>% # Eugen --> Eugene
  mutate(b_lastname = if_else(b_lastname %in% "VANBOREKHOVEN", "VANBROEKHOVEN", b_lastname)) %>%
  mutate(b_lastname = if_else(b_lastname %in% "RODENAS OLALLA", "RODENAS OLAYA", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "WAN MOHD ASHRI", "WAN MOHD ASHRI WAN", b_firstname),
         b_lastname =  if_else(b_lastname %in% "WAN MOHD ASHRI", "DAUD", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "WAN DAUD", "WAN MOHD ASHRI WAN", b_firstname),
         b_lastname =  if_else(b_lastname %in% "WAN DAUD", "DAUD", b_lastname)) %>% 
  mutate(b_lastname = if_else(b_lastname %in% "BJARACHARYA", "BAJRACHARYA", b_lastname)) %>%
  mutate(b_lastname = if_else(b_lastname %in% "HESEMAN", "HESEMANN", b_lastname)) %>%
  mutate(b_lastname = if_else(b_firstname %in% "AIGARS", "PAZHE", b_lastname)) %>% # PAZE --> PAZHE
  mutate(b_lastname = if_else(b_lastname %in% "NEMMATI KHARAT", "NEMATI KHARAT", b_lastname)) %>%
  mutate(b_lastname = if_else(b_lastname %in% "NEGAHDAR", "NEGAHDAR", b_lastname)) %>%
  mutate(b_lastname = if_else(b_lastname %in% "PERA TITUS EXT", "PERA TITUS", b_lastname)) %>%
  mutate(b_lastname = if_else(b_lastname %in% "GALLUCI", "GALLUCCI", b_lastname)) %>% # file with 4129 rows
  mutate(b_firstname = if_else(b_lastname %in% "BURGUETE", "MARIA ISABEL", b_firstname)) %>% # added 23.07.2021
  mutate(b_firstname = if_else(b_lastname %in% "RODE", "CHANDRASHEKHAR", b_firstname)) %>% # added 23.07.2021
  mutate(b_firstname = if_else(b_lastname %in% "KRUGER", "ANDREAS JD", b_firstname)) %>% # added 05.08.2021
  mutate(b_lastname = if_else(b_lastname %in% "SARI SCHMAYSSEM", "SARI CHMAYSSEM", b_lastname)) %>% # added 05.08.2021 # the two forms can be used
  mutate(b_lastname = if_else(b_lastname %in% "WALMSLAY", "WALMSLEY", b_lastname)) %>% # added 05.08.2021
  mutate(b_lastname = if_else(b_lastname %in% "BUTT", "BUT", b_lastname)) %>% # added 05.08.2021
  mutate(b_lastname = if_else(b_lastname %in% "LE BRECHT", "LE BRECH", b_lastname)) %>% # added 05.08.2021
  mutate(b_lastname = if_else(b_lastname %in% "BAUEMEL", "BAUMEL", b_lastname)) %>% # added 05.08.2021
  mutate(b_lastname = if_else(b_lastname %in% "DESHAYESD", "DESHAYES", b_lastname)) %>% # added 05.08.2021
  mutate(b_lastname = if_else(b_lastname %in% "REBUTEAU", "RABUTEAU", b_lastname)) %>% # added 05.08.2021
  mutate(b_lastname = if_else(b_lastname %in% "AL NAJAR", "AL NAJJAR", b_lastname)) %>% # added 05.08.2021
  mutate(b_lastname = if_else(b_lastname %in% "HEVERKERL", "HEVEKERL", b_lastname)) %>% # added 05.08.2021
  mutate(b_lastname = if_else(b_lastname %in% "GUGLIARELLI", "GUIGLIARELLI", b_lastname)) %>% # added 05.08.2021
  mutate(b_lastname = if_else(b_lastname %in% "DEBORA BIZZO BRUM PEREIRA", "BIZZO BRUM PEREIRA", b_lastname)) %>% # added 05.08.2021
  mutate(b_lastname = if_else(b_lastname %in% "GALVEZ", "GALVEZ PARRUCA", b_lastname)) %>% # added 05.08.2021 # sometimes Maria Elena Galvez only
  mutate(b_firstname = if_else(b_lastname %in% "WIKEE", "SAOWANEE", b_firstname)) %>% # added 05.08.2021
  mutate(b_firstname = if_else(b_lastname %in% "BELAABED", "RAJA", b_firstname)) %>% # added 05.08.2021
  mutate(b_firstname = if_else(b_lastname %in% "CAILLOL", "SYLVAIN", b_firstname)) %>% # added 05.08.2021
  mutate(b_firstname = if_else(b_lastname %in% "MARTIN LARA", "MARIA ANGELES", b_firstname)) %>% # added 05.08.2021
  mutate(b_firstname = if_else(b_lastname %in% "GALVEZ PARRUCA", "MARIA ELENA", b_firstname)) %>% # added 05.08.2021
  mutate(b_firstname = if_else(b_lastname %in% "MOUSELMANI", "RIM", b_firstname)) %>% # added 05.08.2021
  mutate(b_firstname = if_else(b_lastname %in% "FICHET", "DENIS", b_firstname)) %>% # added 05.08.2021
  mutate(b_firstname = if_else(b_lastname %in% "BERNAS", "HEIDI", b_firstname)) %>% # added 05.08.2021
  mutate(b_firstname = if_else(b_lastname %in% "HERRERA CANO", "NATIVIDAD", b_firstname)) %>% # added 05.08.2021
  mutate(b_firstname = if_else(b_lastname %in% "KOUMBA YOYA", "GEORGES THIBAUT", b_firstname)) %>% # added 05.08.2021
  mutate(b_firstname = if_else(b_lastname %in% "MADJINZA", "DESIX DIANE", b_firstname)) %>% # added 05.08.2021 also known as Desix Madjinza only
  mutate(b_firstname = if_else(b_firstname %in% "DIVYANAG M", "DIVYANG M", b_firstname)) %>%  # added 05.08.2021
  mutate(b_lastname = if_else(b_firstname %in% "DUMEIGNIL", "DUMEIGNIL", b_lastname), # added 06.08.2021 - name inversion
         b_firstname =  if_else(b_firstname %in% "DUMEIGNIL", "FRANCK", b_firstname)) %>%
  mutate(b_lastname = if_else(b_lastname %in% "NEGHADAR", "NEGAHDAR", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "DA SILVA PEREZ", "DENILSON", b_firstname)) %>%
  mutate(b_lastname = if_else(b_firstname %in% "DENILSON DA", "DA SILVA PEREZ", b_lastname),
         b_firstname = if_else(b_firstname %in% "DENILSON DA", "DENILSON", b_firstname)) %>%
  mutate(b_lastname = if_else(b_lastname %in% "SILVA PEREZ", "DA SILVA PEREZ", b_lastname)) # modif edge.tsv # added 06.08.2021 - 10:00
# 4129 rows


mutate(b_lastname = if_else(b_lastname %in% "M MARRUCHO", "MARRUCHO", b_lastname)) %>%

list <- ncor %>% distinct(b_firstname, b_lastname) %>%
  filter(str_detect(b_lastname, "^(\\w\\s)+"))  # manual action required for A RITA C DUARTE, ANA M MATIAS, L REIS etc.

# (3) check for names included in others names
for (i in unique(ncor$name)){
  j <- str_subset(ncor$name[ ncor$name != i ], i)
  if (length(j)) cat(i, ":", str_c(j, collapse = ","), "\n")
}

ncor <- ncor %>%
  mutate(b_firstname = if_else(b_lastname %in% "C COSTA", "PAULO C", b_firstname),
         b_lastname =  if_else(b_lastname %in% "C COSTA", "COSTA", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "M CASTRO", "ALINE M", b_firstname),
         b_lastname =  if_else(b_lastname %in% "M CASTRO", "CASTRO", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "S CASTRO", "CINTHIA S", b_firstname),
         b_lastname =  if_else(b_lastname %in% "S CASTRO", "CASTRO", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "S DAMASCENO", "AMANDA S", b_firstname),
         b_lastname =  if_else(b_lastname %in% "S DAMASCENO", "DAMASCENO", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "M MATIAS", "ANA M", b_firstname),
         b_lastname =  if_else(b_lastname %in% "M MATIAS", "MATIAS", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "S MORAIS", "EDUARDA S", b_firstname),
         b_lastname =  if_else(b_lastname %in% "S MORAIS", "MORAIS", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "A MANOEL", "EVELIN A", b_firstname),
         b_lastname =  if_else(b_lastname %in% "A MANOEL", "MANOEL", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "M ROBERT", "JULIA M", b_firstname),
         b_lastname =  if_else(b_lastname %in% "M ROBERT", "ROBERT", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "A PRIETO", "MIGUEL A", b_firstname),
         b_lastname =  if_else(b_lastname %in% "A PRIETO", "PRIETO", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "O SOARES", "PEDRO", b_firstname),
         b_lastname =  if_else(b_lastname %in% "O SOARES", "SOARES", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "A O SANTOS", "SONIA A O", b_firstname),
         b_lastname =  if_else(b_lastname %in% "A O SANTOS", "SANTOS", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "E SINTRA", "TANIA E", b_firstname),
         b_lastname =  if_else(b_lastname %in% "E SINTRA", "SINTRA", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "P M VENTURA", "SONIA P M", b_firstname),
         b_lastname =  if_else(b_lastname %in% "P M VENTURA", "VENTURA", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "N H SILVA", "NUNO N H", b_firstname),
         b_lastname =  if_else(b_lastname %in% "N H SILVA", "SILVA", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "M DE SOUZA", "PRISCILLA M", b_firstname),
         b_lastname =  if_else(b_lastname %in% "M DE SOUZA", "DE SOUZA", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "C RODRIGUES", "RAFAEL C", b_firstname),
         b_lastname =  if_else(b_lastname %in% "C RODRIGUES", "RODRIGUES", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "C CALHELHA", "RICARDO C", b_firstname),
         b_lastname =  if_else(b_lastname %in% "C CALHELHA", "CALHELHA", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "C GUEDES DA SILVA", "MARIA FATIMA C", b_firstname), # https://app.dimensions.ai/details/entities/publication/author/ur.010030064533.77
         b_lastname =  if_else(b_lastname %in% "C GUEDES DA SILVA", "GUEDES DA SILVA", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "C F R FERREIRA", "ISABEL C F R", b_firstname),
         b_lastname =  if_else(b_lastname %in% "C F R FERREIRA", "FERREIRA", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "F ALMEIDA", "ISABEL F", b_firstname),
         b_lastname =  if_else(b_lastname %in% "F ALMEIDA", "ALMEIDA", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "V M NUNES", "ANA V M", b_firstname),
         b_lastname =  if_else(b_lastname %in% "V M NUNES", "NUNES", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "V DE ROSSO", "VERIDIANA V", b_firstname),
         b_lastname =  if_else(b_lastname %in% "V DE ROSSO", "DE ROSSO", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "T DA SILVA", "VICTOR T", b_firstname),
         b_lastname =  if_else(b_lastname %in% "T DA SILVA", "DA SILVA", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "C DUARTE", "ANA RITA C", b_firstname),
         b_lastname =  if_else(b_lastname %in% "C DUARTE", "DUARTE", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "L REIS", "RUI L", b_firstname),
         b_lastname =  if_else(b_lastname %in% "L REIS", "REIS", b_lastname)) %>%
  mutate(b_firstname = if_else(name %in% "JOSE, I GARCIA", "JOSE I", b_firstname),
         b_lastname =  if_else(name %in% "JOSE, I GARCIA", "GARCIA", b_lastname)) %>%
  mutate(b_firstname = if_else(name %in% "JOSE, M ASSAF", "JOSE M", b_firstname),
         b_lastname =  if_else(name %in% "JOSE, M ASSAF", "ASSAF", b_lastname)) %>%
  mutate(b_firstname = if_else(name %in% "SIMAO, P PINHO", "SIMAO P", b_firstname),
         b_lastname =  if_else(name %in% "SIMAO, P PINHO", "PINHO", b_lastname)) %>%
  mutate(b_firstname = if_else(name %in% "JOSE CLEITON, S DOS SANTOS", "JOSE CLEITON S", b_firstname), #Jos? Cleiton Sousa dos Santos : https://orcid.org/0000-0002-1511-5180
         b_lastname =  if_else(name %in% "JOSE CLEITON, S DOS SANTOS", "DOS SANTOS", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "WASSERCHEID", "PETER", b_firstname),
         b_lastname =  if_else(b_lastname %in% "WASSERCHEID", "WASSERSCHEID", b_lastname)) %>% # corrected 05.08.2021
  mutate(b_firstname = if_else(b_lastname %in% "SONIA MILENA", "SONIA MILENA", b_firstname),
         b_lastname =  if_else(b_lastname %in% "SONIA MILENA", "AGUILERA SEGURA", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "CONSTABLE", "DAVID J", b_firstname)) %>% # added 22.06.2021 # check if it work! (05.08.2021)
  mutate(b_lastname =  if_else(b_lastname %in% "CONSTABLE", "CHICHESTER CONSTABLE", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "JEAN MICHEL", "JEAN MICHEL", b_firstname), # added 23.07.2021
         b_lastname =  if_else(b_lastname %in% "JEAN MICHEL", "TATIBOUET", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "MOHD ZAINI", "NURUL AQILAH MOHD", b_firstname), # added 23.07.2021
         b_lastname =  if_else(b_lastname %in% "MOHD ZAINI", "ZAINI", b_lastname)) %>%
  mutate(b_lastname =  if_else(b_firstname %in% "PINSOLLE EXTERIEUR", "PINSOLLE", b_lastname)) %>%
  mutate(b_firstname = if_else(b_firstname %in% "PINSOLLE EXTERIEUR", "ALEXANDRE", b_firstname)) %>%  # added 23.07.2021
  mutate(b_firstname = if_else(b_firstname %in% "SAADIA", "SAIDIA", b_firstname)) %>% # added 05.08.2021
  mutate(b_lastname =  if_else(b_lastname %in% "CHERIAF", "CHERIEF", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "THIMOTEO AZEVEDO JORGE", "FERNANDA THIMOTEO AZEVEDO", b_firstname), # added 05.08.2021
         b_lastname =  if_else(b_lastname %in% "THIMOTEO AZEVEDO JORGE", "JORGE", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "OULD DRISS", "AISSA", b_firstname), # added 05.08.2021
         b_lastname =  if_else(b_lastname %in% "OULD DRISS", "OULD DRIS", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "J LEACHY", "JAMES J", b_firstname), # added 05.08.2021
         b_lastname =  if_else(b_lastname %in% "J LEACHY", "LEAHY", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "REMY", "REMY", b_firstname), # added 05.08.2021
         b_lastname =  if_else(b_lastname %in% "REMY", "LAUNEZ", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "ZEINEDINE", "ZEINEDDINE", b_firstname), # added 05.08.2021
         b_lastname =  if_else(b_lastname %in% "ZEINEDINE", "DJEGHABA", b_lastname)) %>%
  mutate(b_lastname = if_else(b_firstname %in% "CONSTABLE", "CHICHESTER CONSTABLE", b_lastname), # added 06.08.2021 - name inversion
         b_firstname =  if_else(b_firstname %in% "CONSTABLE", "DAVID J", b_firstname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "ANDRIANARIVO IRENE", "ANDRIANARIVO IRENE", b_firstname), # added 05.08.2021 - name inversion
         b_lastname =  if_else(b_lastname %in% "ANDRIANARIVO IRENE", "RAHOBINIRINA", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "GELVES", "JOHN FREDDY", b_firstname), # added 05.08.2021 - also known as JF GELVES; https://orcid.org/0000-0002-1238-6911
         b_lastname =  if_else(b_lastname %in% "GELVES", "GELVES DIAZ", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "MICHAELE", "MIARINTSOA MICHAELE", b_firstname), # added 05.08.2021 - registered as Michaele RANARIJAONA only
         b_lastname =  if_else(b_lastname %in% "MICHAELE", "RANARIJAONA", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "JE PEREANEZ", "JUAN ENRIQUE", b_firstname), # added 05.08.2021 - registered as Sacarias, JE PEREANEZ; https://orcid.org/0000-0002-7147-9210
         b_lastname =  if_else(b_lastname %in% "JE PEREANEZ", "PEREANEZ SACARIAS", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "RECHULSKI", "MARCELO DAVID", b_firstname), # added 05.08.2021 - registered as Sacarias, JE PEREANEZ; https://orcid.org/0000-0002-7147-9210
         b_lastname =  if_else(b_lastname %in% "RECHULSKI", "KAUFMAN RECHULSKI", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "A P COUTINHO", "JOAO A P", b_firstname), # modif edge.tsv - added 05.08.2021 18:00
         b_lastname =  if_else(b_lastname %in% "A P COUTINHO", "COUTINHO", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "J D SILVESTRE", "ARMANDO J D", b_firstname), # modif edge.tsv - added 05.08.2021 18:00
         b_lastname =  if_else(b_lastname %in% "J D SILVESTRE", "SILVESTRE", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "CBA ALEGRIA", "ELISABETE CBA", b_firstname), # modif edge.tsv - added 05.08.2021 18:00
         b_lastname =  if_else(b_lastname %in% "CBA ALEGRIA", "ALEGRIA", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "M G FREIRE", "DENISE M G", b_firstname), # modif edge.tsv - added 05.08.2021 18:00
         b_lastname =  if_else(b_lastname %in% "M G FREIRE", "FREIRE", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "S R FREIRE", "CARMEN S R", b_firstname), # modif edge.tsv - added 05.08.2021 18:00
         b_lastname =  if_else(b_lastname %in% "S R FREIRE", "FREIRE", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "G FREIRE", "MARA G", b_firstname), # modif edge.tsv - added 05.08.2021 18:00
         b_lastname =  if_else(b_lastname %in% "G FREIRE", "FREIRE", b_lastname)) %>%
  mutate(b_firstname = if_else(b_firstname %in% "MARA G FREIRE", "MARA G", b_firstname)) %>% # modif edge.tsv - added 05.08.2021 18:00
  mutate(b_firstname = if_else(b_lastname %in% "BC SIMAS", "ALESSANDRO BC", b_firstname), # modif edge.tsv - added 05.08.2021 18:00
         b_lastname =  if_else(b_lastname %in% "BC SIMAS", "SIMAS", b_lastname)) %>%
  mutate(b_lastname = if_else(b_firstname %in% "EDUARDO FERREIRA DA", "FERREIRA DA SILVA", b_lastname), # modif edge.tsv - added 05.08.2021 18:00
         b_firstname =  if_else(b_firstname %in% "EDUARDO FERREIRA DA", "EDUARDO", b_firstname)) %>%
  
  # is Kaufman Rechulski, Marcelo Daniel the same person as Kaufman Rechulski, Marcelo David? and therefore Kaufman Rechulski, Marcelo D.
  
  # Remaining questions:
  # are ANA LOPEZ and ANA LOPEZ CONTRERAS the same person? does not seem so, different institution & country
  # are FRANCISCA MANO and MARIA FRANCISCA MANO the same person? yes, same affiliation
  # are FRANCISCO, VILA and FRANCISCO, VILAPLANA the same person? does not seem so, different institution & country
  # are MARIA, FUENTES and MARIA, FUENTES CAMPOS the same person? yes, same affiliation
  # are LUIS, SERRANO and LUIS, SERRANO CANTADOR the same person? does not seem so, different institution & country
  # are JON, SOLAR and JON, SOLAR IRAZABAL the same person? yes, same affiliation
  
mutate(b_lastname = if_else(name %in% "HELENE, THOMAS", "THOMAS GUYON", b_lastname))%>%
  mutate(b_firstname = if_else(name %in% "PROF DR RUDOLF, PIETSCHNIG", "RUDOLF", b_firstname))%>%
  mutate(b_firstname = if_else(name %in% "DR ARSLAN, AFTAB", "ARSLAN", b_firstname))%>%
  mutate(b_firstname = if_else(name %in% "DR ARSLAN, AFTAB", "ARSLAN", b_firstname))%>%
  mutate(b_firstname = if_else(name %in% "MARIA FRANCISCA, MANO", "FRANCISCA", b_firstname)) %>%
  mutate(b_firstname = if_else(name %in% "T, CANTAT", "THIBAULT", b_firstname)) %>%
  mutate(b_lastname = if_else(name %in% "MARIA, FUENTES", "FUENTES CAMPOS", b_lastname)) %>%
  mutate(b_lastname = if_else(name %in% "JON, SOLAR", "SOLAR IRAZABAL", b_lastname))  %>%
  mutate(b_lastname = if_else(b_lastname %in% "SERRANO", "SERRANO CANTADOR", b_lastname)) %>%#  modif edge.tsv  - added 05.08.2021 18:00
  mutate(b_firstname = if_else(b_lastname %in% "CATHERINE", "CATHERINE", b_firstname), #  modif edge.tsv  - added 05.08.2021 18:00
         b_lastname =  if_else(b_lastname %in% "CATHERINE", "BATIOT DUPEYRAT", b_lastname)) %>%
  mutate(b_firstname = if_else(b_firstname %in% "KRIJN", "KRIJN P", b_firstname)) %>% # modif in edge.tsv - O6.08.2021
  mutate(b_lastname = if_else(b_firstname %in% "MARCOAURELIO", "RODRIGUES", b_lastname), #  modif edge.tsv  - added 13.08.2021 16:30
         b_firstname =  if_else(b_firstname %in% "MARCOAURELIO", "MARCOAURELIO ALMENARA", b_firstname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "ARARIPE TORRES", "FERNANDO ARARIPE", b_firstname), #  modif edge.tsv  - added 13.08.2021 16:30
         b_lastname =  if_else(b_lastname %in% "ARARIPE TORRES", "TORRES", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "ASMELASH", "CHALACHEW ASMELASH", b_firstname), #  modif edge.tsv  - added 13.08.2021 16:30
         b_lastname =  if_else(b_lastname %in% "ASMELASH", "MEBRAHTU", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "AT DA SILVA", "ANGELO AT", b_firstname), #  modif edge.tsv  - added 13.08.2021 16:30
         b_lastname =  if_else(b_lastname %in% "AT DA SILVA", "DA SILVA", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "CC PINTO", "MARTINA CC", b_firstname), #  modif edge.tsv  - added 13.08.2021 16:30
         b_lastname =  if_else(b_lastname %in% "CC PINTO", "PINTO", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "CO MACHADO", "ANTONIO CO", b_firstname), #  modif edge.tsv  - added 13.08.2021 16:30
         b_lastname =  if_else(b_lastname %in% "CO MACHADO", "MACHADO", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "DE CARVALHO IFF", "LUCAS IFF", b_firstname), #  modif edge.tsv  - added 13.08.2021 16:30
         b_lastname =  if_else(b_lastname %in% "DE CARVALHO IFF", "DE CARVALHO", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "FP RIBEIRO", "MARCELA FP", b_firstname), #  modif edge.tsv  - added 13.08.2021 16:30
         b_lastname =  if_else(b_lastname %in% "FP RIBEIRO", "RIBEIRO", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "FRANCA PENNA RIBEIRO", "MARCELA FP", b_firstname), #  modif edge.tsv  - added 13.08.2021 16:30
         b_lastname =  if_else(b_lastname %in% "FRANCA PENNA RIBEIRO", "RIBEIRO", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "JEAN MARC", "JEAN MARC", b_firstname), #  modif edge.tsv  - added 13.08.2021 16:30
         b_lastname =  if_else(b_lastname %in% "JEAN MARC", "CLACENS", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "JEAN PIERRE", "JEAN PIERRE", b_firstname), #  modif edge.tsv  - added 13.08.2021 16:30
         b_lastname =  if_else(b_lastname %in% "JEAN PIERRE", "LLORED", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "MV ABREU", "RUI MV", b_firstname), #  modif edge.tsv  - added 13.08.2021 16:30
         b_lastname =  if_else(b_lastname %in% "MV ABREU", "ABREU", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "OO GONCALVES", "VINICIUS OTTONIO OLIVEIRA", b_firstname), #  modif edge.tsv  - added 13.08.2021 16:30
         b_lastname =  if_else(b_lastname %in% "OO GONCALVES", "GONCALVES", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "OLIVEIRA GONCALVES", "VINICIUS OTTONIO OLIVEIRA", b_firstname), #  modif edge.tsv  - added 13.08.2021 16:30
         b_lastname =  if_else(b_lastname %in% "OLIVEIRA GONCALVES", "GONCALVES", b_lastname)) %>%
  mutate(b_firstname = if_else(b_lastname %in% "RB GONCALVES", "LUCIANA RB", b_firstname), #  modif edge.tsv  - added 13.08.2021 16:30
         b_lastname =  if_else(b_lastname %in% "RB GONCALVES", "GONCALVES", b_lastname)) %>%
  mutate(b_lastname = if_else(b_firstname %in% "KOTESWARA", "KOTESWARA RAO", b_lastname), #  modif edge.tsv  - added 13.08.2021 16:30
         b_firstname =  if_else(b_firstname %in% "KOTESWARA", "NIDADAVOLU", b_firstname)) %>%
  mutate(b_lastname = if_else(b_firstname %in% "VALERIA DE CARVALHO SANTOS", "VALERIA DE CARVALHO", b_lastname), #  modif edge.tsv  - added 13.08.2021 16:30
         b_firstname =  if_else(b_firstname %in% "VALERIA DE CARVALHO SANTOS", "SANTOS EBINUMA", b_firstname))  %>%
  mutate(b_lastname = if_else(b_firstname %in% "VALERIA DE C", "VALERIA DE CARVALHO", b_lastname), #  modif edge.tsv  - added 13.08.2021 16:30
         b_firstname =  if_else(b_firstname %in% "VALERIA DE C", "SANTOS EBINUMA", b_firstname))


# file with 4129 rows

# remaining question: how does LILIANA A, RODRIGUEZ became	LILIANA, RODRIGUEZ and are we sure it is the same person? different city, same country, different year, different email address...

out <- ncor %>%
  distinct(b_firstname, b_lastname) # 3884 unique id # previousely: 3889

# reorder inversed names (from Dimensions) using Aissa's file "name_isgc_inverse"

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

authors_abstracts  <- authors_abstracts %>%
  left_join(authors) %>% # 6485 rows
  drop_na(c_lastname) %>% # remove NA (in a later stage try to find them in another table) --> 6459 rows (26 NA have been removed)
  unite("i", c_firstname:c_lastname, sep = ", ", remove = F) %>%
  distinct() # from 6459 rows to 6451

authors_abstracts %>%
  distinct(i, first_name, family_name) %>%
  write_tsv("authors.tsv")

authors_abstracts <- authors_abstracts %>% rename(family_name = c_lastname, first_name = c_firstname)

# d <- authors_abstracts

length(unique(authors_abstracts$i)) #3845 unique names

authors_abstracts$institution[authors_abstracts$lastname %in% "ROBCIUC"]

write_tsv(authors_abstracts, "data-net/edges-2015-2019-ok.tsv") #issue with TAB in the affiliation column - to be fixed : cf line ALEXANDRA ROBCIUC, 2017_1559_1431

write_csv(authors_abstracts, "data-net/edges-2015-2019.csv")
authors_abstracts <- read_tsv("data-net/edges-2015-2019-ok.tsv")
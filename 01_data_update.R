## ISGC NETCONF SCRIPT 1 - SUNBELT 2020 - 01_data.R
## first draft: 2020-07-14 Bastille Day; last edit: 2021-08-26
## M. Maisonobe & F. Briatte

# load libraries

library(dplyr) # for data wrangling
library(purrr) # for map()
library(readr) # for write_tsv
library(stringr) # for str_extract
library(tidyr) # for unite

# load the 2015-2019 abstracts dataset
abstracts <- read_tsv("data/abstracts-2015-2019.tsv")

# load the 2015-2019 authors-abstract dataset
authors_abstracts <- read_tsv("data/authors-abstracts-2015-2019.tsv")

# remove line breaks, tabs etc. in the affiliations' field to avoid parsing issues
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
  # keep track of the original form
  mutate(across(.cols = c(firstname, lastname), .fns = function(x) {str_to_upper(x) %>%
      str_replace_all("\\.", " ") %>%
      str_remove_all("[*:\\\\]") %>% 
      str_replace_all("\\s+", " ")  %>% 
      str_remove_all("DR |PROF |PR | EXT$| EXTERIEUR$| IFF") %>% # remove titles: Prof, Dr, and other artefacts... (IFF stands for IFF - Instituto Federal Fluminense in Rio de Janeiro)
      str_trim }, .names =  "original_{col}")) %>%  # to keep track of the initials
  # generate a simplified form
  mutate(across(.cols = c(firstname, lastname), .fns = function(x) {
    str_to_upper(x) %>%
      iconv(from = "UTF-8", to = "ASCII//TRANSLIT")  %>%
      str_replace_all("\\.", " ") %>% # to keep track of the initials
      str_remove_all("[\"^'*:`\\\\~]") %>%
      str_replace_all("-|\\s+", " ") %>%
      str_remove_all("DR |PROF |PR | EXT$| EXTERIEUR$| IFF") %>% # remove titles: Prof, Dr, and other artefacts... (IFF stands for IFF - Instituto Federal Fluminense in Rio de Janeiro)
      str_trim }, .names =  "a_{col}")) %>% 
  mutate(email = str_to_lower(email))  # file with 4947 rows

# check for inversed names
# upper_firstname <- authors %>%
# filter(! str_detect(firstname, "[a-z]"))


# reorder inversed names (from Dimensions) using Aissa's file + personnal additions "name_isgc_inverse"

nameinv <- read_csv("index/name_isgc_inverse.csv")  %>%
  select(-source) %>%
  rename(inv = id)

authors <- authors %>%
  left_join(nameinv, by = c("a_lastname" = "family_name", "a_firstname" = "first_name")) %>%
  mutate(b_firstname = ifelse(is.na(inv), a_firstname, a_lastname),
         b_lastname = ifelse(is.na(inv), a_lastname, a_firstname))  %>%
  mutate(original_firstname_cor = ifelse(is.na(inv), original_firstname, original_lastname),
         original_lastname = ifelse(is.na(inv), original_lastname, original_firstname))  %>%
  select(-c(inv, original_firstname)) %>%
  rename(original_firstname = original_firstname_cor)

# set a final and unique name form for each participant

n <- authors %>%
  distinct(b_firstname, b_lastname, email) %>%
  group_by(b_firstname, b_lastname)  %>%
  summarise(email = first(email), nb_email = n()) # file with 4066 rows

n <- n %>%
  
  # use emails to find homonyms
  
  mutate(email = ifelse(email %in% c(1, 1331, 1342), paste0("Tag", "_", b_firstname, "_", b_lastname), email)) %>% # Dont take the organisation team's emails into account (email 1342 and 1)
  filter(! email %in% c("Tag_NA_NA", "Tag_NESSE_NA")) %>% # remove the author's line "NESSE" (name of an organisation, not an author)
  group_by(b_lastname, email) %>%
  mutate(c_firstname = b_firstname[which.max(str_length(b_firstname))], # keep the longest name instead of the first
         c_lastname = first(b_lastname), email = first(email), nb_email = first(nb_email)) %>%
  ungroup() %>%
  group_by(c_firstname, email) %>% #
  mutate(c_lastname = c_lastname[which.max(str_length(c_lastname))]) %>% #
  ungroup() %>%
  mutate(fname = word(c_firstname, 1, sep = " ")) %>% # 1st firstname
  mutate(lname = word(c_lastname, 1, sep = " ")) %>% # lst lastname
  mutate(init = str_extract(c_firstname, "^.{1}")) %>% # 1st character of firstnames
  group_by(fname, c_lastname) %>%
  mutate(c_firstname = c_firstname[which.max(str_length(c_firstname))]) %>% # keep the longest name instead of the first
  filter(!is.na(init), !is.na(c_lastname)) %>% # remove na
  group_by(init, c_lastname) %>%
  mutate(
    # this is useful only for the checks below
    c_firstname_copy = c_firstname,
    c_firstname = if_else(
      n() > 1 & str_length(fname) == 1,
      c_firstname[ which.max(str_length(c_firstname)) ],
      c_firstname
    )
  )  %>%
  
   # CHECKS: (1) find people for which b_firstname was modified
   # n %>%
   # mutate(
    #changed = (b_firstname != c_firstname),
    #id = cur_group_id()
   #) %>%
   # show every group for which at least one c_firstname has changed
    # filter(id %in% id[ changed ]) %>%
    # group_split()
  
  ungroup() %>%
  select(-c(c_firstname_copy)) %>%
  
  # deal with misplaced initials (switching them from lastnames to firstnames)
  
  distinct(b_firstname, b_lastname, .keep_all = T) %>%
  mutate(middle = str_extract(c_lastname, "^(\\w\\s)+")) %>% # detect the initials appearing in the lastname column
  mutate(c_lastname = str_remove(c_lastname, "^(\\w\\s)+")) %>% # remove them from the lastname column
  unite("c_firstname", c(c_firstname, middle), sep = " ", na.rm = TRUE, remove = FALSE)  %>%  # add them at the end of the firstname
  mutate(c_firstname = str_trim(c_firstname), c_lastname = str_trim(c_lastname))  %>%
  mutate(middle = str_extract(c_firstname, "(\\s\\w)+$")) %>%  # alternative: extracting the first letter of each word (except the first one) in the firstname column
  
  # reproduce the previous steps
  
  mutate(fname = word(c_firstname, 1, sep = " "), # 1st firstname
         lname = word(c_lastname, 1, sep = " ")) %>% # lst lastname
  unite("name", c(c_firstname, c_lastname), sep = ", ", remove = F) %>%
  group_by(lname, email) %>%
  # filter(n()> 1) %>% distinct(c_firstname, c_lastname, .keep_all = "T") %>% to check the changes
  mutate(c_firstname = c_firstname[which.max(str_length(name))], # keep the longest name instead of the first
         c_lastname = c_lastname[which.max(str_length(name))], email = first(email), nb_email = first(nb_email)) %>%
  ungroup() %>%
  group_by(fname, email) %>% #
  mutate(c_lastname = c_lastname[which.max(str_length(name))],
  c_firstname = c_firstname[which.max(str_length(name))]) %>% #
  # filter(d_firstname != c_firstname | d_firstname != c_firstname ) (to check the changes)
  ungroup() %>%
  group_by(fname, c_lastname) %>%
  mutate(c_firstname = c_firstname[which.max(str_length(c_firstname))]) %>% # keep the longest name instead of the first
  # filter(c_firstname != c_firstname | c_firstname != c_firstname ) (to check the changes)
  ungroup() %>%
  unite("name", c(c_firstname, c_lastname), sep = ", ", remove = F)
 
 # Following these changes, a few errors need to be fixed
 #! JEONG KEE --> JEONG HOON ; JOSE PINTO --> JOSE CARLOS PINTO ; YOUNG WUN --> YOUNG SUG
 
 n <- n %>%
   mutate(c_firstname = if_else(b_firstname %in% "JEONG KEE", "JEONG KEE", c_firstname)) %>%
   mutate(c_firstname = if_else(b_firstname %in% "YOUNG WUN", "YOUNG WUN", c_firstname)) %>%
   mutate(c_firstname = if_else(b_firstname %in% "JOSE" & b_lastname %in% "PINTO", 
                                "JOSE", c_firstname))

# write_tsv(n, "authors.tsv")

# (2) names with only 1 or 2 different letters:
# library(stringdist)
# for(i in unique(n$name)) {
 # m <- stringdist::stringdist(i, unique(n$name))
 # m <- which(m > 0 & m < 3)
 # if (length(m) > 0)
 # cat(i, ":", str_c("\n ~ ", unique(n$name)[ m ]), "\n\n") 
# }

'AIGARS, PAZE :  ~ AIGARS, PAZHE  
ALI, NEMATI KHARAT :  ~ ALI, NEMMATI KHARAT 
DENEILSON, DA SILVA PEREZ :  ~ DENISLON, DA SILVA PEREZ 
EUGEN, CARMICHAEL :  ~ EUGENE, CARMICHAEL 
FAUSTO, GALLUCCI :  ~ FAUSTO, GALLUCI 
FRANCK, DUMEIGNIL :  ~ FRANK, DUMEIGNIL  
JEAN MICHEL, TTATIBOUET :  ~ JEAN MICHEL, TATIBOUET 
KAROLIEN, VANBOREKHOVEN :  ~ KAROLIEN, VANBROEKHOVEN 
LEILA, NEGAHDAR :  ~ LEILA, NEGHADAR 
PETER, HESEMAN :  ~ PETER, HESEMANN 
SUMAN, BAJRACHARYA :  ~ SUMAN, BJARACHARYA 
VERONIQUE, NARDELLO RATAJ :  ~ VERONQIUE, NARDELLO RATAJ 
WAN MOHD ASHRI, WAN DAUD :  ~ WAN MOHD ASHRI WAN, DAUD 
YOLANDA, RODENAS OLALLA :  ~ YOLANDA, RODENAS OLAYA 
ZHIHONG, WEI :  ~ ZHIHONG, WIE '

# Make manual changes following the results of check 2

n <- n %>%
  mutate(c_lastname = if_else(c_firstname %in% "ZHIHONG", "WEI", c_lastname)) %>% # misspelled lastname "WIE" # https://orcid.org/0000-0002-9460-7908
  mutate(c_lastname = if_else(c_lastname %in% "RODENAS OLALLA", "RODENAS OLAYA", c_lastname)) %>%
  mutate(c_firstname = if_else(c_lastname %in% "WAN DAUD", "WAN MOHD ASHRI WAN", c_firstname),
         c_lastname =  if_else(c_lastname %in% "WAN DAUD", "DAUD", c_lastname)) %>%
  mutate(c_firstname = if_else(c_lastname %in% "NARDELLO RATAJ", "VERONIQUE", c_firstname)) %>% # misspelled firstname "VERONQIUE"
  mutate(c_lastname = if_else(c_lastname %in% "BJARACHARYA", "BAJRACHARYA", c_lastname)) %>%
  mutate(c_lastname = if_else(c_lastname %in% "HESEMAN", "HESEMANN", c_lastname)) %>%
  mutate(c_lastname = if_else(c_lastname %in% "NEGAHDAR", "NEGAHDAR", c_lastname)) %>%
  mutate(c_lastname = if_else(c_lastname %in% "VANBOREKHOVEN", "VANBROEKHOVEN", c_lastname)) %>%
  mutate(c_lastname = if_else(c_firstname %in% "DUMEIGNIL", "DUMEIGNIL", c_lastname), 
         c_firstname =  if_else(c_firstname %in% "DUMEIGNIL", "FRANCK", c_firstname)) %>%
  mutate(c_lastname = if_else(c_lastname %in% "TTATIBOUET", "TATIBOUET", c_lastname)) %>%
  mutate(c_lastname = if_else(c_lastname %in% "GALLUCI", "GALLUCCI", c_lastname)) %>% 
  mutate(c_firstname = if_else(c_lastname %in% "CARMICHAEL", "EUGENE", c_firstname)) %>% # Eugen --> Eugene
  mutate(c_firstname = if_else(c_lastname %in% "DA SILVA PEREZ", "DENILSON", c_firstname)) %>%
  mutate(c_lastname = if_else(c_firstname %in% "DENILSON DA", "DA SILVA PEREZ", c_lastname),
         c_firstname = if_else(c_firstname %in% "DENILSON DA", "DENILSON", c_firstname)) %>%
  mutate(c_lastname = if_else(c_lastname %in% "SILVA PEREZ", "DA SILVA PEREZ", c_lastname)) %>%
  mutate(c_lastname = if_else(c_lastname %in% "NEMMATI KHARAT", "NEMATI KHARAT", c_lastname)) %>%
  mutate(c_lastname = if_else(c_firstname %in% "AIGARS", "PAZHE", c_lastname)) # PAZE --> PAZHE

n <- n %>% 
  unite("name", c(c_firstname, c_lastname), sep = ", ", remove = F)
# write_tsv(n, "authors.tsv")

# (3) check for names included in others names
for (i in unique(n$name)){
  j <- str_subset(n$name[ n$name != i ], i)
  if (length(j)) cat(i, ":", str_c(j, collapse = ","), "\n")
}

# Make manual changes following the results of check 3 # the artifact "EXT" has been discovered thanks to this check and is now taking care of upper in this script

n <- n %>%
  mutate(c_lastname = if_else(name %in% "MARIA, FUENTES", "FUENTES CAMPOS", c_lastname)) %>%
  mutate(c_lastname = if_else(name %in% "JON, SOLAR", "SOLAR IRAZABAL", c_lastname))  %>%
  mutate(c_firstname = if_else(name %in% "MARIA FRANCISCA, MANO", "FRANCISCA", c_firstname)) %>%
  mutate(c_lastname = if_else(c_lastname %in% "SERRANO", "SERRANO CANTADOR", c_lastname))

# questions following check 3:
# are ANA LOPEZ and ANA LOPEZ CONTRERAS the same person? does not seem so, different institution & country
# are FRANCISCA MANO and MARIA FRANCISCA MANO the same person? yes, same affiliation
# are FRANCISCO, VILA and FRANCISCO, VILAPLANA the same person? does not seem so, different institution & country
# are MARIA, FUENTES and MARIA, FUENTES CAMPOS the same person? yes, same affiliation
# are LUIS, SERRANO and LUIS, SERRANO CANTADOR the same person? does not seem so, different institution & country
# are JON, SOLAR and JON, SOLAR IRAZABAL the same person? yes, same affiliation

# 4064 lines

# After a visual screening of all these names and checking those giving no results in the databases BASE and SCOPUS, we found additional changes to make

n <-  n %>%
  
  # repetition (the firstname or the lastname is repeated twice) or parsing issue (part of the lastname within the firstname)
  
  mutate(c_firstname = if_else(c_firstname %in% "MARA G FREIRE", "MARA G", c_firstname)) %>%
  mutate(c_lastname = if_else(c_lastname %in% "DEBORA BIZZO BRUM PEREIRA", "BIZZO BRUM PEREIRA", c_lastname)) %>%
  mutate(c_lastname = if_else(c_firstname %in% "EDUARDO FERREIRA DA", "FERREIRA DA SILVA", c_lastname), 
         c_firstname =  if_else(c_firstname %in% "EDUARDO FERREIRA DA", "EDUARDO", c_firstname)) %>%
  mutate(c_firstname = if_else(c_lastname %in% "GUEDES DA SILVA", "MARIA FATIMA C", c_firstname), # https://app.dimensions.ai/details/entities/publication/author/ur.010030064533.77
         c_lastname =  if_else(c_lastname %in% "GUEDES DA SILVA", "GUEDES DA SILVA", c_lastname)) %>%
  mutate(c_firstname = if_else(c_lastname %in% "OULD DRISS", "AISSA", c_firstname), # + mispelling "OULD DRIS" more used than "OULD DRISS"
         c_lastname =  if_else(c_lastname %in% "OULD DRISS", "OULD DRIS", c_lastname)) %>%
 
   # completing the firstname
  
  mutate(c_firstname = if_else(c_lastname %in% "KOELEWIJN", "STEVEN FRISO", c_firstname)) %>% # Stef --> STEVEN FRISO
  mutate(c_firstname = if_else(c_lastname %in% "BURGUETE", "MARIA ISABEL", c_firstname)) %>% 
  mutate(c_firstname = if_else(c_lastname %in% "MARTIN LARA", "MARIA ANGELES", c_firstname)) %>% 
  mutate(c_firstname = if_else(c_lastname %in% "MADJINZA", "DESIX DIANE", c_firstname)) %>% #  also known as Desix Madjinza only
  mutate(c_firstname = if_else(c_firstname %in% "KRIJN", "KRIJN P", c_firstname)) %>% 
  mutate(c_lastname = if_else(c_firstname %in% "VINICIUS O O", "GONCALVES", c_lastname), 
         c_firstname =  if_else(c_firstname %in% "VINICIUS O O", "VINICIUS OTTONIO OLIVEIRA", c_firstname)) %>%
  mutate(c_firstname = if_else(c_lastname %in% "RANARIJAONA", "MIARINTSOA MICHAELE", c_firstname)) %>% # registered as Michaele RANARIJAONA only
  mutate(c_firstname = if_else(c_firstname %in% "ROSENANI A", "ROSENANI ANWAEUL", c_firstname)) %>% # registred as "Rosenani A" https://orcid.org/0000-0001-7745-2222
  mutate(c_firstname = if_else(c_lastname %in% "BITTER", "JOHANNES HENDRIK", c_firstname)) %>% # from ISGC 2013 == Harry Bitter (same email): entire name = JOHANNES HENDRIK (Scopus ID: Id=7006635683)
  mutate(c_firstname = if_else(c_lastname %in% "VALLEE", "CHRISTOPHE", c_firstname)) %>% # from ISGC 2013
  mutate(c_firstname = if_else(c_lastname %in% "MATIAS", "ANA ALEXANDRA", c_firstname)) %>% # cf: https://novaresearch.unl.pt/en/persons/ana-alexandra-matias/publications/ & https://orcid.org/0000-0002-4888-9414
  
  # completing the firstname with a part of the lastname
 
   mutate(c_lastname = if_else(c_firstname %in% "VINICIUS OTTONIO", "GONCALVES", c_lastname), 
         c_firstname =  if_else(c_firstname %in% "VINICIUS OTTONIO", "VINICIUS OTTONIO OLIVEIRA", c_firstname)) %>%
  mutate(c_firstname = if_else(c_lastname %in% "MOHD ZAINI", "NURUL AQILAH MOHD", c_firstname), 
         c_lastname =  if_else(c_lastname %in% "MOHD ZAINI", "ZAINI", c_lastname)) %>%
  mutate(c_firstname = if_else(c_lastname %in% "THIMOTEO AZEVEDO JORGE", "FERNANDA THIMOTEO AZEVEDO", c_firstname),
         c_lastname =  if_else(c_lastname %in% "THIMOTEO AZEVEDO JORGE", "JORGE", c_lastname)) %>%
  mutate(c_lastname = if_else(c_firstname %in% "MARCOAURELIO", "RODRIGUES", c_lastname), 
         c_firstname =  if_else(c_firstname %in% "MARCOAURELIO", "MARCOAURELIO ALMENARA", c_firstname)) %>%
  mutate(c_firstname = if_else(c_lastname %in% "ARARIPE TORRES", "FERNANDO ARARIPE", c_firstname), 
         c_lastname =  if_else(c_lastname %in% "ARARIPE TORRES", "TORRES", c_lastname)) %>%
  mutate(c_firstname = if_else(c_lastname %in% "FRANCA PENNA RIBEIRO", "MARCELA F P", c_firstname), 
         c_lastname =  if_else(c_lastname %in% "FRANCA PENNA RIBEIRO", "RIBEIRO", c_lastname)) %>%
  mutate(c_firstname = if_else(c_lastname %in% "ASMELASH", "CHALACHEW ASMELASH", c_firstname), 
         c_lastname =  if_else(c_lastname %in% "ASMELASH", "MEBRAHTU", c_lastname)) %>%
  mutate(c_firstname = if_else(c_lastname %in% "SEUANES SERAFIM", "LUISA S", c_firstname), 
         c_lastname =  if_else(c_lastname %in% "SEUANES SERAFIM", "SERAFIM", c_lastname)) %>%
  mutate(c_firstname = if_else(c_lastname %in% "HAQUE MD", "MD ARIFUL", c_firstname), 
         c_lastname =  if_else(c_lastname %in% "HAQUE MD", "HAQUE", c_lastname)) %>% # https://scholar.google.com.hk/citations?user=rSpUq7AAAAAJ&hl=en ; https://www.researchgate.net/profile/Md-Haque-55
  
  # completing the lastname with a part of the firstname ¨+ completing the firstname
  
  mutate(c_lastname = if_else(c_firstname %in% "VALERIA DE CARVALHO SANTOS", "VALERIA DE CARVALHO", c_lastname), 
         c_firstname =  if_else(c_firstname %in% "VALERIA DE CARVALHO SANTOS", "SANTOS EBINUMA", c_firstname))  %>%
  mutate(c_lastname = if_else(c_firstname %in% "IRANTZU SADABA", "IRANTZU", c_lastname), # https://orcid.org/0000-0001-6997-7491
         c_firstname =  if_else(c_firstname %in% "IRANTZU SADABA", "SADABA ZUBIRI", c_firstname)) %>% 
  
  # completing the first and lastnames
  
  mutate(c_lastname = if_else(c_lastname %in% "GALVEZ", "GALVEZ PARRUCA", c_lastname)) %>% # sometimes Maria Elena Galvez only
  mutate(c_firstname = if_else(c_lastname %in% "GALVEZ PARRUCA", "MARIA ELENA", c_firstname)) %>% 
  mutate(c_lastname = if_else(c_firstname %in% "KOTESWARA", "KOTESWARA RAO", c_lastname), 
         c_firstname =  if_else(c_firstname %in% "KOTESWARA", "NIDADAVOLU", c_firstname)) %>%
  mutate(c_firstname = if_else(c_lastname %in% "CONSTABLE", "DAVID J", c_firstname)) %>% 
  mutate(c_lastname =  if_else(c_lastname %in% "CONSTABLE", "CHICHESTER CONSTABLE", c_lastname)) %>%
  mutate(c_firstname = if_else(c_lastname %in% "GELVES", "JOHN FREDDY", c_firstname), # also known as JF GELVES; https://orcid.org/0000-0002-1238-6911
         c_lastname =  if_else(c_lastname %in% "GELVES", "GELVES DIAZ", c_lastname)) %>%
  mutate(c_firstname = if_else(c_lastname %in% "PEREANEZ", "JUAN ENRIQUE", c_firstname), #  registered as Sacarias, JE PEREANEZ; https://orcid.org/0000-0002-7147-9210
         c_lastname =  if_else(c_lastname %in% "PEREANEZ", "PEREANEZ SACARIAS", c_lastname)) %>%
 
  # misspelling
  
  mutate(c_firstname = if_else(c_lastname %in% "KRUGER", "ANDREAS J D", c_firstname)) %>% 
  mutate(c_lastname = if_else(c_lastname %in% "SARI SCHMAYSSEM", "SARI CHMAYSSEM", c_lastname)) %>% # the two forms can be used
  mutate(c_lastname = if_else(c_lastname %in% "WALMSLAY", "WALMSLEY", c_lastname)) %>% 
  mutate(c_lastname = if_else(c_lastname %in% "BUTT", "BUT", c_lastname)) %>% 
  mutate(c_lastname = if_else(c_lastname %in% "LE BRECHT", "LE BRECH", c_lastname)) %>% 
  mutate(c_lastname = if_else(c_lastname %in% "BAUEMEL", "BAUMEL", c_lastname)) %>% 
  mutate(c_lastname = if_else(c_lastname %in% "DESHAYESD", "DESHAYES", c_lastname)) %>% 
  mutate(c_lastname = if_else(c_lastname %in% "REBUTEAU", "RABUTEAU", c_lastname)) %>% 
  mutate(c_lastname = if_else(c_lastname %in% "AL NAJAR", "AL NAJJAR", c_lastname)) %>% 
  mutate(c_lastname = if_else(c_lastname %in% "HEVERKERL", "HEVEKERL", c_lastname)) %>% 
  mutate(c_lastname = if_else(c_lastname %in% "GUGLIARELLI", "GUIGLIARELLI", c_lastname)) %>% 
  mutate(c_firstname = if_else(c_lastname %in% "WIKEE", "SAOWANEE", c_firstname)) %>% 
  mutate(c_firstname = if_else(c_lastname %in% "BELAABED", "RAJA", c_firstname)) %>% 
  mutate(c_firstname = if_else(c_lastname %in% "CAILLOL", "SYLVAIN", c_firstname)) %>% 
  mutate(c_firstname = if_else(c_lastname %in% "MOUSELMANI", "RIM", c_firstname)) %>% 
  mutate(c_firstname = if_else(c_lastname %in% "FICHET", "DENIS", c_firstname)) %>% 
  mutate(c_firstname = if_else(c_lastname %in% "BERNAS", "HEIDI", c_firstname)) %>% 
  mutate(c_firstname = if_else(c_lastname %in% "HERRERA CANO", "NATIVIDAD", c_firstname)) %>% 
  mutate(c_firstname = if_else(c_lastname %in% "KOUMBA YOYA", "GEORGES THIBAUT", c_firstname)) %>% 
  mutate(c_firstname = if_else(c_firstname %in% "DIVYANAG M", "DIVYANG M", c_firstname)) %>%  
  mutate(c_firstname = if_else(c_firstname %in% "SAADIA", "SAIDIA", c_firstname)) %>% 
  mutate(c_lastname =  if_else(c_lastname %in% "CHERIAF", "CHERIEF", c_lastname)) %>%
  mutate(c_lastname = if_else(c_lastname %in% "LEACHY", "LEAHY", c_lastname)) %>%
  mutate(c_firstname = if_else(c_firstname %in% "ZEINEDINE", "ZEINEDDINE", c_firstname)) %>%
  mutate(c_firstname = if_else(c_lastname %in% "WASSERCHEID", "PETER", c_firstname), # + incomplete firstname
         c_lastname =  if_else(c_lastname %in% "WASSERCHEID", "WASSERSCHEID", c_lastname)) 

n <- n %>% 
  unite("name", c( c_firstname, c_lastname), sep = ", ", remove = F)

# %>% write_tsv(n, "authors.tsv")

# file with 4064 rows

n %>%
  distinct(c_firstname, c_lastname) # 3823 unique id # previously: 3889

# remaining question: how does LILIANA A, RODRIGUEZ became	LILIANA, RODRIGUEZ and are we sure it is the same person? different city, same country, different year, different email address...

# integrate cleaned names in 'authors' and 'authors_abstracts' tables - n is an intermediary table

authors <- authors %>%
  left_join(select(n, b_firstname, b_lastname, c_firstname, c_lastname, init, middle)) %>%
  distinct(firstname, lastname, .keep_all = T) %>% # 4666 rows # joining, by = c("b_firstname", "b_lastname") and removing duplicates in original names
  rename(first_name = c_firstname, family_name = c_lastname) %>%
  left_join(read_tsv("index/missing_firstnames.tsv"), by = c("first_name", "family_name")) %>%
              mutate(first_name = ifelse(is.na(complete_firstname), first_name, complete_firstname)) %>%
  select(-complete_firstname)
  
authors_abstracts  <- authors_abstracts %>%
  left_join(select(authors, firstname, lastname, first_name, family_name, init, middle)
            , by = c("firstname", "lastname")) %>% # 6485 rows
  drop_na(family_name) %>% # remove NA (in a later stage try to find them in another table) --> 6459 rows (26 NA have been removed)
  unite("i", c(first_name, family_name), sep = ", ", remove = F) %>%
  distinct() # from 6459 rows to 6451

authors_abstracts %>%
  write_tsv("data-net/edges-2015-2019.tsv")

length(unique(authors_abstracts$i)) #3823 unique names

authors_abstracts %>%
  distinct(i, first_name, family_name) %>%
  write_tsv("data-net/authors.tsv")

authors_index <- select(authors, original_firstname, original_lastname, first_name, family_name) %>%
  drop_na() %>%
  distinct() %>%
  write_tsv("index/authors-index-2015-2019.tsv")

length(unique(authors_abstracts$i)) #3823 unique names

#################################################################################################################

authors_abstracts <- read_tsv("data-net/edges-2015-2019.tsv")

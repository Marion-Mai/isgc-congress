# prepare the author index for publications' retrieval

library(tidyverse)

index_2013 <- read_tsv("index/index-isgc-authors-2013.tsv")

index <- read_tsv("index/authors-index-2015-2019.tsv") %>%
  bind_rows(index_2013)  %>%
  distinct(original_firstname, original_lastname, first_name, family_name) %>%
  # deal with middle initials (put them at the end of the firstname string)
  mutate(middle = str_extract(original_lastname, "^(\\w\\s)+")) %>% # detect the initials appearing in the lastname column
  mutate(original_lastname = str_remove(original_lastname, "^(\\w\\s)+")) %>% # remove them from the lastname column
  unite("original_firstname", c(original_firstname, middle), sep = " ", na.rm = TRUE, remove = FALSE)  %>%  # add them at the end of the firstname
  mutate(original_firstname = str_trim(original_firstname), original_lastname = str_trim(original_lastname))  %>%
  select(-c(middle))

#########################################################################################################

# design functions to include different types of transliteration for russian diacritics and umlaut
# see https://stackoverflow.com/questions/286921/efficiently-replace-all-accented-characters-in-a-string

# function no 1: russian diacritics
rus <-  function(x, var, y) { # to replace the vasil'ev forms by various possibilities
  
  var <- enquo(var)
  nm <- as_label(var)
  
  x %>%
    filter(str_detect(!!var, "(?<=L|T)'(?=E)")) %>%
    mutate(!!nm := str_replace(!!var, "(?<=L|T)'(?=E)", {{ y }})) }

# function no 2: umlaut
umlaut <-  function(x, var) {  # to replace umlaut (a bit risky since it is not relevant for all languages)
  
  var <- enquo(var)
  nm <- as_label(var)
  
  x %>%
    filter(str_detect(!!var, "Ö|Ä|Å")) %>%
    mutate(!!nm := str_replace_all(!!var, c("Ö" = "OE", "Ä" ="AE", "Å" = "A"))) }

########################################################################################################

# include the final form among the original names' forms and apply the above functions
# (the original form then comprises all the possible forms)

index <- index %>%
  bind_rows(rus(., original_lastname, "I"), 
            rus(., original_lastname, "Y"), 
            rus(., original_lastname, "J"),
            umlaut(., original_lastname)) %>% # adds 30 lines
  drop_na() %>%
  bind_rows(
    select(index, first_name, family_name)  %>%
      mutate(original_firstname = first_name,
             original_lastname = family_name)) 

# add known alternative names
# adding alternative forms for publications retrieval (names with multiple names variants)

index <- index %>%
  mutate(source = "ISGC")  %>%
  filter(! (original_firstname %in% "ANA M" & original_lastname %in% "MATIAS")) %>%
  mutate(original_lastname = ifelse(original_lastname %in% "OÂNEILL", "O'NEILL", original_lastname)) %>%
  bind_rows(
    .,
    tibble::tribble(
      ~ original_firstname, ~ original_lastname, ~ first_name, ~ family_name, ~ source,
      "AUDREY", "DENICOURT", "AUDREY", "DENICOURT NOWICKI", "other",
      "JOSE CLEITON", "SOUSA DOS SANTOS", "JOSE CLEITON S", "DOS SANTOS", "other", # https://orcid.org/0000-0002-1511-5180
      "SOLEDAD", "ASPROMONTE", "SOLEDAD GUADALUPE", "ASPROMONTE", "other",
      "JOHN FREDDY", "GELVES", "JOHN FREDDY", "GELVES DIAZ", "other", # https://orcid.org/0000-0002-1238-6911
      "PEREAÑES-SACARÍAS", "JUAN ENRIQUE", "PEREANES SACARIAS", "JUAN ENRIQUE", "other", # https://orcid.org/0000-0002-7147-9210
      "PEREAÑES-SACARIAS", "JUAN ENRIQUE", "PEREANES SACARIAS", "JUAN ENRIQUE","other", 
      "PEREAÑES-SACARÍAS", "J E", "PEREANES SACARIAS", "JUAN ENRIQUE", "other", 
      "PEREAÑES-SACARIAS", "J E", "PEREANES SACARIAS", "JUAN ENRIQUE","other", 
      "MARIA E", "GALVEZ-PARRUCA", "MARIA ELENA", "GALVEZ PARRUCA","other", 
      "GLORIA ESTHER", "ALONSO SANCHEZ", "GLORIA ESTHER", "ALONSO","other", 
      "PAULO JOSÉ LOURENÇO", "ANDRÉ", "PAULO JOSE", "ANDRE","other", 
      "PAULO JOSE LOURENCO", "ANDRE", "PAULO JOSE", "ANDRE","other", 
      "MARCIA", "ARAQUE", "MARCIA CAROLINA", "ARAQUE MARIN","other", 
      "ANNA L", "JONGERIUS", "ANNELIE", "JONGERIUS","other", 
      "A L", "JONGERIUS", "ANNELIE", "JONGERIUS","other", 
      "MARIUS", "BAÜMEL", "MARIUS", "BAUMEL","other", 
      "DENNIS", "KNÖGLER", "DENNIS", "KNOGLER","other", 
      "D", "KNÖGLER", "DENNIS", "KNOGLER", "other",  # not in academia anymore
      "JOAQUÍN", "MARTÍNEZ TRIGUERO", "JOAQUIN", "MARTINEZ TRIGUERO", "other", # https://orcid.org/0000-0003-4590-724X
      "ANDREAS J D", "KRÜGER", "ANDREAS J D", "KRUGER", "other", 
      "CORNELIS", "VAN DER WIJST", "CORNELIS G", "VAN DER WIJST", "other", 
      "RENÉE", "BAKKEMO", "RENEE", "BAKKEMO", "other", 
      "DÉSIX", "MADJINZA", "DESIX", "MADJINZA", "other", 
      "BELÉN", "MAESTRO-MADURGA", "BELEN", "MAESTRO MADURGA", "other", 
      "MARLÈNE", "BEYERLE", "MARLENE", "BEYERLE", "other", 
      "ANDRIANARIVO IRÈNE", "RAHOBINIRINA", "ANDRIANARIVO IRENE", "RAHOBINIRINA", "other", 
      "A", "ALLOUACHE", "AMINA", "ALLOUACHE", "other", # Still a young researcher, her only publications are indexed online with her firstname's initial only
      "FÉLIX ARMANDO", "REANO", "FELIX ARMANDO", "REANO", "other", 
      "JOANNA", "KRYŚCIAK-CZERWENKA", "JOANNA", "KRYSCIAK CZERWENKA", "other", 
      "J", "KRYŚCIAK-CZERWENKA", "JOANNA", "KRYSCIAK CZERWENKA", "other", 
      "DARLIS ADRIANA", "VARÓN-CARDENAS", "DARLIS ADRIANA", "VARON CARDENAS", "other", 
      "NACERA", "LAHOUEL", "NACERA", "LAHOUEL BENABBES", "other", 
      "N", "LAHOUEL", "NACERA", "LAHOUEL BENABBES", "other", 
      "HANI", "AL-NAJJAR", "HANY J", "AL NAJJAR", "other", 
      "HANY", "NAJJAR", "HANY J", "AL NAJJAR", "other", # https://www.researchgate.net/profile/Hany-Najjar
      "NORA", "TOUAHRI", "NOURA", "TOUAHRI", "other", 
      "JULIANA A", "SOUZA", "JULIANA", "DE SOUZA SARTORI", "other",
      "J AP", "DE SOUZA SARTORI", "JULIANA", "DE SOUZA SARTORI", "other",
      "J", "APERICIDA DE SOUZA SARTORI", "JULIANA", "DE SOUZA SARTORI", "other",
      "CHRIS", "HARDACRE", "CHRISTOPHER", "HARDACRE", "other",
      "LAURENCE", "CHOCINSKI ARNAULT", "LAURENCE", "CHOCINSKI", "other",
      "KRISHNA D P", "NIGAM", "KRISHNA DEO PRASAD", "NIGAM", "other",
      "EDITH", "NORRANT", "EDITH L", "NORRANT", "other",
      "KULATHU", "SESHAN", "KULATHUIYER K", "SESHAN", "other",
      "KULATHUYIER", "SESHAN", "KULATHUIYER K", "SESHAN", "other",      
      "BETHEL U", "UKAZU", "BETHEL UGOCHUKWU", "UKAZU", "other")) %>% # https://scholar.google.com/citations?user=3FC1BfMAAAAJ&hl=en

    # order the names
  unite("name", c(original_firstname, original_lastname), sep = ", ", remove = F)  %>%
  distinct() %>%
  arrange(family_name, first_name)

# do we need to add all cases with firstname's initials only and possible accents. only a few have been added here so far since papers were found with initials only and accents for certain names.
# do we need to all all cases with "-" in place of spaces? not always relevant (maybe after "Ait" and "Al"?)

######################################################################################################

# finalise the index with WOS forms (firstname initials only) and some stats from the WOS authors' index
# see: https://images-webofknowledge-com/WOKRS535R111/help/WOS/hs_author.html

index <- index %>%
  # remove letters not preceded by a space (\\s) or the beginning of the string (^)
  mutate(initials = str_remove_all(original_firstname, "(?<!^|\\s)[[:alpha:]]") %>%
           str_replace_all("-|\\s+", "") %>%
           iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
           str_remove_all("[-\"^'*`\\\\~]"), # remove the "-" "'" and  in initials but not in lastnames
         wos_form = str_c(original_lastname, initials, sep = " ") %>%
           iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
           str_remove_all("[\"^*`\\\\~]"))

index %>%
  write_tsv("index/authors-index-isgc.tsv")

# is Kaufman Rechulski, Marcelo Daniel the same person as Kaufman Rechulski, Marcelo David? and therefore Kaufman Rechulski, Marcelo D.


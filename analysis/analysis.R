

### Libraries laden

libs <- c("tidyverse", "lubridate", "readxl",  "scales", "jsonlite", "tidytext", "stopwords", "usethis")
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}
invisible(lapply(libs, library, character.only = T))

### Source Function
source("./R/reduceR.R")

### Set up github

# usethis::use_git_config(user.name="Codo-der-Dritte", user.email="h11706050@s.wu.ac.at")
# usethis::use_git()

### Daten einlesen

ses<- fromJSON("./data/sessions.json")
per<- fromJSON("./data/persons.json")

### Daten in Form bringen

y <- tibble()
for(i in 1:nrow(ses)){
x <- tibble(speaker = ses$sections[[i]]$speaker, text=  ses$sections[[i]]$text)
y <- tibble(rbind(y, x))
}


# Ansprachen mit Personen matchen
dtt <- left_join(y, per, by = c("speaker" = "id"))

# Daten cleanen
dtt <- dtt %>%
  mutate(text = str_replace(text, "^[^:]*:", "")) %>%
  filter(parties != "character(0)",
         parties != "PILZ",
         parties != "STRONACH")

# Some politicians have multiple party affiliations. Reduce those to the 5 large parties.
rule <- c("ÖVP", "SPÖ", "FPÖ", "GRÜNE", "NEOS", "Ohne Klub")

dtt <- reduceR(dtt, "parties")

# Stopwords laden (Füllwörter)

stop_german <- data.frame(word = c(stopwords::stopwords(language = "de", source = "stopwords-iso"),
                                   stopwords::stopwords(language = "de", source = "snowball"),
                                   stopwords::stopwords(language = "de", source = "marimo"),
                                   stopwords::stopwords(language = "de", source = "nltk")
                                   ),
                          stringsAsFactors = FALSE)




# Ansprachen tokenisieren in Worte
dtt <- dtt %>%
  unnest_tokens(output = word, input = text) %>%
  filter(!(str_detect(word, "^\\d+$") |
             str_length(word) < 3) |
           str_detect(word, "[.,]") |
           str_detect(word, "[.,][.,]") |
           str_detect(word, "[.,][.,][.,]") |
           str_detect(word, "\\b(0?[1-9]|[12][0-9]|3[01])\\.(0?[1-9]|1[0-2])\\.(\\d{4})\\b") |
           str_detect(word, "\\b\\d{1,3}(,\\d{3})+(\\.\\d+)?\\b") |
           str_detect(word, "\\b\\d{1,2}\\.(3[2-9]|\\d{3,})\\b")) %>%
  anti_join(stop_german, by = join_by(word))

# Häufigkeiten von worten ausrechnen

probs <- dtt %>%
  mutate(parties = if_else(parties == "BZÖ", list("FPÖ"), parties)) %>%
  group_by(parties) %>%
  count(word)

probs_abs <- probs %>%
  pivot_wider(names_from = parties, values_from = n) %>%
  filter(!str_detect(word, "\\d"))

probs_p <- probs_abs %>%
  mutate(across(2:7, ~ . / sum(., na.rm = T))) %>%
  filter(rowSums(!is.na(.)) > 0)








# Libraries laden ----------------------------------------

libs <- c("tidyverse", "lubridate", "readxl",  "scales", "jsonlite", "tidytext", "stopwords", "usethis")
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}
invisible(lapply(libs, library, character.only = T))

# Source Function -----------------------------------------
source("./R/reduce_parties.R")

# Daten einlesen ------------------------------------------

ses<- fromJSON("./data/sessions.json")
per<- fromJSON("./data/persons.json")

# Daten in Form bringen -----------------------------------

y <- tibble()
for(i in 1:nrow(ses)){
x <- tibble(speaker = ses$sections[[i]]$speaker, text=ses$sections[[i]]$text)
y <- tibble(rbind(y, x))
}


# Ansprachen mit Personen matchen --------------------------
dtt <- left_join(y, per, by = c("speaker" = "id"))

# Daten cleanen --------------------------------------------
dtt <- dtt %>%
  mutate(text = str_replace(text, "^[^:]*:", "")) %>%
  filter(parties != "character(0)",
         parties != "PILZ",
         parties != "STRONACH")

# Some politicians have multiple party affiliations
# Reduce those to the 5 large parties.
rule <- c("ÖVP", "SPÖ", "FPÖ", "GRÜNE", "NEOS", "Ohne Klub")

dtt <- reduce_parties(dtt, "parties")

# Stopwords laden (Füllwörter)

stop_german <- data.frame(word = c(stopwords::stopwords(language = "de", source = "stopwords-iso"),
                                   stopwords::stopwords(language = "de", source = "snowball"),
                                   stopwords::stopwords(language = "de", source = "marimo"),
                                   stopwords::stopwords(language = "de", source = "nltk")
                                   ),
                          stringsAsFactors = FALSE)

# Ansprachen tokenisieren in Worte ---------------------------
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

# Analyse der Daten -------------------------------------

# Häufigkeiten von Worten ausrechnen

probs <- dtt %>%
  mutate(parties = if_else(parties == "BZÖ", list("FPÖ"), parties)) %>%
  filter(!str_detect(word, "\\d")) %>%
  filter(!(str_detect(word, "^\\d+$") |
             str_length(word) < 3) |
           str_detect(word, "[.,]") |
           str_detect(word, "[.,][.,]") |
           str_detect(word, "[.,][.,][.,]") |
           str_detect(word, "\\b(0?[1-9]|[12][0-9]|3[01])\\.(0?[1-9]|1[0-2])\\.(\\d{4})\\b") |
           str_detect(word, "\\b\\d{1,3}(,\\d{3})+(\\.\\d+)?\\b") |
           str_detect(word, "\\b\\d{1,2}\\.(3[2-9]|\\d{3,})\\b")) %>%
  group_by(parties) %>%
  count(word)

probs_abs <- probs %>%
  pivot_wider(names_from = parties, values_from = n) %>%

probs_p <- probs_abs %>%
  mutate(across(2:7, ~ . / sum(., na.rm = T))) %>%
  filter(rowSums(!is.na(.)) > 0)


# Worte nach Häufigkeit gewichten ------------------------

speaker_words <- dtt %>%
  count(speaker, word, sort = TRUE)

total_words <- dtt %>%
  group_by(speaker) %>%
  summarise(total = n())


speaker_words <- left_join(speaker_words, total_words)


freq_by_rank <- speaker_words %>%
  group_by(speaker) %>%
  mutate(rank = row_number(),
         term_frequency = n/total) %>%
  ungroup()

freq_by_rank %>%
  ggplot(aes(rank, term_frequency, color = speaker)) +
  geom_line(linewidth = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()

rank_subset <- freq_by_rank

freq_model <- lm(log10(term_frequency) ~ log10(rank), data = rank_subset)

freq_by_rank %>%
  ggplot(aes(rank, term_frequency, color = speaker)) +
  geom_abline(intercept = freq_model$coefficients[1], slope = freq_model$coefficients[2],
              color = "gray50", linetype = 2) +
  geom_line(linewidth = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()


speaker_tf_idf <- speaker_words %>%
  bind_tf_idf(word, speaker, n)


speaker_tf_idf <- left_join(speaker_tf_idf, per, by = c("speaker" = "id"))

speaker_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

speaker_tf_idf %>%
  group_by(name) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  filter(name %in% c("Dr. Dagmar Belakowitsch", "Mag. Werner Kogler", "Herbert Kickl", "Kai Jan Krainer")) %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~name, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)



# Some data cleaning is necessary to remove numbers and less
# meaningful words. For this a tibble has to be created and then
# we perform an Antijoin with these words. This should lead to more
# meaningful outcome.








# Rede Herbert Kickl -------------------------------------



kickl <- read.delim("./data/Herbert_Kickl_rede.txt", header = F, col.names = "text")

kickl_tok <- kickl %>%
  unnest_tokens(output = "word", input = "text") %>%
  anti_join(stop_german, by = join_by(word))

kickl_abs <- kickl_tok %>%
  count(word)

kickl_p <- left_join(kickl_abs, probs_p, by = "word") %>%
  filter(rowSums(!is.na(.)) > 2)

result <- kickl_p %>%
  mutate(across(3:8, ~ . * 100 * n)) %>%
  summarise(across(3:8, ~ sum(., na.rm =T)))




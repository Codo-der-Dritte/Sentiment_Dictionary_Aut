libs <- c("tidyverse", "lubridate", "readxl",  "scales", "jsonlite", "tidytext", "stopwords", "usethis")
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}
invisible(lapply(libs, library, character.only = T))


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

dtt <- left_join(y, per, by = c("speaker" = "id"))

dtt <- dtt %>%
  mutate(text = str_replace(text, "^[^:]*:", ""))

stop_german <- data.frame(word = stopwords::stopwords("de"), stringsAsFactors = FALSE)

dtt <- dtt %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_german)

dtt %>%
  filter(!str_detect(word, "\\D"),
       str_length(word) != 1)




# P. Kessling
# Challenge 5: Datensatz wiederzusammenfrickeln
# Wir haben einen Datensatz aus der Wikipedia geladen und ihn als JSON abgelegt.
# Leider hat der fiese Dozent einen Fehler bei der Ausgabe gemacht.
#
# * Lade die Datensätze in `data/`, danke an `map_dfr`
# * Stelle daraus eine *flache* Tabelle her. Die Tabelle soll die folgenden Spalten beinhalten: name, lebensdaten, land, wahlkreis, erststimmen, bemerkungen, fraktion.
# * Berechne das durchschnittliche Alter für die einzelnen Fraktionen und sortiere den Datensatz nach dieser Variable.Für durchschnittliches ALter avg_age
# Für die zweite Frage in Challenge 4: der Tibble ans1 soll die folgenden Spalten haben: fraktion, avg_age.

library(assertthat)
library(tidyverse)
library(jsonlite)
# data <- …
# ans1 <- …

# load data 
data <- list.files("data/", pattern = "json", full.names = T) %>%
  map_dfr(fromJSON)

# unnesting "abgeordnete" and mutating "part0et" into "fraktion" and safe in "bt2". mutating with ".keep" https://rdrr.io/cran/dplyr/man/mutate.html 
bt2 <- data %>% 
  unnest(abgeordnete) %>% 
  mutate(part0et, .keep = "unused")

# calculating avg_age and creating new tibble "ans1" with "fraktion" and "avg_age"
ans1 <- bt2 %>%
  group_by(fraktion) %>% 
  summarise(avg_age = 2021 - mean(lebensdaten)) %>% 
  arrange(desc(avg_age))
  

  
if (
  assert_that(
    openssl::md5(paste(map_chr(bt2, paste, collapse = "") , collapse = "")) == "926c50623af03708fa768e0003cc18c6"
  ) &&
  assert_that((
    openssl::md5(paste(map_chr(ans1, paste, collapse = "") , collapse = "")) == "7607c77648da79b8d09ff0d4db41ed0d"
  ))
) {
  writeLines("10/10, gratuliere!")
}

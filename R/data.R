library(tidyverse)
library(textclean)
library(ggplot2)
library(stringr)
library(textclean)

# data ----
tabelOutput <- read.csv("https://raw.githubusercontent.com/eppofahmi/vircit-dachboard/master/data/tabelOutput.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",")
# duplicate teks clean ----
tabelOutput <- tabelOutput %>%
  mutate(is_dup = duplicated(teks_clean)) %>%
  filter(is_dup == FALSE)
tabelOutput$teks_clean <- gsub("\\bselamat pagi\\b", "", tabelOutput$teks_clean)
# word count per rows ----
tabelOutput$wordCount <- str_count(tabelOutput$teks_clean, '\\s+')+1
tabelOutput <- tabelOutput %>% 
  filter(wordCount >= 2)

# cleaning 
tabelOutput <- tabelOutput %>%
  select(kasus, sumber_data, parameter, tanggal, teks, teks_clean)
tabelOutput$parameter <- tolower(tabelOutput$parameter)
tabelOutput$parameter <- gsub("[^[:alnum:][:space:]@_]", "", tabelOutput$parameter)

# parameter petisi change
tabelOutput$parameter <- gsub("\\bsegera cabut\\b", "petisi_change1", tabelOutput$parameter)
tabelOutput$parameter <- gsub("\\brevoke\\b", "petisi_change2", tabelOutput$parameter)
tabelOutput$parameter <- gsub("\\bpak jokowi\\b", "petisi_change3", tabelOutput$parameter)

# parameter petisi gojek
tabelOutput$parameter <- gsub("\\bgojek grabbikedll\\b", "petisi_gojek1", tabelOutput$parameter)

tabelOutput$parameter <- gsub("\\bgojek di purwokerto\\b", "petisi_gojek2", tabelOutput$parameter)
tabelOutput$parameter <- gsub("\\bhentikan permenhub 108\\b", "petisi_gojek3", tabelOutput$parameter)
tabelOutput$parameter <- gsub("\\bkami membutuhkan gojek\\b", "petisi_gojek4", tabelOutput$parameter)
tabelOutput$parameter <- gsub("\\bmemilih transportasi\\b", "petisi_gojek5", tabelOutput$parameter)
tabelOutput$parameter <- gsub("\\btolak pencabutan gojek tasikmalaya\\b", "petisi_gojek6", tabelOutput$parameter)

sumber_data <- tabelOutput %>%
  group_by(kasus) %>%
  count(sumber_data)

parameter <- tabelOutput %>%
  group_by(kasus, sumber_data) %>%
  count(parameter)

write_csv(tabelOutput, path = "/Volumes/mydata/rSkilss/vircit-dachboard/data/tabelOutput.csv")
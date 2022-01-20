pacman::p_load(lexicon)
download.file("https://raw.githubusercontent.com/first20hours/google-10000-english/master/google-10000-english.txt", "top_10000.txt")
pacman::p_load(readr, purrr, tidyr, dplyr)
top_10000 <- read_csv("top_10000.txt", col_names = "words")%>%
  mutate(word_length = purrr::map_int(.$words, nchar))%>%
  filter(word_length==5)

letters<-top_10000$words%>%
map(.,~strsplit(.x, "")%>%
      unlist())
letter_df<-do.call(rbind, letters)%>%
  as_tibble()%>%
  set_names(.,c("pos_1","pos_2","pos_3","pos_4","pos_5"))

### Most common letters

unlist(letters)%>%
  table()%>%
  sort()

### Letter frequency by their position in the word
letters_by_pos<-letter_df%>%
  pivot_longer(everything(), names_to = "pos")%>%
  table()%>%
  as.data.frame.matrix()%>%
  mutate(position = rownames(.))%>%
  pivot_longer(-position,names_to = "letter", values_to = "frequency" )


letters_by_pos%>%
  group_by(position)%>%
  slice_max(frequency, n=2)%>%
  View()




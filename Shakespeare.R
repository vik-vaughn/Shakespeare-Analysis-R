#instaliuojame ir paleidžiame reikiamus paketus

install.packages("dplyr")
install.packages("tm.plugin.webmining")
install.packages("purrr")
install.packages("tidytext")
install.packages("gutenbergr")
install.packages("ggplot2")
install.packages("wordcloud")
install.packages("tidyr")
install.packages("igraph")
install.packages("ggraph")

library("dplyr")
library("tm.plugin.webmining")
library("purrr")
library("tidytext")
library("gutenbergr")
library("ggplot2")
library("wordcloud")
library("tidyr")
library("igraph")
library("ggraph")

#parsisiunčiame "The Complete Works of William Shakespeare by William Shakespeare"

shakespeare <- gutenberg_download(100)

#"unnest_tokens" transformuoja tekstą taip, kad vienoje eilutėje būtų po žodį, o komanda "anti_join" pašalina "stop words" - žodžius, kurie sakiniui neprideda daug reikšmės (o,ar,bet,kad, ir t.t)
# pastaba: nusprendžiau palikti tam tikrus laikmetį atspindinčius žodžius kaip "thou", "thy", "thee", t.t.

tidy_shakespeare <- shakespeare %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_shakespeare %>%
  count(word, sort = TRUE)

tidy_shakespeare %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(aes(fill=n)) +
  scale_fill_gradient(low = "blue", high = "red") +
  scale_y_continuous(breaks = seq(600,6000,by=400)) +
  coord_flip() + ggtitle("Populiariausi žodžiai Viljamo Šekspyro darbuose")

#dabar bandysime atskirti teigiamus ir neigiamus žodžius

bing_word_counts <- tidy_shakespeare %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_word_counts

#atvaizduokime šiuos rezultatus lentelėje

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Žodžių kiekis",
       x = NULL) +
  coord_flip()

#Jei rezultatai blogai matose stulpelinėje diagramoje, galime panaudoti "wordcloud" paketą ir atvaizduoti rezultatus kitaip.

tidy_shakespeare %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, colors=brewer.pal(8, "Dark2")))

#santykiai tarp žodžių - bandome rasti populiariausias žodžiu poras

shakespeare_bigrams <- shakespeare %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
shakespeare_bigrams

#Išfiltruojame "stop-words", sujungiame, ir atvaizduojame

bigrams_separated <- shakespeare_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
bigram_counts
bigram_counts<-bigram_counts[-c(1), ]

bigrams_united <- bigram_counts %>%
  unite(bigram, word1, word2, sep = " ")
bigrams_united

bigram_tf_idf <- bigrams_united
bigram_tf_idf <- bigram_tf_idf %>% filter(n>50)
ggplot(aes(x = reorder(bigram, n), y=n, fill=n), data=bigram_tf_idf) +
  geom_bar(stat = 'identity') +
  ggtitle("Dažniausi žodžių junginiai Šekspyro darbuose") + coord_flip() +
  scale_y_continuous(breaks = seq(50,600,by=50)) +
  scale_fill_gradientn(colours = terrain.colors(7))

#Galime žodžių poras atvaizduoti kitaip - tam naudojame "igraph" bei ""ggraph" paketą.

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "darkgreen", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8) + ggtitle("Populiarūs žodžių junginiai Šekspyro darbuose")


library(tidyverse)
library(quanteda)
library(wordcloud)
#library(wordcloud2)


a <- readRDS("data/epsa2023.rds")

cor <- corpus(a$abstract,docvars = data.frame(section = a$section))

dmat <- cor %>% tokens(remove_symbols = TRUE, remove_punct = TRUE,
               remove_url = TRUE) %>% 
  tokens_tolower() %>%
  tokens_remove("p") %>%
  tokens_remove("rgb") %>%
  tokens_remove("br") %>%
  tokens_remove("*px") %>%
  tokens_remove(c("color","text","style","normal","initial")) %>%
  tokens_remove(c("34","255","also","font-family","sans-serif","nbsp",
                  "em","whether",
                  "2","caret-color","helvetica","arial","font-weight","text-indent","font-style","text-align","none","start")) %>%
  tokens_remove(c("likely","however","can")) %>%
  tokens_remove(stopwords()) %>%
  dfm()

dmat %>%
  dfm_remove("political") %>%
  quanteda.textstats::textstat_frequency(ties_method = "first", 
                                         groups = section,
                                         n=25) %>%
  tibble()  %>%
  rename(word = feature,
         freq = frequency) %>%
  ggplot(aes(x=freq,
             color=group,
             y=fct_reorder(word,freq))) +
  geom_segment(aes(xend=freq,
                   x=0,
                   yend=fct_reorder(word,freq)),
               color="grey80") +
  geom_point(show.legend=FALSE) +
  facet_wrap(~group, scales = "free_y") +
  labs(y="",title = "Frequency of words in EPSA 2023 abstracts",x="") +
  viridis::scale_color_viridis(option="turbo",discrete = TRUE) +
  theme_classic()



dmat %>% 
  dfm_remove("political") %>%
  dfm_trim(min_termfreq = 15) %>%  quanteda.textplots::textplot_wordcloud(ordered_color=TRUE)

dmat %>% 
  dfm_remove("political") %>%
  dfm_subset(section=="Political Communication") %>%
  dfm_trim(min_termfreq = 8) %>%  quanteda.textplots::textplot_wordcloud(ordered_color=TRUE,color="darkorange2")


dmat %>% 
  dfm_subset(section=="Political Communication") %>%
  dfm_remove("political") %>%
  dfm_trim(min_termfreq = 15) %>% fcm() %>% quanteda.textplots::textplot_network(min_freq = .6,edge_alpha = .11)


dmat %>% 
  dfm_subset(section=="Political Communication") %>%
  dfm_remove("political") %>%
  dfm_trim(min_termfreq = 10) %>%  quanteda.textplots::textplot_wordcloud()



  

commW <- dmat %>%
  dfm_subset(section=="Political Communication") %>%
  quanteda.textstats::textstat_frequency(ties_method = "first", n=75) %>%
  tibble()  %>%
  rename(word = feature,
         freq = frequency)

wordcloud(commW$word, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#commW %>% wordcloud2(backgroundColor = "grey80")
#commW %>% wordcloud2(backgroundColor = "grey80",color = rep_len(c("violetred4","tan3"),length.out = 75))


dfm_trim(min_termfreq = 45) %>%
  fcm(tri = TRUE)

#econFCM %>% textplot_network(min_freq = .98, edge_alpha = 0.4, edge_color = "purple4")


econFCM %>%
  #fcm_remove("economy") %>%
  fcmToTidy(min_freq =.9975) %>%
  mutate(centrality = centrality_authority()) %>%
  # mutate(community = as.factor(group_infomap())) %>%
  # mutate(dist_to_center = node_distance_to(node_is_center())) %>%
  ggraph(layout = 'stress') +
  geom_edge_fan(alpha = .22, color="purple") + #  aes(width=log(weight))) +
  geom_node_point(aes(size=log(frequency),
                      color=centrality)) +
  geom_node_text(aes(label = name,
                     size=log(frequency)), repel=T) +
  scale_color_distiller(palette = "RdGy", direction = -1) +
  scale_label_size_continuous(limits=c(.05,10)) +
  #scale_edge_width_continuous(c(.5, 5)) +
  theme(legend.position = "none") + theme_graph()
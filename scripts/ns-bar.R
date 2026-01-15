

source("load-nationscape.R")

pollster::crosstab(df = a, 
                   x = trump_biden, 
                   y = aoc_Favorable,
                   weight = weight,
                   format = "long") %>%
  mutate(label_pct = paste0(round(pct,0),"%")) %>%
  filter(aoc_Favorable==1,
         trump_biden!="Don't Know") %>%
  ggplot(aes(y= fct_reorder(trump_biden,pct), 
             x = pct)) + 
  geom_col(width = .3,
           fill=c("blue3","red4")) + 
  geom_label(aes(label=label_pct)) +
  theme_classic() +
  labs(x="Percent", 
       y = "Supporters of...", 
       title = "AOC favorability")


pollster::crosstab(df = a, 
                   x = trump_biden, 
                   y = aoc_Favorable,
                   weight = weight,
                   format = "long") %>%
  filter(aoc_Favorable==1, 
         trump_biden!="Don't Know") %>%
  ggplot(aes(y= fct_reorder(trump_biden,pct), 
             x = pct)) + 
  geom_col(width = .3,
           fill=c("blue4",
                  "red3")) + 
  theme_classic() +
  labs(x="Percent", 
       y = "Supporters of...", 
       title = "AOC favorability") +
  geom_label(aes(label=round(pct,1)))



pollster::crosstab_3way(df = a, 
                   x = trump_biden, 
                   y = aoc_Favorable,
                   z = gender,
                   weight = weight,
                   format = "long") %>%
  filter(trump_biden != "Don't Know") %>%
  filter(aoc_Favorable==1) %>%
  group_by(trump_biden) %>%
  mutate(prop_female = n / sum(n)) %>%
  ggplot(aes(y=as_factor(trump_biden),
             x=pct,
             fill=gender)) +
  geom_col(position = position_dodge()) +
  labs(x="Percent",y="",fill="") +
  geom_label(aes(label=round(pct,1)),
             position = position_dodge(width = 1))
  #scale_fill_discrete(guide="none")
  
  
pollster::crosstab_3way(df = a %>% filter(trump_biden != 999), 
                        x = trump_biden, 
                        y = aoc_Favorable,
                        z = gender,
                        weight = weight,
                        format = "wide",
                        pct_type = "row") 

  filter(aoc_Favorable==1) %>%
  ggplot(aes(y=as_factor(trump_biden),
             x=pct,
             fill=gender)) +
    geom_col() +
    labs(x="Percent",y="",fill="")



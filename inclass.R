library(tidyverse)
library(haven)
library(labelled)
library(pollster)
#install.packages("pollster")

exp <- read.csv("https://raw.githubusercontent.com/zilinskyjan/datasets/master/public%20opinion/experts/BertsouCaramani-TechnocracySurvey.csv")

exp$AP4BIN <- ifelse(exp$AP4 >=5,1,0)

exp %>%
  group_by(country) %>%
  summarise(M_AP = mean(antipolitics),
            M_POP = mean(popscale)) %>%
  ggplot(aes(x=M_AP,y=M_POP)) +
  geom_point()

exp %>%
ggplot(aes(x=antipolitics, y=popscale) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_continuous(type = "viridis")

exp %>%
  ggplot(aes(x=antipolitics, y=popscale) ) +
  geom_hex(bins = 10) +
  scale_fill_continuous(type = "viridis") +
  theme_bw() 



hib <- read_csv("https://raw.githubusercontent.com/zilinskyjan/datasets/master/politics/hibbing2023_who_should_govern.csv")

hib %>% count(type)

hib$type[hib$type=="Standard"] <- "Establishment" 

H_pal <- rcartocolor::carto_pal(n = 4, name = "ag_Sunset")
col2 <- rcartocolor::carto_pal(n=7,name="Burg")

hib %>%
  ggplot(aes(y=fct_reorder(group,influence_incr),
             x=influence_incr,
             fill=type)) +
  geom_col() +
  scale_fill_manual(values=H_pal[c(4,1,2,3)]) +
  labs(x="Percent",y="",
       title="Whose political power should be increased?",
       caption = "Data: Hibbing et al. (2023)\nChart: JZ",
       fill="") +
  theme_gray() +
  theme(text = element_text(size=13)) +
  theme(plot.caption.position = "plot",
        axis.title.x = element_text(hjust = 1))


#theme_set(theme_light())
#theme_update(text = element_text(family="Source Sans Pro",size=13))

if (!requireNamespace("curl", quietly = TRUE)) {
  install.packages("curl")
}

library(curl)

githubURL <- "https://raw.githubusercontent.com/zilinskyjan/DataViz/temp/data_nationscape2019/Nationscape_first10waves.rds"
download.file(githubURL,"Nationscape_first10waves.rds", method="curl")
a <- readRDS("Nationscape_first10waves.rds")

a %>% group_by(week) %>% tally() %>%
  ggplot(aes(x=week,y=n)) +
  geom_col() + 
  #coord_flip() +
  labs(x="",y="Respondents",
       title="Weekly number of Nationscape respondents")
  
a %>% count(gender) 
a %>% group_by(gender) %>% tally() %>%
  ggplot(aes(x=n, 
             y=as_factor(gender))) +
  geom_point()

a$aoc_Favorable %>% head(n=20)

a$aoc_Favorable %>% table()

a %>% 
  summarise(AOC_fav = mean(aoc_Favorable,na.rm=T))

a %>% filter(is.na(aoc_Favorable)) %>% nrow()
a %>% filter(!is.na(aoc_Favorable)) %>% nrow() 

library(pollster)

pollster::crosstab(df = a, 
                   x = gender, 
                   y = aoc_Favorable,weight = weight)

a %>% count(aoc_Favorable,gender)





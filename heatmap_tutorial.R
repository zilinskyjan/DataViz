heatmap_tutorial.R

insta_pres %>% 
  group_by(`User Name`,monthyear) %>%
  filter(`User Name` %in% c("elizabethwarren","joebiden","berniesanders","pete.buttigieg","amyklobuchar")) %>%
  summarize(n = n()) %>% 
  ggplot(aes(monthyear,`User Name`,fill=n))+
  geom_tile(color= "white",size=0.1) + 
  theme_minimal() + 
  scale_fill_gradient(low="white", high="purple") +
  labs(x="",y="Instagram account",fill="Number\nof posts",
       subtitle="Instagram use of selected Democratic candidates")



# Simple continuous heatpamp
insta_pres %>% 
  group_by(`User Name`,monthyear) %>%
  summarize(n = n()) %>% 
  mutate(account = factor(`User Name`,levels=politics_insta_notrump_reordered[25:1])) %>% 
  arrange(account) %>% 
  ggplot(aes(monthyear,account,fill=n))+
  geom_tile(color= "white",size=0.1) + 
  theme_minimal() + 
  scale_fill_gradient(low="white", high="navy") +
  labs(x="",y="Instagram account",fill="Number\nof posts",
       subtitle="Instagram use of Democrats who ran for president in 2019-2020")


# How to create categories
library(RColorBrewer)
insta_pres %>% 
  group_by(`User Name`,monthyear) %>%
  filter(monthyear >= "2018-01-01") %>%
  summarize(n = n()) %>% 
  mutate(countfactor=cut(n,breaks=c(-1,0,5,10,20,40,60,80,max(n,na.rm=T)),
                         labels=c("0","1-5","6-10","11-20","21-40","41-60","61-80",">80"))) %>%
  mutate(account = factor(`User Name`,levels=politics_insta_notrump_reordered[25:1])) %>% 
  arrange(account) %>% 
  ggplot(aes(monthyear,account,fill=countfactor))+
  geom_tile(color= "white",size=0.1) + theme_minimal() +
  scale_fill_manual(values=brewer.pal(8,"Purples"),na.value="grey90") +
  labs(x="",y="Instagram account",fill="Number\nof posts",
       subtitle="Instagram use of Democrats who ran for president in 2019-2020")


# JOYPLOTS

library(ggridges)

# Prepare data
#   1. reshape wide to long
#   2. aggregate at the level of the account
Joyplot_data <- insta_pres %>% 
  mutate(year = year(date)) %>%
  filter(year >= 2019,
         `User Name` %in% c("elizabethwarren","joebiden",
                            "berniesanders","pete.buttigieg",
                            "amyklobuchar","kamalaharris","andrewyang2020",
                            "mikebloomberg")) %>%
  mutate(`User Name` = recode(`User Name`,
                              `joebiden` = "Joe Biden",
                              `elizabethwarren` = "Elizabeth Warren",
                              `berniesanders` = "Bernie Sanders",
                              `kamalaharris` = "Kamala Harris",
                              `andrewyang2020` = "Andrew Yang",
                              `pete.buttigieg` = "Pete Buttigieg",
                              `amyklobuchar` = "Amy Klobuchar",
                              `mikebloomberg` = "Mike Bloomberg")) %>%
  rename(UN = `User Name`) %>%
  group_by(UN,monthyear) %>%
  tally() %>%
  select(UN,n) %>%
  gather(metric,monthly_posts,-UN) 


# Prepar joyplots, step-by-step

insta_pres %>% 
  mutate(year = year(date)) %>%
  filter(year >= 2019,
         `User Name` %in% c("elizabethwarren","joebiden",
                            "berniesanders","pete.buttigieg",
                            "amyklobuchar","kamalaharris","andrewyang2020",
                            "mikebloomberg")) %>%
  group_by(`User Name`,monthyear) %>%
  tally() %>%
  select(`User Name`,n) %>%
  gather(metric,monthly_posts,-`User Name`)  %>%
  ggplot(aes(x = monthly_posts, y = `User Name`)) +
  geom_density_ridges()
ggsave("jp01.png")

insta_pres %>% 
  mutate(year = year(date)) %>%
  filter(year >= 2019,
         `User Name` %in% c("elizabethwarren","joebiden",
                            "berniesanders","pete.buttigieg",
                            "amyklobuchar","kamalaharris","andrewyang2020",
                            "mikebloomberg")) %>%
  rename(UN = `User Name`) %>%
  group_by(UN,monthyear) %>%
  tally() %>%
  select(UN,n) %>%
  gather(metric,monthly_posts,-UN)  %>%
  ggplot(aes(x = monthly_posts, y = UN)) +
  geom_density_ridges() +
  labs(y="",x="")
ggsave("jp02.png")

insta_pres %>% 
  mutate(year = year(date)) %>%
  filter(year >= 2019,
         `User Name` %in% c("elizabethwarren","joebiden",
                            "berniesanders","pete.buttigieg",
                            "amyklobuchar","kamalaharris","andrewyang2020",
                            "mikebloomberg")) %>%
  rename(UN = `User Name`) %>%
  group_by(UN,monthyear) %>%
  tally() %>%
  select(UN,n) %>%
  gather(metric,monthly_posts,-UN)  %>%
  ggplot(aes(x = monthly_posts, y = UN)) +
  geom_density_ridges() +
  labs(y="",x="",subtitle="Monthly Instagram activity between January 2019 and July 2020")
ggsave("jp03.png")

Joyplot_data %>%
  ggplot(aes(x = monthly_posts, y = UN)) +
  geom_density_ridges() +
  labs(y="",x="",subtitle="Monthly Instagram activity between January 2019 and July 2020")
ggsave("jp04.png")

Joyplot_data %>%
  ggplot(aes(x = monthly_posts, y = UN)) +
  geom_density_ridges(rel_min_height = .005,
                      alpha=.6) +
  labs(y="",x="",subtitle="Monthly Instagram activity between January 2019 and July 2020")
ggsave("jp05.png")

Joyplot_data %>%
  ggplot(aes(x = monthly_posts, y = UN)) +
  geom_density_ridges(aes(fill = UN),
                      rel_min_height = .005,
                      alpha=.6) +
  labs(y="",x="",subtitle="Monthly Instagram activity between January 2019 and July 2020")
ggsave("jp06.png")

Joyplot_data %>%
  ggplot(aes(x = monthly_posts, y = UN)) +
  geom_density_ridges(fill = "#00AFBB",
                      rel_min_height = .005) +
  labs(y="",x="",subtitle="Monthly Instagram activity between January 2019 and July 2020")
ggsave("jp07.png")

Joyplot_data %>%
  ggplot(aes(x = monthly_posts, y = UN)) +
  geom_density_ridges(fill = "#00AFBB",
                      rel_min_height = .005,
                      alpha=.7) +
  labs(y="",x="",subtitle="Monthly Instagram activity between January 2019 and July 2020")
ggsave("jp08.png")

Joyplot_data %>%
  ggplot(aes(x = monthly_posts, y = UN)) +
  geom_density_ridges(fill = "#00AFBB",
                      rel_min_height = .005,
                      alpha=.6) +
  labs(y="",x="",subtitle="Monthly Instagram activity between January 2019 and July 2020") +
  xlim(0,300)
ggsave("jp09.png")

Joyplot_data %>%
  ggplot(aes(x = monthly_posts, y = UN)) +
  geom_density_ridges(fill = "#00AFBB",
                      rel_min_height = .005,
                      alpha=.6) +
  labs(y="",x="",subtitle="Monthly Instagram activity between January 2019 and July 2020") +
  xlim(0,300) + theme_bw()
ggsave("jp10.png")

Joyplot_data %>%
  ggplot(aes(x = monthly_posts, y = UN)) +
  geom_density_ridges(fill = "#00AFBB",
                      rel_min_height = .005,
                      alpha=.6) +
  labs(y="",x="",subtitle="Monthly Instagram activity between January 2019 and July 2020") +
  xlim(0,300) + theme_minimal()
ggsave("jp11.png")

Joyplot_data %>%
  ggplot(aes(x = monthly_posts, y = UN)) +
  geom_density_ridges(fill = "#00AFBB",
                      rel_min_height = .005,
                      alpha=.6) +
  labs(y="",x="",subtitle="Monthly Instagram activity between January 2019 and July 2020") +
  xlim(0,300) + theme_minimal() +
  theme(panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank())
ggsave("jp12.png")

Joyplot_data %>%
  ggplot(aes(x = monthly_posts, y = UN)) +
  geom_density_ridges(fill = "#00AFBB",
                      rel_min_height = .005,
                      alpha=.6) +
  labs(y="",x="",subtitle="Monthly Instagram activity between January 2019 and July 2020") +
  xlim(0,300) + theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 12, family = "Gill Sans"))
ggsave("jp13.png")

Joyplot_data %>%
  ggplot(aes(x = monthly_posts, y = UN)) +
  geom_density_ridges(fill = "#00AFBB",
                      rel_min_height = .005,
                      alpha=.6) +
  labs(y="",x="",subtitle="Monthly Instagram activity between January 2019 and July 2020") +
  xlim(0,300) + theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5, size = rel(1.3)),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      text = element_text(size = 12, family = "Gill Sans"),
      panel.background = element_blank())
ggsave("jp14.png")

Joyplot_data %>%
  ggplot(aes(x = monthly_posts, y = UN)) +
  geom_density_ridges(aes(fill = UN),
                      rel_min_height = .005,
                      alpha=.7) +
  scale_fill_manual(values =c("#0AA455",
                              "#BEAED4",
                              "#FDC086",
                              "#8DF1BD",
                              "#386CB0",
                              "#F0027F",
                              "#00AFBB",
                              "#F2BA42")) +
  xlim(0, 300) +
  labs(y="",x="",subtitle="Monthly Instagram activity between January 2019 and July 2020") +
  theme_minimal()  +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = rel(1.3)),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(size = 12, family = "Gill Sans"),
        panel.background = element_blank())
ggsave("jp15.png")

Joyplot_data %>%
  ggplot(aes(x = monthly_posts, y = UN)) +
  geom_density_ridges(aes(fill = UN),
                      rel_min_height = .005,
                      alpha=.7) +
  scale_fill_manual(values =c("#0AA455","#BEAED4","#FDC086","#8DF1BD",
                              "#386CB0","#F0027F","#00AFBB",
                              "#F2BA42")) +
  xlim(0, 300) +
  labs(y="",x="",subtitle="Monthly Instagram activity between January 2019 and July 2020") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = rel(1.3)),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(size = 12, family = "Gill Sans"),
        panel.background = element_blank(),
        legend.position = "none")
ggsave("jp16.png")

insta_pres %>% 
  mutate(year = year(date)) %>%
  filter(year >= 2019,
         `User Name` %in% c("elizabethwarren","joebiden",
                            "berniesanders","pete.buttigieg",
                            "amyklobuchar","kamalaharris","andrewyang2020",
                            "mikebloomberg")) %>%
  mutate(`User Name` = recode(`User Name`,
                              `joebiden` = "Joe Biden",
                              `elizabethwarren` = "Elizabeth Warren",
                              `berniesanders` = "Bernie Sanders",
                              `kamalaharris` = "Kamala Harris",
                              `andrewyang2020` = "Andrew Yang",
                              `pete.buttigieg` = "Pete Buttigieg",
                              `amyklobuchar` = "Amy Klobuchar",
                              `mikebloomberg` = "Mike Bloomberg")) %>%
  rename(UN = `User Name`) %>%
  mutate(newly_ordered_account = factor(UN,levels = 
                                          c("Pete Buttigieg",
                                            "Joe Biden",
                                            "Kamala Harris",
                                            "Amy Klobuchar",
                                            "Andrew Yang",
                                            "Mike Bloomberg",
                                            "Bernie Sanders",
                                            "Elizabeth Warren"
                                          ))) %>%
  group_by(newly_ordered_account,monthyear) %>%
  tally() %>%
  select(newly_ordered_account,n) %>%
  gather(metric,monthly_posts,-newly_ordered_account) %>%
  ggplot(aes(x = monthly_posts, y = newly_ordered_account)) +
  geom_density_ridges(aes(fill = newly_ordered_account),
                      rel_min_height = .005,
                      alpha=.7) +
  scale_fill_manual(values =c("#0AA455","#BEAED4","#FDC086","#8DF1BD",
                              "#386CB0","#F0027F","#00AFBB",
                              "#F2BA42")) +
  xlim(0, 300) +
  labs(y="",x="",subtitle="Monthly Instagram activity between January 2019 and July 2020") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(size = 12, family = "Gill Sans"),
        panel.background = element_blank(),
        legend.position = "none")
ggsave("jp16B.png")

insta_pres %>% 
  mutate(year = year(date)) %>%
  filter(year >= 2019,
         `User Name` %in% c("elizabethwarren","joebiden",
                            "berniesanders","pete.buttigieg",
                            "amyklobuchar","kamalaharris","andrewyang2020",
                            "mikebloomberg")) %>%
  mutate(`User Name` = recode(`User Name`,
                              `joebiden` = "Joe Biden",
                              `elizabethwarren` = "Elizabeth Warren",
                              `berniesanders` = "Bernie Sanders",
                              `kamalaharris` = "Kamala Harris",
                              `andrewyang2020` = "Andrew Yang",
                              `pete.buttigieg` = "Pete Buttigieg",
                              `amyklobuchar` = "Amy Klobuchar",
                              `mikebloomberg` = "Mike Bloomberg")) %>%
  rename(UN = `User Name`) %>%
  mutate(newly_ordered_account = factor(UN,levels = 
                              c("Pete Buttigieg",
                                "Joe Biden",
                                "Kamala Harris",
                                "Amy Klobuchar",
                                "Andrew Yang",
                                "Mike Bloomberg",
                                "Bernie Sanders",
                                "Elizabeth Warren"
                                ))) %>%
  group_by(newly_ordered_account,monthyear) %>%
  tally() %>%
  select(newly_ordered_account,n) %>%
  gather(metric,monthly_posts,-newly_ordered_account) %>%
  ggplot(aes(x = monthly_posts, y = newly_ordered_account)) +
  geom_density_ridges(aes(fill = newly_ordered_account),
                      rel_min_height = .005,
                      alpha=.7) +
  scale_fill_manual(values =
    c("Amy Klobuchar" = "#0AA455",
      "Andrew Yang" = "#BEAED4",
      "Bernie Sanders" = "#FDC086",
      "Elizabeth Warren" = "#8DF1BD",
      "Joe Biden" = "#386CB0",
      "Kamala Harris" = "#F0027F",
      "Mike Bloomberg" = "#00AFBB",
      "Pete Buttigieg" = "#F2BA42")
  ) +
  xlim(0, 300) +
  labs(y="",x="",subtitle="Monthly Instagram activity between January 2019 and July 2020") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(size = 12, family = "Gill Sans"),
        panel.background = element_blank(),
        legend.position = "none")
ggsave("jp17.png")

insta_pres %>% 
  mutate(year = year(date)) %>%
  filter(year >= 2019,
         `User Name` %in% c("elizabethwarren","joebiden",
                            "berniesanders","pete.buttigieg",
                            "amyklobuchar","kamalaharris","andrewyang2020",
                            "mikebloomberg")) %>%
  mutate(`User Name` = recode(`User Name`,
                              `joebiden` = "Joe Biden",
                              `elizabethwarren` = "Elizabeth Warren",
                              `berniesanders` = "Bernie Sanders",
                              `kamalaharris` = "Kamala Harris",
                              `andrewyang2020` = "Andrew Yang",
                              `pete.buttigieg` = "Pete Buttigieg",
                              `amyklobuchar` = "Amy Klobuchar",
                              `mikebloomberg` = "Mike Bloomberg")) %>%
  rename(UN = `User Name`) %>%
  mutate(newly_ordered_account = factor(UN,levels = 
                                          c("Pete Buttigieg",
                                            "Joe Biden",
                                            "Kamala Harris",
                                            "Amy Klobuchar",
                                            "Andrew Yang",
                                            "Mike Bloomberg",
                                            "Bernie Sanders",
                                            "Elizabeth Warren"
                                          ))) %>%
  group_by(newly_ordered_account,monthyear) %>%
  tally() %>%
  select(newly_ordered_account,n) %>%
  gather(metric,monthly_posts,-newly_ordered_account) %>%
  ggplot(aes(x = monthly_posts, y = newly_ordered_account)) +
  geom_density_ridges(aes(fill = newly_ordered_account),
                      rel_min_height = .005,
                      scale=2.2,
                      alpha=.7) +
  scale_fill_manual(values =
                      c("Amy Klobuchar" = "#0AA455",
                        "Andrew Yang" = "#BEAED4",
                        "Bernie Sanders" = "#FDC086",
                        "Elizabeth Warren" = "#8DF1BD",
                        "Joe Biden" = "#386CB0",
                        "Kamala Harris" = "#F0027F",
                        "Mike Bloomberg" = "#00AFBB",
                        "Pete Buttigieg" = "#F2BA42")
  ) +
  xlim(0, 300) +
  labs(y="",x="",subtitle="Monthly Instagram activity between January 2019 and July 2020") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(size = 12, family = "Gill Sans"),
        panel.background = element_blank(),
        legend.position = "none") -> plot_penultimate

plot_penultimate
ggsave("jp18.png")

plot_penultimate + geom_density_ridges(aes(fill = newly_ordered_account),
                    rel_min_height = .005,
                    alpha=.7,
                    scale=2.2,
                    jittered_points = TRUE,
                    position = position_points_jitter(width = 0.05, height = 0),
                    point_shape = '|', point_size = 2, point_alpha = .6)
ggsave("jp19.png")

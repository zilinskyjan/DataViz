library(tidyverse)
library(haven)
library(pollster)
library(labelled)
library(ggridges)

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

getPCP <- function(x) {
  DAMisc::pre(x)$pcp %>% round(3) *100
}

# 2019 study [had data on social media use but no Covid theory]
d1 <- read_dta("data_AJPS2021/Study1.dta")

# The 2020 study is representative; representative of 2010 US Census records 
# on age, race, sex, income, and education. The survey was conducted October 8-21, 2020.
d2 <- read_dta("data_AJPS2021/Study2.dta")

# And a dataset with many populism items:
d3 <- read_dta("data_AJPS2021/Replication.dta")
d3$consp_Index <- (d3$con1 + d3$con2 + d3$con3 + d3$con4)/4
d3$pop_Index <- (d3$pop1 + d3$pop2 + d3$pop3 + d3$pop4 + d3$pop5 + d3$pop6 + d3$pop7 + d3$pop8 + d3$pop9)/9
d3 <- d3 %>% mutate(pid3 = case_when(
  pid <= 3 ~ "Democrat",
  pid >= 5 ~ "Republica",
  pid == 4 ~ "Indep."
)
)
d3$pid3 <- d3$pid3 %>% factor %>% relevel(ref="Indep.")




d1$consp_Index <- (d1$con1 + d1$con2 + d1$con3 + d1$con4)/4
d1$consp_Index2 <- rescale01(d1$consp_Index)

d1$pop_Index <- (d1$pop1 + d1$pop2)/4
d1$pop_Index2 <- rescale01(d1$pop_Index)

d1$college_grad <- ifelse(d1$edu >= 5,1,0)
d1$Democrat <- ifelse(d1$pid <= 3, 1, 0 )
d1$Republican <- ifelse(d1$pid >=5, 1, 0 )
d1 <- d1 %>% mutate(pid3 = case_when(
  pid <= 3 ~ "Democrat",
  pid >= 5 ~ "Republica",
  pid == 4 ~ "Indep."
))
d1$pid3 <- d1$pid3 %>% factor %>% relevel(ref="Indep.")

d2$birtherBIN <- ifelse(d2$birther >= 4,1,0)
d2$climatechangeBIN <- ifelse(d2$climatechange >= 4,1,0)

d2$college_grad <- ifelse(d2$edu >= 5,1,0)
d2$AffPolParties <- d2$reppartyft - d2$dempartyft
d2$AffDemMinusRep <- d2$dempartyft - d2$reppartyft
d2$AffDemMinusRep2 <- rescale01(d2$AffDemMinusRep)
d2$consp_Index <- (d2$con1 + d2$con2 + d2$con3 + d2$con4)/4
d2$consp_Index2 <- rescale01(d2$consp_Index)
# cor(d2$consp_Index,d2$suspicion, use = "complete.obs")
d2$pid_reg <- as_factor(d2$pid) %>% relevel(ref = 3)
d2$Republican <- ifelse(d2$pid == 4 | d2$pid==5, 1,0)
d2$Democrat <- ifelse(d2$pid <= 2, 1,0)
d2 <- d2 %>% mutate(pid3 = case_when(
  pid <= 2 ~ "Democrat",
  pid >= 4 ~ "Republica",
  pid == 3 ~ "Indep."
))
d2$pid3 <- d2$pid3 %>% factor %>% relevel(ref="Indep.")


# STUDY 1:

val_labels(d1$Republican) <- c("Republicans" = 1,
                             "Democrats and Independents" = 0)

val_labels(d1$attent1) <- c("Strongly disagree" = 1,
                             "Disagree" = 2,
                             "Neither" = 3,
                             "Agree" = 4,
                             "Strongly agree" = 5)

val_labels(d1$goodevil) <- c("Strongly disagree" = 1,
                             "Disagree" = 2,
                             "Neither" = 3,
                             "Agree" = 4,
                             "Strongly agree" = 5)


val_labels(d1$facebook) <- c("Not at all" = 1,
                             "Once a month" = 2,
                             "Several times monthly" = 3,
                             "Several times weekly" = 4,
                             "Every day" = 5)

val_labels(d1$reddit) <- c("Not at all" = 1,
                           "Once a month" = 2,
                           "Several times monthly" = 3,
                           "Several times weekly" = 4,
                           "Every day" = 5)

val_labels(d1$twitter) <- c("Not at all" = 1,
                            "Once a month" = 2,
                            "Several times monthly" = 3,
                            "Several times weekly" = 4,
                            "Every day" = 5)
# STUDY 2 vars relabel
val_labels(d2$goodevil) <- c("Strongly disagree" = 0,
                             "Disagree" = .25,
                             "No view" = .5,
                             "Agree" =.75,
                             "Strongly agree" = 1)

val_labels(d2$pid) <- c(`Strong Democrat` =1,
                        `Democrat`=2,
                        `Independent`=3,
                        `Republican`=4,
                        `Strong Republican`=5)



val_labels(d2$goodevil) <- c("Strongly disagree" = 0,
                             "Disagree" = 1,
                             "Neither agree, nor disagree" = 2,
                             "Agree" =3,
                             "Strongly agree" = 4)

val_labels(d2$pop1) <- c("Strongly disagree" = 1,
                         "Disagree" = 2,
                         "Neither agree, nor disagree" = 3,
                         "Agree" =4,
                         "Strongly agree" = 5)

val_labels(d2$pop2) <- c("Strongly disagree" = 1,
                         "Disagree" = 2,
                         "Neither agree, nor disagree" = 3,
                         "Agree" =4,
                         "Strongly agree" = 5)

val_labels(d2$official) <- c("Strongly disagree" = 1,
                         "Disagree" = 2,
                         "Neither agree, nor disagree" = 3,
                         "Agree" =4,
                         "Strongly agree" = 5)

val_labels(d2$con1) <- c("Strongly disagree" = 1,
                         "Disagree" = 2,
                         "Neither agree, nor disagree" = 3,
                         "Agree" =4,
                         "Strongly agree" = 5)

val_labels(d2$con4) <- c("Strongly disagree" = 1,
                         "Disagree" = 2,
                         "Neither agree, nor disagree" = 3,
                         "Agree" =4,
                         "Strongly agree" = 5)

val_labels(d2$collusion) <- c("Strongly disagree" = 1,
                         "Disagree" = 2,
                         "Neither agree, nor disagree" = 3,
                         "Agree" =4,
                         "Strongly agree" = 5)

d2 <- d2 %>%
  set_value_labels(pid = c(`Strong Democrat` =1,
                           `Democrat`=2,
                           `Independent`=3,
                           `Republican`=4,
                           `Strong Republican`=5))

var_label(d2$consp_Index) <- "Conspiracy Index"
var_label(d2$con1) <- "Even though we live in a democracy, a few people will always run things anyway"
var_label(d2$con4) <- "Much of our lives are being controlled by plots hatched in secret places"                 

d2$trumpft_tercile <- cut(d2$trumpft,3)

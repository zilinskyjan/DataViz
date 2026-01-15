#getting-data-wdi.R

library(WDI)
#remotes::install_github('vincentarelbundock/WDI')

OCED35countries <- c(
  "AUS",
  "KOR",
  "AUT",
  "LVA",
  "BEL",
  "LUX",
  "CAN",
  "MEX",
  "CHL",
  "NLD",
  "CZE",
  "NZL",
  "DNK",
  "NOR",
  "EST",
  "POL",
  "FIN",
  "PRT",
  "FRA",
  "SVK",
  "DEU",
  "SVN",
  "GRC",
  "ESP",
  "HUN",
  "SWE",
  "ISL",
  "CHE",
  "IRL",
  "TUR",
  "ISR",
  "GBR",
  "ITA",
  "USA",
  "JPN"
)

#WDIsearch('inflation')
#WDIsearch('deficit')

# GB.BAL.OVRL.GDP.ZS = Overall budget deficit, including grants (% of GDP)
# FP.CPI.TOTL.ZG' = Inflation, consumer prices (annual %)

D_inf <- WDI(indicator=c('FP.CPI.TOTL.ZG'), extra = T) %>%
  tibble() %>% rename(inflation = FP.CPI.TOTL.ZG)

# CZE has a missing region, fill it in:
D_inf$region <- ifelse(D_inf$iso3c=="CZE","Europe",D_inf$region)

inflation <- D_inf %>% 
  filter(region != "Aggregates") %>%
  filter(!is.na(inflation)) %>%
  select(country, iso3c, inflation, year)

write_csv(inflation,"data_macro/inflation_WDI.csv")



D_deficit_WDI <- WDI(indicator=c('GB.BAL.OVRL.GDP.ZS'), extra = T) %>%
  tibble() %>% rename(deficit = GB.BAL.OVRL.GDP.ZS)

D_spending <- WDI(indicator=c('GB.BAL.OVRL.GDP.ZS'), extra = T) %>%
  tibble() %>% rename(deficit = GB.BAL.OVRL.GDP.ZS)

library(tidyverse)
library(tidyquant)

# Get stocks prices
stocks <- tq_get(c("NVDA", "AVGO","^GSPC"),
<<<<<<< HEAD
                 from = "2022-015-01",
                 to = "2024-06-05")
=======
                 from = "2022-01-01",
                 to = "2024-02-07")
>>>>>>> c8c661aa7505dcf2d0f44473c633b100f88aa626

stocks

# Plot
stocks %>%
 ggplot(aes(x = date, y = adjusted, color = symbol)) +
 geom_line() +
 theme_minimal() +
 labs(title = "NVIDIA vs Broadcom Stock Prices",
      y = "Adjusted Price",
      color = "Stock")

# Calculate returns
returns <- stocks %>%
 group_by(symbol) %>%
 tq_transmute(select = adjusted,
              mutate_fun = periodReturn,
              period = "daily",
              col_rename = "ret") %>%
 ungroup() %>%
 pivot_wider(names_from = symbol,
             values_from = ret) %>%
 na.omit()

# Calculate rolling correlation
returns$roll_cor <- zoo::rollapply(data = returns[, c("NVDA", "AVGO")],
                              width = 30,
                              FUN = function(x) cor(x[,1], x[,2]),
                              by.column = FALSE,
                              align = "right",
                              fill = NA)

returns$roll_corNVSP <- zoo::rollapply(data = returns[, c("NVDA", "^GSPC")],
                                   width = 60,
                                   FUN = function(x) cor(x[,1], x[,2]),
                                   by.column = FALSE,
                                   align = "right",
                                   fill = NA)


# Plot
returns %>%
 filter(date >= "2022-04-01") %>%
ggplot(aes(x = date, y = roll_cor)) +
 geom_line(color = "blue") +
 geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
 theme_minimal() +
 labs(subtitle = "30-Day Rolling Correlation of Daily Returns: NVIDIA vs Broadcom",
      title = "Do semiconductor industry giants move together?",
      y = "Correlation Coefficient",
      x = "Date") +
 scale_y_continuous(limits = c(-.5, 1))

returns %>%
 filter(date >= "2022-04-01") %>%
 ggplot(aes(x = date, y = roll_corNVSP)) +
 geom_line(color = "blue") +
 geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
 theme_minimal() +
 labs(subtitle = "60-Day Rolling Correlation of Daily Returns: NVIDIA vs SP500",
      title = "Do semiconductor industry giants move together?",
      y = "Correlation Coefficient",
      x = "Date") +
 scale_y_continuous(limits = c(-.5, 1))


# Scatter of returns
ggplot(returns, aes(x = NVDA, y = AVGO)) +
 geom_point() +
 geom_smooth(method = "lm") +
 labs(title = "Scatterplot of Daily Returns",
      x = "NVIDIA",
      y = "Broadcom") +
 geom_abline(intercept = 0, slope = 1, 
             color = "red3",linetype=2,linewidth=1) 
 
 

ggplot(returns, aes(x = NVDA, y = AVGO)) +
 geom_point() +
 labs(title = "Scatterplot of Daily Returns",
      x = "NVIDIA",
      y = "Broadcom")

ggplot(returns, aes(x = `^GSPC`, y = AVGO)) +
 geom_point() +
 labs(title = "Scatterplot of Daily Returns",
      x = "S&P500",
      y = "Broadcom")

ggplot(returns, aes(x = `^GSPC`, y = NVDA)) +
 geom_point() +
 labs(title = "Scatterplot of Daily Returns",
      x = "S&P500",
      y = "NVIDIA")



# Let's also look at correlation during different market conditions
returns %>%
 mutate(year = year(date)) %>%
 group_by(year) %>%
 summarize(correlation = cor(NVDA, AVGO)) %>%
 print()


returns %>% mutate(ym = floor_date(date,"month")) %>%
 group_by(ym) %>%
 summarize(correlation = cor(NVDA, `^GSPC`)) %>%
 ggplot(aes(x = ym, y = correlation)) +
 geom_col()
 


# P/E RATIOS
# https://github.com/business-science/alphavantager

library(alphavantager)


alphaKey <- "NXBDE89IQ663FLAB"
av_api_key(alphaKey)
av_api_key("ILBIW8X7MIEZZ4FR")

sp503 <- c(
"AAPL",
"NVDA",
"MSFT",
"AMZN",
"GOOG",
"GOOGL",
"META",
"TSLA",
"AVGO",
"BRK.B",
"WMT",
"LLY",
"JPM",
"V",
"MA",
"ORCL",
"UNH",
"XOM",
"COST",
"NFLX",
"HD",
"PG",
"JNJ",
"BAC",
"ABBV",
"CRM",
"TMUS",
"KO",
"CVX",
"WFC",
"PLTR",
"CSCO",
"ACN",
"IBM",
"MS",
"PM",
"ABT",
"AXP",
"GS",
"MRK",
"GE",
"TMO",
"LIN",
"MCD",
"BX",
"ISRG",
"NOW",
"DIS",
"PEP",
"ADBE",
"QCOM",
"T",
"CAT",
"AMD",
"RTX",
"VZ",
"TXN",
"INTU",
"BKNG",
"SPGI",
"AMGN",
"UBER",
"BSX",
"BLK",
"C",
"SCHW",
"ANET",
"SYK",
"DHR",
"PGR",
"UNP",
"AMAT",
"PFE",
"LOW",
"NEE",
"TJX",
"BA",
"HON",
"KKR",
"FI",
"CMCSA",
"SBUX",
"PANW",
"DE",
"COP",
"ADP",
"ETN",
"VRTX",
"GILD",
"MDT",
"BMY",
"MMC",
"PLD",
"CB",
"LRCX",
"LMT",
"GEV",
"CRWD",
"MU",
"ADI",
"NKE",
"KLAC",
"CEG",
"UPS",
"ICE",
"SO",
"APO",
"MCO",
"ELV",
"SHW",
"WM",
"EQIX",
"MO",
"DUK",
"WELL",
"AMT",
"CME",
"PH",
"MAR",
"ABNB",
"APH",
"AON",
"INTC",
"FTNT",
"CDNS",
"HCA",
"CTAS",
"MMM",
"SNPS",
"AJG",
"MSI",
"TT",
"CI",
"PNC",
"CMG",
"COF",
"ZTS",
"PYPL",
"REGN",
"ORLY",
"MDLZ",
"ITW",
"MCK",
"TDG",
"DELL",
"USB",
"WDAY",
"RCL",
"CL",
"EOG",
"EMR",
"ECL",
"RSG",
"APD",
"GD",
"WMB",
"CVS",
"SPG",
"NOC",
"BDX",
"HLT",
"ADSK",
"CSX",
"TFC",
"BK",
"FDX",
"ROP",
"TGT",
"KMI",
"JCI",
"MET",
"CARR",
"AZO",
"AFL",
"DLR",
"VST",
"NSC",
"SLB",
"CPRT",
"OKE",
"PCAR",
"TRV",
"CHTR",
"FCX",
"NXPI",
"AEP",
"PSA",
"PAYX",
"SRE",
"HWM",
"AMP",
"AXON",
"CMI",
"NEM",
"GWW",
"ALL",
"DFS",
"PSX",
"URI",
"LULU",
"COR",
"NDAQ",
"O",
"MPC",
"GM",
"ROST",
"KR",
"AIG",
"FANG",
"PWR",
"D",
"BKR",
"FICO",
"MNST",
"MSCI",
"FIS",
"TRGP",
"HES",
"OXY",
"TEL",
"DAL",
"KMB",
"GLW",
"CBRE",
"CTSH",
"CTVA",
"AME",
"FAST",
"VLO",
"KDP",
"ODFL",
"GRMN",
"PEG",
"EXC",
"EW",
"VRSK",
"A",
"IT",
"DHI",
"YUM",
"GEHC",
"PRU",
"LHX",
"CCI",
"KVUE",
"XEL",
"IDXX",
"OTIS",
"TTWO",
"IR",
"IQV",
"F",
"VMC",
"UAL",
"WAB",
"KHC",
"ETR",
"ACGL",
"CCL",
"RMD",
"SYY",
"MPWR",
"DXCM",
"EXR",
"RJF",
"LYV",
"EA",
"PCG",
"ED",
"MTB",
"HUM",
"MLM",
"HIG",
"WTW",
"EBAY",
"GIS",
"LEN",
"WEC",
"XYL",
"VICI",
"DD",
"CSGP",
"HSY",
"AVB",
"IRM",
"BRO",
"TPL",
"EFX",
"LVS",
"CAH",
"NUE",
"STZ",
"EQT",
"ROK",
"HPQ",
"ANSS",
"KEYS",
"GDDY",
"IP",
"FITB",
"MTD",
"CNC",
"STT",
"K",
"TSCO",
"BR",
"SW",
"HPE",
"MCHP",
"GPN",
"EQR",
"DOV",
"DOW",
"FTV",
"PPG",
"SYF",
"CPAY",
"EXPE",
"TYL",
"AEE",
"CHD",
"VTR",
"CDW",
"DECK",
"DTE",
"PPL",
"NTAP",
"WBD",
"LYB",
"HBAN",
"TROW",
"VLTO",
"WAT",
"ROL",
"FOXA",
"AWK",
"TDY",
"EL",
"WRB",
"FOX",
"WST",
"FE",
"DRI",
"NVR",
"ATO",
"WDC",
"NTRS",
"RF",
"SBAC",
"HAL",
"CBOE",
"STE",
"ADM",
"ON",
"WY",
"DVN",
"ES",
"IFF",
"ERIE",
"HUBB",
"PHM",
"CINF",
"SMCI",
"CNP",
"CFG",
"VRSN",
"MKC",
"NRG",
"TSN",
"BIIB",
"LH",
"CMS",
"LII",
"STX",
"PTC",
"CTRA",
"ZBH",
"PODD",
"KEY",
"ESS",
"INVH",
"EIX",
"STLD",
"LDOS",
"MAA",
"L",
"ZBRA",
"PKG",
"COO",
"PFG",
"LUV",
"TER",
"CLX",
"TRMB",
"BBY",
"JBL",
"DGX",
"FSLR",
"FFIV",
"SNA",
"FDS",
"BLDR",
"NI",
"NWS",
"ULTA",
"GEN",
"JBHT",
"MAS",
"TPR",
"RL",
"ARE",
"NWSA",
"OMC",
"J",
"GPC",
"PNR",
"DPZ",
"HRL",
"DG",
"MOH",
"ALGN",
"BAX",
"DLTR",
"UDR",
"EXPD",
"LNT",
"KIM",
"APTV",
"BALL",
"EVRG",
"AKAM",
"IEX",
"EPAM",
"CF",
"BF.B",
"EG",
"HOLX",
"AMCR",
"RVTY",
"AVY",
"INCY",
"DVA",
"DOC",
"TXT",
"REG",
"SWK",
"KMX",
"CPT",
"BXP",
"VTRS",
"JKHY",
"SOLV",
"POOL",
"MRNA",
"UHS",
"NDSN",
"NCLH",
"CAG",
"JNPR",
"HST",
"TECH",
"EMN",
"CHRW",
"CPB",
"ALLE",
"TAP",
"PAYC",
"AIZ",
"SJM",
"BEN",
"SWKS",
"GL",
"DAY",
"MGM",
"IPG",
"PNW",
"HSIC",
"BG",
"LKQ",
"FRT",
"AOS",
"ALB",
"WYNN",
"WBA",
"ENPH",
"MOS",
"MTCH",
"IVZ",
"GNRC",
"LW",
"CRL",
"HAS",
"APA",
"TFX",
"MHK",
"MKTX",
"CZR",
"AES",
"CE",
"PARA",
"HII",
"BWA",
"FMC")


latest <- tq_get(sp503,
             get        = "alphavantage",
             av_fun     = "OVERVIEW",
             outputsize = "full")


 latest %>% count(rank_group)
 latest %>% count(symbol)

 
company_names <- latest %>%
  filter(rank_group == "Name") %>%
  select(symbol,Name=value)

data <- latest %>%
  filter(rank_group %in% c(
    "PERatio",
 "ProfitMargin",
 "ReturnOnEquityTTM",
 "ReturnOnAssetsTTM",
 "ForwardPE",
 "EVToEBITDA",
 "DividendYield")) %>%
  pivot_wider(names_from = rank_group, values_from = value) %>%
  left_join(company_names, by="symbol")


data %>%
  ggplot(aes(x=ProfitMargin,y=PERatio)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label=Name)) 

data %>%
  ggplot(aes(x=EVToEBITDA,y=ReturnOnEquityTTM)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label=Name)) 



library(httr)
library(jsonlite)

# Example for getting overview (includes P/E ratio)
get_fundamentals <- function(symbol, apikey) {
  url <- paste0("https://www.alphavantage.co/query?function=INCOME_STATEMENT&symbol=", 
                symbol, "&apikey=", apikey)
  response <- GET(url)
  data <- fromJSON(rawToChar(response$content))
  return(data)
}

# this works by the json returns a bunch of string vectors...
m <- get_fundamentals("MSFT","alphaKey")
mq <- tibble(m$quarterlyReports,symbol="MSFT")
mq$pm <- as.numeric(mq$grossProfit)/as.numeric(mq$totalRevenue)

hist(mq$pm)

# Define the tickers for the companies you want to analyze
tickers <- c("AAPL", "MSFT", "GOOG", "AMZN")  # Example tickers

# Define the date range for your data
start_date <- "2020-01-01"
end_date <- "2023-10-27" # Today's date (or any date you want)

# Function to fetch fundamental data for a single ticker
get_fundamental_data <- function(ticker) {
  tryCatch({  # Handle potential errors gracefully
    av_get(symbol = ticker, tq_function = "FUNDAMENTAL_DATA", interval = "annual") %>% # or "quarterly"
      mutate(symbol = ticker) # Add a symbol column for identification
  }, error = function(e) {
    print(paste("Error fetching data for", ticker, ":", e$message))
    return(NULL) # Return NULL if there's an error
  })
}
m <- av_get(symbol = "MSFT", av_fun = "OVERVIEW", interval = "annual")


# Fetch fundamental data for all tickers, handling errors
fundamental_data <- tickers %>%
  map_df(get_fundamental_data) # map_df applies the function to each ticker and combines the results

# Clean and process the data (example)
fundamental_data_cleaned <- fundamental_data %>%
  # Convert relevant columns to numeric (important!)
  mutate(
    `P/E Ratio` = as.numeric(`P/E Ratio`), # Backticks are crucial for column names with spaces/special chars
    `Profit Margin` = as.numeric(`Profit Margin`),
    #... convert other needed columns
  ) %>%
  filter(!is.na(`P/E Ratio`),!is.na(`Profit Margin`)) %>% # Remove rows with NA values after conversion
  #... any other cleaning/calculations...
  arrange(symbol, fiscalDateEnding) # Sort by ticker and date




# NOT WORKING
# pe_ratios <- tq_get(c("NVDA", "AVGO"),
#                     from = "2019-01-01",
#                     to = "2024-01-28",
#                     get = "alphavantager")
# 
# library(quantmod)
# getQuote(c("NVDA", "AVGO"), what = yahooQF(c("P/E Ratio")))

library(tidyverse)
library(tidyquant)
library(rvest)
library(plotly)
library(here)

if(!dir.exists("data")) dir.create("data")

# Scrape B3 Tickers -------------------------------------------------------

pag <- 
  read_html("https://www.dadosdemercado.com.br/bolsa/acoes")

tickers <- pag %>% 
  html_elements("strong > a") %>% 
  html_text()

# tickers <- c("PETR4", "VALE3", "ITUB4", "BBDC4", "B3SA3",
#               "ABEV3", "LREN3", "CIEL3", "GGBR4", "WEGE3")

get_yearly_yield <- function(tickers, from = Sys.Date() - 30,
                             to = Sys.Date(), format_perc = FALSE){
  
  safe_tq_get <- safely(tq_get)
  
  stocks <- tq_get(
    x = paste0(tickers, ".SA"),
    get = "stock.prices",
    from = from,
    to = to
  )
  
  dividends <- 
    map(tickers,
        .f = function(ticker){
          safe_tq_get(
            x = paste0(ticker, ".SA"),
            get = "dividends",
            from = from,
            to = to
          )
        }
    ) %>% 
    map("result") %>% 
    compact() %>% 
    list_rbind()
  
  # tq_get(
  #   x = paste0(tickers, ".SA"),
  #   get = "dividends",
  #   from = from,
  #   to = to
  # ) %>% 
  # arrange(symbol, date)
  
  df <- stocks %>% 
    group_by(symbol,
             date = year(date)) %>% 
    summarize(stock_price = dplyr::last(adjusted)) %>% 
    inner_join(
      dividends %>% 
        group_by(symbol, 
                 date = year(date)) %>%
        summarize(dividends = sum(value)),
      by = c("date","symbol")
    ) %>% 
    mutate(yield = dividends / stock_price,
           symbol = str_remove_all(symbol, "\\.SA$")) %>% 
    ungroup()    
  #yield = ifelse(format_perc, round(yield * 100, digits =  2), yield))
  
  return(df)  
}

tictoc::tic()
df <- get_yearly_yield(tickers[1:329], from = "2020-01-01") %>% 
  mutate(date = as.integer(date))
tictoc::toc()

df %>% 
  write_csv(here("data", "yield_data.csv"),
            append = TRUE)

high_pay_tickers <- df %>% 
  group_by(symbol) %>%
  summarize(yield = mean(yield)) %>%
  arrange(desc(yield)) %>% 
  head(12) %>% 
  pull(symbol)


df %>%
  filter(symbol %in% high_pay_tickers) %>% 
  mutate(symbol = fct_reorder(symbol, -yield),
         date = factor(date)) %>%  
  ggplot(aes(date, yield)) +
  geom_col(fill = "#459b45") +
  facet_wrap(vars(symbol), scales = "free")


df %>% 
  filter(symbol %in% high_pay_tickers) %>% 
  group_by(symbol) %>% 
  summarize(first_yield = first(yield),
            last_yield = last(yield),
            diff_yield = last(yield) - first(yield)) %>% 
  arrange(desc(last_yield)) %>% 
  knitr::kable(digits = 2,
               align = "c",
               col.names = c("Symbol", "First Yield", "Last Yield", "Diff Yield"))


df









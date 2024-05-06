library(tidyverse)
library(tidyquant)
library(rvest)
library(plotly)
library(here)
library(gt)

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
                             to = Sys.Date()){
  
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
  
  return(df)  
}

tictoc::tic()
df <- get_yearly_yield(tickers[1:329], from = "2017-01-01") %>% 
  mutate(date = as.integer(date))
tictoc::toc()

df %>% 
  write_csv(here("data", "yield_data.csv"),
            col_names = TRUE,
            append = TRUE)

# df <-
#   read_csv("data/yield_data.csv",
#            col_names = c("symbol", "date", "stock_price", "dividend", "yield"))

high_pay_tickers <- df %>% 
  group_by(symbol) %>%
  summarize(yield = mean(yield)) %>%
  arrange(desc(yield)) %>% 
  head(12) %>% 
  pull(symbol)


df %>%
  filter(symbol %in% high_pay_tickers) %>% 
  mutate(symbol = fct_reorder(symbol, -yield)) %>%  
  ggplot(aes(date, yield)) +
  geom_area(fill = "#459b45", alpha = 0.4, color = "#45cf49") +
  facet_wrap(vars(symbol), scales = "free")


df %>% 
  filter(symbol %in% high_pay_tickers) %>% 
  group_by(symbol) %>% 
  summarize(first_yield = first(yield),
            last_yield = last(yield),
            diff_yield = last(yield) - first(yield),
            min_date = min(date),
            max_date = max(date)) %>% 
  mutate(symbol = str_glue("{symbol} ({min_date}-{max_date})")) %>% 
  select(-c(min_date, max_date)) %>% 
  arrange(desc(diff_yield)) %>% 
  gt() %>% 
  cols_label_with(
    columns = everything(),
    fn = \(x) x %>% 
      str_replace("_", " ") %>% 
      str_to_title()
  ) %>% 
  fmt_percent(
    columns = everything(),
    decimals = 3
  ) %>%
  data_color(
    columns = "diff_yield",
    method = "numeric",
    palette = "YlGn"
  )

df









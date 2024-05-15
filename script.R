library(tidyverse)
library(tidyquant)
library(rvest)
library(here)
library(gt)
library(highcharter)

if(!dir.exists("data")) dir.create("data")

# Scrape B3 Tickers -------------------------------------------------------

pag <- 
  read_html("https://www.dadosdemercado.com.br/bolsa/acoes")

tickers <- pag %>% 
  html_elements("strong > a") %>% 
  html_text()

# tickers <- c("PETR4", "VALE3", "ITUB4", "BBDC4", "B3SA3",
#               "ABEV3", "LREN3", "CIEL3", "GGBR4", "WEGE3")


# Function to collect data ------------------------------------------------

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
df <- get_yearly_yield(tickers, from = "2017-01-01") %>% 
  mutate(date = as.integer(date))
tictoc::toc()

df %>% 
  write_csv(here("data", "yield_data.csv"),
            col_names = TRUE)
  
# df <-
#   read_csv("data/yield_data.csv",
#            col_names = c("symbol", "date", "stock_price", "dividend", "yield"))

high_pay_tickers <- df %>% 
  group_by(symbol) %>%
  filter(length(symbol) > 2) %>% 
  reframe(
    years_diff = last(date) - first(date),
    diff_yield = yield - lag(yield, n = years_diff)
  ) %>%  
  ungroup() %>% 
  drop_na() %>% 
  slice_max(diff_yield, n = 20) %>% 
  pull(symbol)

df %>%
  filter(symbol %in% high_pay_tickers) %>% 
  mutate(symbol = fct_reorder(symbol, -yield)) %>%  
  hchart(
    "line",
    hcaes(date, yield, group = symbol)
  )

gt_table <- df %>% 
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
  cols_width(
    symbol ~ pct(60)
  ) %>% 
  cols_label(
    symbol = "Ativo",
    first_yield = "Primeiro",
    last_yield = "Último",
    diff_yield = "Diferença"
  ) %>% 
  fmt_percent(
    columns = everything(),
    decimals = 2
  ) %>%
  data_color(
    columns = "diff_yield",
    method  = "numeric",
    palette = "YlGn"
  ) %>% 
  tab_spanner(
    label = md("**Dividendos**"),
    columns = c(first_yield, last_yield)
  ) %>% 
  tab_header(
    title = "Maiores pagadores de dividendos",
    subtitle = "Considerando apenas ativos com mais de 2 anos de amostra"
  ) %>% 
  tab_footnote("Fonte: B3 API")

gtsave(gt_table,
       filename = "maiores_yields.png")






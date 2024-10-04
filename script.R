library(tidyverse)
library(tidyquant)
library(rvest)
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
    map(
      tickers,
      .f = function(ticker){
        safe_tq_get(
          x = paste0(ticker, ".SA"),
          get = "dividends",
          from = from,
          to = to
        )
      }, .progress = TRUE
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
  
df <-
  read_csv("data/yield_data.csv")

high_pay_tickers <- df %>% 
  group_by(symbol) %>% 
  # Filtra os papéis com mais de 2 anos de amostra
  filter(length(symbol) > 2) %>% 
  ungroup() %>%
  # Filtra para a data maxima da amostra (2024)
  filter(date == max(date)) %>% 
  # Seleciona os tickers com maiores yields
  slice_max(yield, n = 20) %>% 
  # Extrai somente os nomes dos ticker em formato de vetor
  pull(symbol)


gt_table <- df %>% 
  filter(symbol %in% high_pay_tickers) %>% 
  group_by(symbol) %>% 
  summarize(
    first_yield = first(yield),
    last_yield = last(yield),
    diff_yield = last(yield) - first(yield),
    min_date = min(date),
    max_date = max(date),
    first_dividend = first(dividends),
    last_dividend = last(dividends)
  ) %>%  
  mutate(symbol = str_glue("{symbol} ({min_date}-{max_date})")) %>% 
  select(-c(min_date, max_date)) %>% 
  arrange(desc(diff_yield)) %>% 
  gt() %>% 
  cols_width(
    symbol ~ pct(30)
  ) %>% 
  cols_label(
    symbol = "Ativo",
    first_yield = "Primeiro Ano",
    last_yield = "Último Ano",
    diff_yield = "Variação"
  ) %>% 
  fmt_percent(
    columns = ends_with("_yield"),
    decimals = 2
  ) %>%
  fmt_currency(
    columns = ends_with("dividend"),
    currency = "BRL",
    decimals = 2,
    drop_trailing_dec_mark = TRUE
  ) %>% 
  cols_merge(
    columns = starts_with("first_"),
    pattern = "{2} ({1})"
  ) %>% 
  cols_merge(
    columns = starts_with("last_"),
    pattern = "{2} ({1})"
  ) %>% 
  data_color(
    columns = "diff_yield",
    method  = "numeric",
    palette = "YlGn"
  ) %>% 
  cols_align(align = "center") %>% 
  tab_spanner(
    label = md("**Dividendos por ação\n(Dividend Yield)**"),
    columns = c(first_yield, last_yield)
  ) %>% 
  tab_header(
    title = "Papéis da B3 que o rendimento mais cresceu (yield)",
    subtitle = "Considerando apenas ativos com mais de 2 anos de amostra"
  ) %>% 
  tab_footnote("Fonte: B3 API")

gtsave(gt_table,
       filename = "maiores_yields.png")

df %>%
  filter(symbol %in% high_pay_tickers[1:5]) %>% 
  mutate(symbol = fct_reorder(symbol, -yield)) %>% 
  ggplot(aes(date, yield, color = symbol)) +
  geom_line()

  
  
  
  
  
  
  
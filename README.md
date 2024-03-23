
<!-- README.md is generated from README.Rmd. Please edit that file -->

# yield_data

This project aims to collect and analyze data from B3 stock index.

``` r
data_sample <- read.csv("data/yield_data.csv",
                        col.names = c("Symbol", "Year", "Stock Price", "Dividends", "Yield")) 

knitr::kable(head(data_sample, 20), digits = 2)
```

| Symbol | Year | Stock.Price | Dividends | Yield |
|:-------|-----:|------------:|----------:|------:|
| ABCB4  | 2020 |       12.08 |      0.63 |  0.05 |
| ABCB4  | 2021 |       13.14 |      0.98 |  0.07 |
| ABCB4  | 2022 |       17.53 |      1.25 |  0.07 |
| ABCB4  | 2023 |       24.10 |      1.19 |  0.05 |
| ABCB4  | 2024 |       24.18 |      0.74 |  0.03 |
| ABEV3  | 2020 |       13.53 |      0.41 |  0.03 |
| ABEV3  | 2021 |       13.92 |      0.68 |  0.05 |
| ABEV3  | 2022 |       13.80 |      0.76 |  0.06 |
| ABEV3  | 2023 |       13.73 |      0.73 |  0.05 |
| AERI3  | 2022 |        1.13 |      0.02 |  0.02 |
| AESB3  | 2021 |       10.90 |      0.22 |  0.02 |
| AESB3  | 2022 |        9.66 |      0.11 |  0.01 |
| AGRO3  | 2020 |       17.21 |      0.71 |  0.04 |
| AGRO3  | 2021 |       21.80 |      2.62 |  0.12 |
| AGRO3  | 2022 |       26.77 |      5.25 |  0.20 |
| AGRO3  | 2023 |       26.71 |      3.21 |  0.12 |
| AGXY3  | 2022 |        9.80 |      0.17 |  0.02 |
| AGXY3  | 2023 |        3.63 |      0.04 |  0.01 |
| ALLD3  | 2021 |       13.54 |      0.76 |  0.06 |
| ALLD3  | 2022 |        5.25 |      0.92 |  0.18 |

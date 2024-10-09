library(nflreadr)
library(tidyverse)

df <- load_contracts() |> 
  filter(year_signed >= 2015) 


df|>
  filter(position != "QB") |> 
  arrange(-apy_cap_pct) |> 
  head(25) |> 
  view

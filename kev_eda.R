library(nflreadr)
library(tidyverse)

df <- load_contracts() |> 
  
  filter(year_signed >= 2015) |> 
  filter(!is.na(years))


# LOWER CLASS
a <- load_contracts() |> 
  filter(!is.na(years)) |> 
  group_by(otc_id) |> 
  filter(sum(guaranteed) < .1) |> 
  ungroup()|> 
  filter(year_signed >= 2015)





df |>
  filter(position != "QB") |> 
  arrange(-apy_cap_pct) |> 
  filter(player == "Nate Wiggins")


df_added <- df |> 
  mutate( has_guaranteed = case_when(
    guaranteed > 1 ~ "Lots",
    guaranteed <= 1 & guaranteed > 0 ~ "Some",
    guaranteed == 0 ~ "None"
  ))


to_remove <- df_added |> group_by(otc_id) %>% filter(
           sum(years) < 3
         ) |> 
  summarize(player_name = player[1],total_years = sum(years))


df_added |> 
  group_by(has_guaranteed) |> 
  summarize(sum_years = sum(years))


# Mailroom ----------------------------------------------------------------

library(nflreadr)
library(tidyclust)
library(tidyverse)
library(tidymodels)
set.seed(11042004)
library(wesanderson)
# Data stuff --------------------------------------------------------------

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





# setup for kmeans --------------------------------------------------------
kmeans_df <- df |> 
  drop_na(c(guaranteed, apy)) |> 
  select(player,year_signed, apy, guaranteed)



# shuffle rows
kmeans_df <- kmeans_df |> 
  slice_sample(n = nrow(kmeans_df))
# WSS ---------------------------------------------------------------------

n <- 10
wss <- numeric(n)

for (i in 1:n) {
  # Fit the model: km.out
  kmeans_spec <- k_means(num_clusters = i)
  
  kmeans_fit <- kmeans_spec |> 
    fit(~ apy + guaranteed,
        data = kmeans_df
    )
  kmeans_summary <- kmeans_fit |>
    extract_fit_summary()
  # Save the within cluster sum of squares
  wss[i] <- sum(kmeans_summary$sse_within_total_total)
}



wss_df <- tibble(clusters = 1:n, wss = wss)

scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab('Number of clusters') +
  geom_hline(
    yintercept = wss, 
    linetype = 'dashed', 
    col = "black"
  )
scree_plot

# Final + plot ------------------------------------------------------------

kmeans_spec <- k_means(num_clusters = 3)

kmeans_fit <- kmeans_spec |> 
  fit(~ apy + guaranteed,
      data = kmeans_df
  )


final <- kmeans_fit |> 
  augment(kmeans_df)

final |> 
  ggplot(aes(apy, guaranteed, colour = .pred_cluster, shape = .pred_cluster)) +
  geom_point() 

# plot ------------------------------------------------------------


plot_2015 <- final |> 
  filter(year_signed %in% c(2015,2016,2017, 2022,2023,2024)) |> 
  mutate(
    Time = case_when(
      year_signed %in% c(2015,2016,2017) ~ "Past 2015-2017",
      year_signed %in% c(2022,2023,2024) ~ "Current 2022-2024",
    )
  )


plot_2015 |> 
  ggplot(aes(fill = Time, x = .pred_cluster, y = after_stat(prop), group = fct_rev(Time), label = round(after_stat(prop),3))) +
  geom_bar(position = "dodge") +
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6))+
  scale_fill_manual(values=wesanderson::wes_palette(n=2, name="GrandBudapest1"))



# Mailroom ----------------------------------------------------------------
set.seed(11042004)

library(nflreadr)
library(tidyclust)
library(tidyverse)
library(tidymodels) 
library(wesanderson)
library(gt)
library(ggthemes)
library(patchwork)
library(gtExtras)

# Data stuff --------------------------------------------------------------

verse <- load_players()

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
  drop_na(c(guaranteed, apy_cap_pct)) |> 
  select(gsis_id, player,year_signed, apy,years, guaranteed, is_active, inflated_apy, inflated_guaranteed)



# shuffle rows
kmeans_df <- kmeans_df |> 
  arrange(inflated_apy)
# WSS ---------------------------------------------------------------------

n <- 10
wss <- numeric(n)

for (i in 1:n) {
  # Fit the model: km.out
  kmeans_spec <- k_means(num_clusters = i)
  
  kmeans_fit <- kmeans_spec |> 
    fit(~ inflated_apy + inflated_guaranteed,
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

kmeans_spec <- k_means(num_clusters = 4)

kmeans_fit <- kmeans_spec |> 
  fit(~ inflated_apy + inflated_guaranteed,
      data = kmeans_df
  )


final <- kmeans_fit |> 
  augment(kmeans_df) |> 
  mutate(added = inflated_apy + inflated_guaranteed)|> 
  filter(!(added > 15 & inflated_guaranteed == 0))

final |> 
  ggplot(aes(inflated_apy, inflated_guaranteed, colour = .pred_cluster, shape = .pred_cluster)) +
  geom_point(alpha = 0.7) 

# plot ------------------------------------------------------------


working <- final |> 
  filter(year_signed %in% c(2015,2016,2017, 2022,2023,2024)) |> 
  mutate(
    Time = case_when(
      year_signed %in% c(2015,2016,2017) ~ "Past 2015-2017",
      year_signed %in% c(2022,2023,2024) ~ "Current 2022-2024",
    )
  ) |>   # These players were briefly on the franchise tag but guaranteed money is wrong
  arrange(-inflated_apy) |> 
  distinct(gsis_id, Time, .keep_all=TRUE)
  
working |> 
  ggplot(aes(fill = Time, x = .pred_cluster, y = after_stat(prop*100), 
             group = fct_rev(Time), 
             label = paste0(round(after_stat(prop*100),1),"%"))) +
  geom_bar(position = "dodge") +
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) +
  ylab("Percentage of players") +
  xlab("Clusters")+
  theme(
    legend.position = "inside", legend.position.inside = c(0.9, 0.8))+
  scale_fill_manual(values=wesanderson::wes_palette(n=2, name="GrandBudapest1"), drop = FALSE,  
                    guide = guide_legend(reverse = TRUE))+
  theme_clean()

era1players <- working |> 
  filter(Time == "Past 2015-2017")

presentplayers <- working |> 
  filter(Time == "Current 2022-2024") 

cluster1players <- working |> 
  filter(.pred_cluster == "Cluster_1") 
  
  
cluster2players <- working |> 
  filter(.pred_cluster == "Cluster_2") 

cluster3players <- working |> 
  filter(.pred_cluster == "Cluster_3") 

cluster4players <- working |> 
  filter(.pred_cluster == "Cluster_4") 


pal <- wes_palette(2, name = "GrandBudapest1")
pal <- unclass(pal)



set.seed(11042004)

c4 <- cluster4players |> 
  select(year_signed, player, inflated_apy, inflated_guaranteed) |>
  slice_sample(n = 10) |> 
  mutate(inflated_apy = round(inflated_apy, 2),inflated_guaranteed= round(inflated_guaranteed, 2)) |> 
  gt() |> 
  tab_style(
    style = cell_fill(color = pal[1]),
    locations = cells_body(
      columns = year_signed,
      rows = year_signed >= 2022
    )) |> 
  tab_style(
    style = cell_fill(color = pal[2]),
    locations = cells_body(
      columns = year_signed,
      rows = year_signed < 2022
    ))|> 
  tab_header(
    title = md("Sample of **Cluster 4**"),
    subtitle = md("random selection of 10")
  )


c3 <- cluster3players |> 
  select(year_signed, player, inflated_apy, inflated_guaranteed) |>
  slice_sample(n = 10) |> 
  mutate(inflated_apy = round(inflated_apy, 2),inflated_guaranteed= round(inflated_guaranteed, 2)) |> 
  gt() |> 
  tab_style(
    style = cell_fill(color = pal[1]),
    locations = cells_body(
      columns = year_signed,
      rows = year_signed >= 2022
    )) |> 
  tab_style(
    style = cell_fill(color = pal[2]),
    locations = cells_body(
      columns = year_signed,
      rows = year_signed < 2022
    ))|> 
  tab_header(
    title = md("Sample of **Cluster 3**"),
    subtitle = md("random selection of 10")
  )


c2 <- cluster2players |> 
  select(year_signed, player, inflated_apy, inflated_guaranteed) |>
  slice_sample(n = 10) |> 
  mutate(inflated_apy = round(inflated_apy, 2),inflated_guaranteed= round(inflated_guaranteed, 2)) |> 
  gt() |> 
  tab_style(
    style = cell_fill(color = pal[1]),
    locations = cells_body(
      columns = year_signed,
      rows = year_signed >= 2022
    )) |> 
  tab_style(
    style = cell_fill(color = pal[2]),
    locations = cells_body(
      columns = year_signed,
      rows = year_signed < 2022
    )) |> 
  tab_header(
    title = md("Sample of **Cluster 2**"),
    subtitle = md("random selection of 10")
  )

c1 <- cluster1players |> 
  select(year_signed, player, inflated_apy, inflated_guaranteed) |>
  slice_sample(n = 10) |> 
  mutate(inflated_apy = round(inflated_apy, 2),inflated_guaranteed= round(inflated_guaranteed, 2)) |> 
  gt() |> 
  tab_style(
    style = cell_fill(color = pal[1]),
    locations = cells_body(
      columns = year_signed,
      rows = year_signed >= 2022
    )) |> 
  tab_style(
    style = cell_fill(color = pal[2]),
    locations = cells_body(
      columns = year_signed,
      rows = year_signed < 2022
    )) |> 
  tab_header(
    title = md("Sample of **Cluster 1**"),
    subtitle = md("random selection of 10")
  )

Breakdown <- working |> 
  group_by(.pred_cluster, Time) |> 
  summarize(mean_apy = round(mean(inflated_apy),2), 
            mean_guaranteed = round(mean(inflated_guaranteed),2),
            median_apy = round(median(inflated_apy),2), 
            median_guaranteed = round(median(inflated_guaranteed),2)) |> 
  gt() |> 
  tab_style(
    style = cell_fill(color = pal[1]),
    locations = cells_body(
      columns = Time,
      rows = Time == "Current 2022-2024"
    )) |> 
  tab_style(
    style = cell_fill(color = pal[2]),
    locations = cells_body(
      columns = Time,
      rows = Time != "Current 2022-2024"
    )) |> 
  tab_header(
    title = md("Breakdown of Clusters")
  ) |> 
  gt_theme_538() |> 
  gt_add_divider(columns = "mean_guaranteed") |> 
  cols_align("center")

c1

gt_two_column_layout(list(c1, c2))
gt_two_column_layout(list(c3, c4))

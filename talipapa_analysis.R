# Tayo na sa talipapa data analysis --------------------------------------------
 
## Clear workspace and perform garbage collection ----
rm(list = ls())
gc(full = TRUE)

## Set working directory ----
setwd("/home/ernestguevarra/Documents/GitHub/tayo-na-sa-talipapa")

## Load libraries/dependencies ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggbump)
library(ggrepel)
library(oxthema)

## Read talipapa scores data ----
talipapa_scores <- read.csv("data/scores.csv")

## Summary metrics ----

talipapa_scores_processsed <- talipapa_scores |>
  mutate(
    ### Get per round ranking ----
    across(
      .cols = baseline:round3,
      .fns = \(x) rank(x, ties.method = "min"),
      .names = "{.col}_rank"
    ),
    ### Get change between rounds ----
    round1_change = round1 - baseline,
    round2_change = round2 - round1,
    round3_change = round3 - round2,
    ### Get net change over 3 rounds ----
    net_change = round1_change + round2_change + round3_change,
    ### net change as a proportion of baseline score ----
    net_prop = net_change / baseline
  )

### Save summary metrics table ----
write.csv(
  talipapa_scores_processsed, "data/talipapa_scores_metrics.csv", 
  row.names = FALSE
)


## Summary plots ----

### Create long format talipapa dataset for ggplot ----
talipapa_scores_long <- talipapa_scores |>
  pivot_longer(
    cols = baseline:round3, names_to = "game_round", values_to = "score"
  ) |>
  left_join(
    talipapa_scores |>
      mutate(
        #### Get change between rounds ----
        baseline_change = NA,
        round1_change = round1 - baseline,
        round2_change = round2 - round1,
        round3_change = round3 - round2
      ) |>
      select(
        id, year, baseline_change, round1_change, round2_change, round3_change
      ) |>
      pivot_longer(
        cols = baseline_change:round3_change, names_to = "game_round", 
        values_to = "change"
      ) |>
      mutate(
        game_round = gsub(pattern = "_change", replacement = "", x = game_round)
      ),
    by = c("id", "year", "game_round")
  ) |>
  group_by(game_round) |>
  #### Rank based on negative value so that rank 1 is highest ----
  mutate(
    rank_score = rank(-score, ties.method = "min"),
    rank_change = rank(-change, ties.method = "min")
  ) |>
  arrange(id, game_round) |>
  ungroup() |>
  mutate(
    game_round = case_when(
      game_round == "baseline" ~ 0L,
      game_round == "round1" ~ 1L,
      game_round == "round2" ~ 2L,
      game_round == "round3" ~ 3L
    )
  )


### Plot bump chart of per round score ranking ----

talipapa_scores_long |>
  ggplot(mapping = aes(x = game_round, y = rank_score, colour = id)) +
  geom_point(size = 5, show.legend = FALSE) +
  geom_bump(linewidth = 3, smooth = 8, show.legend = FALSE) +
  geom_text_repel(
    data = talipapa_scores_long |> filter(game_round == 3L),
    mapping = aes(x = game_round, label = id), size = 6, vjust = 2,
    show.legend = FALSE, max.overlaps = 20
  ) +
  theme_oxford(base_size = 12, grid = FALSE) +
  theme(legend.position = NULL) +
  labs(
    title = "Rank of participants for each round based on total points scored",
    subtitle = "Tayo na sa talipapa 2025",
    caption = "Participants with same total points scored are given the same rank",
    x = "Round", y = "Rank"
  ) +
  scale_y_reverse(breaks = seq(from = 1, to = 12, by = 1)) +
  scale_color_manual(
    values = oxford_theme_palettes()[c(5, 4, 3)] |> 
      unlist() |>
      setNames(nm = NULL)
  )

#### Save plot ----
ggsave(
  filename = "outputs/bump_chart_per_round_score_rank.png",
  width = 8, height = 10, units = "in"
)

### Plot bump chart of per round change in points scored ranking ----

talipapa_scores_long |>
  filter(game_round != 0L) |>
  ggplot(mapping = aes(x = game_round, y = rank_change, colour = id)) +
  geom_point(size = 5, show.legend = FALSE) +
  geom_bump(linewidth = 3, smooth = 8, show.legend = FALSE) +
  geom_text_repel(
    data = talipapa_scores_long |> filter(game_round == 3L),
    mapping = aes(x = game_round, label = id), size = 6, vjust = 2,
    max.overlaps = 20, show.legend = FALSE
  ) +
  theme_oxford(base_size = 12, grid = FALSE) +
  theme(legend.position = "top") +
  labs(
    title = "Rank of participants for per round change in total points scored",
    subtitle = "Tayo na sa talipapa 2025",
    caption = "Participants with same change in points scored are given the same rank",
    x = "Round", y = "Rank"
  ) +
  scale_x_continuous(breaks = seq(from = 1, to = 3, by = 1)) +
  scale_y_reverse(breaks = seq(from = 1, to = 12, by = 1)) +
  scale_color_manual(
    values = oxford_theme_palettes()[c(5, 4, 3)] |> 
      unlist() |>
      setNames(nm = NULL)
  )

#### Save plot ----
ggsave(
  filename = "outputs/bump_chart_per_round_change_rank.png",
  width = 8, height = 10, units = "in"
)


### Plot per round points scored

talipapa_scores_long |>
  ggplot(mapping = aes(x = game_round, y = score, group = id)) +
  geom_point(size = 2, colour = get_oxford_colour("sky")) +
  geom_line(linewidth = 1, colour = get_oxford_colour("sky")) +
  labs(
    title = "Total points scored per round",
    subtitle = "Tayo na sa talipapa 2025",
    x = "Round", y = "Score"
  ) +
  scale_y_continuous(
    breaks = seq(from = 0, to = 400, by = 100), limits = c(0, 400)
  ) +
  facet_wrap(. ~ id, ncol = 4) +
  theme_oxford(
    base_size = 12, grid = "Yy", strip_text_size = 14, strip_text_face = "bold"
  ) +
  theme(panel.grid = element_line(linewidth = 1))

#### Save plot ----
ggsave(
  filename = "outputs/line_chart_per_round_score.png",
  width = 8, height = 10, units = "in"
)


### Plot per round change in points scored

talipapa_scores_processsed |>
  select(id, round1_change, round2_change, round3_change) |>
  pivot_longer(
    cols = round1_change:round3_change, 
    names_to = "game_round", values_to = "change"
  ) |>
  mutate(
    game_round = gsub("_change", replacement = "", x = game_round) |>
      gsub("round", replacement = "", x = _) |>
      as.integer()
  ) |>
  ggplot(mapping = aes(x = game_round, y = change, group = id)) +
  geom_point(size = 2, colour = get_oxford_colour("sky")) +
  geom_line(linewidth = 1, colour = get_oxford_colour("sky")) +
  labs(
    title = "Net change in total points scored per round",
    subtitle = "Tayo na sa talipapa 2025",
    x = "Round", y = "Net change"
  ) +
  scale_x_continuous(breaks = c(1, 2, 3)) +
  scale_y_continuous(
    breaks = seq(from = -180, to = 180, by = 60), limits = c(-180, 180)
  ) +
  facet_wrap(. ~ id, ncol = 4) +
  theme_oxford(
    base_size = 12, 
    grid = "Yy",  
    strip_text_size = 14, strip_text_face = "bold"
  ) +
  theme(panel.grid = element_line(linewidth = 1))

#### Save plot ----
ggsave(
  filename = "outputs/line_chart_per_round_score_change.png",
  width = 8, height = 10, units = "in"
)

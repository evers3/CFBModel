library(tidyverse)
library(ggrepel)
library(cfbfastR)
library(usethis)
library(dplyr)
library(formattable)
library(Matrix)
library(ggimage)
Sys.setenv(CFBD_API_KEY = "zbq1vCPX7e7QzatDoIKgwG3RA+JXsYIj25zrILchfUeE7i7tciMF9yz26da20zFX")


## Loading in the 2023 season data
pbp_2023 <-load_cfb_pbp(
  seasons = 2023,
  dbConnection = NULL,
  tablename = NULL
)

## Loading in the FBS teams
FbsTeams <- cfbd_team_info(year = 2023, only_fbs = TRUE)


## Looking at the col names
colnames(pbp_2023)


## Grouping by offense team, filtering out garbage time(WP < .05)
pbpByTeam <- pbp_2023 |> 
  group_by(pos_team) |> 
  filter(!is.na(EPA)) |>
  filter(wp_before > .05) |> 
  filter(pos_team %in% FbsTeams$school & def_pos_team %in% FbsTeams$school ) |> 
  summarize(epa = sum(EPA),
            plays = n(),
            epa_play = epa / plays) |> 
  filter(plays > 100)

## Looking at Offensive Neutral situation EPA/Play (1st and second down, between 4 and 12 yards to go)
ONeutralSituationEPAPlay <- pbp_2023 |> 
  group_by(pos_team) |> 
  filter(!is.na(EPA)) |>
  filter(wp_before > .05) |> 
  filter(down == 1 | down == 2) |>
  filter(distance >= 4 | distance <= 12) |> 
  filter(pos_team %in% FbsTeams$school & def_pos_team %in% FbsTeams$school ) |>
  summarize(epa = sum(EPA),
            plays = n(),
            epa_play = epa / plays)|> 
  filter(plays > 70)

## Defensive EPA in Neutral Situations
DNeutralSituationEPAPlay <- pbp_2023 |> 
  group_by(def_pos_team) |> 
  filter(!is.na(EPA)) |>
  filter(wp_before > .05) |> 
  filter(down == 1 | down == 2) |>
  filter(distance >= 4 | distance <= 12) |> 
  filter(pos_team %in% FbsTeams$school & def_pos_team %in% FbsTeams$school ) |>
  summarize(epa = sum(EPA),
            plays = n(),
            epa_play = epa / plays)|> 
  filter(plays > 70)

## Merging the Data Sets
NeutalSituationEPAPLay <- left_join(ONeutralSituationEPAPlay, DNeutralSituationEPAPlay, by = c('pos_team' = 'def_pos_team'))

## Importing the Logos and colors for FBS teams
NeutalSituationEPAPLay <- left_join(NeutalSituationEPAPLay, FbsTeams, by = c('pos_team' = 'school'))


NeutalSituationEPAPLay <-  NeutalSituationEPAPLay|> 
  rename("OEPA" ="epa.x",
         "OPlays" = "plays.x", 
         "OEPA_Play" ="epa_play.x",
         "DEPA" = "epa.y",
         "DPlays" = "plays.y",
         "DEPA_Play" = "epa_play.y")

NeutalSituationEPAPLay |> 
ggplot(aes(x = OEPA_Play, y = DEPA_Play)) + 
  geom_image(aes(image= logo), asp = 16/9, size= 0.05)+
  scale_color_identity(aesthetics = c("fill", "color"))+ 
  theme_bw()+
  geom_hline(yintercept = mean(NeutalSituationEPAPLay$OEPA_Play), linetype = "dashed")+
  geom_vline(xintercept = mean(NeutalSituationEPAPLay$DEPA_Play), linetype = "dashed")+
  labs(
    x = "Offense Neutral Situation EPA/Play",
    y = "Defense Neutral Situation EPA/Play",
    title = "Offense Neutral Situation EPA/Play vs 
    Defense Neutral Situation EPA/Play for the 2023 Season",
    subtitle = "Neutral Situation is 1st or 2nd down, 
    between 4 and 12 yards to go")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8))+
  scale_y_reverse()+
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 12, hjust = 0.5))

NeutalSituationEPAPLay <- NeutalSituationEPAPLay |> 
  mutate(EPADiff = OEPA_Play - DEPA_Play)


epaPerGame <- pbp_2023 |> 
  group_by(game_id) |> 
  filter(!is.na(EPA)) |>
  filter(wp_before > .05) |>
  summarise(homeEpa = sum(home_EPA),
            roadEpa = sum(road_EPA))
  
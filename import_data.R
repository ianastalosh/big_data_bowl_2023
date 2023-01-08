
# Import packages
library(tidyverse) # dplyr, ggplot
library(zoo) # For rolling functions, ie. na.locf
library(gganimate) # Animated plots
library(xgboost) # Model training
library(magick) # Animated plots
library(plotly) # Interactive plots 
library(xgboost) # model training
library(pROC) # Get ROC AUC
library(ModelMetrics) # Get Brier Score
library(kableExtra) # Nice tables
library(reshape2) # To get the melt function

set.seed(335)

# Import relevant scripts
source('utils.R')
source('feature_engineering.R')

# Import context csv data
games = read_csv('data/games.csv')

pff_raw = read_csv('data/pffScoutingData.csv')
pff = feature_engineer_pff(pff_raw)

players = read_csv('data/players.csv')
plays = read_csv('data/plays.csv') %>%
  mutate(game_play_id = paste(gameId, playId, sep='-'))

# Import pbp
tracking_files = c('data/week1.csv',
                    'data/week2.csv',
                    'data/week3.csv',
                    'data/week4.csv',
                    'data/week5.csv',
                    'data/week6.csv',
                    'data/week7.csv',
                    'data/week8.csv')

DATA_SUBSET = FALSE
if (DATA_SUBSET) {
  tracking_files = tracking_files[1]
}

tracking_data = map(tracking_files, read_csv) %>% bind_rows()

# Feature engineer
fe_tracking = feature_engineer_tracking_data(tracking_data)
rm(tracking_data)

preprocessing_summary = fe_tracking %>% 
  group_by(game_play_id) %>% 
  summarize(rows = n(), 
            num_during = sum(occurs_during_pass_rush))

# Join jersey numbers to the players df (some players have worn two numbers - we just include one for simplicity sake)
unique_jersey_numbers = fe_tracking %>% select(nflId, jerseyNumber) %>% distinct()
players = players %>% left_join(unique_jersey_numbers, by='nflId') %>% group_by(nflId) %>% filter(row_number() == 1)

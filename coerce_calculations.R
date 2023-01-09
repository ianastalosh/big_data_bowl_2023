
OUTPUT_DIRECTORY = 'output/plots/coerce_plots'
TEAM_LEVEL_PROB_FILEPATH = 'output/models/team_level_hhs_per_frame_20230107_224414/total_probs.csv'
RUSHER_LEVEL_PROB_FILEPATH = 'output/models/rusher_level_hhs_per_frame_20230108_131603/total_probs.csv'

dir.create(OUTPUT_DIRECTORY)

# Creation of early pressure metrics
raw_team_level_pressure_probs = read_csv(TEAM_LEVEL_PROB_FILEPATH)
raw_rusher_level_pressure_probs = read_csv(RUSHER_LEVEL_PROB_FILEPATH)

# Join play metadata to probability dfs 
play_metadata = plays %>% 
  mutate(yardline_100 = ifelse(possessionTeam == yardlineSide, 100 - yardlineNumber, yardlineNumber)) %>% 
  select(game_play_id, quarter, down, yardsToGo, yardline_100, offense = possessionTeam, defense = defensiveTeam, 
         passResult, preSnapHomeScore, preSnapVisitorScore, playResult, penaltyYards, foulName1, 
         offenseFormation, personnelO, defendersInBox, personnelD, dropBackType, pff_passCoverage, pff_passCoverage, pff_passCoverageType)

team_level_pressure_probs = play_metadata %>%
  left_join(raw_team_level_pressure_probs, by='game_play_id') %>%
  group_by(game_play_id) %>% 
  filter(!is.na(frame_id)) %>% 
  mutate(adjusted_frame = frame_id - min(frame_id) + 1,  # Rescale so the 1st frame after snap is now '1'
         seconds_after_snap = adjusted_frame / 10,
         frame_weight = 1 / (1 + (adjusted_frame / 11.13)^2.14))

rusher_level_pressure_probs = play_metadata %>%
  left_join(raw_rusher_level_pressure_probs, by='game_play_id') %>% 
  left_join(players, by=c('rush_id' = 'nflId')) %>%
  group_by(game_play_id) %>% 
  filter(!is.na(frame_id)) %>% 
  mutate(adjusted_frame = frame_id - min(frame_id) + 1, # Rescale so the 1st frame after snap is now '1'
         seconds_after_snap = adjusted_frame / 10,
         frame_weight = 1 / (1 + (adjusted_frame / 11.13)^2.14)) 

# TEAM LEVEL

# Calculate average per frame across all teams 
frame_average = team_level_pressure_probs %>% 
  group_by(seconds_after_snap) %>% 
  summarize(num_plays = n(),
            avg_prob = mean(prob),
            max_prob = max(prob),
            min_prob = min(prob))

pressure_by_frame_plot = ggplot(frame_average, aes(x = seconds_after_snap, y=avg_prob)) + 
  geom_point(aes(size = num_plays), color = 'darkorchid3', alpha = 0.3) + 
  # geom_point(data=team_level_pressure_probs, aes(x=seconds_after_snap, y=prob), colour='grey', alpha=0.01) +
  theme_minimal() + 
  labs(title = 'Avg. QB Pressure As Play Progresses',
       x = 'Seconds After Snap',
       y = 'Avg. Prob of Hit, Hurry or Sack on Play',
       size = 'Number of Plays')

ggsave(paste0(OUTPUT_DIRECTORY, '/avg_pressure_over_time.png'), pressure_by_frame_plot, height=6, width=8, bg='white')

## COMPUTATION OF COERCE ## 
# Compute weighted average by team
coerce_by_defense = team_level_pressure_probs %>%
  group_by(defense) %>%
  summarize(unique_plays = length(unique(game_play_id)), 
            average_prob = mean(prob),
            coerce = sum(prob * frame_weight) / sum(frame_weight)) %>%
  arrange(desc(average_prob)) %>%
  mutate(avg_ranking = row_number()) %>%
  arrange(desc(coerce)) %>%
  mutate(coerce_ranking = row_number())


coerce_by_offense = team_level_pressure_probs %>%
  group_by(offense) %>%
  summarize(unique_plays = length(unique(game_play_id)), 
            average_prob = mean(prob),
            coerce = sum(prob * frame_weight) / sum(frame_weight)) %>%
  arrange(average_prob) %>%
  mutate(avg_ranking = row_number()) %>%
  arrange(coerce) %>%
  mutate(coerce_ranking = row_number())

coerce_by_pass_rusher = rusher_level_pressure_probs %>%
  group_by(rush_id, displayName, team, officialPosition) %>%
  summarize(unique_plays = length(unique(game_play_id)), 
            time_spent_on_pass_rush = n()/0.1, 
            average_prob = mean(prob),
            coerce = sum(prob * frame_weight) / sum(frame_weight)) %>%
  filter(unique_plays >= 10) %>%
  group_by(officialPosition) %>%
  arrange(desc(average_prob)) %>%
  mutate(avg_ranking_for_position = row_number()) %>%
  arrange(desc(coerce)) %>%
  mutate(coerce_ranking_for_position = row_number())

top_5_per_position = coerce_by_pass_rusher %>%
  filter(coerce_ranking_for_position %in% 1:5,
         officialPosition %in% c('DE', 'DT', 'NT', 'ILB', 'MLB', 'OLB')) %>%
  arrange(officialPosition, desc(coerce))

# Create kable plots and save
COERCE_DEFENSE_RANKINGS_FILEPATH = paste0(OUTPUT_DIRECTORY, '/coerce_defense_rankings.png')
COERCE_OFFENSE_RANKINGS_FILEPATH = paste0(OUTPUT_DIRECTORY, '/coerce_offense_allowed_rankings.png')
COERCE_PASS_RUSHER_RANKINGS_FILEPATH = paste0(OUTPUT_DIRECTORY, '/coerce_pass_rusher_rankings.png')

defense_coerce_kable = coerce_by_defense %>%
  select(`Defense` = defense,
         `Unique Plays` = unique_plays,
         `Avg. Pressure` = average_prob,
         `Avg. Ranking` = avg_ranking,
         `COERCE` = coerce,
         `COERCE Ranking` = coerce_ranking) %>%
  kbl(caption = 'COERCE Ratings by Defensive Unit') %>%
  kable_material(c('striped')) %>%
  column_spec(5, color = 'white', background = spec_color(coerce_by_defense$coerce, end = 0.7, option='C')) %>% 
  column_spec(6, color = 'white', background = spec_color(coerce_by_defense$coerce_ranking, end = 0.7, option='C', direction=-1)) 

save_kable(defense_coerce_kable, COERCE_DEFENSE_RANKINGS_FILEPATH, zoom = 2)

offense_coerce_kable = coerce_by_offense %>%
  select(`Offense` = offense,
         `Unique Plays` = unique_plays,
         `Avg. Pressure Allowed` = average_prob,
         `Avg. Ranking` = avg_ranking,
         `COERCE Allowed` = coerce,
         `COERCE Ranking` = coerce_ranking) %>%
  kbl(caption = 'COERCE Ratings by Offensive Line (ie. which offenses allow early pressure)') %>%
  kable_material(c('striped')) %>%
  column_spec(5, color = 'white', background = spec_color(coerce_by_offense$coerce, end = 0.7, option='C', direction = -1)) %>% 
  column_spec(6, color = 'white', background = spec_color(coerce_by_offense$coerce_ranking, end = 0.7, option='C', direction=-1)) 

save_kable(offense_coerce_kable, COERCE_OFFENSE_RANKINGS_FILEPATH, zoom = 2)

pass_rusher_coerce_kable = top_5_per_position %>%
  select(`Player` = displayName,
         `Team` = team,
         `Position` = officialPosition,
         `Unique Plays` = unique_plays,
         `Avg. Pressure` = average_prob,
         `Avg. Ranking in Position` = avg_ranking_for_position,
         `COERCE` = coerce,
         `COERCE Ranking in Position` = coerce_ranking_for_position) %>%
  kbl(caption = 'COERCE Ratings by Individual Pass Rusher, per Position (min 10 plays)') %>%
  kable_material(c('striped')) %>%
  column_spec(7, color = 'white', background = spec_color(top_5_per_position$coerce, end = 0.7, option='C', direction = 1)) %>% 
  column_spec(8, color = 'white', background = spec_color(top_5_per_position$coerce_ranking_for_position, end = 0.7, option='C', direction=-1)) 

save_kable(pass_rusher_coerce_kable, COERCE_PASS_RUSHER_RANKINGS_FILEPATH, zoom = 2)

# COERCE on play
sample_play_team_coerce = team_level_pressure_probs %>%
  filter(game_play_id == SAMPLE_PLAY) %>% 
  group_by(game_play_id, defense) %>%
  summarize(unique_plays = length(unique(game_play_id)), 
            average_prob = mean(prob),
            coerce = sum(prob * frame_weight) / sum(frame_weight))

sample_play_rusher_coerce = rusher_level_pressure_probs %>%
  filter(game_play_id == SAMPLE_PLAY) %>% 
  group_by(game_play_id, rush_id, jerseyNumber) %>%
  summarize(unique_plays = length(unique(game_play_id)), 
            average_prob = mean(prob),
            coerce = sum(prob * frame_weight) / sum(frame_weight))

# Compute COERCE for all plays
all_plays_team_coerce = team_level_pressure_probs %>%
  group_by(game_play_id, defense, hit_hurry_or_sack) %>%
  summarize(unique_plays = length(unique(game_play_id)), 
            average_prob = mean(prob),
            coerce = sum(prob * frame_weight) / sum(frame_weight))

all_plays_rusher_coerce = rusher_level_pressure_probs %>%
  group_by(game_play_id, rush_id, jerseyNumber, hit_hurry_or_sack) %>%
  summarize(unique_plays = length(unique(game_play_id)), 
            average_prob = mean(prob),
            coerce = sum(prob * frame_weight) / sum(frame_weight))

write.csv(all_plays_team_coerce, paste0(OUTPUT_DIRECTORY, '/play_team_coerce.csv'), row.names=FALSE)
write.csv(all_plays_rusher_coerce, paste0(OUTPUT_DIRECTORY, '/play_rusher_coerce.csv'), row.names=FALSE)

# Output example plays and coerce calculations
SAMPLE_PLAY1 = '2021091203-601'
sample_play_art1 = generate_play_art_from_game_play_id(SAMPLE_PLAY1, raw_team_level_pressure_probs, raw_rusher_level_pressure_probs, 'output/plots/play-2021091203-601-burrow-pressure.gif')
team_coerce_on_play1 = all_plays_team_coerce %>% filter(game_play_id == SAMPLE_PLAY1)
rusher_coerce_on_play1 = all_plays_rusher_coerce %>% filter(game_play_id == SAMPLE_PLAY1)

write.csv(team_coerce_on_play1, paste0(OUTPUT_DIRECTORY, '/play-', SAMPLE_PLAY1, '-team-level-coerce.csv'), row.names = FALSE)
write.csv(rusher_coerce_on_play1, paste0(OUTPUT_DIRECTORY, '/play-', SAMPLE_PLAY1, '-rusher-level-coerce.csv'), row.names = FALSE)

SAMPLE_PLAY2 = '2021090900-2279'
sample_play_art2 = generate_play_art_from_game_play_id(SAMPLE_PLAY2, raw_team_level_pressure_probs, raw_rusher_level_pressure_probs, 'output/plots/play-2021090900-2279-dak-late-pressure.gif')
team_coerce_on_play2 = all_plays_team_coerce %>% filter(game_play_id == SAMPLE_PLAY2)
rusher_coerce_on_play2 = all_plays_rusher_coerce %>% filter(game_play_id == SAMPLE_PLAY2)

SAMPLE_PLAY3 = '2021091212-2857'
sample_play_art3 = generate_play_art_from_game_play_id(SAMPLE_PLAY3, raw_team_level_pressure_probs, raw_rusher_level_pressure_probs, 'output/plots/play-2021091212-2857-daniel-jones-central-pressure.gif')
team_coerce_on_play3 = all_plays_team_coerce %>% filter(game_play_id == SAMPLE_PLAY3)
rusher_coerce_on_play3 = all_plays_rusher_coerce %>% filter(game_play_id == SAMPLE_PLAY3)

write.csv(team_coerce_on_play3, paste0(OUTPUT_DIRECTORY, '/play-', SAMPLE_PLAY3, '-team-level-coerce.csv'), row.names = FALSE)
write.csv(rusher_coerce_on_play3, paste0(OUTPUT_DIRECTORY, '/play-', SAMPLE_PLAY3, '-rusher-level-coerce.csv'), row.names = FALSE)


# Preprocessing for pass rusher sack probability

#' For each frame:
#' Identify the pass rushers
#' Compute the difference between the given pass rusher and every other player
#' Sort by the distance and order as follows: QB | OLINE | DLINE |. 
#' Then, join to a different data frame to keep only the 5 closest blockers and 5 closest pass rushers to the pass rusher in question
#' (if there are less than 5, fill with 0s)

start = Sys.time()
NUM_PLAYERS_TO_CONSIDER = 5

print("--- CREATING FEATURES ----")

# Firstly, get only pass rushers
all_pass_rushers = fe_tracking %>% 
  filter(pff_role %in% DEFENSE_POSITIONS) %>% 
  select(game_play_id, frameId, 
         rush_id = nflId, rush_x = xNorm, rush_y = yNorm, rush_dir = dirNorm, rush_o = oNorm, rush_s = s, rush_a = a)

# For each play, we want to join every pass rusher to every other row 
tracking_with_rusher_joined = fe_tracking %>%
  filter(team != 'football') %>%
  filter(occurs_during_pass_rush == 1) %>%
  left_join(all_pass_rushers, by=c('game_play_id', 'frameId')) %>%
  filter(nflId != rush_id) %>%
  mutate(dist_to_rusher = sqrt((xNorm - rush_x)^2 + (yNorm - rush_y)^2),
         x_diff = xNorm - rush_x,
         y_diff = xNorm - rush_y,
         o_diff = oNorm - rush_o,
         dir_diff = dirNorm - rush_dir,
         a_diff = a - rush_a,
         s_diff = s - rush_s) %>%
  select(game_play_id, frameId, rush_id, pff_role,
         dist_to_rusher, x_diff, y_diff, o_diff, dir_diff, a_diff, s_diff) %>%
  arrange(game_play_id, frameId, rush_id, pff_role, dist_to_rusher) %>%
  group_by(game_play_id, frameId, rush_id, pff_role) %>% 
  mutate(team_proximity_number = row_number()) %>%
  ungroup()


print('---- MERGING DFS TO GET QB, n CLOSEST BLOCKERS, n CLOSEST PASS RUSHERS ----')
unique_rushers = tracking_with_rusher_joined %>%
  select(game_play_id, frameId, rush_id) %>% 
  distinct()

data_structure = data.frame(pff_role = c('Pass', rep('Pass Block', NUM_PLAYERS_TO_CONSIDER), rep('Pass Rush', NUM_PLAYERS_TO_CONSIDER)),
                            team_proximity_number = c(1, 1:NUM_PLAYERS_TO_CONSIDER, 1:NUM_PLAYERS_TO_CONSIDER))

model_training_data = crossing(unique_rushers, data_structure)

model_training_with_values = model_training_data %>%
  left_join(tracking_with_rusher_joined, by=c('game_play_id', 'frameId', 'rush_id', 'pff_role', 'team_proximity_number'))

# rm(tracking_with_rusher_joined)
# rm(model_training_data)

print('---- RESHAPING INTO ONE ROW PER RUSHER PER FRAME PER PLAY ----')

FEATURES_PER_PLAYER = c('dist_to_rusher', 'x_diff', 'y_diff', 'o_diff', 'dir_diff', 'a_diff', 's_diff')

# Now, this flattens into a matrix with the 7 relevant columns for each player we consider
flat_features_matrix = model_training_with_values %>% select(all_of(FEATURES_PER_PLAYER)) %>% as.matrix()
flat_features_matrix[is.na(flat_features_matrix)] = 0

flat_features = matrix(t(flat_features_matrix), ncol=(1 + 2*NUM_PLAYERS_TO_CONSIDER) * length(FEATURES_PER_PLAYER), byrow=TRUE)

# rm(flat_features_matrix)

print('---- ATTACHING THE TARGET VARIABLE ----')
# Attach the hhs variable to determine if they got a hit, hurry or, to be used as the target 
pff_targets_only = pff %>% filter(pff_role == 'Pass Rush') %>% select(game_play_id, nflId, pff_hit, pff_hurry, pff_sack, hit_hurry_or_sack)

qb_colnames = paste0('qb_', FEATURES_PER_PLAYER)
blocker_colname_prefix = paste0('blocker', 1:NUM_PLAYERS_TO_CONSIDER_PER_TEAM)
rusher_colname_prefix = paste0('rusher', 1:NUM_PLAYERS_TO_CONSIDER_PER_TEAM)

blocker_colnames = expand.grid(prefix = blocker_colname_prefix, feature = FEATURES_PER_PLAYER) %>% 
  arrange(prefix) %>% 
  mutate(name = paste(prefix, feature, sep='_')) %>% 
  select(name) %>% unlist() %>% unname() 
rusher_colnames = str_replace_all(blocker_colnames, 'blocker', 'rusher')

# Join the features to the information
final_feature_set = cbind(unique_rushers, flat_features) %>% 
  left_join(pff_targets_only, by=c( 'game_play_id', 'rush_id' = 'nflId'))
colnames(final_feature_set) = c('game_play_id', 'frame_id', 'rush_id',
                                qb_colnames, blocker_colnames, rusher_colnames,
                                'pff_hit', 'pff_hurry', 'pff_sack', 'hit_hurry_or_sack')

print('---- WRITING CSV ----')

current_datetime = Sys.time()
current_timestamp = format(current_datetime, '%Y%m%d_%H%M%S')

OUTPUT_DIRECTORY = paste0('output/features')
dir.create(OUTPUT_DIRECTORY)

FEATURE_OUTPUT_NAME = paste0(OUTPUT_DIRECTORY, '/rusher_level_hhs_on_play_features_', current_timestamp, '.csv')
write.csv(final_feature_set, FEATURE_OUTPUT_NAME, row.names=FALSE)

print('---- All finished :) ----')

stop = Sys.time()
print(stop-start)

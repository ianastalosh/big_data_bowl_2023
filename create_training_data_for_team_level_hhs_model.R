
# Script to create training data to predict the probability of a sack occurring 

# Preprocessing for pass rusher sack probability

#' For each frame:
#' Compute the difference in the various tracking statistics between the QB and the various o-linemen and pass rushers
#' Sort by the distance and order as follows:  OLINE | DLINE |. 
#' Then, join to a different data frame to keep only the 5 closest blockers and 5 closest pass rushers to the QB
#' (if there are less than 5, fill with 0s)

start = Sys.time()
NUM_PLAYERS_TO_CONSIDER_PER_TEAM = 5

print("---- SUMMARISE PFF DATA AT THE PLAY LEVEL ----")
# Determine if a hit, hurry or sack occurred at any point during the play
pff_play_level = pff %>% 
  group_by(game_play_id, gameId, playId) %>% 
  summarize(num_qbs = sum(pff_role == 'Pass', na.rm=TRUE), 
            num_rush = sum(pff_role == 'Pass Rush', na.rm=TRUE),
            num_block = sum(pff_role == 'Pass Block', na.rm=TRUE),
            hit_on_play = ifelse(sum(pff_hit, na.rm = TRUE) > 0, 1, 0),
            hurry_on_play = ifelse(sum(pff_hurry, na.rm = TRUE) > 0, 1, 0),
            sack_on_play = ifelse(sum(pff_sack, na.rm = TRUE) > 0, 1, 0),
            hit_hurry_or_sack = ifelse(hit_on_play + hurry_on_play + sack_on_play > 0, 1, 0)) %>% 
  ungroup()

# Optional: Count the number of passers, pass rushers and pass blockers per play, defined by PFF role 
# print(paste("Max qbs per play:", max(pff_play_level$num_qbs))) # 1 
# print(paste("Max rush per play:", max(pff_play_level$num_rush))) # 8
# print(paste("Max block per play:", max(pff_play_level$num_block))) # 9

print("--- CREATING FEATURES ----")

# Get all the tracking data for the passer on each play
# Isolate the QB tracking from the other tracking
qb_tracking = fe_tracking %>% 
  filter(pff_role == 'Pass') %>% 
  select(game_play_id, frameId, qb_xnorm = xNorm, qb_ynorm = yNorm, 
         qb_dirnorm = dirNorm, qb_onorm = oNorm, qb_s = s, qb_a = a)

# Get the tracking for the linemen and compute the differences between the QB and the other players for each frame
lineman_tracking = fe_tracking %>% 
  filter(pff_role != 'Pass') %>% 
  left_join(qb_tracking, by=c('game_play_id', 'frameId')) %>%
  mutate(dist_to_qb = sqrt((xNorm - qb_xnorm)^2 + (yNorm - qb_ynorm)^2),
         x_diff = xNorm - qb_xnorm,
         y_diff = yNorm - qb_ynorm,
         o_diff = oNorm - qb_onorm,
         dir_diff = dirNorm - qb_dirnorm,
         a_diff = a - qb_a,
         s_diff = s - qb_s) %>% 
  arrange(game_play_id, frameId, pff_role, dist_to_qb) %>%
  group_by(game_play_id, frameId, pff_role) %>% 
  mutate(qb_team_proximity_ranking = row_number()) # Add a variable indicating on their team what number close to the QB they are

# For each play, we want to get the 5 closest blockers and 5 closest pass rushers to the quarterback 
print('---- MERGING DFS n CLOSEST BLOCKERS, n CLOSEST PASS RUSHERS ----')

data_structure = data.frame(pff_role = c(rep('Pass Block', NUM_PLAYERS_TO_CONSIDER_PER_TEAM), 
                                         rep('Pass Rush', NUM_PLAYERS_TO_CONSIDER_PER_TEAM)),
                            qb_team_proximity_ranking = c(1:NUM_PLAYERS_TO_CONSIDER_PER_TEAM, 1:NUM_PLAYERS_TO_CONSIDER_PER_TEAM))

unique_plays = fe_tracking %>% 
  filter(occurs_during_pass_rush == 1) %>%
  select(game_play_id, frameId) %>% 
  distinct() %>% 
  arrange(game_play_id, frameId)

model_training_data = crossing(unique_plays, data_structure)
model_training_with_values = model_training_data %>%
  left_join(lineman_tracking, by=c('game_play_id', 'frameId', 'pff_role', 'qb_team_proximity_ranking')) %>% 
  arrange(game_play_id, frameId)

print('---- RESHAPING INTO ONE ROW PER FRAME PER PLAY ----')

FEATURES_PER_PLAYER = c('dist_to_qb', 'x_diff', 'y_diff', 'o_diff', 'dir_diff', 'a_diff', 's_diff')

# Put the desired features into a matrix
flat_features_matrix = model_training_with_values %>% select(all_of(FEATURES_PER_PLAYER)) %>% as.matrix()
flat_features_matrix[is.na(flat_features_matrix)] = 0

# Reshape this matrix so the information for the players is all contained in a single row
# There are NUM_PLAYERS_TO_CONSIDER * length(FEATURES_PER_PLAYER) columns
flat_features = matrix(t(flat_features_matrix), ncol=(2 * NUM_PLAYERS_TO_CONSIDER_PER_TEAM) * length(FEATURES_PER_PLAYER), byrow=TRUE)

# rm(flat_features_matrix)

print('---- ATTACHING THE TARGET VARIABLE ----')

# Create the column names for the dataframe
blocker_colname_prefix = paste0('blocker', 1:NUM_PLAYERS_TO_CONSIDER_PER_TEAM)
rusher_colname_prefix = paste0('rusher', 1:NUM_PLAYERS_TO_CONSIDER_PER_TEAM)

blocker_colnames = expand.grid(prefix = blocker_colname_prefix, feature = FEATURES_PER_PLAYER) %>% 
  arrange(prefix) %>% 
  mutate(name = paste(prefix, feature, sep='_')) %>% 
  select(name) %>% unlist() %>% unname() 
rusher_colnames = str_replace_all(blocker_colnames, 'blocker', 'rusher')

# Attach the hhs variable to determine if they got a hit, hurry or, to be used as the target 

final_feature_set = cbind(unique_plays, flat_features) %>% 
  left_join(pff_play_level %>% select(game_play_id, hit_on_play, hurry_on_play, sack_on_play, hit_hurry_or_sack), 
            by=c( 'game_play_id'))
colnames(final_feature_set) = c('game_play_id', 'frame_id', blocker_colnames, rusher_colnames,
                                'hit_on_play', 'hurry_on_play', 'sack_on_play', 'hit_hurry_or_sack')

print('---- WRITING CSV ----')

current_datetime = Sys.time()
current_timestamp = format(current_datetime, '%Y%m%d_%H%M%S')

OUTPUT_DIRECTORY = paste0('output/features')
dir.create(OUTPUT_DIRECTORY)

FEATURE_OUTPUT_NAME = paste0(OUTPUT_DIRECTORY, '/team_level_hhs_on_play_features_', current_timestamp, '.csv')
write.csv(final_feature_set, FEATURE_OUTPUT_NAME, row.names=FALSE)

print('---- All finished :) ----')

stop = Sys.time()
print(stop-start)

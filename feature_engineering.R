
# Preprocessing functions
feature_engineer_tracking_data = function(raw_tracking) {
  
  pff_subset = pff %>% select(gameId, playId, nflId, pff_role, pff_positionLinedUp, position_lined_up_code, pff_nflIdBlockedPlayer)
  
  # Get
  added_features = raw_tracking %>% 
    # Add normalising features
    mutate(
      # x = ifelse(playDirection == 'left', X_MAX - x, x),
      # y = ifelse(playDirection == 'left', Y_MAX - y, y),
      # dir = (dir + 180) %% 360,
      # o = (o + 180) %% 360,
      game_play_id = paste(gameId, playId, sep='-'),
      xNorm = ifelse(playDirection == 'left', X_MAX - x, x),
      yNorm = ifelse(playDirection == 'left', Y_MAX - y, y),
      dirNorm = ifelse(playDirection == 'left', (dir + 180) %% 360, dir),
      dirMirror = 360 - dirNorm,
      dirNorm = ifelse(dirMirror + 90 >= 360, dirMirror + 90 - 360, dirMirror + 90),
      oNorm = ifelse(playDirection == 'left', (o + 180) %% 360, dir)) %>%
    # Add game specific features
    group_by(game_play_id, nflId) %>%
    mutate(eventWithNA = ifelse(event == 'None', NA, event),
           eventRecentWithLeading = na.locf(eventWithNA, na.rm = FALSE),
           eventRecent = replace_na(eventRecentWithLeading, 'presnap'),
           occurs_after_ball_snap = cumsum(ifelse(event %in% PLAY_STARTING_EVENT, 1, 0)),
           occurs_after_pass_rush_end = cumsum(ifelse(event %in% PASS_RUSH_ENDING_EVENT, 1, 0)),
           occurs_during_pass_rush = ifelse((occurs_after_ball_snap > 0) & (occurs_after_pass_rush_end == 0), 1, 0),
           nflId = replace_na(nflId, -999)) %>%
    select(-x, -y, -dir, -o, -eventWithNA, -eventRecentWithLeading, -occurs_after_ball_snap, -occurs_after_pass_rush_end) %>%
    ungroup() %>%
    left_join(pff_subset, by=c('gameId', 'playId', 'nflId')) %>%
    filter((pff_role %in% RELEVANT_POSITIONS) | (!is.na(pff_nflIdBlockedPlayer)) | (team == 'football')) %>%
    select(-playDirection) %>% 
    select(game_play_id, everything())
  
  return(added_features)
  
}

feature_engineer_pff = function(pff_data) {
  pff_feature = pff_data %>% 
                  mutate(game_play_id = paste(gameId, playId, sep='-'),
                         position_lined_up_code = case_when(pff_positionLinedUp == 'QB' ~ 0,
                                                            pff_positionLinedUp %in% c('LT', 'RT') ~ 1,
                                                            pff_positionLinedUp %in% c('LG', 'RG') ~ 2,
                                                            pff_positionLinedUp == 'C' ~ 3,
                                                            str_detect(pff_positionLinedUp, 'HB|FB') ~ 4,
                                                            str_detect(pff_positionLinedUp, 'TE') ~ 5,
                                                            str_detect(pff_positionLinedUp, 'WR') ~ 6,
                                                            pff_role == 'Pass Route' ~ 10,
                                                            pff_role == 'Coverage' ~ 20,
                                                            TRUE ~ -999),
                         hhs = pff_hit + pff_hurry + pff_sack,
                         hit_hurry_or_sack = ifelse(hhs > 0, 1, 0),
                         hhs_allowed = pff_hitAllowed + pff_hurryAllowed + pff_sackAllowed,
                         hit_hurry_or_sack_allowed = ifelse(hhs_allowed > 0, 1, 0))
  return(pff_feature)
}

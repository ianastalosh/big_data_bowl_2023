
# Train pass rusher model - the probability of each rusher at each frame getting a sack
pass_rush_sack_features = read_csv('output/features/rusher_level_hhs_on_play_features_20230108_124133.csv')

# Split dataset into train and test. Use Weeks 1-6 for train, Weeks 7-8 for test.

train_game_ids = games %>% filter(week %in% 1:6) %>% select(gameId) %>% unlist()
test_game_ids = games %>% filter(week %in% 7:8) %>% select(gameId) %>% unlist()

# Add weights, based on how many frames away from the end of the play it is
formatted_training_data = pass_rush_sack_features %>% 
  group_by(game_play_id) %>% 
  mutate(frame_weight = 1)

# Split into training and test sets
training_set = formatted_training_data %>% filter(str_detect(game_play_id, paste(train_game_ids, collapse='|')))
test_set = formatted_training_data %>% filter(str_detect(game_play_id, paste(test_game_ids, collapse='|')))

TARGET_VARIABLE = 'hit_hurry_or_sack'

training_x = training_set[, 4:80]
training_y = training_set[, TARGET_VARIABLE]
training_weights = training_set[, 'frame_weight']

test_x = test_set[, 4:80]
test_y = test_set[, TARGET_VARIABLE]
test_weights = test_set[, 'frame_weight']

total_x = formatted_training_data[, 4:80]
total_y = formatted_training_data[, TARGET_VARIABLE]
total_weights = formatted_training_data[, 'frame_weight']

# Train using XGBoost
train_dmat = xgb.DMatrix(data=as.matrix(training_x), label=as.matrix(training_y), weight=as.matrix(training_weights))
val_dmat = xgb.DMatrix(data=as.matrix(test_x), label=as.matrix(test_y), weight=as.matrix(test_weights))
total_dmat = xgb.DMatrix(data=as.matrix(total_x), label=as.matrix(total_y), weight=as.matrix(total_weights))

param = list(max_depth = 6, eta = 0.1, colsample_bytree=0.98,
             objective = 'binary:logistic', eval_metric = 'logloss')

model_object = xgb.train(data=train_dmat, 
                         params = param,
                         watchlist=list(train=train_dmat, validation=val_dmat), 
                         nrounds=250,
                         early_stopping_rounds=10)

# Write output
current_datetime = Sys.time()
current_timestamp = format(current_datetime, '%Y%m%d_%H%M%S')

OUTPUT_DIRECTORY = paste0('output/models/rusher_level_hhs_per_frame_', current_timestamp)
dir.create(OUTPUT_DIRECTORY)

MODEL_OUTPUT_NAME = paste0(OUTPUT_DIRECTORY, '/xgb_model.model')
TEST_PROBS_OUTPUT_NAME = paste0(OUTPUT_DIRECTORY, '/test_probs.csv')
TOTAL_PROBS_OUTPUT_NAME = paste0(OUTPUT_DIRECTORY, '/total_probs.csv')
TRAINING_SUMMARY_OUTPUT_NAME = paste0(OUTPUT_DIRECTORY, '/training_summary.csv')
MODEL_FEATURE_IMPORTANCE_OUTPUT_NAME = paste0(OUTPUT_DIRECTORY, '/model_feature_importance.csv')
PRETTY_TRAINING_SUMMARY_RUSHER_FILEPATH = paste0(OUTPUT_DIRECTORY, '/kable_rusher_training_summary.png')
PRETTY_FEATURE_IMPORTANCE_RUSHER_FILEPATH = paste0(OUTPUT_DIRECTORY, '/kable_rusher_feature_importance.png')

# Save XGB model
xgb.save(model_object, MODEL_OUTPUT_NAME)

# Predictions
raw_test_values = predict(model_object, as.matrix(test_x))
test_values = test_set %>%
  ungroup() %>%
  mutate(prob = raw_test_values) %>%
  select(game_play_id, frame_id, rush_id, hit_hurry_or_sack, prob)
write.csv(test_values, TEST_PROBS_OUTPUT_NAME, row.names = FALSE)

# Values with predicted
raw_total_values = predict(model_object, as.matrix(total_x))
total_values = formatted_training_data %>%
  ungroup() %>%
  mutate(prob = raw_total_values) %>%
  select(game_play_id, frame_id, rush_id, hit_hurry_or_sack, prob)

write.csv(total_values, TOTAL_PROBS_OUTPUT_NAME, row.names = FALSE)

# Show output of training
xgb_feature_importance = xgb.importance(model=model_object)

best_output = strsplit(model_object$best_msg, split='\t')
train_ll = strsplit(best_output[[1]][[2]], ':')[[1]][[2]]
test_ll = strsplit(best_output[[1]][[3]], ':')[[1]][[2]]

# Create roc curve
roc_object = roc(test_values$hit_hurry_or_sack, test_values$prob)
auc = pROC::auc(roc_object)
brier_score = brier(test_values$hit_hurry_or_sack, test_values$prob)

training_summary = data.frame(num_train_examples = nrow(training_x),
                              num_test_examples = nrow(test_x),
                              train_log_loss = as.numeric(train_ll),
                              test_log_loss = as.numeric(test_ll),
                              test_roc_auc = auc,
                              test_brier_score = brier_score)

write.csv(training_summary, TRAINING_SUMMARY_OUTPUT_NAME, row.names = FALSE)
write.csv(xgb_feature_importance, MODEL_FEATURE_IMPORTANCE_OUTPUT_NAME, row.names = FALSE)

# Write kable tables
kable_training_summary = training_summary %>% 
  rename(`Num. Training Examples` = num_train_examples,
         `Num. Test Examples` = num_test_examples,
         `Train Log Loss` = train_log_loss,
         `Test Log Loss` = test_log_loss,
         `Test ROC AUC` = test_roc_auc,
         `Test Brier Score` = test_brier_score) %>%
  kbl(caption = 'Rusher Model Training Summary') %>%
  kable_material(c('striped'), font_size=20)

save_kable(kable_training_summary, PRETTY_TRAINING_SUMMARY_RUSHER_FILEPATH, zoom=2)

kable_feature_importance = xgb_feature_importance[1:10, ] %>%
  kbl(caption = 'Rusher Model Feature Importance') %>%
  kable_material(c('striped'), font_size=20)

save_kable(kable_feature_importance, PRETTY_FEATURE_IMPORTANCE_RUSHER_FILEPATH, zoom=2)

rm(list = ls())
# Standard includes
library(ggplot2)
library(MASS)
# k-NN library
library(class)
# Random forest library
library(randomForest)
# SVM library
library(e1071)

source('./helper_funcs.R')
data = get_data()
data_start_idx_true = 2
data_stop_idx_true = ncol(data) - 1
num_features_true = data_stop_idx_true - data_start_idx_true + 1
num_classes = 99

num_training = 4
num_validation = 4
num_axes = 4

data_results = data.frame(data = matrix(nrow = 0, ncol = 6))
colnames(data_results) = c('replicate', 'classifier', 'param_1', 'param_2', 'error_count', 'error_pct')

data_knn_all = data.frame(data = matrix(nrow = 0, ncol = 3))
colnames(data_knn_all) = c('k', 'error_pct', 'replicate')
data_rf_all = data.frame(data = matrix(nrow = 0, ncol = 4))
colnames(data_rf_all) = c('max_nodes', 'num_trees', 'error_pct', 'replicate')
data_svm_all = data.frame(data = matrix(nrow = 0, ncol = 3))
colnames(data_svm_all) = c('cost', 'error_pct', 'replicate')

confusion_mat_knn_all = matrix(nrow = num_classes, ncol = num_classes, data = rep(0, num_classes^2))
confusion_mat_rf_all = matrix(nrow = num_classes, ncol = num_classes, data = rep(0, num_classes^2))
confusion_mat_svm_all = matrix(nrow = num_classes, ncol = num_classes, data = rep(0, num_classes^2))
confusion_mat_bayes_all = matrix(nrow = num_classes, ncol = num_classes, data = rep(0, num_classes^2))

for(replicate in 1:30){
  print(paste0('Begin replicate: ', replicate))
  data_split = split_data(data, num_training, num_validation, 'class', seed = replicate * 14)
  data_training = data_split[data_split$set == 'train',]
  data_validation = data_split[data_split$set == 'validation',]
  data_testing = data_split[data_split$set == 'test',]

  # Get mda-transformed value for each training sample
  mda_matrix = get_mda_matrix(data_training, num_features_true, data_start_idx_true, 'class', num_axes) 
  data_training_mda = get_mda_points(data_training, mda_matrix, num_features_true, data_start_idx_true, 'class', num_axes)   
  data_validation_mda = get_mda_points(data_validation, mda_matrix, num_features_true, data_start_idx_true, 'class', num_axes)   
  data_testing_mda = get_mda_points(data_testing, mda_matrix, num_features_true, data_start_idx_true, 'class', num_axes)   
  for(col_idx in 1:num_axes){
    data_training[,paste0('mda_', col_idx)] = data_training_mda[,col_idx + 1] # +1 offset to skip class
    data_validation[,paste0('mda_', col_idx)] = data_validation_mda[,col_idx + 1] 
    data_testing[,paste0('mda_', col_idx)] = data_testing_mda[,col_idx + 1] 
  }
  data_start_idx = ncol(data_training) - num_axes + 1
  data_stop_idx = ncol(data_training) 
  num_features = data_stop_idx - data_start_idx + 1
  
  # k-nearest neighbor 
  knn_data = run_knn(data_training, data_validation, data_start_idx, data_stop_idx)
  data_knn_all = rbind(data_knn_all, knn_data)
  best_k= knn_data[knn_data$error_pct == min(knn_data),]$k[1]
  knn_results = knn(
    data_training[, data_start_idx:data_stop_idx], 
    data_testing[, data_start_idx:data_stop_idx], 
    cl = data_training$class,
    k = best_k)
  knn_error_count = sum(data_testing$class != knn_results)
  knn_error_pct = knn_error_count / nrow(data_testing)
  data_results[nrow(data_results) + 1,] = c(replicate, 'knn', best_k, -1, knn_error_count, knn_error_pct)
  
  confusion_mat = get_confusion_matrix(data_testing$class, knn_results)
  write.matrix(confusion_mat, paste0('./confusion_matrices/mda_', num_axes, '__train_', num_training, '__val_', num_validation, '__', replicate, '_knn.csv'))
  confusion_mat_knn_all = confusion_mat_knn_all + confusion_mat
  data_confusion = get_confusion_data_frame(data_testing$class, knn_results)
  ggplot(data_confusion, aes(x = x, y = -y, fill = count)) + 
    geom_tile() + 
    xlab('Predicted class') +
    ylab('True Class') + 
    ggsave(paste0('./plots/mda_', num_axes, '__train_', num_training, '__val_', num_validation, '__', replicate, '_knn.pdf'), units = 'in', width = 6, height = 6)
  
  
  
  # Random Forest 
  rf_data = run_random_forest(data_training, data_validation, data_start_idx, data_stop_idx)
  data_rf_all = rbind(data_rf_all, rf_data)
  best_max_nodes = rf_data[rf_data$error_pct == min(rf_data$error_pct),]$max_nodes[1]
  best_num_trees = rf_data[rf_data$error_pct == min(rf_data$error_pct),]$num_trees[1]
  rf_model = randomForest(class ~ ., data = data_training[,c(1, data_start_idx:data_stop_idx)], importance = T, proximity = T, 
                          maxnodes = best_max_nodes, ntrees = best_num_trees)
  rf_results = predict(rf_model, data_testing[,data_start_idx:data_stop_idx])
  rf_error_count = sum(data_testing$class != rf_results)
  rf_error_pct = rf_error_count / nrow(data_testing)
  data_results[nrow(data_results) + 1,] = c(replicate, 'rf', best_num_trees, best_max_nodes, rf_error_count, rf_error_pct)
  
  confusion_mat = get_confusion_matrix(data_testing$class, rf_results)
  write.matrix(confusion_mat, paste0('./confusion_matrices/mda_', num_axes, '__train_', num_training, '__val_', num_validation, '__', replicate, '_rf.csv'))
  confusion_mat_rf_all = confusion_mat_rf_all + confusion_mat
  data_confusion = get_confusion_data_frame(data_testing$class, rf_results)
  ggplot(data_confusion, aes(x = x, y = -y, fill = count)) + 
    geom_tile() + 
    xlab('Predicted class') +
    ylab('True Class') + 
    ggsave(paste0('./plots/mda_', num_axes, '__train_', num_training, '__val_', num_validation, '__', replicate, '_rf.pdf'), units = 'in', width = 6, height = 6)
  
  
  
  
  # Support vector machines 
  svm_data = run_svm(data_training, data_validation, data_start_idx, data_stop_idx)
  data_svm_all = rbind(data_svm_all, svm_data)
  best_cost = svm_data[svm_data$error_pct == min(svm_data$error_pct),]$cost[1]
  svm_model = svm(
    x = data_training[,data_start_idx:data_stop_idx],
    y = data_training$class,
    kernel = 'linear',
    scale = F,
    cost = best_cost
    )
  svm_results = predict(svm_model, data_testing[,data_start_idx:data_stop_idx])
  svm_error_count = sum(data_testing$class != svm_results)
  svm_error_pct = svm_error_count / nrow(data_testing)
  data_results[nrow(data_results) + 1,] = c(replicate, 'svm', best_cost, -1, svm_error_count, svm_error_pct)
  
  confusion_mat = get_confusion_matrix(data_testing$class, svm_results)
  write.matrix(confusion_mat, paste0('./confusion_matrices/mda_', num_axes, '__train_', num_training, '__val_', num_validation, '__', replicate, '_svm.csv'))
  confusion_mat_svm_all = confusion_mat_svm_all + confusion_mat
  data_confusion = get_confusion_data_frame(data_testing$class, svm_results)
  ggplot(data_confusion, aes(x = x, y = -y, fill = count)) + 
    geom_tile() + 
    xlab('Predicted class') +
    ylab('True Class') + 
    ggsave(paste0('./plots/mda_', num_axes, '__train_', num_training, '__val_', num_validation, '__', replicate, '_svm.pdf'), units = 'in', width = 6, height = 6)
  
  
  
  #data_bayes = run_bayes(data_training, data_validation, data_start_idx, data_stop_idx)
  data_bayes_unif = run_bayes_unif(data_training, data_validation, data_start_idx, data_stop_idx)
  feature_vec = data_start_idx:data_stop_idx
  bayes_results = rep(NA, length(data_testing))
  class_vec = unique(data_training$class)
  for(test_row_idx in 1:nrow(data_testing)){
    bayes_results[test_row_idx] = predict_bayes_unif(
      as.numeric(data_testing[test_row_idx, data_start_idx:data_stop_idx]),
      num_features,
      feature_vec,
      class_vec, 
      data_bayes_unif
    )
  }
  bayes_error_count = sum(data_testing$class != bayes_results)
  bayes_error_pct = bayes_error_count / nrow(data_testing)
  data_results[nrow(data_results) + 1,] = c(replicate, 'bayes', -1, -1, bayes_error_count, bayes_error_pct)
  
  confusion_mat = get_confusion_matrix(data_testing$class, factor(bayes_results, levels = levels(data_testing$class)))
  write.matrix(confusion_mat, paste0('./confusion_matrices/mda_', num_axes, '__train_', num_training, '__val_', num_validation, '__', replicate, '_bayes.csv'))
  confusion_mat_bayes_all = confusion_mat_bayes_all + confusion_mat
  data_confusion = get_confusion_data_frame(data_testing$class, factor(bayes_results, levels = levels(data_testing$class)))
  ggplot(data_confusion, aes(x = x, y = -y, fill = count)) + 
    geom_tile() + 
    xlab('Predicted class') +
    ylab('True Class') + 
    ggsave(paste0('./plots/mda_', num_axes, '__train_', num_training, '__val_', num_validation, '__', replicate, '_bayes.pdf'), units = 'in', width = 6, height = 6)
  
}
write.matrix(confusion_mat_knn_all, paste0('./confusion_matrices/mda_', num_axes, '__train_', num_training, '__val_', num_validation, '__', 'knn_all.csv'))
write.matrix(confusion_mat_rf_all,  paste0('./confusion_matrices/mda_', num_axes, '__train_', num_training, '__val_', num_validation, '__', 'rf_all.csv'))
write.matrix(confusion_mat_svm_all, paste0('./confusion_matrices/mda_', num_axes, '__train_', num_training, '__val_', num_validation, '__', 'svm_all.csv'))
write.matrix(confusion_mat_bayes_all, paste0('./confusion_matrices/mda_', num_axes, '__train_', num_training, '__val_', num_validation, '__', 'bayes_all.csv'))
write.csv(data_knn_all, paste0('./mda_', num_axes, '__train_', num_training, '__val_', num_validation, '__', 'knn_all.csv'))
write.csv(data_rf_all,  paste0('./mda_', num_axes, '__train_', num_training, '__val_', num_validation, '__', 'rf_all.csv'))
write.csv(data_svm_all, paste0('./mda_', num_axes, '__train_', num_training, '__val_', num_validation, '__', 'svm_all.csv'))
write.csv(data_results, paste0('./mda_', num_axes, '__train_', num_training, '__val_', num_validation, '__', 'results.csv'))

data_results$error_pct = as.numeric(data_results$error_pct)
data_results$classifier_str = 'NA'
data_results[data_results$classifier == 'knn',]$classifier_str = 'k-Nearest Neighbor'
data_results[data_results$classifier == 'rf',]$classifier_str = 'Random Forest'
data_results[data_results$classifier == 'svm',]$classifier_str = 'Support Vector Machine'
data_results[data_results$classifier == 'bayes',]$classifier_str = 'Bayesian'
ggplot(data_results, aes(x = as.factor(classifier_str), y = error_pct)) + 
  geom_boxplot() + 
  xlab('Classifier Type') + 
  ylab('Error Percentage') + 
  scale_y_continuous(limits = c(0,1.1), breaks = c(0, 0.25, 0.5, 0.75, 1)) + 
  ggsave(paste0('./mda_', num_axes, '__train_', num_training, '__val_', num_validation, '__', 'results.pdf'), units = 'in', width = 6, height = 6)



# feature_vec = data_start_idx:data_stop_idx
# bayes_results = rep(NA, length(data_testing))
# class_vec = unique(data_training$class)
# for(test_row_idx in 1:nrow(data_testing)){
#   print(test_row_idx / nrow(data_testing))
#   bayes_results[test_row_idx] = predict_class(
#     as.numeric(data_testing[test_row_idx, data_start_idx:data_stop_idx]),
#     num_features,
#     feature_vec,
#     class_vec, 
#     data_bayes
#   )
# }

#feature_vec = data_start_idx:data_stop_idx
#bayes_results = rep(NA, length(data_testing))
#class_vec = unique(data_training$class)
#for(test_row_idx in 1:nrow(data_testing)){
#  print(test_row_idx / nrow(data_testing))
#  bayes_results[test_row_idx] = predict_bayes_unif(
#    as.numeric(data_testing[test_row_idx, data_start_idx:data_stop_idx]),
#    data_stop_idx - data_start_idx + 1,
#    feature_vec,
#    class_vec, 
#    data_bayes_unif
#  )
#}
run_knn = function(data_training, data_validation, data_start_idx, data_stop_idx){
  # k-nearest neighbor classifier
  data_knn = data.frame(data = matrix(nrow = 0, ncol = 2))
  colnames(data_knn) = c('k', 'error_pct')
  
  for(k in 1:16){
    knn_results = knn(
      data_training[, data_start_idx:data_stop_idx], 
      data_validation[, data_start_idx:data_stop_idx], 
      cl = data_training$class,
      k = 5)
    
    knn_error_num = sum(data_validation$class != knn_results)
    knn_error_pct = knn_error_num / nrow(data_validation)
    
    data_knn[nrow(data_knn) + 1, ] = c(k, knn_error_pct)
  }
  return(data_knn)
}

run_random_forest = function(data_training, data_validation, data_start_idx, data_stop_idx){
  # Look into max number of terminal nodes
  data_rf = data.frame(data = matrix(nrow = 0, ncol = 3))
  colnames(data_rf) = c('max_nodes', 'num_trees', 'error_pct')
  for(max_nodes in c(50,75,100)){
    for(num_trees in c(100,250,500)){
    rf_model = randomForest(class ~ ., data = data_training[,c(1, data_start_idx:data_stop_idx)], importance = T, proximity = T, maxnodes = max_nodes, ntrees = num_trees)
      rf_results = predict(rf_model, data_validation[,data_start_idx:data_stop_idx])
      rf_error_num = sum(data_validation$class != rf_results)
      rf_error_pct = rf_error_num / nrow(data_validation)
      data_rf[nrow(data_rf) + 1,] = c(max_nodes, num_trees, rf_error_pct)
    }
  }
  return(data_rf)
}
  

run_svm = function(data_training, data_validation, data_start_idx, data_stop_idx){
  data_svm = data.frame(data = matrix(nrow = 0, ncol = 2))
  colnames(data_svm) = c('cost', 'error_pct')
  for(cost in c(1, 10, 25, 50, 75, 100)){
    svm_model = svm(
      x = data_training[,data_start_idx:data_stop_idx],
      y = data_training$class,
      kernel = 'linear',
      scale = F,
      cost = cost
      )
    svm_results = predict(svm_model, data_validation[,data_start_idx:data_stop_idx])
    svm_error_num = sum(data_validation$class != svm_results)
    svm_error_pct = svm_error_num / nrow(data_validation)
    data_svm[nrow(data_svm) + 1,] = c(cost, svm_error_pct)
  }
  return(data_svm)
}

predict_bayes = function(x, num_features, feature_vec, class_vec, data_gaussian){
 max_prob = 0
 best_class = NA
 for(class_val in unique(class_vec)){
   cur_prob = 1
   for(feature in 1:num_features){
     row = data_gaussian[data_gaussian$class == class_val & data_gaussian$feature == feature_vec[feature],]
     if(row$mean != 0 & row$variance != 0){
       cur_prob = cur_prob * dnorm(x = x[feature], mean = row$mean, sd = row$sd, log = F)
     }
   }
   if(cur_prob > max_prob){
     max_prob = cur_prob
     best_class = class_val
   }
 }
 return(best_class)
}

run_bayes  = function(data_training, data_validation, data_start_idx, data_stop_idx){
  num_classes = length(unique(data_training$class))
  num_features = data_stop_idx - data_start_idx + 1
  data_bayes = data.frame(data = matrix(nrow = num_classes * num_features, ncol = 4))
  colnames(data_bayes) = c('class', 'feature_idx', 'mean', 'variance')
 
  #Iterate through each class
  cur_row_idx = 1
  for(cur_class in unique(data_training$class)){
    # Iterate through each feature
    for(feature_idx in data_start_idx:data_stop_idx){
      # Get the training data for this feature and class
      data_iter = data_training[data_training$class == cur_class, feature_idx]
      # Calculate mean and biased variance
      mean_iter = mean(data_iter)
      var_iter = (var(data_iter) / (length(data_iter) - 1)) * length(data_iter)
      data_bayes[cur_row_idx,] = c(cur_class, feature_idx, mean_iter, var_iter)
      cur_row_idx = cur_row_idx + 1
    }
  }
  data_bayes$mean = as.numeric(data_bayes$mean)
  data_bayes$variance = as.numeric(data_bayes$variance)
  data_bayes$sd = sqrt(data_bayes$variance)
  return(data_bayes)
}

predict_bayes_unif = function(x, num_features, feature_vec, class_vec, data_unif){
 max_prob = 0
 best_class = NA
 for(class_val in unique(class_vec)){
   cur_prob = 0
   for(feature in 1:num_features){
     row = data_unif[data_unif$class == class_val & data_unif$feature == feature_vec[feature],]
     if(row$max != 0){
        if(x[feature] > row$min && x[feature] < row$max){
          cur_prob = cur_prob + 1 / (row$max - row$min) 
        }
     }
   }
   if(cur_prob > max_prob){
     max_prob = cur_prob
     best_class = class_val
   }
 }
 return(best_class)
}

run_bayes_unif = function(data_training, data_validation, data_start_idx, data_stop_idx){
  num_classes = length(unique(data_training$class))
  num_features = data_stop_idx - data_start_idx + 1
  data_bayes = data.frame(data = matrix(nrow = num_classes * num_features, ncol = 4))
  colnames(data_bayes) = c('class', 'feature_idx', 'min', 'max')
 
  #Iterate through each class
  cur_row_idx = 1
  for(cur_class in unique(data_training$class)){
    # Iterate through each feature
    for(feature_idx in data_start_idx:data_stop_idx){
      # Get the training data for this feature and class
      data_iter = data_training[data_training$class == cur_class, feature_idx]
      # Calculate mean and biased variance
      min_iter = min(data_iter)
      max_iter = max(data_iter)
      data_bayes[cur_row_idx,] = c(cur_class, feature_idx, min_iter, max_iter)
      cur_row_idx = cur_row_idx + 1
    }
  }
  data_bayes$min = as.numeric(data_bayes$min)
  data_bayes$max = as.numeric(data_bayes$max)
  return(data_bayes)
}

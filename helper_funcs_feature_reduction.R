library(MASS)

######## PCA ########
get_pca_matrix = function(data, features_count, features_start, grouping_var, num_axes){
  features_idxs = features_start:(features_start + features_count - 1)
  # Get the numerical representation of the grouping variable
  grouping_col = (1:ncol(data))[colnames(data) == grouping_var]
  
  # Calculate the mean of all vectors
  mean_vec = as.numeric(colMeans(data[,features_idxs]))
  
  # Calculate the scatter matrix
  S = matrix(nrow = features_count, ncol = features_count, data = rep(0, features_count^2))
  for(row_idx in 1:nrow(data)){
    row_vec = as.numeric(data[row_idx,features_idxs]) 
    S = S + ((row_vec - mean_vec) %*% t(row_vec - mean_vec))
  }
  
  # Compute the eigenpairs
  eigen_results = eigen(x = S)
  eigen_matrix = eigen_results$vectors
  pca_trans_mat = eigen_matrix[,1:num_axes]

  return(pca_trans_mat)
}
get_pca_points = function(data, pca_trans_mat, features_count, features_start, grouping_var, num_axes){
  features_idxs = features_start:(features_start + features_count - 1)
  # Get the numerical representation of the grouping variable
  grouping_col = (1:ncol(data))[colnames(data) == grouping_var]
  # Create a new data frame for the transformed data
  data_pca = data.frame(data = matrix(nrow = 0, ncol = num_axes + 1))
  colnames(data_pca) = c(grouping_var, paste0('pca_', 1:num_axes))
  for(row_idx in 1:nrow(data)){
    row_vec = as.numeric(data[row_idx, features_idxs]) 
    pca = t(pca_trans_mat) %*% row_vec
    data_pca[nrow(data_pca) + 1,] = c(data[row_idx, grouping_col], pca[1:num_axes])
  }
  
  # Return the transformed data
  data_pca[,1] = as.factor(data_pca[,1])
  for(col_idx in 2:ncol(data_pca)){
    data_pca[,col_idx] = as.numeric(data_pca[,col_idx])
  }
  return(data_pca)
}


######## MDA ######
get_mda_matrix = function(data, features_count, features_start, grouping_var, num_axes){
  features_idxs = features_start:(features_start + features_count - 1)
  # Get the numerical representation of the grouping variable
  grouping_col = (1:ncol(data))[colnames(data) == grouping_var]
  
  # Calculate the mean of all vectors
  mean_vec_all = as.numeric(colMeans(data[,features_idxs]))
  
  # Calculate the overall within and between class scatter matrices
  S_w = matrix(nrow = features_count, ncol = features_count, data = rep(0,features_count^2))
  S_b = matrix(nrow = features_count, ncol = features_count, data = rep(0,features_count^2))
  for(group in unique(data[,grouping_col])){
    mean_vec = as.numeric(colMeans(data[data[,grouping_col] == group, features_idxs]))
    S_i = matrix(nrow = features_count, ncol = features_count, data = rep(0,features_count^2))
    for(row_idx in 1:nrow(data[data[,grouping_col] == group,])){
      row_vec = as.numeric(data[data[,grouping_col] == group,][row_idx, 1:features_count]) 
      S_i = S_i + ((row_vec - mean_vec) %*% t(row_vec - mean_vec))
    }
    S_w = S_w + S_i
    S_b = S_b + nrow(data[data[,grouping_col] == group,]) * ((mean_vec - mean_vec_all) %*% t(mean_vec - mean_vec_all))
  }
  
  # Compute the eigenpairs
  eigen_results = eigen(x = (ginv(S_w) %*% S_b))
  eigen_matrix = eigen_results$vectors
  mda_trans_mat = eigen_matrix[,1:num_axes]
  
  return(mda_trans_mat)
}
 
get_mda_points = function(data, mda_trans_mat, features_count, features_start, grouping_var, num_axes){
  features_idxs = features_start:(features_start + features_count - 1)
  # Get the numerical representation of the grouping variable
  grouping_col = (1:ncol(data))[colnames(data) == grouping_var]
  # Create new data frame of transformed points
  num_axes = nrow(mda_trans_mat)
  data_mda = data.frame(data = matrix(nrow = 0, ncol = num_axes + 1))
  colnames(data_mda) = c(grouping_var, paste0('mda_', 1:num_axes))
  for(row_idx in 1:nrow(data)){
    row_vec = as.numeric(data[row_idx, features_idxs]) 
    mda = t(mda_trans_mat) %*% row_vec
    data_mda[nrow(data_mda) + 1,] = c(data[row_idx, grouping_col], mda[1:num_axes])
  }
  
  # Return the transformed points
  data_mda[,1] = as.factor(data_mda[,1])
  for(col_idx in 2:ncol(data_mda)){
    data_mda[,col_idx] = as.numeric(data_mda[,col_idx])
  }
  return(data_mda)
}
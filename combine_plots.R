library(ggplot2)

######## Standard treatments ########
rm(list = ls())

training_counts = c(2,3,4,5,6)
combined_count = 8

data_initialized = F

for(training_count in training_counts){
  data_tmp = read.csv(paste0('./train_', training_count, '__val_', combined_count - training_count, '__results.csv'))
  data_tmp$training_count = training_count
  data_tmp$validation_count = combined_count - training_count
  if(!data_initialized){
    data = data_tmp
    data_initialized = T
  }
  else{
    data = rbind(data, data_tmp)
  }
}

data$error_pct = as.numeric(data$error_pct)
data$classifier_str = 'NA'
data[data$classifier == 'knn',]$classifier_str = 'k-Nearest Neighbor'
data[data$classifier == 'rf',]$classifier_str = 'Random Forest'
data[data$classifier == 'svm',]$classifier_str = 'Support Vector Machine'
data[data$classifier == 'bayes',]$classifier_str = 'Bayesian'

ggplot(data, aes(x = factor(data$classifier_str), y = error_pct, fill = as.factor(data$training_count))) + 
  geom_boxplot() + 
  xlab('Classifier Type') + 
  ylab('Error Rate') + 
  labs(fill = 'Training samples per class') +
  scale_y_continuous(limits = c(0,1)) +
  theme(legend.position = 'bottom') + 
  ggsave('overall_standard.pdf', units = 'in', width = 6, height = 6)

######## PCA treatments ########
rm(list = ls())

pca_vals = c(2,4,8,16)
data_initialized = F

for(pca_val in pca_vals){
  data_tmp = read.csv(paste0('./pca_', pca_val, '__train_4__val_4__results.csv'))
  data_tmp$pca_axes = pca_val
  if(!data_initialized){
    data = data_tmp
    data_initialized = T
  }
  else{
    data = rbind(data, data_tmp)
  }
}

data$error_pct = as.numeric(data$error_pct)
data$classifier_str = 'NA'
data[data$classifier == 'knn',]$classifier_str = 'k-Nearest Neighbor'
data[data$classifier == 'rf',]$classifier_str = 'Random Forest'
data[data$classifier == 'svm',]$classifier_str = 'Support Vector Machine'
data[data$classifier == 'bayes',]$classifier_str = 'Bayesian'

ggplot(data, aes(x = factor(data$classifier_str), y = error_pct, fill = as.factor(data$pca_axes))) + 
  geom_boxplot() + 
  xlab('Classifier Type') + 
  ylab('Error Rate') + 
  labs(fill = 'Number of PCA axes') +
  scale_y_continuous(limits = c(0,1)) +
  theme(legend.position = 'bottom') + 
  ggsave('overall_pca.pdf', units = 'in', width = 6, height = 6)


######## MDA treatments ########
rm(list = ls())

mda_vals = c(2,4,8,16)
data_initialized = F

for(mda_val in mda_vals){
  data_tmp = read.csv(paste0('./mda_', mda_val, '__train_4__val_4__results.csv'))
  data_tmp$mda_axes = mda_val
  if(!data_initialized){
    data = data_tmp
    data_initialized = T
  }
  else{
    data = rbind(data, data_tmp)
  }
}

data$error_pct = as.numeric(data$error_pct)
data$classifier_str = 'NA'
data[data$classifier == 'knn',]$classifier_str = 'k-Nearest Neighbor'
data[data$classifier == 'rf',]$classifier_str = 'Random Forest'
data[data$classifier == 'svm',]$classifier_str = 'Support Vector Machine'
data[data$classifier == 'bayes',]$classifier_str = 'Bayesian'

ggplot(data, aes(x = factor(data$classifier_str), y = error_pct, fill = as.factor(data$mda_axes))) + 
  geom_boxplot() + 
  xlab('Classifier Type') + 
  ylab('Error Rate') + 
  labs(fill = 'Number of MDA axes') +
  scale_y_continuous(limits = c(0,1)) +
  theme(legend.position = 'bottom') + 
  ggsave('overall_mda.pdf', units = 'in', width = 6, height = 6)


######## Overall confusion matrices plots ########
rm(list = ls())
num_replicates = 30

# k-nearest neighbors
knn_initialized  = F
for(replicate_id in 1:num_replicates){
  knn_tmp_mat = as.matrix(read.csv(paste0('./confusion_matrices/train_4__val_4__', replicate_id, '_knn.csv'), header = F, sep = ' '))
  tmp = sapply(knn_tmp_mat, FUN = as.numeric)
  if(!knn_initialized){
    knn_overall_mat = knn_tmp_mat
    knn_initialized = T
  } else{
    knn_overall_mat = knn_overall_mat + knn_tmp_mat
  }
}
num_classes = nrow(knn_overall_mat)
df_knn = data.frame(data = matrix(nrow = num_classes^2, ncol = 3))
colnames(df_knn) = c('x', 'y', 'count')
for(y in 1:num_classes){
  for(x in 1:num_classes){
    df_knn[(y - 1) * num_classes + x,] = c(x, y, knn_overall_mat[y,x])
  }
}
ggplot(df_knn, aes(x = x, y = -y, fill = count)) + 
  xlab('Predicted class') + 
  ylab('True class') + 
  geom_tile() + 
  ggsave('overall_knn_confusion_matrix_4_train_4_val.pdf', units = 'in', width = 6, height = 6)

# Random forest
rf_initialized  = F
for(replicate_id in 1:num_replicates){
  rf_tmp_mat = as.matrix(read.csv(paste0('./confusion_matrices/train_4__val_4__', replicate_id, '_rf.csv'), header = F, sep = ' '))
  tmp = sapply(rf_tmp_mat, FUN = as.numeric)
  if(!rf_initialized){
    rf_overall_mat = rf_tmp_mat
    rf_initialized = T
  } else{
    rf_overall_mat = rf_overall_mat + rf_tmp_mat
  }
}
num_classes = nrow(rf_overall_mat)
df_rf = data.frame(data = matrix(nrow = num_classes^2, ncol = 3))
colnames(df_rf) = c('x', 'y', 'count')
for(y in 1:num_classes){
  for(x in 1:num_classes){
    df_rf[(y - 1) * num_classes + x,] = c(x, y, rf_overall_mat[y,x])
  }
}
ggplot(df_rf, aes(x = x, y = -y, fill = count)) + 
  xlab('Predicted class') + 
  ylab('True class') + 
  geom_tile() + 
  ggsave('overall_rf_confusion_matrix_4_train_4_val.pdf', units = 'in', width = 6, height = 6)


# Support vector machine
svm_initialized  = F
for(replicate_id in 1:num_replicates){
  svm_tmp_mat = as.matrix(read.csv(paste0('./confusion_matrices/train_4__val_4__', replicate_id, '_svm.csv'), header = F, sep = ' '))
  tmp = sapply(svm_tmp_mat, FUN = as.numeric)
  if(!svm_initialized){
    svm_overall_mat = svm_tmp_mat
    svm_initialized = T
  } else{
    svm_overall_mat = svm_overall_mat + svm_tmp_mat
  }
}
num_classes = nrow(svm_overall_mat)
df_svm = data.frame(data = matrix(nrow = num_classes^2, ncol = 3))
colnames(df_svm) = c('x', 'y', 'count')
for(y in 1:num_classes){
  for(x in 1:num_classes){
    df_svm[(y - 1) * num_classes + x,] = c(x, y, svm_overall_mat[y,x])
  }
}
ggplot(df_svm, aes(x = x, y = -y, fill = count)) + 
  xlab('Predicted class') + 
  ylab('True class') + 
  geom_tile() + 
  ggsave('overall_svm_confusion_matrix_4_train_4_val.pdf', units = 'in', width = 6, height = 6)

# Combine them!
df_knn$classifier = 'k-nearest neighbor'
df_rf$classifier =  'Random forest'
df_svm$classifier = 'Support vector machine'
df_all = rbind(df_knn, df_rf, df_svm)

ggplot(df_all, aes(x = x, y = -y, fill = count)) + 
  xlab('Predicted class') + 
  ylab('True class') + 
  geom_tile() + 
  facet_grid(cols = vars(classifier)) +
  ggsave('overall_all_confusion_matrix_4_train_4_val.pdf', units = 'in', width = 18, height = 6)

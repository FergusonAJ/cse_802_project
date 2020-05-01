split_data = function(data, training_samples_per_group, validation_samples_per_group, grouping_var, seed = NA){
  if(!is.na(seed)){
    set.seed(seed)
  }
  data$set = 'test'
  for(group_val in unique(data[,grouping_var])){
    testing_samples = nrow(data[data[,grouping_var] == group_val,]) - training_samples_per_group - validation_samples_per_group
    if(testing_samples < 0){
      print(paste0('Error! Not enough sample in group ', group_val, ' for desired split!'))
    }
    else if(testing_samples == 0){
      print(paste0('Error! No samples in group ', group_val, ' left for test set!'))
    }
    # Randomly permute set vec so we get a random split with the desired number of each type
    set_vec = c(rep('train', training_samples_per_group), rep('validation', validation_samples_per_group), rep('test', testing_samples))
    data[data[,grouping_var] == group_val,'set'] = sample(set_vec)
  }
  return(data)
}

# data_2 = split_data(data, 3, 4, 'class')
# print(paste0('test: ', nrow(data_2[data_2$set == 'test',]) / 99))
# print(paste0('validation: ', nrow(data_2[data_2$set == 'validation',]) / 99))
# print(paste0('training: ', nrow(data_2[data_2$set == 'train',]) / 99))

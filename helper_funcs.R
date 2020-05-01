source('helper_funcs_feature_reduction.R')
source('helper_funcs_data_splitting.R')
source('helper_funcs_confusion_matrix.R')
source('helper_funcs_classifiers.R')

get_data = function(){
  # Load in all three tables
  data_tex = read.csv('~/documents/school/cur_sp20/pattern_rec/project/100_plants/100_leaves_plant_species/data_Tex_64.txt', header=F)
  data_shape = read.csv('~/documents/school/cur_sp20/pattern_rec/project/100_plants/100_leaves_plant_species/data_Sha_64.txt', header=F)
  data_margin = read.csv('~/documents/school/cur_sp20/pattern_rec/project/100_plants/100_leaves_plant_species/data_Mar_64.txt', header=F)
  
  # One file was missing row -> drop that class
  to_exclude = c('Acer Campestre')
  data_tex =    data_tex[!(data_tex[,1] %in% to_exclude),]
  data_shape =  data_shape[!(data_shape[,1] %in% to_exclude),]
  data_margin = data_margin[!(data_margin[,1] %in% to_exclude),]
  
  # Add column names
  colnames(data_tex) = c('class', paste0('tex_', 1:64))
  colnames(data_shape) = c('class', paste0('sha_', 1:64))
  colnames(data_margin) = c('class', paste0('mar_', 1:64))
  
  # Merge vectors together, but exclude class from second and third tables
  data = cbind(data_tex, data_shape[,2:ncol(data_shape)], data_margin[,2:ncol(data_shape)])
  # Ensure class is a factor, but exclude unused levels (like the species we had to cut)
  data$class = as.factor(as.character(data$class))
  
  return(data)
}

get_data_genus = function(){
  data = get_data()
  # Extract genus, recreate the factor so iit only has 99 levels
  data$class = as.character(data$class)
  data$genus = NA
  for(class in unique(data$class)){
    data[data$class == class,]$genus = strsplit(class, ' ')[[1]][1]
  }
  data$class = as.factor(data$class)
  data$genus = as.factor(data$genus)
  return(data) 
}
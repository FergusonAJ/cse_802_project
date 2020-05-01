library(ggplot2)

# Get the matrix representation of the confusion matrix
get_confusion_matrix = function(true_classes, predicted_classes){
  element_count = length(unique(true_classes))
  mat = matrix(nrow = element_count, ncol = element_count, rep(0, element_count^2))
  # For each element in true classes (row), add one to the predicted column
  for(idx in 1:length(true_classes)){
     mat[true_classes[idx], predicted_classes[idx]] = mat[true_classes[idx], predicted_classes[idx]] + 1
  }
  return(mat)
}

# Get confusion matrix and convert to data frame for easy plotting
get_confusion_data_frame = function(true_classes, predicted_classes){
  mat = get_confusion_matrix(true_classes, predicted_classes)
  num_classes = nrow(mat)
  df = data.frame(data = matrix(nrow = num_classes^2, ncol = 3))
  colnames(df) = c('x', 'y', 'count')
  for(y in 1:num_classes){
    for(x in 1:num_classes){
      df[(y - 1) * num_classes + x,] = c(x, y, mat[y,x])
    }
  }
  return(df)
}


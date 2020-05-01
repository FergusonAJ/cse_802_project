# Do a pca based on class
pca_matrix = get_pca_matrix(data_training, features_count, features_start, 'class', 2)
data_pca_class = get_pca_points(data_training, pca_matrix, features_count, features_start, 'class', 2)
ggplot(data_pca_class, aes(x = pca_1, y = pca_2, color = class)) + 
  geom_point() + 
  theme(legend.position = 'none')

# Do a mda based on class
mda_matrix_class = get_mda_matrix(data_training, features_count, features_start, 'class', 2)
data_mda_class = get_mda_points(data_training, mda_matrix_class, features_count, features_start, 'class', 2)
ggplot(data_mda_class, aes(x = mda_1, y = mda_2, color = class)) + 
  geom_point() + 
  theme(legend.position = 'none')

# Do a mda based on class
mda_matrix_genus = get_mda_matrix(data_training, features_count, features_start, 'genus', 2)
data_mda_genus = get_mda_points(data_training, mda_matrix_genus, features_count, features_start, 'genus', 2)
data_mda_genus$genus = data_training$genus
ggplot(data_mda_genus, aes(x = mda_1, y = mda_2, color = genus)) + 
  geom_point() + 
  theme(legend.position = 'bottom')


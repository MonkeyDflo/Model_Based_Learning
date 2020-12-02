source("/home/florent/Documents/Model Based Learning/EMalgo_continous_categorical_data.R", chdir = TRUE)
library(testthat)

# test TransformCategoricalVectorToBinaryMatrice ####
test = sample(iris[,5], 10)
#print(test)
#print( length(TransformCategoricalVectorToBinaryMatrice(test)) )
#result =
test_that(" TransformCategoricalVectorToBinaryMatrice ", {
  expect_length(TransformCategoricalVectorToBinaryMatrice(test), 30)
})

# you can execute this file or execute this command in the R console
# testthat::test_dir("/home/florent/Documents/Model Based Learning/tests/test_EM")
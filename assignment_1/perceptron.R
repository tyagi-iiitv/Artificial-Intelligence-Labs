calculate_output <- function(theta, weights, x1, x2, x3, x4){
  sum <- x1*weights[1] + x2*weights[2] + x3*weights[3] + x4*weights[4] + weights[5]
  if(sum >= theta){
    return (1)
  }
  else{
    return (0)
  }
}


#Perceptron on Iris_dataset
x <- read.csv("Iris_dataset.csv") #x stores the data set
ran_numbers <- rnorm(5) #generate 4 random numbers
weights <- dnorm(ran_numbers)
iteration <- 0
theta <- 2
instances <- 150
learning_rate <- 0.1
max_iter <- 100
repeat{
  iteration <- iteration + 1
  global_error <- 0
  for(p in 1:150){
      output <- calculate_output(theta, weights, x[[2]][p], x[[3]][p], x[[4]][p], x[[5]][p])
      local_error <- x[[1]][p] - output
      weights[1] <- weights[1] + (learning_rate*local_error*x[[2]][p])
      weights[2] <- weights[2] + (learning_rate*local_error*x[[3]][p])
      weights[3] <- weights[3] + (learning_rate*local_error*x[[4]][p])
      weights[4] <- weights[4] + (learning_rate*local_error*x[[5]][p])
      weights[5] <- weights[5] + (learning_rate*local_error)
      global_error <- global_error + (local_error^2)
  }
  if(global_error == 0 || iteration == max_iter){
    break
  }
}


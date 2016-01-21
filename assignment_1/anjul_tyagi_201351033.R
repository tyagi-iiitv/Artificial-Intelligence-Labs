calculate_output <- function(theta, weights, x1, x2, x3, x4){  #Calculate the output for the given weights
  sum <- x1*weights[1] + x2*weights[2] + x3*weights[3] + x4*weights[4] + weights[5]
  if(sum >= theta){
    return (1)
  }
  else{
    return (-1)
  }
}

accuracy <- function(x, weights, start, end, theta){ #Returns the accuracy of the data
  error_count <- 0
  for(i in start:end){
    if(calculate_output(theta, weights, x[[2]][z[i]], x[[3]][z[i]], x[[4]][z[i]], x[[5]][z[i]]) != x[[1]][z[i]]){
      error_count <- error_count + 1
    }
  }
  # print(end)
  # print(start)
  found_accuracy <- ((end-start-error_count)/(end-start))
  # print(found_accuracy)
  return(found_accuracy)
}

#Perceptron on Iris_dataset
x <- read.csv("Iris_dataset.csv") #x stores the data set
ran_numbers <- rnorm(5) #generate 5 random numbers
weights <- dnorm(ran_numbers)#Stores the random numbers for initial_weights
weights_org <- weights #To have a look at the initial weights
iteration <- 1 #Count of the number of iterations
theta <- 0 #Threshold value to compare the equation with
#instances <- 150
learning_rate <- 1
max_iter <- 50 #Check for 50 test data randomy selected from the test data
p <- 1 #Iterator for the test data
z <- floor(runif(150,1,151)) #This is a vector that stores random numbers form 1 to 150
repeat{
  p <- p + 1
  # print(z[p])
  # print("...")
  output <- calculate_output(theta, weights, x[[2]][z[p]], x[[3]][z[p]], x[[4]][z[p]], x[[5]][z[p]])
  local_error <- x[[1]][z[p]] - output
  weights[1] <- weights[1] + (learning_rate*local_error*x[[2]][z[p]])
  weights[2] <- weights[2] + (learning_rate*local_error*x[[3]][z[p]])
  weights[3] <- weights[3] + (learning_rate*local_error*x[[4]][z[p]])
  weights[4] <- weights[4] + (learning_rate*local_error*x[[5]][z[p]])
  weights[5] <- weights[5] + (learning_rate*local_error)
  if(p == max_iter){
    break
  }
}
print("accuracy is :")
print(accuracy(x, weights, 111, 150, theta))

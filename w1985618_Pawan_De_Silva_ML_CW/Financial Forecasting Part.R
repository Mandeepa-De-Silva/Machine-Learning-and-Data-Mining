library(ggpubr)
library(neuralnet)
library(readxl)
library(Metrics)
library(keras)
library(ggplot2)

exchangeUSD <- read_excel("ExchangeUSD.xlsx")
colnames(exchangeUSD) <- c('YYYY/MM/DD', 'Wdy', 'USD/EUR')

summary(exchangeUSD)
boxplot(exchangeUSD[,3])


#Normalize function 
normalize <- function(x){
  return ((x - min(x)) / (max(x) - min(x))) 
}

#Unnormalize function 
unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min ) 
}

#normalizing the data 
exchangeUSD_norm <- as.data.frame(lapply(exchangeUSD[,3], normalize))

summary(exchangeUSD_norm)
boxplot(exchangeUSD_norm)

train_data <- exchangeUSD_norm[1:400 ,]
test_data <- exchangeUSD_norm[401:500 ,]

#create I/O matrix function for testing and training
create_io_df <- function(data, num_cols){
  dataInput <- matrix(, nrow = 0, ncol = num_cols)
  dataOutput <- c() # initialize vector for store the output
  
  for(i in 1:length(data)){ #start from the first row
    lastValue <- i + (num_cols - 1)
    if(lastValue+1>length(data)){
      break
    }
    input <- data[i:lastValue]
    output <- data[lastValue+1]
    
    dataInput <- rbind(dataInput, input)
    dataOutput <- append(dataOutput, output)
  }
  dataIoDf <- cbind(as.data.frame(dataInput), dataOutput) #combine the input matrix with output vector
  return(dataIoDf)
}

#create io_matrix for training and testing for t-4
create_t4IoDf <- function(data, num_cols=5) { 
  dataInput <- matrix( ,nrow = 0, ncol = num_cols) #matrix to extract input values 
  dataOutput <- c() #vector to store output value
  
  for (i in 4:length(data)) { #starting from 4th row, use a lag of 4 days in input vector
    lastValue <- i + (num_cols - 2)
    t4_value <- (lastValue - 3)
    if (lastValue + 1 > length(data)) {
      break
    }
    input <- data[i:lastValue-1]
    t4_input <- data[t4_value]
    input <- append(input, t4_input)
    output <- data[lastValue + 1] 
    
    dataInput <- rbind(dataInput, input) #add the new input vector to input matrix 
    dataOutput <- append(dataOutput, output) #add new output value to output vector
  }
  
  data_4tIoDf <- cbind(as.data.frame(dataInput), dataOutput)
  return(data_4tIoDf)
}

#creating io_matrix for training
training_io_1 <- create_io_df(train_data, 1)
training_io_2 <- create_io_df(train_data, 2)
training_io_3 <- create_io_df(train_data, 3)
training_io_4 <- create_t4IoDf(train_data, 4)

#creating io_matrix for testing
testing_io_1 <- create_io_df(test_data, 1)
testing_io_2 <- create_io_df(test_data, 2)
testing_io_3 <- create_io_df(test_data, 3)
testing_io_4 <- create_t4IoDf(test_data, 4)


#create neural network func
create_neural_network <- function(data, input_cols, hidden_layers, act_fct) {
  formula <- as.formula(paste("dataOutput ~", paste(input_cols, collapse = " + ")))
  set.seed(12345)
  neural_network_model <- neuralnet(formula, data = data, hidden = hidden_layers, 
                        act.fct = act_fct, linear.output = TRUE) #create neural network model
  return(neural_network_model)
}

# Training Models 
input1_nn_1 <- create_neural_network(training_io_1, c("V1"), c(5), "logistic") # with logistic sigmoid function 
input1_nn_2 <- create_neural_network(training_io_1, c("V1"), c(7,7), "tanh")   # with hyperbolic tangent function
input1_nn_3 <- create_neural_network(training_io_1, c("V1"), c(7,10), "logistic") 
input2_nn_1 <- create_neural_network(training_io_2, c("V1", "V2"), c(5), "logistic")
input2_nn_2 <- create_neural_network(training_io_2, c("V1", "V2"), c(5,7), "tanh")
input2_nn_3 <- create_neural_network(training_io_2, c("V1", "V2"), c(7,10), "logistic")
input3_nn_1 <- create_neural_network(training_io_3, c("V1", "V2", "V3"), c(5), "logistic")
input3_nn_2 <- create_neural_network(training_io_3, c("V1", "V2", "V3"), c(10), "logistic")
input3_nn_3 <- create_neural_network(training_io_3, c("V1", "V2", "V3"), c(5,7), "tanh")
input3_nn_4 <- create_neural_network(training_io_3, c("V1", "V2", "V3"), c(7,10), "tanh")
input4_nn_1 <- create_neural_network(training_io_4, c("V1", "V2", "V3", "V4"), c(5), "logistic")
input4_nn_2 <- create_neural_network(training_io_4, c("V1", "V2", "V3", "V4"), c(10), "tanh")
input4_nn_3 <- create_neural_network(training_io_4, c("V1", "V2", "V3", "V4"), c(5,5), "logistic")
input4_nn_4 <- create_neural_network(training_io_4, c("V1", "V2", "V3", "V4"), c(7,10), "logistic")

#testing and evaluating function for NN model
evaluating_neural_network <- function(ordern_neural_network, ordern_test_io, cons_data = exchangeUSD) { 
  #test the neural networks with test data
  number_col_i <- ncol(ordern_test_io)#number of columns in test I/O data
  testing_data <- data.frame(ordern_test_io[,1:(number_col_i - 1)])
  set.seed(12345)
  ordern_nn_results <- compute(ordern_neural_network, testing_data) # uses the trained NN to compute predictions 
  # results of NN 
  results <- data.frame(actual = ordern_test_io$dataOutput, 
                        prediction = ordern_nn_results$net.result) #actual and predicted data frame
  
  resultsMin <- min(cons_data$"USD/EUR") #store the minimum rate from the dataset 
  resultsMax <- max(cons_data$"USD/EUR") #store the maximum rate from the dataset
  
  #unnormalized the predicted and actual outputs
  comparison <- data.frame(
    predicted = unnormalize(results$prediction, resultsMin, resultsMax),
    actual = unnormalize(results$actual, resultsMin, resultsMax)
  )
  
  #1st layer weights 
  weights_layer1 <- ordern_neural_network$weights[[1]]
  #calculate the number of hidden layers in NN
  hidden_layer_no <- length(weights_layer1) - 1
  if(hidden_layer_no == 2) { 
    hidden_layer_config_1 <- ncol(ordern_neural_network[["weights"]][[1]][[1]])# number of neuron in the 1st hidden layer
    hidden_layer_config_2 <- ncol(ordern_neural_network[["weights"]][[1]][[2]])# number of neurons in the second layer
    hidden_layer_config <- c(hidden_layer_config_1, hidden_layer_config_2)
  }else {
    hidden_layer_config_1 <- ncol(ordern_neural_network[["weights"]][[1]][[1]])
    hidden_layer_config <- c(hidden_layer_config_1)#combine the number of neurons in the hidden layer
  }
  
  #statistic indices
  RMSE <- rmse(comparison$actual, comparison$predicted) #Root Mean Square Error
  MAE <- mae(comparison$actual, comparison$predicted) # Mean Absolute Error
  MAPE <- mape(comparison$actual, comparison$predicted) # Mean Absolute Percentage Error
  sMAPE <- smape(comparison$actual, comparison$predicted) # Symmetric Mean Absolute Percentage Error
  
  #crate a data frame to store the evaluation matrics with information about the NNA
  evaluation_metrix <- data.frame(No_of_inputs = (number_col_i-1), No_of_hidden_layers = hidden_layer_no,
                         Hidden_layer_cofig = paste(hidden_layer_config, collapse = " + "), 
                         RMSE = RMSE, MAE = MAE, MAPE = MAPE, sMAPE = sMAPE)
  plot(ordern_neural_network) #Generate the plots 
  
  return(list(evaluation_metrix = evaluation_metrix, comparison = comparison))
}


training_models <- list(input1_nn_1, input1_nn_2, input1_nn_3, input2_nn_1, input2_nn_2, input2_nn_3,
               input3_nn_1, input3_nn_2, input3_nn_3,input3_nn_4, input4_nn_1, input4_nn_2, input4_nn_3, 
               input4_nn_4)

#list of testing I/O matrices for each NN models
testing_data <- list(testing_io_1, testing_io_1, testing_io_1, 
                     testing_io_2, testing_io_2, testing_io_2, 
                     testing_io_3, testing_io_3, testing_io_3,
                     testing_io_3, testing_io_4, testing_io_4, 
                     testing_io_4,testing_io_4)

#testing and evaluating NN models with testing_io matrix 
evaluating_results <- list()
for (i in 1:length(training_models)) {
  evaluating_results[[i]] <- evaluating_neural_network(training_models[[i]], testing_data[[i]])
}

#create the comparison dataframe with indices
eval_metrix_comparison_df <- do.call(rbind, lapply(evaluating_results, function(x) x$evaluation_metrix))


# Identify the best MLP network based on evaluation results
best_model_index <- which.min(eval_metrix_comparison_df$RMSE)  # Assuming RMSE is the metric for selection
best_model <- training_models[[best_model_index]]
best_model_eval <- evaluating_results[[best_model_index]]

# Prepare data for visualization
results <- best_model_eval$comparison


ggplot(results, aes(x = actual, y = predicted),col) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +  # Add a line of perfect prediction
  labs(x = "Actual", y = "Predicted", title = "MLP Network Prediction vs. Actual") +
  theme_minimal()

ggplot(results, aes(x = 1:nrow(results), y = actual),) +
  geom_line(color = "blue") +
  geom_line(aes(y = predicted), color = "red") +
  labs(x = "Data Point", y = "USD/EUR Exchange Rate", title = "Actual vs. Predicted Exchange Rate") +
  theme_minimal()
# Display Statistical Indices
print(best_model_eval$evaluation_metrix)


linear_modeling<-function(data, x, y){
  data<-as.data.frame(data)
  formula<-paste(y, x, sep = "~")
  fitted_model <- stats::lm(formula, data = data)
  ggplot(data, aes_string(x = x, y = y)) + 
    geom_point() + 
    stat_smooth(method = "lm", col = "red")
}

recursive_partitioning_modeling<-function(data, x, y){
  #rpart::rpart()
  data<-as.data.frame(data)
  formula <- paste(y, x, sep = "~")
  fitted_model <- rpart::rpart(formula, data, cp = 0.002)
  # we cannot use ggplot with rpart, it throws an error as it is a step function
  # and not really a smoothing function
  fitted_model
  
}
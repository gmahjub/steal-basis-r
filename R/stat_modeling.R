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
  fitted_model <- rpart::rpart(formula, data, control = rpart.control(cp = 0.05))
  ggplot(data, aes_string(x = x, y = y)) +
    geom_point() +
    geom_smooth(method = "rpart", col = "red", method.args = list(control = rpart.control, list(cp = 0.05)))
  # geom_smooth and stat_smooth are for all intents and purpose, the same thing.
}
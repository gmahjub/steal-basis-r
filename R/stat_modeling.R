linear_modeling<-function(data, x, y){
  data<-as.data.frame(data)
  formula<-paste(y, x, sep = "~")
  fitted_model <- stats::lm(formula, data = data)
  ggplot(data, aes_string(x = x, y = y)) + 
    geom_point() + 
    stat_smooth(method = "lm", col = "red")
}

recursive_partitioning_modeling<-function(data, x, y, cp = 0.002){
  #rpart::rpart()
  data<-as.data.frame(data)
  formula <- paste(y, x, sep = "~")
  fitted_model <- rpart::rpart(formula, data, cp = cp)
  # we cannot use ggplot with rpart, it throws an error as it is a step function
  # and not really a smoothing function
  return (rpart_model)
}

prp_impl<-function(rpart_model){
  # prp is what we use to draw the relationships that rpart finds
  # in the data.
  prp(rpart_model, type = 3)
}

build_base_model<-function(formula, data){
  # lm()
  # rpart()
  return (base_model)
}

build_aug_model<-function(formula, data){
  # a different formuala, compare this model to the base model.
  return (aug_model)
}

predict_model<-function(model, data){
  # in statisticalModeling package, a call to evaluate_model would be made.
  # the result of evaluate model would be sent to calc_MSE().
  return (predictions)
}

split_train_test<-function(data){
  data$train <- rnorm(nrow(data)) > 0
  # inserts a column into the dataframe, called training, which is either a 1 or
  # 0. If it is a 1, then the data is to be used for training, else it is used for
  # testing.
  return (data)
}

calc_MSE<-function(data){
  # since we are not using the statisticalModeling package, we will leave this func
  # empty for now, where predictions is the result of predict_model call (evaluate_model)
  # with(data = predictions, mean((net - model_output)^2))
}

calc_in_sample_MSE<-function(training_data){
  # this would involve evaluate_model, on the TRAINING DATA
  # in_sample_error<-with(training_data, mean((acutal - model_output)^2, na.rm = TRUE))
  return (in_sample_error)
}

run_cross_validation_trials<-function(base__model, aug_model_1 = NULL, aug_model_2 = NULL, 
                                      aug_model_3 = NULL){
  # in statisticalModeling package, this would be a call to cv_pred_error()
  # cv_trials<-cv_pred_error(model)
  # possible to have multiple models passed into the cv_pred_error function
  # cv_trials<-cv_pred_error(base_model, aug_model_1)
  return (cv_trials)
}

conduct_T_test<-function(cv_trials, in_sample_error = NA){
  # this function takes as input the cross validation trials from running the cross validation erro
  # function. It also takes the in_sample_error ( MSE in sample trial).
  if (is.na(in_sample_error)){
    t_test_res<-mosaic::t.test(~ mse, mu = in_sample_error, data = cv_trials)
  } else {
    # if in_sample_error is NA, then we must have multiple models in the cv_trials dataframe.
    t_test_res <- mosaic::t.test(mse ~ model, data = cv_trials)
  }
  conf_interval_btm_range<-t_test_res$conf.int[1]
  conf_interval_top_range<-t_test_res$conf.int[2]
  conf_level<-arttr(t_test_res, "conf.level")
  if (in_sample_error > conf_interval_btm_range && in_sample_error < conf_interval_top_range){
    message(paste("in sample mean is in range of", conf_level, "CI", sep = " "))
  } else {
    message(paste("in sample mean is NOT in range of", conf_level, "CI", sep = " "))
  }
  return (t_test_res)
}

create_categorical_null_model<-function(data, resp_var){
  # the Null model has the same response for every input.
  data$null_var<-1
  formula<-paste(resp_var, "~", null_var, sep = "")
  null_model<-rpart(formula, data = data)
  # null_model_output<-evaluate_model(mull_model, data = data, type = "class") # class=classification model
  # with(data = eval_model_output, mean(model_output != resp_var, na.rm = TRUE))
  return (null_model_output)
}

create_categorical_random_guess_model<-function(null_model_output){
  # the random guess model is just that, a random guess, really not based on inputs.
  null_model_output$random_guess<-mosaic::shuffle(data) # where data is the response variable we are trying to predict
  # with(data = null_model_output, mean(model_output != resp_var, na.rm = TRUE) )
  return (null_model_output)
}

categorical_error_rate<-function(data, resp_var, explain_vars, cp = 0.002){
  formula<-paste(resp_var, explain_vars, sep = "~")
  model <-rpart(formula, data = data, cp = cp)
  model_output <- evaluate_model(model, data = data, type = "class") # classifier model
  # resp_var below cannot be written like.
  cat_error_rate <- with(data = model_output, mean(model_output != resp_var, na.rm = TRUE))
  return (cat_error_rate)
}


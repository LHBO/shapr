if (FALSE) {


  # Setup ---------------------------------------------------------------------------------------
  setwd("~/PhD/Paper2/shapr_devel/shapr/R")
  pkgload::load_all()

  library(xgboost)
  library(shapr)

  data("airquality")
  data <- data.table::as.data.table(airquality)
  data <- data[complete.cases(data), ]

  x_var <- c("Solar.R", "Wind", "Temp", "Month")
  y_var <- "Ozone"

  ind_x_explain <- 1:6
  x_train <- data[-ind_x_explain, ..x_var]
  y_train <- data[-ind_x_explain, get(y_var)]
  x_explain <- data[ind_x_explain, ..x_var]

  # Looking at the dependence between the features
  cor(x_train)

  # Fitting a basic xgboost model to the training data
  model <- xgboost(
    data = as.matrix(x_train),
    label = y_train,
    nround = 20,
    verbose = FALSE
  )

  library(caret)
  fitControl <- trainControl(method = "CV",
                             number = 4,
                             verboseIter = FALSE)
  fit = caret::train(
    x = as.matrix(x_train),
    y = as.vector(y_train),
    method = 'xgbTree',
    tuneGrid = NULL,
    trControl = fitControl,
    verbose = TRUE
  )


  nrow(fit$results)
  predict(fit, as.matrix(x_train))
  predict(fit$finalModel, as.matrix(x_train))

  # Specifying the phi_0, i.e. the expected prediction without any features
  p0 <- mean(y_train)

  {
    approach = "gaussian"
    approach = "vaeac"
    group = NULL
    prediction_zero = p0
    n_combinations = 6
    n_combinations = NULL
    n_samples = 25
    n_batches = 1
    seed = 1
    group = NULL
    keep_samp_for_vS = TRUE
    predict_model = NULL
    get_model_specs = NULL
    is_python = FALSE

    epochs = 10
    save_VAEAC_every_nth_epoch = 3
    num_different_vaeac_initiate = 2
    verbose = TRUE
  }



  # Explain the model ---------------------------------------------------------------------------

  # Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
  # the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
  pkgload::load_all()
  explanation <- explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    approach = approach,
    prediction_zero = p0,
    n_combinations = n_combinations,
    n_batches = n_batches,
    n_samples = n_samples,
    keep_samp_for_vS = keep_samp_for_vS,
    vaeac.epochs = epochs,
    vaeac.save_VAEAC_every_nth_epoch = save_VAEAC_every_nth_epoch,
    vaeac.num_different_vaeac_initiate = num_different_vaeac_initiate ,
    vaeac.verbose = verbose,
    seed = seed
    #method = "not_future"
  )
  explanation$internal$parameters$vaeac.seed
  explanation$internal$parameters$VAEAC$parameters$seed



  # Printing the Shapley values for the test data.
  # For more information about the interpretation of the values in the table, see ?shapr::explain.
  print(explanation$shapley_values)

  # Finally we plot the resulting explanations
  plot(explanation)



  # Check that setting seed works ----------------------------------------------------------------
  explanation_copy <- explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    approach = approach,
    prediction_zero = p0,
    n_combinations = n_combinations,
    n_samples = n_samples,
    n_batches = n_batches,
    keep_samp_for_vS = keep_samp_for_vS,
    vaeac.epochs = epochs,
    vaeac.save_VAEAC_every_nth_epoch = save_VAEAC_every_nth_epoch,
    vaeac.num_different_vaeac_initiate = num_different_vaeac_initiate ,
    vaeac.verbose = verbose,
    seed = seed
  )

  explanation$shapley_values
  explanation_copy$shapley_values

  # Check that they are equal
  all.equal(explanation$internal$output$dt_samp_for_vS, explanation_copy$internal$output$dt_samp_for_vS)

  # So setting the seed works.



  # Pre-trained VAEAC ---------------------------------------------------------------------------
  # send the pre-trained vaeac model
  explanation_pretrained_VAEAC_model <- explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    approach = approach,
    prediction_zero = p0,
    n_combinations = n_combinations,
    n_batches = n_batches,
    n_samples = n_samples,
    keep_samp_for_vS = keep_samp_for_vS,
    vaeac.pretrained_VAEAC_model = explanation$internal$parameters$VAEAC,
    seed = seed
  )

  # Check that they are equal
  all.equal(explanation$internal$output$dt_samp_for_vS, explanation_pretrained_VAEAC_model$internal$output$dt_samp_for_vS)

  # Providing a pre-trained VAEAC model works.






  # Pre-trained VAEAC path ----------------------------------------------------------------------
  # send the pre-trained vaeac path
  explanation_pretrained_VAEAC_model_path <- explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    approach = approach,
    prediction_zero = p0,
    n_combinations = n_combinations,
    n_batches = n_batches,
    n_samples = n_samples,
    keep_samp_for_vS = keep_samp_for_vS,
    vaeac.pretrained_VAEAC_model = explanation$internal$parameters$VAEAC$models$best,
    seed = seed
  )

  # Check that they are equal
  all.equal(explanation$internal$output$dt_samp_for_vS, explanation_pretrained_VAEAC_model_path$internal$output$dt_samp_for_vS)

  # Providing a pre-trained VAEAC path works.




  # Continue training ---------------------------------------------------------------------------
  # Look at the training and validation errors. Not happy and want to train more.
  VAEAC_training_vlb_and_validation_iwae_shapr(explanation,
                                               plot_from_nth_epoch = 2)

  explanation$internal$parameters$VAEAC$models
  explanation$internal$parameters$VAEAC$results
  explanation$internal$parameters$VAEAC$training_time

  # Continue to train the VAEAC model some more epochs
  explanation$internal$parameters$VAEAC =
    continue_train_VAEAC_model_shapr(explanation = explanation,
                                     epochs_new = 10,
                                     training_data = x_train,
                                     verbose = TRUE)

  explanation$internal$parameters$VAEAC$training_time
  explanation$internal$parameters$VAEAC$models
  explanation$internal$parameters$VAEAC$results

  # Look at the training and validation errors. Still not sure
  VAEAC_training_vlb_and_validation_iwae_shapr(explanation,
                                               plot_from_nth_epoch = 2)

  # Continue to train the VAEAC model some more epochs
  explanation$internal$parameters$VAEAC =
    continue_train_VAEAC_model_shapr(explanation = explanation,
                                     epochs_new = 30,
                                     training_data = x_train,
                                     verbose = TRUE)

  explanation$internal$parameters$VAEAC$training_time
  explanation$internal$parameters$VAEAC$models
  explanation$internal$parameters$VAEAC$results

  # Look at the training and validation errors.
  VAEAC_training_vlb_and_validation_iwae_shapr(explanation,
                                               plot_from_nth_epoch = 2)

  # Can also see how well VAEAC generates data from the full joint distribution
  ggpairs_plot_imputed_and_true_data_shapr(explanation = explanation,
                                           which_vaeac_model = "best",
                                           true_data = x_train)
  ggpairs(x_train) # Bimodal data for solar.R.

  # Use extra trained VAEAC model to compute Shapley values again.
  explanation_extra_trained_VAEAC_model <- explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    approach = approach,
    prediction_zero = p0,
    n_combinations = n_combinations,
    n_batches = n_batches,
    n_samples = n_samples,
    keep_samp_for_vS = keep_samp_for_vS,
    vaeac.pretrained_VAEAC_model = explanation$internal$parameters$VAEAC,
    seed = seed
  )

  # We see that the Shapley values have changed
  print(explanation$shapley_values)
  print(explanation_extra_trained_VAEAC_model$shapley_values)


  # Can see that the extra trainin has decreased the MSE_Frye evaluation criterion
  evaluate_approach(explanation)$mse_frye
  evaluate_approach(explanation_extra_trained_VAEAC_model)$mse_frye



  ## Check n_combinations and more batches -------------------------------------------------------
  # send the pre-trained vaeac path
  explanation_batches_combinations <- explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    approach = approach,
    prediction_zero = p0,
    n_combinations = 10,
    n_batches = 5,
    n_samples = n_samples,
    keep_samp_for_vS = keep_samp_for_vS,
    vaeac.pretrained_VAEAC_model = explanation$internal$parameters$VAEAC,
    seed = seed,
    vaeac.verbose = TRUE
  )

  # Gives different values, as samples are generated in different batches,
  # hence, setting the seed does not help.
  explanation_batches_combinations$shapley_values
  explanation$shapley_values

  explanation_batches_combinations$internal$objects$X

  evaluate_approach(explanation_batches_combinations)



  # Group ---------------------------------------------------------------------------------------
  group_list <- list(A = c("Temp", "Month"), B = c("Wind", "Solar.R"))
  approach = "vaeac"
  n_combinations = NULL
  n_batches = 2
  explanation_group <- explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    approach = "vaeac",
    prediction_zero = p0,
    group = group_list,
    n_combinations = n_combinations,
    n_batches = n_batches,
    n_samples = n_samples,
    keep_samp_for_vS = keep_samp_for_vS,
    vaeac.epochs = 10,
    vaeac.num_different_vaeac_initiate = 3,
    vaeac.verbose = TRUE
  )
  #explanation_group$internal$objects

  # Printing the Shapley values for the test data.
  # For more information about the interpretation of the values in the table, see ?shapr::explain.
  print(explanation_group$shapley_values)

  # Finally we plot the resulting explanations
  plot(explanation_group)



  explanation_group$internal$objects$X



  # Bug -----------------------------------------------------------------------------------------
  group_list <- list(A = c("Temp", "Month"), B = c("Wind", "Solar.R"))
  n_combinations = 4 # Her så er n_combinations = 4
  n_batches = 3 # Koden vil kræsje når n_batches > n_combinations - 2
  # Dette gjelder også når vi fjerner group, men da blir n_combinations 16.
  explanation_group <- explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    approach = "gaussian",
    prediction_zero = p0,
    group = group_list,
    n_combinations = n_combinations,
    n_batches = n_batches,
    n_samples = n_samples,
    keep_samp_for_vS = keep_samp_for_vS,
    vaeac.epochs = 10,
    vaeac.num_different_vaeac_initiate = 3,
    vaeac.verbose = TRUE
  )

  # Denne krasæjer også
  n_combinations = 10 # Her så er n_combinations = 4
  n_batches = 10 # Koden vil kræsje når n_batches > n_combinations - 2
  explanation_group2 <- explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    approach = "gaussian",
    prediction_zero = p0,
    n_combinations = n_combinations,
    n_batches = n_batches,
    n_samples = n_samples,
    keep_samp_for_vS = keep_samp_for_vS,
    vaeac.epochs = 4,
    vaeac.num_different_vaeac_initiate = 3,
    vaeac.verbose = TRUE
  )



  # Mixed data ----------------------------------------------------------------------------------
  library(ranger)
  #library(shapr)
  #pkgload::load_all()
  data("Boston", package = "MASS")
  x_var <- c("lstat", "rm", "dis", "indus")
  y_var <- "medv"

  # Convert two features as factors
  dt <- Boston[, c(x_var, y_var)]
  dt$rm <- as.factor(round(dt$rm/3))
  dt$dis <- as.factor(round(dt$dis/4))

  xy_train_cat <- dt[-1:-6, ]
  y_train_cat <- dt[-1:-6, y_var]
  x_train_cat <- dt[-1:-6, x_var]
  x_test_cat <- dt[1:6, x_var]

  # Fit a basic linear regression model to the training data
  model <- ranger(medv ~ lstat + rm + dis + indus, data = xy_train_cat)

  # Specifying the phi_0, i.e. the expected prediction without any features
  p0 <- mean(y_train_cat)

  approach = "ctree"
  prediction_zero = p0
  n_combinations = 6
  n_combinations = NULL
  group = NULL
  n_samples = 25
  n_batches = 1
  seed = 1
  keep_samp_for_vS = TRUE
  predict_model = NULL
  get_model_specs = NULL
  is_python = FALSE

  # Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
  # the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
  explanation_ctree <- explain(
    model = model,
    x_explain = x_test_cat,
    x_train = x_train_cat,
    approach = "ctree",
    prediction_zero = p0,
    n_combinations = n_combinations,
    n_batches = n_batches,
    keep_samp_for_vS = keep_samp_for_vS,
    group = group,
    ctree.minbucket = 10
  )
  explanation_ctree$internal$objects


  # Printing the Shapley values for the test data.
  # For more information about the interpretation of the values in the table, see ?shapr::explain.
  print(explanation_ctree$shapley_values)

  # Finally we plot the resulting explanations
  plot(explanation_ctree, plot_phi0 = FALSE)



  # Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
  # the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
  explanation_vaeac <- explain(
    model = model,
    x_explain = x_test_cat,
    x_train = x_train_cat,
    approach = "vaeac",
    prediction_zero = p0,
    n_combinations = n_combinations,
    n_batches = n_batches,
    n_samples = n_samples,
    keep_samp_for_vS = keep_samp_for_vS,
    vaeac.epochs = 20,
    vaeac.num_different_vaeac_initiate = 4,
    vaeac.width = 10,
    vaeac.depth = 2,
    vaeac.latent_dim = 4,
    vaeac.use_skip_connections = FALSE,
    vaeac.use_skip_connections_between_masked_encoder_and_decoder = FALSE,
    vaeac.verbose = TRUE
  )
  #explanation_vaeac$internal
  plot(explanation_vaeac$internal$parameters$VAEAC$results$train_vlb)
  plot(explanation_vaeac$internal$parameters$VAEAC$results$validation_iwae)

  # Printing the Shapley values for the test data.
  # For more information about the interpretation of the values in the table, see ?shapr::explain.
  print(explanation_vaeac$shapley_values)

  # Finally we plot the resulting explanations
  plot(explanation_vaeac, plot_phi0 = FALSE)


  # Can see that the extra trainin has decreased the MSE_Frye evaluation criterion
  evaluate_approach(explanation_ctree)$mse_frye
  evaluate_approach(explanation_vaeac)$mse_frye

  explanation_vaeac_complex <- explain(
    model = model,
    x_explain = x_test_cat,
    x_train = x_train_cat,
    approach = "vaeac",
    prediction_zero = p0,
    n_combinations = n_combinations,
    n_batches = n_batches,
    keep_samp_for_vS = keep_samp_for_vS,
    group = group,
    vaeac.epochs = 100,
    vaeac.num_different_vaeac_initiate = 4,
    vaeac.width = 10,
    vaeac.depth = 2,
    vaeac.latent_dim = 4,
    vaeac.use_skip_connections = TRUE,
    vaeac.use_skip_connections_between_masked_encoder_and_decoder = TRUE,
    vaeac.verbose = TRUE
  )

  plot(explanation_vaeac_complex$internal$parameters$VAEAC$results$train_vlb)
  plot(explanation_vaeac_complex$internal$parameters$VAEAC$results$validation_iwae)

  # Printing the Shapley values for the test data.
  # For more information about the interpretation of the values in the table, see ?shapr::explain.
  print(explanation_vaeac_complex$shapley_values)

  # Finally we plot the resulting explanations
  plot(explanation_vaeac_complex, plot_phi0 = FALSE)


  # Can see that the extra trainin has decreased the MSE_Frye evaluation criterion
  evaluate_approach(explanation_ctree)$mse_frye
  evaluate_approach(explanation_vaeac)$mse_frye
  evaluate_approach(explanation_vaeac_complex)$mse_frye










  # Bug -----------------------------------------------------------------------------------------
  data("airquality")
  data <- data.table::as.data.table(airquality)
  data <- data[complete.cases(data), ]

  x_var <- c("Solar.R", "Wind", "Temp", "Month")
  y_var <- "Ozone"

  ind_x_explain <- 1:6
  x_train <- data[-ind_x_explain, ..x_var]
  y_train <- data[-ind_x_explain, get(y_var)]
  x_explain <- data[ind_x_explain, ..x_var]

  # Looking at the dependence between the features
  cor(x_train)

  # Fitting a basic xgboost model to the training data
  model <- xgboost(
    data = as.matrix(x_train),
    label = y_train,
    nround = 20,
    verbose = FALSE
  )

  # Specifying the phi_0, i.e. the expected prediction without any features
  p0 <- mean(y_train)

  explanation_bug <- explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    approach = "vaeac",
    prediction_zero = p0,
    n_combinations = 5,
    n_batches = 1,
    n_samples = n_samples,
    keep_samp_for_vS = keep_samp_for_vS,
    seed = seed
    #method = "not_future"
  )




  # Large example -------------------------------------------------------------------------------
  library(data.table)
  n = 100
  p = 10
  data = data.table(matrix(rnorm(n*p), ncol = p, nrow = n))
  data_with_response = copy(data)
  data_with_response$response = rowSums(data)

  model = lm(response ~ . , data = data_with_response)

  library(progressr)
  handlers("txtprogressbar")
  with_progress({
    explanation <- explain(
      model = model,
      x_explain = data[1:2,],
      x_train = data,
      approach = "gaussian",
      prediction_zero = 10,
      n_samples = 50,
      n_combinations = NULL,
      n_batches = 10,
      keep_samp_for_vS = TRUE
    )})
  explanation$internal$objects

}











# Paper3 playing around -----------------------------------------------------------------------
{
  library(xgboost)
  library(data.table)

  data("airquality")
  data <- data.table::as.data.table(airquality)
  data <- data[complete.cases(data), ]

  x_var <- c("Solar.R", "Wind", "Temp", "Month")
  y_var <- "Ozone"

  ind_x_explain <- 1:6
  x_train <- data[-ind_x_explain, ..x_var]
  y_train <- data[-ind_x_explain, get(y_var)]
  x_explain <- data[ind_x_explain, ..x_var]

  # Fitting a basic xgboost model to the training data
  model <- xgboost::xgboost(
    data = as.matrix(x_train),
    label = y_train,
    nround = 20,
    verbose = FALSE
  )

  # Specifying the phi_0, i.e. the expected prediction without any features
  p0 <- mean(y_train)

  # Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
  # the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
  explanation <- explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    approach = "gaussian",
    prediction_zero = p0,
    keep_samp_for_vS = TRUE
  )

  # Printing the Shapley values for the test data.
  # For more information about the interpretation of the values in the table, see ?shapr::explain.
  print(explanation$shapley_values)

  # Plot the resulting explanations for observations 1 and 6
  plot(explanation, plot_phi0 = FALSE, index_x_explain = c(1, 6))





  explanation$internal$output$dt_vS

  explanation$internal$output$dt_samp_for_vS[,.N, by = c("id", "id_combination")]

  explanation$internal$output$dt_samp_for_vS[,mean(p_hat), by = c("id", "id_combination")]


  explanation$internal$output$dt_vS

  internal = explanation$internal


  compute_vS(internal, model, predict_model, method = method)


  finalize_explanation(explanation$internal$output$dt_vS, explanation$internal)




  dt_vS = explanation$internal$output$dt_vS
  W = internal$objects$W

  kshap <- t(W %*% as.matrix(dt_vS[, -"id_combination"]))
  dt_kshap <- data.table::as.data.table(kshap)

  dt_kshap
  explanation$shapley_values


  W

  test_obs = as.matrix(dt_vS[, -"id_combination"][,1])

  W * t(test_obs)
  aux1 = sweep(x = W, MARGIN = 2, STATS = test_obs, FUN = "*")

  rowSums(aux1)
  t(W %*% test_obs)


  p0

  aux2 = aux1
  aux2

  aux3 = apply(aux2, 1, cumsum)



  matplot(aux3, type = "l", lty = 1, xlab = "Number of Coalitions", ylab = "Shapley values")


  aux4 = sweep(x = aux3, MARGIN = 2, STATS = t(W %*% test_obs), FUN = "-")

  aux4
  matplot(aux4, type = "l", lty = 1, xlab = "Number of Coalitions", ylab = "Shapley values")


  matplot(abs(aux4), type = "l", lty = 1, xlab = "Number of Coalitions", ylab = "Absolute Shapley values")



  # IF we order each feature by itself.
  aux_without_null = aux1[-1,]

  # Get the Shapley values based on all coalitions
  rowSums(aux_without_null)

  # Get the Shapley values based on increasing the number of coalitions.
  # Here we include the additional coalitions in the order they are made in the S matrix in shapr
  apply(aux_without_null, 1, cumsum)

  aux_without_null
  a1 = apply(abs(aux_without_null), 1, sort, decreasing = TRUE)
  a1
  a2 = apply(abs(aux_without_null), 1, order, decreasing = TRUE)
  a2

  a3 = sapply(seq(ncol(a2)), function(col_ind) aux_without_null[col_ind, a2[,col_ind]])

  # Get the Shapley values
  colSums(a3)
  a3

  a4 = apply(a3, 2, cumsum)
  a4
  matplot(a4, type = "l", lty = 1, xlab = "Number of Coalitions", ylab = "Shapley values")

  a5 = sweep(x = a4, MARGIN = 2, STATS = t(W %*% test_obs)[-1], FUN = "-")
  matplot(a5, type = "l", lty = 1, xlab = "Number of Coalitions", ylab = "Shapley values")

  matplot(abs(a5), type = "l", lty = 1, xlab = "Number of Coalitions", ylab = "Abs. Shapley values")

  plot(rowMeans(abs(a5)), type = "l", lty = 1, xlab = "Number of Coalitions", ylab = "MAE (Shapley values)")













  # Sampling of few combinations/coalitions
  explanation_few_comb <- explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    approach = "gaussian",
    prediction_zero = p0,
    keep_samp_for_vS = TRUE,
    n_combinations = 10
  )


  explanation_few_comb$internal$objects








  # Parameters
  M = 5
  mu = rep(0, times = M)
  rho = 0.7 # Old
  rho = 0.3
  sigma = matrix(rho, ncol = M, nrow = M) # Old
  for (i in seq(1, M-1)) {
    for (j in seq(i+1, M))
      sigma[i,j] = sigma[j,i] = rho^abs(i-j)
  }
  diag(sigma) = 1
  sigma

  # Make Gaussian data
  library(mvtnorm)
  library(condMVNorm)
  n_train = 1000
  n_test = 250
  set.seed(1996)
  data_train = data.table(rmvnorm(n = n_train, mean = mu, sigma = sigma))
  data_test  = data.table(rmvnorm(n = n_test,  mean = mu, sigma = sigma))
  colnames(data_train) = paste("X", seq(M), sep = "")
  colnames(data_test) = paste("X", seq(M), sep = "")

  # # True model: Y = 1 + 1*X1 + 0*X2 - 2*X3 + 1.5*X4*X5 #OLD
  # response_train = unlist(1 + 1*data_train[,1] + 0*data_train[,2] - 2*data_train[,3] + 1.5*data_train[,4]*data_train[,5])
  # response_test = unlist(1 + 1*data_test[,1] + 0*data_test[,2] - 2*data_test[,3] + 1.5*data_test[,4]*data_test[,5])

  # True model: Y = 15 + 1*X1 + 0*X2 - 5*X3 + 1.5*X4*X5^2
  response_train = unlist(15 + 1*data_train[,1] + 0*data_train[,2] - 5*data_train[,3] + 1.5*data_train[,4]*data_train[,5]^2)
  response_test = unlist(15 + 1*data_test[,1] + 0*data_test[,2] - 5*data_test[,3] + 1.5*data_test[,4]*data_test[,5]^2)

  # Just take a look at pairs plot
  plot(data_train$X4, data_train$X5)
  plot(data_train$X4, data_train$X5^2)


  # Put together the data
  data_train_with_response = copy(data_train)[,y := response_train]
  data_test_with_response  = copy(data_test) [,y := response_test]


  hist(response_train, breaks = 50)


  # Fit a gam model
  library(mgcv)
  predictive_model_s = gam(y ~ s(X1) + s(X2) + s(X3) + s(X4) + s(X5) + s(X4, X5), data = data_train_with_response)
  predictive_model_s
  predictive_model = gam(y ~ ti(X1) + ti(X2) + ti(X3) + ti(X4) + ti(X5) + ti(X4, X5), data = data_train_with_response)
  predictive_model


  # Test it
  mean((predict(predictive_model_s, data_test_with_response) - data_test_with_response$y)^2)
  mean((predict(predictive_model, data_test_with_response) - data_test_with_response$y)^2)

  plot(data_test_with_response$y, predict(predictive_model_s, data_test_with_response))
  plot(data_test_with_response$y, predict(predictive_model, data_test_with_response))


  #
  prediction_zero = mean(response_train)

  # Sampling of few combinations/coalitions
  explanation_all_coalitions <- explain(
    model = predictive_model,
    x_explain = data_test,
    x_train = data_train,
    approach = "gaussian",
    prediction_zero = prediction_zero,
    keep_samp_for_vS = TRUE,
    n_combinations = 100,
    n_samples = 1000
  )

  explanation_20_coalitions_pre <- explain(
    model = predictive_model,
    x_explain = data_test,
    x_train = data_train,
    approach = "gaussian",
    prediction_zero = prediction_zero,
    keep_samp_for_vS = TRUE,
    n_combinations = 20,
    n_batches = 1,
    n_samples = 1000,
    precomputed_vS = explanation_all_coalitions$internal$output
  )

  mean_absolute_and_squared_errors(explanation_all_coalitions$shapley_values, explanation_20_coalitions$shapley_values)$mae
  mean_absolute_and_squared_errors(explanation_all_coalitions$shapley_values, explanation_20_coalitions_pre$shapley_values)$mae


  explanation_2_coalitions <- explain(
    model = predictive_model,
    x_explain = data_test,
    x_train = data_train,
    approach = "gaussian",
    prediction_zero = prediction_zero,
    keep_samp_for_vS = TRUE,
    n_combinations = 2,
    n_batches = 1,
    n_samples = 1000
  )

  explanation_6_coalitions <- explain(
    model = predictive_model,
    x_explain = data_test,
    x_train = data_train,
    approach = "gaussian",
    prediction_zero = prediction_zero,
    keep_samp_for_vS = TRUE,
    n_combinations = 6,
    n_batches = 1,
    n_samples = 1000
  )

  explanation_6_coalitions$internal$objects$X


  explanation_10_coalitions <- explain(
    model = predictive_model,
    x_explain = data_test,
    x_train = data_train,
    approach = "gaussian",
    prediction_zero = prediction_zero,
    keep_samp_for_vS = TRUE,
    n_combinations = 10,
    n_batches = 1,
    n_samples = 1000
  )



  explanation_20_coalitions <- explain(
    model = predictive_model,
    x_explain = data_test,
    x_train = data_train,
    approach = "gaussian",
    prediction_zero = prediction_zero,
    keep_samp_for_vS = TRUE,
    n_combinations = 20,
    n_batches = 1,
    n_samples = 1000
  )


  explanation_20_coalitions_v2 <- explain(
    model = predictive_model,
    x_explain = data_test,
    x_train = data_train,
    approach = "gaussian",
    prediction_zero = prediction_zero,
    keep_samp_for_vS = TRUE,
    n_combinations = 20,
    n_samples = 1000,
    n_batches = 1,
    seed = 12345
  )

  explanation_25_coalitions <- explain(
    model = predictive_model,
    x_explain = data_test,
    x_train = data_train,
    approach = "gaussian",
    prediction_zero = prediction_zero,
    keep_samp_for_vS = TRUE,
    n_combinations = 25,
    n_batches = 1,
    n_samples = 1000
  )

  explanation$shapley_values


  explanation_all_coalitions$internal


  explanation


  # Get the errors
  errors_6   = mean_absolute_and_squared_errors(explanation_all_coalitions$shapley_values, explanation_6_coalitions$shapley_values)
  errors_10  = mean_absolute_and_squared_errors(explanation_all_coalitions$shapley_values, explanation_10_coalitions$shapley_values)
  errors_20  = mean_absolute_and_squared_errors(explanation_all_coalitions$shapley_values, explanation_20_coalitions$shapley_values)
  errors_25  = mean_absolute_and_squared_errors(explanation_all_coalitions$shapley_values, explanation_25_coalitions$shapley_values)
  errors_all = mean_absolute_and_squared_errors(explanation_all_coalitions$shapley_values, explanation_all_coalitions$shapley_values)
  errors_20_v2  = mean_absolute_and_squared_errors(explanation_all_coalitions$shapley_values, explanation_20_coalitions_v2$shapley_values)

  errors = list(errors_6,errors_10,errors_20,errors_25,errors_all)
  errors_mae_individual = sapply(errors, "[[", "mae_individual")
  matplot(t(errors_mae_individual), type = "l")
  matplot(t(apply(errors_mae_individual, 2, function(x) {c(quantile(x, c(0.05, 0.5, 0.95)))})), type = "l")

  print(object.size(explanation_all_coalitions), unit = "MB")
  print(object.size(explanation_all_coalitions$internal$output$dt_samp_for_vS), unit = "MB")


  errors_MAE = c(errors_6$mae, errors_10$mae, errors_20$mae, errors_25$mae, errors_all$mae)
  errors_MAE_v2 = c(errors_6$mae, errors_10$mae, errors_20_v2$mae, errors_25$mae, errors_all$mae)
  matplot(cbind(errors_MAE, errors_MAE_v2), type = "l", lty = 1)


  #
  explanation_all_coalitions$internal$output$dt_vS[,1:4]

  explanation_all_coalitions$internal$objects$S
  explanation_all_coalitions$internal$objects$W

  explanation_all_coalitions$internal$objects$X

  explanation_all_coalitions$internal$output$dt_vS



  # Specify the number of combinations we are looking at and the number of repetitions
  # for each of the specified n_combinations.
  # We also specify different seed.
  n_combinations_array = c(10, 15, 20, 25, 30, 32)
  n_repetitions_per_n_combination = 10
  seed_array = seq(n_repetitions_per_n_combination)

  # List structure to store the results
  results = lapply(seq(n_combinations_array), function(not_used) lapply(seq(n_repetitions_per_n_combination), function(not_used) NULL))
  results_MAE = matrix(NA, nrow = length(n_combinations_array), ncol = n_repetitions_per_n_combination)

  # Iterate over the number of combinations we are going to draw.
  n_combinations_idx = 1
  for (n_combinations_idx in seq(length(n_combinations_array))) {

    # Extract the current n_combinations
    current_n_combinations = n_combinations_array[n_combinations_idx]

    # Iterate over the number of times we are repeating the experiments
    repetition_idx = 1
    for (repetition_idx in seq(n_repetitions_per_n_combination)) {

      # Small printout
      cat(sprintf("Working on: n_combinations(=%d) %d of %d and repetition %d of %d.\n",
                  n_combinations, n_combinations_idx, length(n_combinations_array),
                  repetition_idx, n_repetitions_per_n_combination))

      # Extract the current seed
      current_seed = seed_array[repetition_idx]

      # Compute the Shapley values
      current_explanation <- explain(
        model = predictive_model,
        x_explain = data_test,
        x_train = data_train,
        approach = "gaussian",
        prediction_zero = prediction_zero,
        keep_samp_for_vS = FALSE,       # To save memory we temporarily remove
        n_combinations = current_n_combinations,
        seed = current_seed,
        n_samples = 250
      )

      # Compute the MSE and MAE Shapley value errors
      current_explanation$errors = mean_absolute_and_squared_errors(explanation_all_coalitions$shapley_values, current_explanation$shapley_values)

      # print(object.size(current_explanation), units = "MB")

      # Save the explanation and the MAE
      results[[n_combinations_idx]][[repetition_idx]] = current_explanation
      results_MAE[n_combinations_idx, repetition_idx] = current_explanation$errors$mae

      # Small printout and plot to the user
      print(results_MAE)
      matplot(n_combinations_array, results_MAE, xlab = "Number of Coalitions", ylab = "MAE (Shapley values)", type = "l", lty = 1, lwd = 2)
    }
  }

  results_MAE


  results_MSE = t(sapply(1:5, function(n_combinations_idx) sapply(1:10, function(repetition_idx) results[[n_combinations_idx]][[repetition_idx]]$errors$mse)))
  results_MSE
  results_MSE
  matplot(n_combinations_array, results_MSE, xlab = "Number of Coalitions", ylab = "MAE (Shapley values)", type = "l", lty = 1, lwd = 2)



  results[[1]][[2]]$internal$objects$X
  results[[1]][[2]]$errors$mse

  results[[1]][[9]]$internal$objects$X
  results[[1]][[9]]$errors$mse


  #TODO: Logikkfeil i setup_computation som gjør at man ikke kan velge n_combinations = 2^m-1.




  # La oss nå se på en å legge til basert på de som har størst vekt.





  aux =  explanation_all_coalitions$internal$output$dt_vS[,1:4]







  # Plot to just see the Shapley kernel weights of the
  {
    par(mfrow = c(1,1))
    plot(explanation_all_coalitions$internal$objects$X$shapley_weight, ylim = c(0, 1), type = "b",
         xlab = "Coalition index", ylab = "Shapley kernel weights", main = "Shapley kernel weights for the different coalitions")
  }

  # We see that the smallest and largest coalitions are weighted higher than the mid-sized coalitions,
  # however, there are more of them. Nonetheless, if we sum the weights for each coalition size we see that
  # there are still less likely to sample a mid sized coalition size even if there are more of them.
  # This is because the Shapley kernel weight is given by (M-1)/(choose(M,|S|)*|S|*(M-|S|)), while the number
  # of coalitions of size |S| is given by choose(M,|S|). That means that the probability of sampling a coalition
  # of the different sizes are given by (M-1)/(|S|*(M-|S|)).
  table(explanation_all_coalitions$internal$objects$X$shapley_weight)
  unique(explanation_all_coalitions$internal$objects$X$shapley_weight*explanation_all_coalitions$internal$objects$X$N)
  (M-1)/(0:M*(M-0:M))


  # We now progress to the setting where we look at the weights in W matrix which allows us to compute the
  # Shapley values as a matrix product, i.e., Phi = W %*% v(s).
  # Plot to look at how the weights changes with the different coalitions
  {
    par(mfrow = c(3,2))
    for (row_idx in seq(nrow(W))) {
      plot(W[row_idx,], type = "l")
    }
  }

  # Here we remove first column related to phi0, as it is 1 for the empty coalitions and technically 0 for the rest.
  # We see that the weights are symmetric.
  {
    par(mfrow = c(1,2))
    matplot(t(W[-1,]), type = "b", lwd = 1.5, pch = 1, xlab = "Coalition index", ylab = "W weights", main = "Weights for Phi_j")
    abline(h = 0, col = "gray", lwd = 1.5)
    abline(v = 2^(M-1)+0.5, col = "gray", lwd = 1.5)
    legend("bottom", legend = seq(1,5), lty = 1:5, col = 1:5, lwd = 2, pch = 1)

    # Look at them in the absolute value too
    matplot(abs(t(W[-1,])), type = "b", pch = 1, lwd = 1.5, xlab = "Coalition index", ylab = "Absolute W weights", main = "Absolute weights for Phi_j", ylim = c(0, max(abs(t(W[-1,])))))
    abline(h = 0, col = "gray", lwd = 1.5)
    abline(v = 2^(M-1)+0.5, col = "gray", lwd = 1.5)
    legend("top", legend = seq(1,5), lty = 1:5, col = 1:5, lwd = 2,  pch = 1)
  }

  # These weights are then matrix multiplied with the v(s) to form R1 + R2 +... R(n_combinations) and these
  # summed together form the Shapley values phi.
  # So the point of article three is to not only use the Shapley kernel weights in the sampling procedure,
  # but also to include pilot estimates of v(s), such that we can see if some of the R's are very important
  # and that the corresponding coalition should be include when we properly estimate the Shapley values.
  # Recall that we compute the R's at an individual basis, thus, we can get that different coalitions
  # are important for different test observations.
  # There are two ways to handle this problem:
  #   1) Use a method which does not need to train a new method for each coalition, or one that does so fast.
  #      Here we can use for example independence, empirical, parametric, vaeac or a fast regression method.
  #   2) We can average over the test observations and use the coalitions which are the most important on average,
  #      and then just focus on training good methods for these coalitions.



  # We will now look at a the size of the R's for some individuals

  # Extract the contribution functions and the W matrix (phi = W %*% v)
  dt_vS = explanation_all_coalitions$internal$output$dt_vS
  W = explanation_all_coalitions$internal$objects$W

  # Just to check that these multiplied together form the Shapley values
  kshap <- t(W %*% as.matrix(dt_vS[, -"id_combination"]))
  dt_kshap <- data.table::as.data.table(kshap)
  colnames(dt_kshap) = colnames(explanation_all_coalitions$shapley_values)

  # Check that they are the same as the shapr::explanaition function output
  all.equal(dt_kshap, explanation_all_coalitions$shapley_values)

  # We look at the v(s) for different test observations
  # Can also look at several at the same time.
  # We see that there is some variability
  par(mfrow = c(1,1))
  matplot(as.matrix(dt_vS[, -"id_combination"])[,c(1,4,5,10,200)], type = "b", lwd = 1.5, pch = 1, lty = 1,
          xlab = "Coalition index", ylab = "Contribution function v(S)", main = "V(s) for different test observations")

  # The variability is further seen when we include all test observations
  matplot(as.matrix(dt_vS[, -"id_combination"])[,], type = "l", lwd = 1.5, lty = 1,
          xlab = "Coalition index", ylab = "Contribution function v(S)", main = "V(s) for different test observations")

  # Here we plot summaries for all test observations, as the previous plot is chaotic.
  {
    par(mfrow = c(1,1), bty = "l")
    dt_vS_summarized = cbind(
      apply(as.matrix(dt_vS[, -"id_combination"]), 1, min),
      apply(as.matrix(dt_vS[, -"id_combination"]), 1, quantile, 0.05),
      apply(as.matrix(dt_vS[, -"id_combination"]), 1, quantile, 0.25),
      apply(as.matrix(dt_vS[, -"id_combination"]), 1, quantile, 0.5),
      apply(as.matrix(dt_vS[, -"id_combination"]), 1, quantile, 0.75),
      apply(as.matrix(dt_vS[, -"id_combination"]), 1, quantile, 0.95),
      apply(as.matrix(dt_vS[, -"id_combination"]), 1, max),
      apply(as.matrix(dt_vS[, -"id_combination"]), 1, mean)
    )
    matplot(dt_vS_summarized, col = c(2,3,4,5,4,3,2,1), lty = c(2,3,4,5,4,3,2,1), type = "b", pch = 1, lwd = 1.5,
            xlab = "Coalition index", ylab = "Contribution function v(S)",
            main = "Summarizes of the V(s) for all test observations")
    legend("topleft", legend = c("max and min", "0.05 and 0.95 quantile", "0.25 and 0.75 quantile", "median", "mean"),
           col = c(2,3,4,5,1), lty = c(2,3,4,5,1), pch = 1, lwd = 1.5, bty = "n")
  }


  # look at which coalitions index is which coalition
  # All the coalitions with the low min and high max are the coalitions where x3 is known, i.e., in S.
  # This makes sense as it is a very important feature in our model.
  explanation_all_coalitions$internal$objects$S[order(dt_vS_summarized[,1], decreasing = FALSE), ] # HERE WE LOOK AT MIN
  explanation_all_coalitions$internal$objects$S[order(dt_vS_summarized[,7], decreasing = TRUE), ]  # HERE WE LOOK AT MAX

  # We see that x3 are included most in the minimum (the maximum will be ish identical)
  apply(explanation_all_coalitions$internal$objects$S[order(dt_vS_summarized[,1]), ], 2, cumsum)
  {
    matplot(apply(explanation_all_coalitions$internal$objects$S[order(dt_vS_summarized[,1]), ], 2, cumsum),
            lty = 1:5, pch = 1, type = "b", lwd = 2,
            xlab = "Number of coalitions", ylab = "Number of times the features has been included")
    legend("topleft", legend = paste("X", 1:M, sep = ""), lty = 1:5, pch = 1, col = 1:M, bty = "n")
  }


  # What if we look at the mean? What do we see then?
  # It is hard to see in the dt_vS_summarized, but the same order ish remains.
  explanation_all_coalitions$internal$objects$S[order(dt_vS_summarized[,8]), ]



  # Se which coalitions are furthest from the empty coalition, when considering the contribution function.
  plot(abs(dt_vS_summarized[,8]-dt_vS_summarized[1,8]))
  explanation_all_coalitions$internal$objects$S[order(abs(dt_vS_summarized[,8]-dt_vS_summarized[1,8]), decreasing = TRUE),]
  apply(explanation_all_coalitions$internal$objects$S[order(abs(dt_vS_summarized[,8]-dt_vS_summarized[1,8]), decreasing = TRUE),], 2, cumsum)
  {
    matplot(apply(explanation_all_coalitions$internal$objects$S[order(abs(dt_vS_summarized[,8]-dt_vS_summarized[1,8]), decreasing = TRUE),], 2, cumsum),
            lty = 1:5, pch = 1, type = "b", lwd = 2,
            xlab = "Number of coalitions", ylab = "Number of times the features has been included")
    legend("topleft", legend = paste("X", 1:M, sep = ""), lty = 1:5, pch = 1, col = 1:M, bty = "n")
  }



  # We just look at one test observation.
  test_obs_idx = 16

  # We want to find the contributions function that contributed the most.
  test_obs_vs = as.matrix(dt_vS[, -"id_combination"])[,test_obs_idx]

  # Plot the v(s) for the the different coalitions for the test observations
  plot(test_obs_vs, type = "b", lwd = 1.5, pch = 1, xlab = "Coalition index", ylab = "Contribution function v(S)",
       main = "Contribution function for the different coalitions for one test observation")
  abline(h = prediction_zero, col = "gray", lwd = 2)

  # We go back and look at just the test observation.
  # Can look at the final Shapley values in many ways.
  t(W %*% test_obs_vs)
  dt_kshap[test_obs_idx,]

  # Compute the R's. I.e., W * v(s), but without adding them together.
  # So not a matrix product, but rather an element wise multiplication.
  R_matrix_for_one_individual = sweep(x = W, MARGIN = 2, STATS = test_obs_vs, FUN = "*")
  # Instead of using sweep, we could have done: t(t(W)*test_obs_vs)
  # This is equivalent. We take the column vector v(s) and elementwise multiply it with the
  # rows in W.

  # Can also do it for all test observations and store the results in a list,
  # where each element in the list corresponds to the
  R_matrix_list_all_individuals = lapply(seq(ncol(dt_vS[, -"id_combination"])), function (test_obs_idx) t(t(W)*as.matrix(dt_vS[, -"id_combination"])[,test_obs_idx]))
  # Check that we got the same, and yes we did.
  all.equal(R_matrix_for_one_individual, R_matrix_list_all_individuals[[test_obs_idx]])

  # From Efficiency axiom, we have that Sum_{j=1}^M phi_j = v(M) - v(Ø) = f(x) - y_bar = f(x) - phi_0.
  # Check that is correct.
  # First version checks that Sum_{j=0}^M phi_j = f(x). Correct. Note that j starts at 0 here.
  # Only machine tolerance error
  sum(rowSums(R_matrix_for_one_individual))
  sum(R_matrix_for_one_individual)
  explanation_all_coalitions$pred_explain[test_obs_idx]
  all.equal(sum(rowSums(R_matrix_for_one_individual)), explanation_all_coalitions$pred_explain[test_obs_idx])

  # First version checks that Sum_{j=1}^M phi_j = f(x) - y_bar. Correct. Note that j starts at 1 here.
  # Only machine tolerance error
  sum(rowSums(R_matrix_for_one_individual)[-1])
  explanation_all_coalitions$pred_explain[test_obs_idx] - prediction_zero
  all.equal(sum(rowSums(R_matrix_for_one_individual)[-1]), explanation_all_coalitions$pred_explain[test_obs_idx] - prediction_zero)




  # If we sum the R's, that is the same as doing the matrix multiplication and we get
  rowSums(R_matrix_for_one_individual)
  t(W %*% test_obs_vs)
  dt_kshap[test_obs_idx,]
  rowSums(t(t(W) * test_obs_vs))

  # TODO: WHy does these become phi0=1 and then 0 for the feature.
  rowSums(t(t(W) * test_obs_vs[1]))
  rowSums(t(t(W) * 1))
  rowSums(t(t(W)))
  rowSums(W)

  {
    par(mfrow = c(3,1))
    # Plot the v(s) for the the different coalitions for the test observations
    plot(test_obs_vs, type = "b", lwd = 1.5, pch = 1, xlab = "Coalition index", ylab = "Contribution function v(S)",
         main = "Contribution function for the different coalitions for one test observation")
    abline(h = prediction_zero, col = "gray", lwd = 2)


    # Look at the elements in the W matrix, before we multiply them with the v(S) values for the test observation.
    # Note that they are now perfectly symmetric.
    matplot(t(W[-1,]), type = "b", lwd = 1.5, pch = 1,
            xlab = "Coalition index", ylab = "W elements/terms", main = "Elements/terms in W matrix (phi = W %*% v(S))")
    abline(h = 0, col = "gray", lty = 1, lwd = 1)
    legend("bottom", legend = paste("phi_", 1:M, sep = ""), pch = 1, lty = 1, col = 1:M, bty = "n")

    # We look at the elements/terms that we add together to constitute the phi_j values
    # For the first test observation that I tried.
    # Note that they are now not perfectly symmetric.
    # This means that some of the coalitions will have had a larger impact.

    matplot(t(R_matrix_for_one_individual[-1,]), type = "b", lwd = 1.5, pch = 1,
            xlab = "Coalition index", ylab = "R elements/terms", main = "Elements/terms for Phi_j for a singel test observation")
    abline(h = 0, col = "gray", lty = 1, lwd = 1)
    legend("bottom", legend = paste("phi_", 1:M, sep = ""), pch = 1, lty = 1, col = 1:M, bty = "n")
  }

  # Which feature we want to look more into
  # Note that we add 1 to investigate_feature_number as feature 0 (the phi0) counts as the first feature, so need to offset it by one.
  investigate_feature_number = 4

  # Plot the R values for all test observation for one particular feature.
  {
    par(mfrow = c(2,1))
    matplot(1:2^M, sapply(R_matrix_list_all_individuals, "[", investigate_feature_number+1,), type = "b", lty = 1, pch = 1,
            xlab = "Coalition index", ylab = "R elements/terms", main = paste("Elements/terms for Phi_",investigate_feature_number," for all test observation", sep = ""))
    abline(h = 0)
    matplot(1:2^M, abs(sapply(R_matrix_list_all_individuals, "[", investigate_feature_number+1,)), type = "b", lty = 1, pch = 1,
            xlab = "Coalition index", ylab = "Absolute R elements/terms", main = paste("Absolute Elements/terms for Phi_",investigate_feature_number," for all test observation", sep = ""))
    abline(h = 0)

    # Here we have removed the v(empty) and v(M) since they are not estimated by the method.
    # But it is nice to include them to see their effect/magnitude.
    # matplot(2:(2^M-1), sapply(R_matrix_list_all_individuals, "[", investigate_feature_number+1,)[-c(1, 2^M),], type = "b", lty = 1, pch = 1,
    #         xlab = "Coalition index", ylab = "R elements/terms", main = paste("Elements/terms for Phi_",investigate_feature_number," for all test observation", sep = ""))
    # matplot(2:(2^M-1), abs(sapply(R_matrix_list_all_individuals, "[", investigate_feature_number+1,)[-c(1, 2^M),]), type = "b", lty = 1, pch = 1,
    #         xlab = "Coalition index", ylab = "Absolute R elements/terms", main = paste("Absolute Elements/terms for Phi_",investigate_feature_number," for all test observation", sep = ""))
    #
  }


  # Take a look at which coalitions corresponds to the different coalitions indices.
  explanation_all_coalitions$internal$objects$S
  # True model: Y = 1 + 1*X1 + 0*X2 - 2*X3 + 1.5*X4*X5
  # Here we
  {
    par(mfrow = c(2,3))
    for (investigate_feature_idx in seq(0,M)) {
      matplot(1:2^M, sapply(R_matrix_list_all_individuals, "[", investigate_feature_idx+1,), type = "b", lty = 1, pch = 1,
              xlab = "Coalition index", ylab = "R elements/terms"
              , main = paste("Elements/terms for Phi_",investigate_feature_idx ," for all test observation", sep = ""))
    }

    for (investigate_feature_idx in seq(0,M)) {
      R_matrix_list_all_individuals_one_feature = sapply(R_matrix_list_all_individuals, "[", investigate_feature_idx+1,)
      matplot(1:2^M, rowMeans(R_matrix_list_all_individuals_one_feature), type = "b", lty = 1, pch = 1,
              xlab = "Coalition index", ylab = "R elements/terms",
              main = paste("Elements/terms for Phi_",investigate_feature_idx ," for all test observation", sep = ""))
      abline(h = 0)
    }

    rowMeans(R_matrix_list_all_individuals_one_feature) + rev(rowMeans(R_matrix_list_all_individuals_one_feature))

    for (investigate_feature_idx in seq(0,M)) {
      matplot(1:2^M, abs(sapply(R_matrix_list_all_individuals, "[", investigate_feature_idx+1,)), type = "b", lty = 1, pch = 1,
              xlab = "Coalition index", ylab = "Absolute R elements/terms",
              main = paste("Absolute Elements/terms for Phi_", investigate_feature_idx ," for all test observation", sep = ""))
      abline(h = 0)
    }
  }







  # We see that including the coalition S that only includes the feature of interest,
  # and the coalition where that is the only one that is missing is important to include here.
  # As they have high importance
  # Note that they often seem to be relative negative of each other.
  # This sort of supports the finding of Covert which recommends paired sampling.
  # Because they often cancel each other out ish.
  # TODO: do we see other results for other setups. What about less dependence. Will then middle coalitions
  # which include the feature be more important?
  investigate_feature_number = 2
  plot(apply(abs(sapply(R_matrix_list_all_individuals, "[", investigate_feature_number+1,)[-c(1, 2^M),]), 1, mean), type = "b")

  # Note that mean of all R elements tends to be close to zero.
  # TODO: is this a coincidence or some deeper mathematical meaning?
  mean(sapply(R_matrix_list_all_individuals, "[", investigate_feature_number+1,)[-c(1, 2^M),])

  # sum(apply((sapply(R_matrix_list_all_individuals, "[", investigate_feature_number+1,)[-c(1, 2^M),]), 1, mean))


  # We look at the absolute elements/terms that we add together to constitute the phi_j values
  {
    matplot(abs(t(R_matrix_for_one_individual[-1,])), type = "b", lwd = 1.5, pch = 1,
            xlab = "Coalition index", ylab = "absolute R elements/terms", main = "Absolute elements/terms for Phi_j for a singel test observation",
            ylim = c(0, max(abs(t(R_matrix_for_one_individual[-1,])))))
    abline(h = 0, col = "gray", lty = 1, lwd = 1)
    legend("top", legend = paste("phi_", 1:M, sep = ""), pch = 1, lty = 1, col = 1:M, bty = "n")
  }

  explanation_all_coalitions$internal$data$x_explain[test_obs_idx,]
  explanation_all_coalitions$internal$objects$S[14,]









  ## Look at which coalitions are included ---------------------------------------------------------------------------
  list_of_explanations = list()
  for (idx in seq(M+1, 2^M)) {
    list_of_explanations[[idx]] = explain(
      model = predictive_model,
      x_explain = data_test[1,],
      x_train = data_train,
      approach = "gaussian",
      prediction_zero = prediction_zero,
      keep_samp_for_vS = FALSE,
      n_combinations = idx,
      n_samples = 10,
      n_batches = 1,
      seed = 1
    )
  }
  list_of_explanations[[6]]$internal$objects$X
  list_of_explanations[[7]]$internal$objects$X
  list_of_explanations[[8]]$internal$objects$X




  model = predictive_model
  x_explain = data_test[1,]
  x_train = data_train
  approach = "gaussian"
  prediction_zero = prediction_zero
  keep_samp_for_vS = FALSE
  n_combinations = idx
  n_samples = 10
  n_batches = 1
  seed = 1


# New sampling methods --------------------------------------------------------------------------------------------
  sampling_methods = c("unique",
                      "unique_paired",
                      "non-unique",
                      "chronological_order_increasing",
                      "chronological_order_decreasing",
                      "largest_weights",
                      "largest_weights_combination_size",
                      "smallest_weights",
                      "smallest_weights_combination_size")

  list_repeated_runs_different_methods = list()
  for (sampling_method in sampling_methods) {

  }


  list_repeated_runs_different_methods = list("unique" = repeated_runs_250,
                                              "unique_paired" = repeated_runs_paired_250,
                                              "largest_weights" = repeated_runs_largest_250,
                                              "smallest_weights" = repeated_runs_smallest_250,
                                              "chronological_order_increasing" = repeated_runs_chronological_increasing_250)

  plot_results = aggregate_and_plot_results(repeated_explanations_list = list_repeated_runs_different_methods,
                                            true_explanations = true_explanations_n_samples_5000_seed_2,
                                            evaluation_criterion = "MAE",
                                            level = 0.95,
                                            plot_figures = TRUE)


  true_explanations_n_samples_5000_seed_2$internal$output
  true_explanations$internal$output

  repeated_runs_250 = List_list_of_explanations_unique_250

  repeated_runs_paired_250 = repeated_explanations(model = predictive_model,
                                                   x_explain = data_test[1:100,],
                                                   x_train = data_train,
                                                   approach = "gaussian",
                                                   gaussian.cov_mat = sigma,
                                                   gaussian.mu = mu,
                                                   prediction_zero = prediction_zero,
                                                   keep_samp_for_vS = FALSE,
                                                   n_repetitions = 10,
                                                   n_samples = 250,
                                                   n_batches = 4,
                                                   seed_start_value = 1,
                                                   n_combinations_increment = 2,
                                                   sampling_method = "unique_paired")

  repeated_runs_largest_250 = repeated_explanations(model = predictive_model,
                                                   x_explain = data_test[1:100,],
                                                   x_train = data_train,
                                                   approach = "gaussian",
                                                   gaussian.cov_mat = sigma,
                                                   gaussian.mu = mu,
                                                   prediction_zero = prediction_zero,
                                                   keep_samp_for_vS = FALSE,
                                                   n_repetitions = 10,
                                                   n_samples = 250,
                                                   n_batches = 1,
                                                   seed_start_value = 1,
                                                   n_combinations_increment = 2,
                                                   sampling_method = "largest_weights")

  repeated_runs_smallest_250 = repeated_explanations(model = predictive_model,
                                                    x_explain = data_test[1:100,],
                                                    x_train = data_train,
                                                    approach = "gaussian",
                                                    gaussian.cov_mat = sigma,
                                                    gaussian.mu = mu,
                                                    prediction_zero = prediction_zero,
                                                    keep_samp_for_vS = FALSE,
                                                    n_repetitions = 10,
                                                    n_samples = 250,
                                                    n_batches = 1,
                                                    seed_start_value = 1,
                                                    n_combinations_increment = 2,
                                                    sampling_method = "smallest_weights")

  repeated_runs_chronological_increasing_250 = repeated_explanations(model = predictive_model,
                                                     x_explain = data_test[1:100,],
                                                     x_train = data_train,
                                                     approach = "gaussian",
                                                     gaussian.cov_mat = sigma,
                                                     gaussian.mu = mu,
                                                     prediction_zero = prediction_zero,
                                                     keep_samp_for_vS = FALSE,
                                                     n_repetitions = 10,
                                                     n_samples = 250,
                                                     n_batches = 1,
                                                     seed_start_value = 1,
                                                     n_combinations_increment = 1,
                                                     sampling_method = "chronological_order_increasing")


  repeated_runs_chronological_increasing_250_v2 = repeated_explanations(model = predictive_model,
                                                                     x_explain = data_test[1:100,],
                                                                     x_train = data_train,
                                                                     approach = "gaussian",
                                                                     gaussian.cov_mat = sigma,
                                                                     gaussian.mu = mu,
                                                                     prediction_zero = prediction_zero,
                                                                     keep_samp_for_vS = FALSE,
                                                                     n_repetitions = 10,
                                                                     n_samples = 10,
                                                                     n_batches = 1,
                                                                     seed_start_value = 1,
                                                                     n_combinations_from = 2,
                                                                     n_combinations_increment = 1,
                                                                     use_precomputed_vS = TRUE,
                                                                     sampling_method = c("unique",
                                                                                         "unique_paired"))

  test_fig = aggregate_and_plot_results(repeated_runs_chronological_increasing_250_v2,
                             true_explanations,
                             plot_figures = TRUE)
  test_fig$figures$figure_lines
  test_fig$figures$figure_boxplot

  repeated_runs_chronological_increasing_250_v2


  repeated_runs_paired_250

  tmp = sapply(repeated_runs_paired_250, function(y) {
    sapply(y, function(x) {
      mean_absolute_and_squared_errors(true_explanations_n_samples_5000_seed_2$shapley_values, x$shapley_values)$mae
    })
  })
  matplot(cbind(tmp, res_largest), type = "l", lty = 1)

  matplot(cbind(tmp, res_largest))
  matplot(tmp[,1:5])
  mean_cl_boot(tmp)


  repeated_runs_paired_250[[1]][[6]]$internal$objects$X
  repeated_runs_paired_250[[1]][[8]]$internal$objects$X
  repeated_runs_paired_250[[1]][[10]]$internal$objects$X
  repeated_runs_paired_250[[1]][[12]]$internal$objects$X
  repeated_runs_paired_250[[1]][[14]]$internal$objects$X


  tmp
  median_hilow(tmp[6,])
  apply(tmp, 1, median)
  median_and_ci = apply(tmp, 1, quantile, 0.976, p = c(0.025, 0.5, 0.975))
  tmp_dt = data.table(id = seq(2^M), CI_lower = median_and_ci[1,], median = median_and_ci[2,], CI_upper = median_and_ci[3,])

  tmp_dt2 = tmp_dt[!is.na(tmp_dt$median)]

  ggplot(tmp_dt2, aes(x = id, y = median)) +
    geom_line(na.rm = TRUE) +
    geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.3)



  tmp_list = list("M1" = tmp, "M2" = tmp2)

  repeated_explanations_list = list("Unique" = repeated_runs_250,
                                    "Unique_paired" = repeated_runs_paired_250)
  a = 1
  b = 2
  ll = list(a, b)







  tmp_dt_comb6 = rbindlist(lapply(tmp_list, function(x) {
    median_and_ci = apply(x, 1, quantile, p = c(0.025, 0.5, 0.975))
    tmp_dt = data.table(n_coalitions = seq(nrow(x)), CI_lower = median_and_ci[1,], median = median_and_ci[2,], CI_upper = median_and_ci[3,])
  }), idcol = "Sampling")

  ggplot(tmp_dt_comb[!is.na(tmp_dt_comb$median)], aes(x = n_coalitions, y = median, col = Sampling)) +
    geom_line() +
    geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper, fill = Sampling), alpha = 0.3)


  quantile(0:10, 0.95)

  library(progressr)
  progressr::handlers(global = TRUE)
  progressr::handlers('cli')


  List_list_of_explanations_unique_250 = list()
  for (rep in seq(10)) {
    list_of_explanations_unique_250 = list()
    for (idx in unique(c(seq(M+1, 2^M, 1), 2^M))) {
      print(idx)
      list_of_explanations_unique_250[[idx]] = explain(
        model = predictive_model,
        x_explain = data_test[1:100,],
        x_train = data_train,
        approach = "gaussian",
        prediction_zero = prediction_zero,
        keep_samp_for_vS = FALSE,
        n_combinations = idx,
        n_samples = 250,
        n_batches = 1,
        seed = rep,
        gaussian.cov_mat = sigma,
        gaussian.mu = mu,
        sampling_method = "unique"
      )
    }
    List_list_of_explanations_unique_250[[rep]] = list_of_explanations_unique_250
  }


  list_of_explanations_unique_paired_250 = list()
  for (idx in unique(c(seq(M+1, 2^M, 2), 2^M))) {
    print(idx)
    list_of_explanations_unique_paired_250[[idx]] = explain(
      model = predictive_model,
      x_explain = data_test[1:100,],
      x_train = data_train,
      approach = "gaussian",
      prediction_zero = prediction_zero,
      keep_samp_for_vS = FALSE,
      n_combinations = idx,
      n_samples = 250,
      n_batches = 1,
      seed = 1,
      gaussian.cov_mat = sigma,
      gaussian.mu = mu,
      sampling_method = "unique_paired"
    )
  }

  list_of_explanations_smallest_weights_250 = list()
  for (idx in unique(c(seq(M+1, 2^M, 1), 2^M))) {
    print(idx)
    list_of_explanations_smallest_weights_250[[idx]] = explain(
      model = predictive_model,
      x_explain = data_test[1:100,],
      x_train = data_train,
      approach = "gaussian",
      prediction_zero = prediction_zero,
      keep_samp_for_vS = FALSE,
      n_combinations = idx,
      n_samples = 250,
      n_batches = 1,
      seed = 1,
      gaussian.cov_mat = sigma,
      gaussian.mu = mu,
      sampling_method = "smallest_weights"
    )
  }

  list_of_explanations_smallest_weights_constant_SW_250 = list()
  for (idx in unique(c(seq(M+1, 2^M, 1), 2^M))) {
    print(idx)
    list_of_explanations_smallest_weights_constant_SW_250[[idx]] = explain(
      model = predictive_model,
      x_explain = data_test[1:100,],
      x_train = data_train,
      approach = "gaussian",
      prediction_zero = prediction_zero,
      keep_samp_for_vS = FALSE,
      n_combinations = idx,
      n_samples = 250,
      n_batches = 1,
      seed = 1,
      gaussian.cov_mat = sigma,
      gaussian.mu = mu,
      sampling_method = "smallest_weights_constant_SW"
    )
  }

  list_of_explanations_chronological_order_increasing_250 = list()
  for (idx in unique(c(seq(M+1, 2^M, 1), 2^M))) {
    print(idx)
    list_of_explanations_chronological_order_increasing_250[[idx]] = explain(
      model = predictive_model,
      x_explain = data_test[1:100,],
      x_train = data_train,
      approach = "gaussian",
      prediction_zero = prediction_zero,
      keep_samp_for_vS = FALSE,
      n_combinations = idx,
      n_samples = 250,
      n_batches = 1,
      seed = 1,
      gaussian.cov_mat = sigma,
      gaussian.mu = mu,
      sampling_method = "chronological_order_increasing"
    )
  }

  list_of_explanations_chronological_order_decreasing_250 = list()
  for (idx in unique(c(seq(M+1, 2^M, 1), 2^M))) {
    print(idx)
    list_of_explanations_chronological_order_decreasing_250[[idx]] = explain(
      model = predictive_model,
      x_explain = data_test[1:100,],
      x_train = data_train,
      approach = "gaussian",
      prediction_zero = prediction_zero,
      keep_samp_for_vS = FALSE,
      n_combinations = idx,
      n_samples = 250,
      n_batches = 1,
      seed = 1,
      gaussian.cov_mat = sigma,
      gaussian.mu = mu,
      sampling_method = "chronological_order_decreasing"
    )
  }

  list_of_explanations_largest_weights_1000 = list()
  for (idx in unique(c(seq(M+1, 2^M, 1), 2^M))) {
    print(idx)
    list_of_explanations_largest_weights_1000[[idx]] = explain(
      model = predictive_model,
      x_explain = data_test[1:100,],
      x_train = data_train,
      approach = "gaussian",
      prediction_zero = prediction_zero,
      keep_samp_for_vS = FALSE,
      n_combinations = idx,
      n_samples = 1000,
      n_batches = 5,
      seed = 1,
      gaussian.cov_mat = sigma,
      gaussian.mu = mu,
      sampling_method = "largest_weights"
    )
  }


  true_explanations_n_samples_5000_seed_2 = explain(
    model = predictive_model,
    x_explain = data_test[1:100,],
    x_train = data_train,
    approach = "gaussian",
    prediction_zero = prediction_zero,
    keep_samp_for_vS = FALSE,
    n_combinations = idx,
    n_samples = 5000,
    n_batches = 1,
    seed = 2,
    gaussian.cov_mat = sigma,
    gaussian.mu = mu
  )
  #true_explanations_n_samples_5000_seed_2_backup = true_explanations_n_samples_5000_seed_2
  #true_explanations_n_samples_5000_seed_2 = true_explanations_n_samples_5000_seed_2_backup
  true_explanations_n_samples_5000_seed_2 = list_of_explanations_unique_250[[32]]
  sapply(list_of_explanations_chronological_order_increasing, function(x) {
    mean_absolute_and_squared_errors(true_explanations$shapley_values, x$shapley_values)$mae
  })

  sapply(list_of_explanations_chronological_order_increasing_250, function(x) {
    mean_absolute_and_squared_errors(true_explanations$shapley_values, x$shapley_values)$mae
  })


  res_unique = sapply(list_of_explanations_unique_250, function(x) {
    mean_absolute_and_squared_errors(true_explanations_n_samples_5000_seed_2$shapley_values, x$shapley_values)$mae
  })


  res_unique_repeated = sapply(List_list_of_explanations_unique_250, function(y) {
    sapply(y, function(x) {
      mean_absolute_and_squared_errors(true_explanations_n_samples_5000_seed_2$shapley_values, x$shapley_values)$mae
    })
  })
  matplot(cbind(res_unique_repeated, res_largest), type = "l", lty = 1)


  res_unique_paired = sapply(list_of_explanations_unique_paired_250, function(x) {
    mean_absolute_and_squared_errors(true_explanations_n_samples_5000_seed_2$shapley_values, x$shapley_values)$mae
  })

  res_chron_incre = sapply(list_of_explanations_chronological_order_increasing_250, function(x) {
    mean_absolute_and_squared_errors(true_explanations_n_samples_5000_seed_2$shapley_values, x$shapley_values)$mae
  })

  res_chron_decre = sapply(list_of_explanations_chronological_order_decreasing_250, function(x) {
    mean_absolute_and_squared_errors(true_explanations_n_samples_5000_seed_2$shapley_values, x$shapley_values)$mae
  })



  res_largest = sapply(list_of_explanations_largest_weights_250, function(x) {
    mean_absolute_and_squared_errors(true_explanations_n_samples_5000_seed_2$shapley_values, x$shapley_values)$mae
  })

  res_largest_1000 = sapply(list_of_explanations_largest_weights_1000, function(x) {
    mean_absolute_and_squared_errors(true_explanations_n_samples_5000_seed_2$shapley_values, x$shapley_values)$mae
  })

  res_smallest = sapply(list_of_explanations_smallest_weights_250, function(x) {
    mean_absolute_and_squared_errors(true_explanations_n_samples_5000_seed_2$shapley_values, x$shapley_values)$mae
  })

  res_smallest_constant_SW = sapply(list_of_explanations_smallest_weights_constant_SW_250, function(x) {
    mean_absolute_and_squared_errors(true_explanations_n_samples_5000_seed_2$shapley_values, x$shapley_values)$mae
  })



  matplot(cbind(res_unique, res_chron, res_largest, res_smallest, res_largest_1000), type = "l", lty = 1, lwd = 2, ylim = c(0, 1.5))

  lines(seq_along(res_unique_paired)[!is.na(res_unique_paired)], res_unique_paired[!is.na(res_unique_paired)], lty = 2)



  dt_res = data.table(res_unique, res_unique_paired, res_chron_incre, res_chron_decre, res_largest, res_largest_1000, res_smallest,res_smallest_constant_SW)
  dt_res = data.table(res_smallest,res_smallest_constant_SW)
  dt_res[, n_combinations := .I]
  dt_res
  dt_res_long = melt(data = dt_res,
                     id.vars = "n_combinations",
                     variable.name = "Sampling_method",
                     value.name = "MAE",
                     na.rm = TRUE)
  ggplot(data = dt_res_long, aes(x = n_combinations, y = MAE)) +
    geom_line(linetype = "solid", ggplot2::aes(group = Sampling_method, col = Sampling_method))




# Other stuff -----------------------------------------------------------------------------------------------------








  rowSums(aux1)
  t(W %*% test_obs)


  p0

  aux2 = aux1
  aux2

  aux3 = apply(aux2, 1, cumsum)



  matplot(aux3, type = "l", lty = 1, xlab = "Number of Coalitions", ylab = "Shapley values")


  aux4 = sweep(x = aux3, MARGIN = 2, STATS = t(W %*% test_obs), FUN = "-")

  aux4
  matplot(aux4, type = "l", lty = 1, xlab = "Number of Coalitions", ylab = "Shapley values")


  matplot(abs(aux4), type = "l", lty = 1, xlab = "Number of Coalitions", ylab = "Absolute Shapley values")



  # IF we order each feature by itself.
  aux_without_null = aux1[-1,]

  # Get the Shapley values based on all coalitions
  rowSums(aux_without_null)

  # Get the Shapley values based on increasing the number of coalitions.
  # Here we include the additional coalitions in the order they are made in the S matrix in shapr
  apply(aux_without_null, 1, cumsum)

  aux_without_null
  a1 = apply(abs(aux_without_null), 1, sort, decreasing = TRUE)
  a1
  a2 = apply(abs(aux_without_null), 1, order, decreasing = TRUE)
  a2

  a3 = sapply(seq(ncol(a2)), function(col_ind) aux_without_null[col_ind, a2[,col_ind]])

  # Get the Shapley values
  colSums(a3)
  a3

  a4 = apply(a3, 2, cumsum)
  a4
  matplot(a4, type = "l", lty = 1, xlab = "Number of Coalitions", ylab = "Shapley values")

  a5 = sweep(x = a4, MARGIN = 2, STATS = t(W %*% test_obs)[-1], FUN = "-")
  matplot(a5, type = "l", lty = 1, xlab = "Number of Coalitions", ylab = "Shapley values")

  matplot(abs(a5), type = "l", lty = 1, xlab = "Number of Coalitions", ylab = "Abs. Shapley values")

  plot(rowMeans(abs(a5)), type = "l", lty = 1, xlab = "Number of Coalitions", ylab = "MAE (Shapley values)")





}


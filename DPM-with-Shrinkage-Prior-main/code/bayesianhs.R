library(monomvn); library(lars); library(glmnet); library(miscTools) 
library(readxl)
library(xlsx)
library(caret)

n_rep = 5
mae_all = rep(0,n_rep)
mse_all = rep(0,n_rep)

blasso_cal <- function(seed_ind){
  set.seed(seed_ind)
  df = read.xlsx2("Residential-Building-Data-Set.xlsx", 3, startRow = 2, header = TRUE)
  df["profit"] = log(as.numeric(as.character(df[["sale"]])) - as.numeric(as.character(df[["cost"]])))
  y = as.numeric(as.character(df[["profit"]]))
  df_X = df[,!names(df) %in% c("profit","sale","cost")]
  
  indx <- sapply(df_X, is.factor)
  df_X[indx] <- lapply(df_X[indx], function(x) as.numeric(as.character(x)))
  X = data.matrix(df_X)
  n = dim(X)[1]
  p = dim(X)[2]
  
  
  ### split train test and scale
  smp_size <- floor(0.8 * nrow(X))
  train_ind <- sample(seq_len(nrow(X)), size = smp_size)
  
  X_train = X[train_ind,]
  y_train = y[train_ind]
  X_test = X[-train_ind,]
  y_test = y[-train_ind]
  
  preprocessParams <- preProcess(X_train, method=c("scale"))
  X_train <- predict(preprocessParams, X_train)
  X_test <- predict(preprocessParams, X_test)
  
  
  
  # define the burn-in period, number of mcmc samples to be drawn and initial values 
  burnin <- 2000
  iter <- 5000
  
  
  # initial.beta <- rep(0, dim(X)[2]) # assigning an extreme initial value for all betas
  # initial.lambda2 <- 1 # assigning an extreme initial value for lambda (penalty parameter)
  # initial.variance <- 1 # assigning an extreme initial value for variance parameter
  
  # starting the Gibbs sampler here
  Bhs <- bhs(X = X_train, # covariate matrix with dimensions 442 x 64
                  y = y_train,  # response vector with length of 442
                  T = iter) # number of iterations
  # beta = initial.beta,
  # lambda2 = initial.lambda2,  
  # s2 = initial.variance)
  
  mu_sample = Bhs$mu[-seq(burnin)]
  beta_sample = Bhs$beta[-seq(burnin),]
  
  expectation = rep(0,n)
  mse = 0
  mae = 0
  n_test = dim(X_test)[1]
  for (i in seq(n_test)){
    expectation[i] = mean(mu_sample + beta_sample%*%(t(X_test)[,i]))
    mae = mae + abs(expectation[i] - y_test[i])
    mse = mse + (expectation[i] - y_test[i])^2
  }
  mae = mae / n_test
  mse = mse / n_test
  return(list("mae" = mae, "mse" = mse))
}


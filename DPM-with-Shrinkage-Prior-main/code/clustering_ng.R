library(RcppCNPy)
library(GreedyEPL) # Rastelli method
# library(mcclust.ext) # Wade method
library(fossil) # Rand Index
library(clues) # Adjusted Rand Index
library(argparse)
parser <- ArgumentParser()
parser$add_argument("--n", help="Sample size n", type="integer")
parser$add_argument("--p", help="Number of covariates", type="integer")
parser$add_argument("--J", help="Number of clusters", type="integer")
parser$add_argument("--ind", help="Repetition index", type="integer")

args <- parser$parse_args()
n = args$n
p = args$p
n_clus = args$J
ind = args$ind

model_name = "ng"

dir_current = sprintf("n%d_p%d_clus%d_ind%d/%s/", n,p,n_clus,ind,model_name)

### true clustering
cluster_true <- npyLoad(paste(dir_current, "cluster_true.npy", sep = ""),"integer")
cluster_true = as.vector(cluster_true)

### clustering from least-square rule
cluster_ls <- npyLoad(paste(dir_current, "cluster_ls.npy", sep = ""),"integer")

### load posterior clusterings
Z <- npyLoad(paste(dir_current, "Z.npy", sep = ""),"integer")
Z = t(Z)
Z = Z + 1
dim(Z)

######################################################
### Rastelli Method
######################################################
p__MinimiseAverageVI <- function(sample_of_partitions, weights, decision_init) {
  .Call('GreedyEPL_p__MinimiseAverageVI', PACKAGE = 'GreedyEPL', sample_of_partitions, weights, decision_init)
}

MinimiseEPL_noCollapse <- function(sample_of_partitions, pars = list())
{
  N <- ncol(sample_of_partitions)
  niter <- nrow(sample_of_partitions)
  if (missing(pars) || is.null(pars$weights)) weights = rep(1,niter) else weights = pars$weights
  # cat("\nThe following weigths are being used ", weights)
  if (missing(pars) || is.null(pars$Kup))  Kup = N else Kup = pars$Kup
  if (missing(pars) || is.null(pars$decision_init)) 
  {
    # cat("\nCreating a random starting partition with ", Kup, " groups")
    # decision_init = sample(x = 1:Kup, size = N, replace = T)
    decision_init = sample_of_partitions[1,]
  }
  else decision_init = as.numeric(pars$decision_init)
  if (missing(pars) || is.null(pars$loss_type)) loss_type = "VI" else loss_type = pars$loss_type
  
  if (sum(decision_init <= 0) > 0) stop("Negative entries in decision_init")
  
  # for (iter in 1:niter) sample_of_partitions[iter,] = CollapseLabels(decision = sample_of_partitions[iter,])
  # decision_init = CollapseLabels(decision = decision_init)
  
  if (loss_type == "VI") output <- p__MinimiseAverageVI(sample_of_partitions = sample_of_partitions - 1, weights = weights, decision_init = decision_init - 1)
  else if (loss_type == "B") output <- p__MinimiseAverageB(sample_of_partitions = sample_of_partitions - 1, weights = weights, decision_init = decision_init - 1)
  else if (loss_type == "NVI") output <- p__MinimiseAverageNVI(sample_of_partitions = sample_of_partitions - 1, weights = weights, decision_init = decision_init - 1)
  else if (loss_type == "NID") output <- p__MinimiseAverageNID(sample_of_partitions = sample_of_partitions - 1, weights = weights, decision_init = decision_init - 1)
  else stop("Loss function not recognised")
  
  list(EPL_stored_values = as.numeric(output$EPL_stored_values), EPL = output$EPL, decision = output$decision)
}

Kup <- 20
loss_type = "VI"
output <- MinimiseEPL_noCollapse(Z, list(Kup = Kup, loss_type = loss_type)) 
cluster =  output$decision
cluster = as.vector(cluster)
### check clustering by Rastelli
cluster
unique(cluster)

# cluster_ind_sort = unique(cluster)

# table(cluster)[as.character(cluster_ind_sort)]
# table(cluster)[as.character(unique(cluster))]

### check rand index
rand_rastelli = rand.index(cluster_true, cluster)
ari_rastelli = adjustedRand(cluster_true, cluster)["HA"]

######################################################
### Least-Square Method by Dahl
######################################################
unique(cluster_ls)
cluster_ind_sort = unique(cluster_ls)

rand_dahl = rand.index(cluster_true, cluster_ls)
ari_dahl = adjustedRand(cluster_true, cluster_ls)["HA"]

save(cluster_ind_sort, cluster, cluster_ls, ari_rastelli, ari_dahl, rand_rastelli, rand_dahl, file = paste(dir_current, "cluster.Rdata", sep = ""))


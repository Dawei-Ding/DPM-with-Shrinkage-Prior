import os
import numpy as np
import pickle
import matplotlib.pyplot as plt
import seaborn as sns
import scipy
from scipy.stats import geninvgauss
from scipy.stats import norm
from scipy.stats import wishart
from scipy import special
from sklearn.metrics import silhouette_score
from multiprocessing import Pool, Process
import time
import warnings
import argparse
from collections import Counter
import sklearn.metrics as metrics
warnings.filterwarnings('error')

start = time.time()

model_name = "hs"
# n = 200 # sample size
# p = 50 # number of covariates
# n_clus = 4 # number of clusters

parser = argparse.ArgumentParser()
parser.add_argument("--n", help="Sample size n",
                    type=int)
parser.add_argument("--p", help="Number of covariates",
                    type=int)
parser.add_argument("--J", help="Number of clusters",
                    type=int)
parser.add_argument("--ind", help="repetition index",
                    type=int)

args = parser.parse_args()
n = args.n
p = args.p
n_clus = args.J
ind = args.ind

n_test = 40

error_variance = 1

num_MCMC = 5000
burn_in = 2000

# =============================================================================
# Create folder
# =============================================================================
dir_origin = "n%i_p%i_clus%i_ind%i/" %(n,p,n_clus,ind)
# define the name of the directory to be created
dir_current = "n%i_p%i_clus%i_ind%i/%s/" %(n,p,n_clus,ind,model_name)

try:
    os.mkdir(dir_current)
except OSError:
    print ("Creation of the directory %s failed" % dir_current)
else:
    print ("Successfully created the directory %s " % dir_current)

# =============================================================================
# Parameter Setup
# =============================================================================
### specify cluster index for each sample
cluster_number = np.ones(n_clus) * int(n / n_clus)
if sum(cluster_number) != n:
    warnings.warn('Summation of cluster sizes not equal to total size!')
cluster_number = [int(i) for i in list(cluster_number)]

cluster_cumsum = np.cumsum(cluster_number)
cluster_true = [[i]*cluster_number[i]  for i in range(len(cluster_number))]
cluster_true = np.array([val for sublist in cluster_true for val in sublist])

cluster_index = np.array([])
for i in range(n_clus):
    cluster_index = np.concatenate([cluster_index, np.repeat(i+1,cluster_number[i])]) if cluster_index.size else np.repeat(i+1,cluster_number[i])

### true \mu and \beta    
beta_cand = np.zeros((10, p+1))
beta_cand[0,:6] = np.array([10, 3, 3, 3,3,3])
beta_cand[1,:6] = np.array([8,  3, 3, 3,3,0])
beta_cand[2,:6] = np.array([6,  3, 3, 3,0,0])
beta_cand[3,:6] = np.array([4,  3, 3, 0,0,0])
beta_cand[4,:6] = np.array([2,  3, 0, 0,0,0])
beta_cand[5,:] = (-1) * beta_cand[4,:]
beta_cand[6,:] = (-1) * beta_cand[3,:]
beta_cand[7,:] = (-1) * beta_cand[2,:]
beta_cand[8,:] = (-1) * beta_cand[1,:]
beta_cand[9,:] = (-1) * beta_cand[0,:]

beta_true = beta_cand[:n_clus,:]

mu_true = beta_true[:,0]
beta_true = beta_true[:,1:]

### true error variance
sigma2_true = np.ones(n_clus) * error_variance

### true m and \tau's
m_cand = np.zeros((10,p))
for ii in range(10):
    if ii<5:
        m_cand[ii,:] = np.ones(p) * (2 + 2*ii)
    else:
        m_cand[ii,:] = np.ones(p) * 2 * (ii - 10)
m_true = m_cand[:n_clus,:]

tau_cand = np.ones((10,p))
tau_true = tau_cand[:n_clus,:]

# =============================================================================
# Utility Function
# =============================================================================
def inverseGamma(alpha,theta,size = None):
    tmp = np.random.gamma(shape = alpha, scale = 1/theta, size = size)
    return 1/tmp

# =============================================================================
# Data Generation
# =============================================================================

###### Training Data
with open(dir_origin + "data_train_n%i_p%i_cluster%i.pickle" %(n,p,n_clus),"rb") as f:
    X,y = pickle.load(f)[:2]
x = X[:,1:]

######  Test Data
with open(dir_origin + "data_test_n%i_p%i_cluster%i.pickle" %(n,p,n_clus),"rb") as f:
    X_test,y_test = pickle.load(f)[:2]
x_test = X_test[:,1:]

# =============================================================================
# =============================================================================
# # BNP with Normal-Gamma(Variance-Gamma) prior
# =============================================================================
# =============================================================================

### Hyperparameters Initialization
clus_size_threshold = 1

### DP prior
a = 1
b = 1
b_alpha = 2
b_beta = 2

# Inverse-Gamma param for \sigma2
alpha_1 = 2
theta_1 = 2

# hyperparameters for m's and \tau's
m_0 = np.zeros(p)
n_0 = np.ones(p) * 0.1
nu_0 = np.ones(p) * 2
s2_0 = np.ones(p) * 2

### Initialization
s = 0 # iteration index
K = 2 * n_clus # initial number of clusters

upper = 10 * K
beta_cand = np.zeros((num_MCMC,p,upper))  # store \beta candidates
mu_cand = np.zeros((num_MCMC,1,upper))   # store \mu candidates
m_cand = np.zeros((num_MCMC,p,upper))  # store m candidates
tau_cand = np.zeros((num_MCMC,p,upper))  # store \tau candidates

di = np.random.randint(K, size=n) # initial cluster index for each sample

M = max(di) + 1

# =============================================================================
# Prediction Parameters Initialization
# =============================================================================
## parameters related to y
reg_mu_bnp = np.zeros(num_MCMC)
reg_beta_bnp = np.zeros((p,num_MCMC))
reg_sigma2_bnp = np.zeros(num_MCMC)
reg_lamda2_bnp = np.zeros((p+1,num_MCMC))
reg_tau2_bnp = np.zeros(num_MCMC)
reg_nu_bnp = np.zeros((p+1,num_MCMC))
reg_xi_bnp = np.zeros(num_MCMC)
## parameters related to X
reg_m_bnp = np.zeros((p,num_MCMC))
reg_tau_bnp = np.zeros((p,num_MCMC))

### Initialize \lambda, \gamma^(-2), D_\psi
reg_lamda2 = np.ones(p+1)
reg_tau2 = 1
reg_nu = np.ones(p+1)
reg_xi = 1

### Initialize \mu,\beta
if n > (p+1):
    fi = np.linalg.inv(X.T.dot(X)).dot(X.T).dot(y)
else:
    fi = X.T.dot(np.linalg.inv(X.dot(X.T))).dot(y)
reg_mu = fi[0]
reg_beta = fi[1:]

### Initialize m and \tau's
reg_m = np.zeros(p)
reg_tau = np.ones(p) * 10
### Initialize \sigma^2
reg_sigma2 = inverseGamma(alpha_1,theta_1)


# =============================================================================
# Parameters Initialization
# =============================================================================
## parameters related to y
mu_bnp = np.zeros(num_MCMC)
beta_bnp = np.zeros((p,num_MCMC))
sigma2_bnp = np.zeros(num_MCMC)
lamda2_bnp = np.zeros((p+1,num_MCMC))
tau2_bnp = np.zeros(num_MCMC)
nu_bnp = np.zeros((p+1,num_MCMC))
xi_bnp = np.zeros(num_MCMC)

## parameters related to X
m_bnp = np.zeros((p,num_MCMC))
tau_bnp = np.zeros((p,num_MCMC))

## parameters related to cluster label
b_bnp = np.zeros(num_MCMC)  # store mass parameter of DP prior
di_bnp = np.zeros((n,num_MCMC))  # store cluster index for each sample
K_bnp = np.zeros(num_MCMC)  # store number of clusters

# =============================================================================
# For each cluster, initalize 
# =============================================================================
### Initialize \lambda, \gamma^(-2), D_\psi
lamda2 = []
tau2 = []
nu = []
xi = []
for j in range(M):
    lamda2.append([1]*(p+1))
    tau2.append(1)
    nu.append([1]*(p+1))
    xi.append(1)
lamda2 = np.array(lamda2,dtype=float).T
tau2 = np.array(tau2,dtype=float)
nu = np.array(nu,dtype=float).T
xi = np.array(xi,dtype=float)

### Regular Initialize \mu,\beta
mu = []
for j in range(M):
    mu.append(np.random.normal(loc=0,scale=10))
mu = np.array(mu)

beta = np.array([])
for j in range(M):
    tmp_beta = np.random.multivariate_normal(mean = np.zeros(p), cov = np.diag([10]*p))
    beta = np.hstack([beta,tmp_beta.reshape(-1,1)]) if beta.size else tmp_beta.reshape(-1,1)

### Initialize m and \tau's
m = np.zeros((p,M))
tau = np.ones((p,M)) * 10

### Initialize \sigma^2
sigma2 = inverseGamma(alpha_1,theta_1)

# =============================================================================
# =============================================================================
inc = upper
w_bnp = np.zeros((inc,num_MCMC))

for s in range(num_MCMC):
    if s % 100 == 0:
       print("iteration %i \n" % s)
    
    ###### Step 0: First update parameters of G_0
    ### 0.1 update \mu and \beta    
    reg_A_inv = np.linalg.inv(X.T.dot(X) + np.diag(1/(reg_tau2 * reg_lamda2)))
    reg_mean_fi = reg_A_inv.dot(X.T).dot(y)
    reg_var_fi = reg_A_inv * reg_sigma2

    reg_fi = np.random.multivariate_normal( mean = reg_mean_fi, cov = reg_var_fi, tol = 1e-4 )
    reg_mu = reg_fi[0]
    reg_beta = reg_fi[1:]
    reg_mu_bnp[s] = reg_mu
    reg_beta_bnp[:,s] = reg_beta
    ### 0.2 update \lambda^2
    for l in range(p+1):
        reg_tmp_scale = reg_fi[l]**2 / (2*reg_tau2*reg_sigma2) + 1/reg_nu[l]
        reg_tmp_lamda = inverseGamma(alpha = 1, theta = reg_tmp_scale)
        reg_lamda2[l] = reg_tmp_lamda
    reg_lamda2_bnp[:,s] = reg_lamda2
    ### 0.3 update \tau^2
    reg_tau_shape = 0.5 * (p+2)
    reg_tau_scale = 1/reg_xi + 0.5/reg_sigma2 * np.sum(reg_fi**2/reg_lamda2)
    reg_tau2 = inverseGamma(alpha = reg_tau_shape, theta = reg_tau_scale)
    reg_tau2_bnp[s] = reg_tau2

    ### 0.4 update \nu
    for l in range(p+1):
        reg_tmp_nu = inverseGamma(alpha = 1, theta = 1 + 1/reg_lamda2[l])
        reg_nu[l] = reg_tmp_nu
    reg_nu_bnp[:,s] = reg_nu
    
    ### 0.5 update \xi
    reg_xi = inverseGamma(alpha = 1, theta = 1 + 1/reg_tau2)
    reg_xi_bnp[s] = reg_xi

    ### 0.6   Sample m and \tau
    for l in range(p):
        reg_m_star = (n * np.mean(x[:,l]) + n_0[l]*m_0[l]) / (n + n_0[l])
        reg_n_star = n + n_0[l]
        reg_nu_star = nu_0[l] + n
        reg_s2 = np.std(x[:,l], ddof = 1) ** 2
        reg_s2_star = 1 / reg_nu_star * ( reg_s2*(n-1) + s2_0[l]*nu_0[l] + n_0[l]*n/reg_n_star * (np.mean(x[:,l])-m_0[l])**2 )
        
        reg_tau[l] = inverseGamma(alpha=reg_nu_star/2, theta = (reg_nu_star*reg_s2_star)/2)
        reg_m[l] = np.random.normal(loc = reg_m_star, scale = np.sqrt(reg_tau[l]/reg_n_star) )
    reg_tau_bnp[:,s] = reg_tau
    reg_m_bnp[:,s] = reg_m
    
    ### 0.7   Sample \sigma^2    
    reg_tmp_shape = (n+p+1) / 2 + alpha_1    
    reg_mu_ones = reg_mu * np.ones(n)    
    reg_residual_tmp = y - reg_mu_ones - np.dot(x, reg_beta)
    reg_tmp_rate = 0.5 * sum(reg_residual_tmp**2) + 0.5/reg_tau2 * np.sum(reg_fi**2/reg_lamda2) + theta_1
    reg_sigma2 = inverseGamma(alpha = reg_tmp_shape, theta = reg_tmp_rate)    
    reg_sigma2_bnp[s] = reg_sigma2
    
    
    
    ### Step 1
    M = max(di) + 1
    nj = [0] * inc
    mj = [0] * inc
    for j in range(inc):
        nj[j] = sum(di == j)
        mj[j] = sum(di > j)
    
    v = []
    for j in range(inc):
        v.append(np.random.beta(a = a+nj[j], b = b+mj[j]))

    w = np.concatenate([np.array([1]),np.cumprod(1-np.array(v[0:len(v)-1])) ]) * np.array(v)
    w_bnp[:,s] = w
    u = []
    for i in range(n):
        u.append( np.random.uniform(low=0, high=w[di[i]]) )
    
    if max(np.cumsum(w)) < max([1-i for i in u]):
        N = inc
    else:
        N = np.argmax(np.cumsum(w) > max([1-i for i in u])) + 1

    ### Step 2 Gibbs Sampling for each candidate cluster
    for j in range(M):
        ### when j is less than N^(s-1), use regular Gibbs sampling
        if j < len(tau2):
            ### Step 2.1   Sample \mu and \beta    
            X_tmp = X[di == j,:]
            x_tmp = x[di == j,:]
            y_tmp = y[di == j]
            n_tmp = x_tmp.shape[0]

            A_inv = np.linalg.inv(X_tmp.T.dot(X_tmp) + np.diag(1/(tau2[j] * lamda2[:,j])))
            mean_fi = A_inv.dot(X_tmp.T).dot(y_tmp)
            var_fi = A_inv * sigma2
            fi = np.random.multivariate_normal( mean = mean_fi, cov = var_fi, tol = 1e-4 )
            mu[j] = fi[0]
            beta[:,j] = fi[1:]

            ### Step 2.2   Sample \lambda^2
            for l in range(p+1):
                tmp_scale = fi[l]**2 / (2*tau2[j]*sigma2) + 1/nu[l,j]
                tmp_lamda = inverseGamma(alpha = 1, theta = tmp_scale)
                lamda2[l,j] = tmp_lamda

            ### step 2.3   Sample \tau^2
            tau_shape = 0.5 * (p+2)
            tau_scale = 1/xi[j] + 0.5/sigma2 * np.sum(fi**2/lamda2[:,j])
            tau2[j] = inverseGamma(alpha = tau_shape, theta = tau_scale)
            
            ### Step 2.3   Sample \nu
            for l in range(p+1):
                tmp_nu = inverseGamma(alpha = 1, theta = 1 + 1/lamda2[l,j])
                nu[l,j] = tmp_nu
            
            ### Step 2.4   Sample \xi
            xi[j] = inverseGamma(alpha = 1, theta = 1 + 1/tau2[j])
            
            ### Step 2.5   Sample m and \tau
            if n_tmp > clus_size_threshold:
                for l in range(p):
                    m_star = (n_tmp * np.mean(x_tmp[:,l]) + n_0[l]*m_0[l]) / (n_tmp + n_0[l])
                    n_star = n_tmp + n_0[l]
                    nu_star = nu_0[l] + n_tmp
                    s2 = np.std(x_tmp[:,l], ddof = 1) ** 2
                    s2_star = 1 / nu_star * ( s2*(n_tmp-1) + s2_0[l]*nu_0[l] + n_0[l]*n_tmp/n_star * (np.mean(x_tmp[:,l])-m_0[l])**2 )
                    
                    tau[l,j] = inverseGamma(alpha=nu_star/2, theta = (nu_star*s2_star)/2)
                    m[l,j] = np.random.normal(loc = m_star, scale = np.sqrt(tau[l,j]/n_star) )
            else:
                for l in range(p):
                    tau[l,j] = inverseGamma(alpha = nu_0[l]/2, theta = (nu_0[l]*s2_0[l])/2 )
                    m[l,j] = np.random.normal(loc = m_0[l], scale = np.sqrt(tau[l,j]/n_0[l]) )
        ### when j is equal or greater than N^(s-1), since no previous values available, draw from prior
        else:
            # nu^2
            for l in range(p+1):
                nu[l,j] = inverseGamma(alpha = 0.5, theta = 1)
            # xi
            xi[j] = inverseGamma(alpha = 0.5, theta = 1)
            # lambda^2
            for l in range(p+1):
                lamda2[l,j] = inverseGamma(alpha = 0.5, theta = 1/nu[l,j])
            # tau^2
            tau2[j] = inverseGamma(alpha = 0.5, theta = 1/xi[j])
            # \mu,\beta
            var_fi = sigma2*tau2[j]*lamda2[:,j]
            fi = np.zeros(p+1)
            for l in range(p+1):
                fi[l] = np.random.normal(loc = 0, scale = np.sqrt(var_fi[l]))
            mu[j] = fi[0]
            beta[:,j] = fi[1:]

            # \tau and m
            for l in range(p):
                tau[l,j] = inverseGamma(alpha = nu_0[l]/2, theta = (nu_0[l]*s2_0[l])/2 )
                m[l,j] = np.random.normal(loc = m_0[l], scale = np.sqrt(tau[l,j]/n_0[l]) )
         
    ### Only keep first M cluster's values
    mu = mu[:M]
    beta = beta[:,:M]
    lamda2 = lamda2[:,:M]
    tau2 = tau2[:M]
    nu = nu[:,:M]
    xi = xi[:M]

    m = m[:,:M]
    tau = tau[:,:M]
    
    ### if N>M, then draw parameters from their priors
    if N > M:
        mu = np.concatenate((mu, np.zeros(N-M)))
        beta = np.hstack((beta, np.zeros((p,N-M))))
        lamda2 = np.hstack((lamda2, np.zeros((p+1,N-M))))
        tau2 = np.concatenate((tau2, np.zeros(N-M)))
        nu = np.hstack((nu, np.zeros((p+1,N-M))))
        xi = np.concatenate((xi, np.zeros(N-M)))

        m = np.hstack((m, np.zeros((p,N-M))))
        tau = np.hstack((tau, np.zeros((p,N-M))))
    for j in range(M,N):
        # nu^2
        for l in range(p+1):
            nu[l,j] = inverseGamma(alpha = 0.5, theta = 1)
        # xi
        xi[j] = inverseGamma(alpha = 0.5, theta = 1)
        # lambda^2
        for l in range(p+1):
            lamda2[l,j] = inverseGamma(alpha = 0.5, theta = 1/nu[l,j])
        # tau^2
        tau2[j] = inverseGamma(alpha = 0.5, theta = 1/xi[j])
        # \mu,\beta
        var_fi = sigma2*tau2[j]*lamda2[:,j]
        fi = np.zeros(p+1)
        for l in range(p+1):
            fi[l] = np.random.normal(loc = 0, scale = np.sqrt(var_fi[l]))
        mu[j] = fi[0]
        beta[:,j] = fi[1:]

        # \tau and m
        for l in range(p):
            tau[l,j] = inverseGamma(alpha = nu_0[l]/2, theta = (nu_0[l]*s2_0[l])/2 )
            m[l,j] = np.random.normal(loc = m_0[l], scale = np.sqrt(tau[l,j]/n_0[l]) )

    ### store \beta candidates
    beta_cand[s,:,:beta.shape[1]] = beta
    ### store \mu candidates
    mu_cand[s,:,:mu.shape[0]] = mu
    
    ### store m candidates
    m_cand[s,:,:m.shape[1]] = m
    ### store \tau candidates
    tau_cand[s,:,:tau.shape[1]] = tau

    ### Step 3   
    rho = np.random.uniform(0,1)
    j_clus = np.argmax(np.cumsum(w) > rho) # assign parameters to 'j_clus'th cluster's parameters
    if j_clus >= N:
        # nu^2
        for l in range(p+1):
            nu_bnp[l,s] = inverseGamma(alpha = 0.5, theta = 1)
        # xi
        xi_bnp[s] = inverseGamma(alpha = 0.5, theta = 1)
        # lambda^2
        for l in range(p+1):
            lamda2_bnp[l,s] = inverseGamma(alpha = 0.5, theta = 1/nu_bnp[l,s])
        # tau^2
        tau2_bnp[s] = inverseGamma(alpha = 0.5, theta = 1/xi_bnp[s])
        # \mu,\beta
        var_fi = sigma2_bnp[s]*tau2_bnp[s]*lamda2_bnp[:,s]
        fi = np.zeros(p+1)
        for l in range(p+1):
            fi[l] = np.random.normal(loc = 0, scale = np.sqrt(var_fi[l]))
        mu_bnp[s] = fi[0]
        beta_bnp[:,s] = fi[1:]

        # \tau and m
        for l in range(p):
            tau_bnp[l,s] = inverseGamma(alpha = nu_0[l]/2, theta = (nu_0[l]*s2_0[l])/2 )
            m_bnp[l,s] = np.random.normal(loc = m_0[l], scale = np.sqrt(tau[l,j]/n_0[l]) )
    else:
        mu_bnp[s] = mu[j_clus]
        beta_bnp[:,s] = beta[:,j_clus]
        lamda2_bnp[:,s] = lamda2[:,j_clus]
        tau2_bnp[s] = tau2[j_clus]
        nu_bnp[:,s] = nu[:,j_clus]
        xi_bnp[s] = xi[j_clus]

        tau_bnp[:,s] = tau[:,j_clus]
        m_bnp[:,s] = m[:,j_clus]
    
    mu = mu[:N]
    beta = beta[:,:N]
    lamda2 = lamda2[:,:N]
    tau2 = tau2[:N]
    nu = nu[:,:N]
    xi = xi[:N]
    tau = tau[:,:N]
    m = m[:,:N]
        
    ### Step 4
    for i in range(n):
        log_likelihood_y = -0.5 * np.log(2*np.pi*sigma2) + -0.5 / sigma2 * ( np.array([y[i]] * N).T - mu - np.dot(x[i,:], beta) ) ** 2        
        log_likelihood_x = 0
        for l in range(p):
            log_likelihood_x = log_likelihood_x + -0.5 * np.log(2*np.pi*tau[l,:]) + -0.5 / tau[l,:] * ( np.array([x[i,l]] * N).T - m[l,:] ) ** 2        
        likelihood = np.exp(log_likelihood_y + log_likelihood_x)
#        likelihood_y = 1 / np.sqrt(2*np.pi*sigma2) * np.exp( -0.5 / sigma2 * ( np.array([y[i]] * N).T - mu - np.dot(x[i,:], beta) ) ** 2 )        
#        likelihood_x = 1
#        for l in range(p):
#            likelihood_x = likelihood_x * 1 / np.sqrt(2*np.pi*tau[l,:]) * np.exp( -0.5 / tau[l,:] * ( np.array([x[i,l]] * N).T - m[l,:] ) ** 2 )
#        likelihood = likelihood_y * likelihood_x        
        prob = (w[:N] > u[i]) * likelihood
        if sum(prob) > 0:
            prob_scale = prob / sum(prob)
        else:
            prob_scale = 1 / N * np.ones(N)
        di[i] = np.random.choice(np.arange(N), p = prob_scale)
    di_bnp[:,s] = di
    
    K = len(np.unique(di))
    K_bnp[s] = K
    
    ### Step 5 Update \sigma^2
    tmp_shape = (n + K*(p+1)) / 2 + alpha_1
    tmp_sum = 0
    for i in range(n):
        tmp_sum = tmp_sum + 0.5 * ( y[i] - mu[di[i]] - np.dot(x[i,:], beta[:,di[i]]) )**2
    for j_ind in np.unique(di):
        tmp_fi = np.concatenate(([mu[j_ind]], beta[:,j_ind]))
        tmp_sum = tmp_sum + 0.5/tau2[j_ind] * np.sum(tmp_fi**2/lamda2[:,j_ind])

    tmp_rate = tmp_sum + theta_1
    sigma2 = inverseGamma(alpha = tmp_shape, theta = tmp_rate)
    sigma2_bnp[s] = sigma2
    
    ### Step 6 Update DP prior param
    eta = np.random.beta(b+1,n)
    u_tmp = np.random.uniform(0,1)
    O = (b_alpha+K-1) / ((b_beta-np.log(eta))*n)
    b = np.random.gamma(shape = b_alpha+K-(u_tmp>(O/(1+O))), scale = b_beta-np.log(eta))
    b_bnp[s] = b

# =============================================================================
# Variable Selection  
# =============================================================================

### Create dictionary to save pair of \beta and cluster index
clus_beta = {}
for count, item in enumerate(range(upper)):
#    clus_beta[item] = np.median( beta_cand[burn_in:,:,item], axis = 0 )
   clus_beta[item] = ( beta_cand[burn_in:,:,item], np.median( beta_cand[burn_in:,:,item], axis = 0 ) )

_, idx = np.unique(di_bnp[:,-1], return_index=True)
unique_clus_ind = list(di_bnp[:,-1][np.sort(idx)].astype(int))

### variable selection inclusion rate
exclusion_rate_all = []
for idx, unique_id in enumerate(unique_clus_ind):
    tmp_clus_beta = clus_beta[unique_id][0][clus_beta[unique_id][0][:,0] != 0,:] # drop values not really recorded
    median = np.median(tmp_clus_beta, axis = 0) # median of posterior \beta for the cluster
    var_beta = np.var(tmp_clus_beta, axis = 0)
    std_beta = np.sqrt(var_beta)
    exclusion_rate = []
    for l in range(p):
        exclusion_rate.append( np.sum( np.where((tmp_clus_beta[:,l] < std_beta[l]) & (tmp_clus_beta[:,l] > -std_beta[l]),1,0) ) / tmp_clus_beta[:,l].shape[0] )
    exclusion_rate_all.append(exclusion_rate)

exclusion_rate_all = np.array(exclusion_rate_all)
inclusion_rate_all = 1 - exclusion_rate_all
inclusion_true = (beta_true != 0).astype(int)
exclusion_true = (beta_true == 0).astype(int)

obj_to_save = (inclusion_true, exclusion_true, inclusion_rate_all, exclusion_rate_all)
with open(dir_current + "include_exclude_rate_n%i_p%i_cluster%i_ind%i.pickle" %(n,p,n_clus,ind),"wb") as f:
   pickle.dump(obj_to_save, f)


roc_auc_all = []
auc_all = []
precision_all = []
recall_all = []
f1_all = []

for j in range(inclusion_rate_all.shape[0]):
    ### calculate AUC
    fpr,tpr,threshold = metrics.roc_curve(inclusion_true[j], inclusion_rate_all[j],drop_intermediate = False)
    roc_auc = metrics.auc(fpr, tpr) 
    roc_auc_all.append(roc_auc)
    plt.plot(fpr, tpr, 'b', label = 'AUC = %0.2f' % roc_auc)
    plt.legend(loc = 'lower right')
    plt.savefig(dir_current + "ROC_cluster_%i.png" % (j))
    plt.clf()


    precision, recall, thresholds = metrics.precision_recall_curve(inclusion_true[j], inclusion_rate_all[j])
    auc = metrics.auc(recall, precision)
    auc_all.append(auc)
    no_skill = np.sum(inclusion_true[j]) / len(inclusion_true[j])
    plt.plot([0, 1], [no_skill, no_skill], linestyle='--', label='No Skill')
    plt.plot(recall, precision, marker='.', label='Precision Recall Curve')
    plt.legend()
    plt.savefig(dir_current + "precision_recall_cluster_%i.png" % (j))
    plt.clf()
    
    tmp_pred = (inclusion_rate_all[j] > 0.5).astype(int)
    precision_all.append(metrics.precision_score(inclusion_true[j], tmp_pred, zero_division=0))
    recall_all.append(metrics.recall_score(inclusion_true[j], tmp_pred, zero_division=0))    
    f1 = metrics.f1_score(inclusion_true[j], tmp_pred, zero_division=0)
    f1_all.append(f1)

a_file = open(dir_current + "vs_result.txt", "w")

np.savetxt(a_file, np.array(roc_auc_all), fmt = "%.3f", header = "roc_auc")
np.savetxt(a_file, np.array(auc_all), fmt = "%.3f", header = "precision_recall_auc")
np.savetxt(a_file, np.array(precision_all), fmt = "%.3f", header = "precision")
np.savetxt(a_file, np.array(recall_all), fmt = "%.3f", header = "recall")
np.savetxt(a_file, np.array(f1_all), fmt = "%.3f", header = "f1")
a_file.close()


# =============================================================================
# Clustering Analysis   
# =============================================================================
num_post_burn_in = num_MCMC - burn_in
# record pairwise probability matrix throughout MCMC to check its variation
pi_bnp = []
gap = 1000

def Delta_cluster(di):
   Delta_mat = di[:, np.newaxis] == di[np.newaxis, :]
   return np.where(Delta_mat, 1, 0)

### calculate pairwise probability matrix of clustering
for num_iter in range(gap-1, num_post_burn_in+gap-1, gap):
   Delta_tmp = np.zeros((n,n))
   for i in range(num_iter):
       Delta_tmp = Delta_tmp + Delta_cluster(di_bnp[:,burn_in+i])
   pi_tmp = Delta_tmp / num_iter
   pi_bnp.append(pi_tmp)

### get least-square clustering c_ls with variance scale
ls_dist = []
for i in range(num_post_burn_in):
   ls_dist.append( np.sum((Delta_cluster(di_bnp[:,burn_in+i]) - pi_bnp[-1])**2)  ) 
ls_dist = np.array(ls_dist)
c_ls = np.argmin(ls_dist)

### clustering index corresponding to clustering c_ls
clus_ind = di_bnp[:,burn_in+c_ls]
clus_ind = clus_ind.astype(int)
unique_clus_ind = np.unique(clus_ind)

### Create dictionary to save pair of \beta and cluster index
clus_beta = {}
for count, item in enumerate(range(upper)):
#    clus_beta[item] = np.median( beta_cand[burn_in:,:,item], axis = 0 )
   clus_beta[item] = ( beta_cand[burn_in:,:,item], np.median( beta_cand[burn_in:,:,item], axis = 0 ) )
   
# =============================================================================
# Save true clustering, true \beta, estimated clustering, estimated \beta for each cluster index,
# \beta in MCMC, \mu in MCMC, \sigma^2 in MCMC
# =============================================================================
obj_to_save = (cluster_index,beta_true,clus_beta)
with open(dir_current + "result_n%i_p%i_cluster%i.pickle" %(n,p,n_clus),"wb") as f:
   pickle.dump(obj_to_save, f)

### save posterior clustering in MCMC
Z = di_bnp[:,burn_in:].astype(int)
np.save(dir_current + 'Z.npy', Z)
### save true cluster index
np.save(dir_current + 'cluster_true.npy', cluster_true)
### save clustering from least square rule
np.save(dir_current + 'cluster_ls.npy', clus_ind)


# =============================================================================
# Prediction on test data
# =============================================================================
# approximate integral part in the first term of E[Y|x,\theta_1:n]
def cal_integral_pred(x_test,i,num_MCMC,burn_in):
    integral_sum = 0
    for s in range(num_MCMC - burn_in):
        mu_pred = reg_mu_bnp[burn_in+s]
        beta_pred = reg_beta_bnp[:,burn_in+s]
        m_pred = reg_m_bnp[:,burn_in+s]
        tau_pred = reg_tau_bnp[:,burn_in+s]
        
        tmp_x_prod = 1
        for l in range(p):
            tmp_x_prod = tmp_x_prod * norm.pdf(x_test[i,l], loc = m_pred[l], scale = np.sqrt(tau_pred[l]))
        integral_sum = integral_sum + ( mu_pred + np.dot(x_test[i,:],beta_pred) ) * tmp_x_prod
    integral_pred = integral_sum / (num_MCMC -  burn_in)
    return integral_pred
    
# direct calculate integral part in the first term of b
def cal_integral_weight(x_test,i,num_MCMC,burn_in):
    integral_weight = 1
    for l in range(p):
        integral_weight = integral_weight * scipy.stats.t.pdf(x_test[i,l], df = nu_0[l], loc = m_0[l], scale = np.sqrt(s2_0[l]**2 + (s2_0[l]**2)/n_0[l]))
    return integral_weight

# store predictions for each iteration in mcmc
y_pred = np.zeros( (x_test.shape[0],num_MCMC-burn_in) )

for i in range(x_test.shape[0]):
    print(i)
    integral_pred = cal_integral_pred(x_test,i,num_MCMC,burn_in)
    integral_weight = cal_integral_weight(x_test,i,num_MCMC,burn_in)
    # approximate with MCMC
    y_pred_tmp = []
    for s in range(num_MCMC - burn_in):
        lib = Counter(di_bnp[:,burn_in+s]) # count size and index of clusters
        b_second = 0  # second term in b
        pred_second = 0  # second term in prediction
        for key in lib.keys():
            key = int(key)
            b_tmp = 1
            second_tmp = 1
            for l in range(p):
                b_tmp = b_tmp * norm.pdf( x_test[i,l], loc = m_cand[burn_in+s,l,key], scale = np.sqrt(tau_cand[burn_in+s,l,key]))
            second_tmp = b_tmp * ( mu_cand[burn_in+s,:,key] + np.dot(x_test[i,:],beta_cand[burn_in+s,:,key]) )
            b_second = b_second + b_tmp * lib[key]
            pred_second = pred_second + second_tmp * lib[key]
        # b_total = b_bnp[burn_in+s] * integral_weight + b_second
        b_total = b_second
        # y_pred_tmp.append( 1 / b_total * ( b_bnp[burn_in+s] * integral_pred + pred_second ) )
        y_pred_tmp.append( 1 / b_total * ( pred_second ) )
#    y_pred_tmp = np.array(y_pred_tmp)
    y_pred[i,:] = y_pred_tmp

y_pred_mean = np.mean(y_pred, axis = 1)
y_pred_low, y_pred_high = np.percentile(y_pred, [2.5, 97.5], axis=1)


###### plot x_0 against y_test, y_pred_mean, and 95% credible interval
tmp = sorted(list(zip(x_test[:,0],y_test,y_pred_mean,y_pred_low,y_pred_high)), key = lambda x: x[0])
x_test_tmp = np.array([i[0] for i in tmp])
y_test_tmp = np.array([i[1] for i in tmp])
y_pred_mean_tmp = np.array([i[2] for i in tmp])
y_pred_low_tmp = np.array([i[3] for i in tmp])
y_pred_high_tmp = np.array([i[4] for i in tmp])

low_error = y_pred_mean_tmp - y_pred_low_tmp
high_error = y_pred_high_tmp - y_pred_mean_tmp
y_error = np.vstack((low_error,high_error))


fig, ax = plt.subplots(figsize=(8, 6))
ax.scatter(x_test_tmp, y_test_tmp, color='red', alpha=1, marker = '.', label = 'true');
#ax.errorbar(x_test_tmp,y_pred_mean_tmp, yerr = y_error, fmt='.b', ecolor = 'blue')
ax.plot(x_test_tmp,y_pred_mean_tmp, color='black', lw=2, alpha=1, label = 'prediction');
ax.plot(x_test_tmp,y_pred_low_tmp, color='grey', linestyle = 'dashed', lw=1, alpha=0.5, label = '95% credible interval');
ax.plot(x_test_tmp,y_pred_high_tmp, color='grey', linestyle = 'dashed', lw=1, alpha=0.5);
ax.set_xlabel('x1');
ax.set_ylabel('E[y|x]');
ax.legend();
plt.savefig(dir_current + "prediction.png")

### save y_test and y_pred
obj_to_save = (y_test, y_pred)
with open(dir_current + "y_test_y_pred_n%i_p%i_cluster%i.pickle" %(n,p,n_clus),"wb") as f:
   pickle.dump(obj_to_save, f)

pred_mse = np.mean((y_pred_mean-y_test)**2)
pred_mae = np.mean(np.abs(y_pred_mean-y_test))
output = 'prediction mse: %.4f \n' % pred_mse
output = output + 'prediction mae: %.4f \n' % pred_mae



time_used = (time.time()-start) / 3600
print("Total Time: %.2f hours" % time_used)

output = output + "Total Time: %.2f hours \n" % time_used
with open(dir_current + "Output.txt", "w") as text_file:
   text_file.write(output)

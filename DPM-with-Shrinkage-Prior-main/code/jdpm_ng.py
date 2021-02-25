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

model_name = "ng"
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

def MH(lamda,gamma2,psi,sigma2_lamda):
    lamda_0 = np.exp(sigma2_lamda * np.random.normal(0,1)) * lamda # proposed value
    gamma2_0 = lamda/lamda_0*gamma2
    term_0 = np.log(lamda_0) - np.log(lamda)
    term_1 = -lamda_0 + lamda
    term_2 = p * ( np.log(special.gamma(lamda)) - np.log(special.gamma(lamda_0)) )
    term_3 = (p*lamda) * np.log(2/gamma2) - (p*lamda_0) * np.log(2/gamma2_0)
    term_4 = -np.sum(psi)/(2/gamma2_0) + np.sum(psi)/(2/gamma2)
    term_5 = (lamda_0 - lamda) * np.sum(np.log(psi))    
    accept_prob = np.minimum(1, np.exp(term_0 + term_1 + term_2 + term_3 + term_4 + term_5))    
    u = np.random.uniform()
    if u < accept_prob:
        output = lamda_0
    else:
        output = lamda
    return output

#def loglik(x,gamma2,psi):
#    tmp = -np.exp(x) - p*np.exp(x)*np.log(2/gamma2) - p*np.log(special.gamma(np.exp(x))) + np.exp(x) * np.sum(np.log(psi)) + x - np.random.exponential(1)
#    return tmp
#
#def SliceSample(s,slice_diff_sample,lamda,gamma2,psi,j, step_w = 1, step_m = 10):
#    if s > 10:
#        step_w = np.mean(slice_diff_sample[:s,j])
#    
#    old_lamda = lamda[j]
#    log_lamda = np.log(lamda[j])
#    old_log_lamda = np.log(old_lamda)
#    slice_z = loglik(old_log_lamda,gamma2[j],psi[:,j])
#    # generate interval
#    U = np.random.uniform()
#    L = old_log_lamda - step_w * U
#    R = L + step_w
#    V = np.random.uniform()
#    J = np.floor(step_m * V)
#    K = (step_m-1) - J
#    while J > 0 and slice_z < loglik(L,gamma2[j],psi[:,j]):
#        L = L - step_w
#        J = J - 1
#    while K > 0 and slice_z < loglik(R,gamma2[j],psi[:,j]):
#        R = R + step_w
#        K = K - 1
#    # sample from the interval
#    L_bar = L
#    R_bar = R
#    stop = True
#    while stop:
#        U = np.random.uniform()
#        log_lamda_cand = L_bar + U * (R_bar - L_bar)
#        if slice_z < loglik(log_lamda_cand,gamma2[j],psi[:,j]):
#            log_lamda = log_lamda_cand
#            stop = False
#            break
#        if log_lamda_cand < log_lamda:
#            L_bar = log_lamda_cand
#        else:
#            R_bar = log_lamda_cand
#    # record diff of old and new log(lamda), for estimation of step_w
#    diff_w = np.abs(old_log_lamda - log_lamda)
#    return np.exp(log_lamda), diff_w
#
#def SliceSample_reg(s,reg_slice_diff_sample,reg_lamda,reg_gamma2,reg_psi, step_w = 1, step_m = 10):
#    if s > 10:
#        step_w = np.mean(reg_slice_diff_sample[:s])
#    
#    old_lamda = reg_lamda
#    log_lamda = np.log(reg_lamda)
#    old_log_lamda = np.log(old_lamda)
#    slice_z = loglik(old_log_lamda,reg_gamma2,reg_psi)
#    # generate interval
#    U = np.random.uniform()
#    L = old_log_lamda - step_w * U
#    R = L + step_w
#    V = np.random.uniform()
#    J = np.floor(step_m * V)
#    K = (step_m-1) - J
#    while J > 0 and slice_z < loglik(L,reg_gamma2,reg_psi):
#        L = L - step_w
#        J = J - 1
#    while K > 0 and slice_z < loglik(R,reg_gamma2,reg_psi):
#        R = R + step_w
#        K = K - 1
#    # sample from the interval
#    L_bar = L
#    R_bar = R
#    stop = True
#    while stop:
#        U = np.random.uniform()
#        log_lamda_cand = L_bar + U * (R_bar - L_bar)
#        if slice_z < loglik(log_lamda_cand,reg_gamma2,reg_psi):
#            log_lamda = log_lamda_cand
#            stop = False
#            break
#        if log_lamda_cand < log_lamda:
#            L_bar = log_lamda_cand
#        else:
#            R_bar = log_lamda_cand
#    # record diff of old and new log(lamda), for estimation of step_w
#    diff_w = np.abs(old_log_lamda - log_lamda)
#    return np.exp(log_lamda), diff_w


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
if n >= (p+1):
    beta_hat_true = np.linalg.inv(X.T.dot(X)).dot(X.T).dot(y)[1:]
    V_all = 1 / p * np.sum(beta_hat_true ** 2)
else:
    beta_hat_true = X.T.dot(np.linalg.inv(X.dot(X.T))).dot(y)[1:]
    V_all = 1 / n * np.sum(beta_hat_true ** 2)

clus_size_threshold = 1

### DP prior
a = 1
b = 1
b_alpha = 2
b_beta = 2

# Inverse-Gamma param for \sigma2
alpha_1 = 2
theta_1 = 2

# hyperparameters for slice sampling of \lambda
sigma2_lamda = 0.05 # set to let average acceptance rate be around 20%~30%

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
reg_lamda_bnp = np.zeros(num_MCMC)
reg_gamma2_bnp = np.zeros(num_MCMC)
reg_psi_bnp = np.zeros((p,num_MCMC))
reg_mu_bnp = np.zeros(num_MCMC)
reg_beta_bnp = np.zeros((p,num_MCMC))
reg_sigma2_bnp = np.zeros(num_MCMC)
## parameters related to X
reg_m_bnp = np.zeros((p,num_MCMC))
reg_tau_bnp = np.zeros((p,num_MCMC))


### Initialize \lambda, \gamma^(-2), D_\psi
reg_lamda = np.random.exponential(scale = 1)
reg_gamma2 = np.random.gamma(shape = 2, scale = 1 / (2*V_all/reg_lamda))
reg_psi = []
for l in range(p):
    reg_psi.append( np.random.gamma(shape = reg_lamda, scale = 1 / (2*reg_gamma2)) )
reg_psi = np.array(reg_psi)
### Initialize \mu,\beta
reg_mu = np.random.normal(loc=0,scale=10)
reg_beta = np.random.multivariate_normal(mean = np.zeros(p), cov = np.diag(reg_psi))
### Initialize m and \tau's
reg_m = np.zeros(p)
reg_tau = np.ones(p) * 10
### Initialize \sigma^2
reg_sigma2 = inverseGamma(alpha_1,theta_1)


# =============================================================================
# Parameters Initialization
# =============================================================================
## parameters related to y
lamda_bnp = np.zeros(num_MCMC)
gamma2_bnp = np.zeros(num_MCMC)
psi_bnp = np.zeros((p,num_MCMC))
mu_bnp = np.zeros(num_MCMC)
beta_bnp = np.zeros((p,num_MCMC))
sigma2_bnp = np.zeros(num_MCMC)

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
lamda = []
gamma2 = []
psi = np.array([])
for j in range(M):
    lamda.append( np.random.exponential(scale = 1) )
    gamma2.append( np.random.gamma(shape = 2, scale = 1 / (2*V_all/lamda[j])) )
    tmp_psi = []
    for l in range(p):
        tmp_psi.append( np.random.gamma(shape = lamda[j], scale = 1 / (2*gamma2[j])) )
    psi = np.hstack([psi,np.array(tmp_psi).reshape(-1,1)]) if psi.size else np.array(tmp_psi).reshape(-1,1)
psi = psi
lamda = np.array(lamda)
gamma2 = np.array(gamma2)

### Regular Initialize \mu,\beta
mu = []
for j in range(M):
    mu.append(np.random.normal(loc=0,scale=10))
mu = np.array(mu)

beta = np.array([])
for j in range(M):
    tmp_beta = np.random.multivariate_normal(mean = np.zeros(p), cov = np.diag(psi[:,j]))
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
    ### 0.1 update \lambda
    reg_lamda = MH(reg_lamda,reg_gamma2,reg_psi,sigma2_lamda)    
#    reg_lamda_0 = np.exp(sigma2_lamda * np.random.normal(0,1)) * reg_lamda # proposed value
#    reg_gamma2_0 = reg_lamda/reg_lamda_0*reg_gamma2
#    term_0 = np.log(reg_lamda_0) - np.log(reg_lamda)
#    term_1 = -reg_lamda_0 + reg_lamda
#    term_2 = p * ( np.log(special.gamma(reg_lamda)) - np.log(special.gamma(reg_lamda_0)) )
#    term_3 = (p*reg_lamda) * np.log(2/reg_gamma2) - (p*reg_lamda_0) * np.log(2/reg_gamma2_0)
#    term_4 = -np.sum(reg_psi)/(2/reg_gamma2_0) + np.sum(reg_psi)/(2/reg_gamma2)
#    term_5 = (reg_lamda_0 - reg_lamda) * np.sum(np.log(reg_psi))
#    accept_prob = np.minimum(1, np.exp(term_0 + term_1 + term_2 + term_3 + term_4 + term_5))
#    
#    u = np.random.uniform()
#    if u < accept_prob:
#        reg_lamda = reg_lamda_0
#    else:
#        reg_lamda = reg_lamda    
    reg_lamda_bnp[s] = reg_lamda
    
    ### 0.2 update \gamma2
    reg_gamma2 = np.random.gamma(shape = p*reg_lamda+2, scale = 1 / (0.5*np.sum(reg_psi) + V_all/(2*reg_lamda)))
    reg_gamma2_bnp[s] = reg_gamma2
    ### 0.3 update D_\psi
    reg_mm = reg_lamda - 0.5
    reg_c = reg_gamma2    
    for l in range(p):    
        reg_d = reg_beta[l] ** 2
        if reg_d < 1e-10 and reg_lamda > 0.5:
            reg_psi[l] = np.random.gamma(shape = reg_mm, scale = 1 / reg_c)
        elif reg_d < 1e-10 and reg_lamda < 0.5:
            reg_psi[l] = np.random.gamma(shape = reg_lamda, scale = 2 / reg_gamma2)
        elif reg_c < 1e-10 and reg_lamda < 0.5:
            reg_psi[l] = inverseGamma(alpha = -reg_mm , theta = reg_d / 2)           
        else:       
            reg_bb = np.sqrt(reg_c*reg_d)        
            reg_psi[l] = np.sqrt(reg_d/reg_c) * geninvgauss.rvs(p = reg_mm, b = reg_bb)
        reg_psi[l] = np.maximum(1e-10,reg_psi[l])
    reg_psi_bnp[:,s] = reg_psi
    ### 0.4 update \mu and \beta    
    if n > (p+1):
        reg_Lambda = np.diag( np.concatenate((np.array([0]),1 / reg_psi)) )
        reg_fi_tmp = np.linalg.inv(X.T.dot(X) + reg_sigma2 * reg_Lambda)
        reg_mean_fi = reg_fi_tmp.dot(X.T).dot(y)
        reg_var_fi = reg_sigma2 * reg_fi_tmp            
    # when n<p+1
    else:
        reg_res_tmp = np.linalg.svd(X, full_matrices=False)
        reg_Ft, reg_D, reg_At = reg_res_tmp[0:3]
        reg_F = reg_Ft.T
        reg_A = reg_At.T

        reg_theta_hat = np.diag(1/reg_D).dot(reg_F).dot(y)  
        reg_Psi = np.diag( np.concatenate((np.array([100]),reg_psi)) )
        reg_psi_0 = reg_At.dot(reg_Psi).dot(reg_A)        
        reg_Lambda_star = np.diag(1 / (reg_D ** 2))   
        reg_fi_tmp = reg_Psi.dot(reg_A).dot(np.linalg.inv(reg_psi_0 + reg_sigma2 * reg_Lambda_star))
        reg_mean_fi = reg_fi_tmp.dot(reg_theta_hat)
        reg_var_fi = reg_Psi - reg_fi_tmp.dot(reg_At).dot(reg_Psi)
    reg_var_fi = 0.5 * (reg_var_fi + reg_var_fi.T)
    min_eig = np.min(np.real(np.linalg.eigvals(reg_var_fi)))
    if min_eig < 0:
        reg_var_fi -= 10*min_eig * np.eye(*reg_var_fi.shape)
    reg_fi = np.random.multivariate_normal( mean = reg_mean_fi, cov = reg_var_fi )
    reg_mu = reg_fi[0]
    reg_beta = reg_fi[1:]
    reg_mu_bnp[s] = reg_mu
    reg_beta_bnp[:,s] = reg_beta
    ### 0.5   Sample m and \tau
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
    ### 0.6   Sample \sigma^2    
    reg_tmp_shape = n / 2 + alpha_1    
    reg_mu_ones = reg_mu * np.ones(n)    
    reg_residual_tmp = y - reg_mu_ones - np.dot(x, reg_beta)
    reg_tmp_rate = 0.5 * sum(reg_residual_tmp**2) + theta_1
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
        if j < len(lamda):
            ### Step 2.1   Sample \lambda
            tmp_lamda = MH(lamda[j],gamma2[j],psi[:,j],sigma2_lamda)
            lamda[j] = tmp_lamda
            
            ### step 2.2   Sample \gamma^(-2)          
            gamma2[j] = np.random.gamma(shape = p*lamda[j]+2, scale = 1 / (0.5*np.sum(psi[:,j]) + V_all/(2*lamda[j])))
            
            ### Step 2.3   Sample D_\psi
            mm = lamda[j] - 0.5
            c = gamma2[j]
            
            for l in range(p):    
                d = beta[l,j] ** 2
                if d < 1e-10 and lamda[j] > 0.5:
                    psi[l,j] = np.random.gamma(shape = mm, scale = 1 / c)
                elif d < 1e-10 and lamda[j] < 0.5:
                    psi[l,j] = np.random.gamma(shape = lamda[j], scale = 2 / gamma2[j])
                elif c < 1e-10 and lamda[j] < 0.5:
                    psi[l,j] = inverseGamma(alpha = -mm , theta = d / 2)           
                else:       
                    bb = np.sqrt(c*d)        
                    psi[l,j] = np.sqrt(d/c) * geninvgauss.rvs(p = mm, b = bb)
                psi[l,j] = np.maximum(1e-10,psi[l,j])
                                   
            ### Step 2.4   Sample \mu and \beta    
            X_tmp = X[di == j,:]
            x_tmp = x[di == j,:]
            y_tmp = y[di == j]
            n_tmp = x_tmp.shape[0]
            if n_tmp > clus_size_threshold:
                if n_tmp > (p+1):
                    Lambda = np.diag( np.concatenate((np.array([0]),1 / psi[:,j])) )
                    fi_tmp = np.linalg.inv(X_tmp.T.dot(X_tmp) + sigma2 * Lambda)
                    mean_fi = fi_tmp.dot(X_tmp.T).dot(y_tmp)
                    var_fi = sigma2 * fi_tmp            
                # when n<p+1
                else:
                    res_tmp = np.linalg.svd(X_tmp, full_matrices=False)
                    Ft, D, At = res_tmp[0:3]
                    F = Ft.T
                    A = At.T
            
                    theta_hat = np.diag(1/D).dot(F).dot(y_tmp)  
                    Psi = np.diag( np.concatenate((np.array([100]),psi[:,j])) )
                    psi_0 = At.dot(Psi).dot(A)        
                    Lambda_star = np.diag(1 / (D ** 2))   
                    fi_tmp = Psi.dot(A).dot(np.linalg.inv(psi_0 + sigma2 * Lambda_star))
                    mean_fi = fi_tmp.dot(theta_hat)
                    var_fi = Psi - fi_tmp.dot(At).dot(Psi)
                var_fi = 0.5 * (var_fi + var_fi.T)
                fi = np.random.multivariate_normal( mean = mean_fi, cov = var_fi )
                mu[j] = fi[0]
                beta[:,j] = fi[1:]
            else:
                beta[:,j] = np.random.multivariate_normal( mean = np.zeros(p), cov = np.diag(psi[:,j]) )
                mu[j] = np.random.normal( loc = 0, scale = 10 )
            
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
            # \lambda
            lamda[j] = np.random.exponential(scale = 1)
            # \gamma^(-2)
            gamma2[j] = np.random.gamma(shape = 2, scale  = 2*lamda[j]/V_all)
            # D_\psi
            for l in range(p):
                psi[l,j] = np.random.gamma(shape = lamda[j], scale = 2 / gamma2[j] )
                psi[l,j] = np.maximum(1e-10,psi[l,j])
            # \mu,\beta
            beta[:,j] = np.random.multivariate_normal( mean = np.zeros(p), cov = np.diag(psi[:,j]) )
            mu[j] = np.random.normal( loc = 0, scale = 10 )             
            # \tau and m
            for l in range(p):
                tau[l,j] = inverseGamma(alpha = nu_0[l]/2, theta = (nu_0[l]*s2_0[l])/2 )
                m[l,j] = np.random.normal(loc = m_0[l], scale = np.sqrt(tau[l,j]/n_0[l]) )
         
    ### Only keep first M cluster's values
    lamda = lamda[:M]
    gamma2 = gamma2[:M]
    psi = psi[:,:M]
    beta = beta[:,:M]
    mu = mu[:M]
    m = m[:,:M]
    tau = tau[:,:M]
    
    ### if N>M, then draw parameters from their priors
    if N > M:
        lamda = np.concatenate((lamda, np.zeros(N-M)))
        gamma2 = np.concatenate((gamma2, np.zeros(N-M)))        
        psi = np.hstack((psi, np.zeros((p,N-M))))
        beta = np.hstack((beta, np.zeros((p,N-M))))
        mu = np.concatenate((mu, np.zeros(N-M)))
        m = np.hstack((m, np.zeros((p,N-M))))
        tau = np.hstack((tau, np.zeros((p,N-M))))
    for j in range(M,N):
        # \lambda
        lamda[j] = np.random.exponential(scale = 1)
        # \gamma^(-2)
        gamma2[j] = np.random.gamma(shape = 2, scale  = 2*lamda[j]/V_all)
        # D_\psi
        for l in range(p):
            psi[l,j] = np.random.gamma(shape = lamda[j], scale = 2 / gamma2[j] )
            psi[l,j] = np.maximum(1e-5,psi[l,j])
        # \mu,\beta
        beta[:,j] = np.random.multivariate_normal( mean = np.zeros(p), cov = np.diag(psi[:,j]) )
        mu[j] = np.random.normal( loc = 0, scale = 10 ) 
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
        # \lambda
        lamda_bnp[s] = np.random.exponential(scale = 1)
        # \gamma^(-2)
        gamma2_bnp[s] = np.random.gamma(shape = 2, scale  = 2*lamda_bnp[s]/V_all)
        # \psi        
        for l in range(p):
            psi_bnp[l,s] = np.random.gamma(shape = lamda_bnp[s], scale = 2 / gamma2_bnp[s] )
            psi_bnp[l,s] = np.maximum(1e-10,psi_bnp[l,s])
        # \mu and \beta
        beta_bnp[:,s] = np.random.multivariate_normal( mean = np.zeros(p), cov = np.diag(psi_bnp[:,s]) )
        mu_bnp[s] = np.random.normal( loc = 0, scale = 10 )
        # \tau and m
        for l in range(p):
            tau_bnp[l,s] = inverseGamma(alpha = nu_0[l]/2, theta = (nu_0[l]*s2_0[l])/2 )
            m_bnp[l,s] = np.random.normal(loc = m_0[l], scale = np.sqrt(tau[l,j]/n_0[l]) )        
    else:
        lamda_bnp[s] = lamda[j_clus]
        gamma2_bnp[s] = gamma2[j_clus]
        psi_bnp[:,s] = psi[:,j_clus]
        beta_bnp[:,s] = beta[:,j_clus]
        mu_bnp[s] = mu[j_clus]
        tau_bnp[:,s] = tau[:,j_clus]
        m_bnp[:,s] = m[:,j_clus]
    
    lamda = lamda[:N]
    gamma2 = gamma2[:N]
    psi = psi[:,:N]
    beta = beta[:,:N]
    mu = mu[:N]
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
    tmp_shape = n / 2 + alpha_1
    tmp_sum = 0
    for i in range(n):
        tmp_sum = tmp_sum + 0.5 * ( y[i] - mu[di[i]] - np.dot(x[i,:], beta[:,di[i]]) )**2
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

from asgl import weights
import pandas as pd 
import os
import math
import numpy as np
import asgl
import scipy.linalg as sp

#lambda1 = 10.0 ** np.arange(-3, 1.51, 0.1)
os.chdir(os.path.expanduser("~/Desktop/simulation"))
# capire che vairbile ottenere
os.chdir("QTL_10")

file = open("general_info/snp1.txt_clean").readlines()
file = [hh.split() for hh in file]
snp = [list(map(int,hh[1])) for hh in file]
ani = [hh[0] for hh in file]
snp = np.matrix(snp,dtype = int)
ani = np.array(ani)
del(file)
snp.shape

# un attimo sistamre 
# created Z amtrix 
p = np.array(snp.mean(0)) /2
sum2pq = (p *(1-p)).sum()*2
P = np.zeros(snp.shape)
for i in range(P.shape[0]):
    P[i,:] = p*2

Z = snp - P
Z[:10,:10]
Z = Z / math.sqrt(sum2pq)

G = Z.dot(Z.T) / sum2pq
G[:10,:10] 
np.diagonal(G)
os.getcwd()


sol = pd.read_csv("no_qtl_include/Gblup/solutions",skiprows=1,sep="\s+",header=None)[3]
sol = sol[1:] # rimuovo la media
snp_eff = pd.read_csv("no_qtl_include/Gblup/snp_sol",skiprows=0,sep="\s+")[["snp_effect"]]


lambda1 = 10.0 ** np.arange(-3, 1.51, 0.1)  # PANALITY FOR GRID SERCH 

tvt_alasso = asgl.TVT(model='lm', penalization='alasso', lambda1= lambda1 , 
                    parallel=   True,
                    weight_technique='lasso', error_type='MSE',
                    random_state=1, 
                    train_size=int(sol.shape[0])  , 
                    validate_size=int(sol.shape[0]*(1/3)))

alasso_result = tvt_alasso.train_validate_test(x=Z.T, y=np.array(sol))

alasso_prediction_error = alasso_result['test_error']
alasso_betas = alasso_result['optimal_betas'][1:] 


# studiare !!!!!!


# Authors: Alexandre Gramfort <firstname.lastname@inria.fr>
#
# License: BSD (3-clause)

#code form scratch 
"""
lasso with grandient descendent 
lasso apdaptive
grid serch paramenter
"""
import numpy as np
from sklearn.linear_model import Lasso
import copy
x=Z
y = np.array(sol)

def CoordinateDescent_Lasso(x, y,epochs,learning_rate,Lambda):        
     m=x.shape[0]
     #X = np.concatenate((np.ones((m,1)),x),axis=1)
     xMat=np.mat(x)
     yMat = y
     w = np.ones(x.shape[1]).reshape(-1,1)
          
     for n in range(epochs):
            #Deep copy
            out_w = copy.copy(w)
            for i,item in enumerate(w):
                #The convergence point of loss function is found on every W value
                for j in range(epochs):
                    h = xMat * w 
                    gradient = xMat[:,i].T * (h - yMat)/m + Lambda * np.sign(w[i])
                    w[i] = w[i] - gradient* learning_rate
                    if abs(gradient)<1e-3:
                        break
            out_w = np.array(list(map(lambda x:abs(x)<1e-3, out_w-w)))
            if out_w.all():
                break
            return w

CoordinateDescent_Lasso(x, y,500,0.00001,0.1)

alpha = 0.1

g = lambda w: np.sqrt(np.abs(w))
gprime = lambda w: 1. / (2. * np.sqrt(np.abs(w)) + np.finfo(float).eps)

# Or another option:
# ll = 0.01
# g = lambda w: np.log(ll + np.abs(w))
# gprime = lambda w: 1. / (ll + np.abs(w))

n_samples, n_features = X.shape
p_obj = lambda w: 1. / (2 * n_samples) * np.sum((y - np.dot(X, w)) ** 2) \
                  + alpha * np.sum(g(w))

weights = np.ones(n_features)
n_lasso_iterations = 5

for k in range(n_lasso_iterations):
    X_w = X / weights[np.newaxis, :]
    clf = Lasso(alpha=alpha, fit_intercept=False)   # INPLEMENTA A MANO 
    clf.fit(X_w, y)
    coef_ = clf.coef_ / weights
    weights = gprime(coef_)
    print p_obj(coef_)  # should go down

print np.mean((clf.coef_ != 0.0) == (coef != 0.0))





# grid scherc lambda
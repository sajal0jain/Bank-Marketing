# -*- coding: utf-8 -*-


from skimage.feature import greycomatrix, greycoprops
from skimage import data
import os
from scipy import misc
import urllib2
import json
import mysql.connector
import datetime
import webbrowser
import urllib
import re
import numpy as np
import os
import sklearn
from numpy import linalg as LA
import matplotlib.pyplot as plt
from sklearn import metrics
from sklearn.cluster import KMeans
from sklearn.decomposition import PCA
from sklearn.preprocessing import scale
from sklearn.cluster import KMeans, MiniBatchKMeans
import os, sys
import scipy
from scipy.spatial import distance
import csv
import numpy
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.decomposition import PCA
import numpy as np
from sklearn import mixture
from sklearn.neighbors import NearestNeighbors
from sklearn import preprocessing
from sklearn.covariance import EllipticEnvelope
from sklearn.svm import OneClassSVM

client_data=numpy.loadtxt(open("C:\Users\Jayanta\Documents\client_data.csv","rb"),delimiter=",",skiprows=1)#load client data.csv
X=client_data
full_data=pd.read_csv('C:\Users\Jayanta\Documents\\bank-additional-full.csv', sep=',',header=0)#load bank additional full.csv
true_labels=full_data.ix[:,20]
def label_conv(x):
    if(x=='yes'):
        return 1
    else:
        return 0
numeric_labels=map(label_conv,true_labels)
Dist=np.array(())
#hierachical clustering
hier=sklearn.cluster.AgglomerativeClustering(n_clusters=2, affinity='euclidean',linkage="complete")
hier.fit(X)

for j in xrange(1,11):
    i = 50*j
    km = sklearn.cluster.MiniBatchKMeans(i)
    km.fit(X) 
    cluster_labels=km.labels_
    centers= km.cluster_centers_
    n_clusters= len(set(cluster_labels))
    label_list=[[] for t in xrange(n_clusters)]
    A=set(cluster_labels)
    A=list(A)
    for k in A:
        for u,v in enumerate(cluster_labels):
            if v==k:
                label_list[v].append(u)
    D = 0 
    for u in xrange(n_clusters):
        for v in xrange(len(label_list[u])):
            D += distance.euclidean(X[label_list[u][v]],centers[u])
    Dist= np.append(Dist,D)
plt.plot([100*i for i in xrange(1,11)],Dist)


#dbscan clustering
db=sklearn.cluster.DBSCAN(eps=3,min_samples=5000,metric='euclidean')
db.fit(X)
birch=sklearn.cluster.Birch( n_clusters=1000,compute_labels=True, copy=True)
birch.fit(X)
birch.set_params(n_clusters=1000)
birch.partial_fit()
cluster_labels=birch.labels_
core_samples_mask = np.zeros_like(db.labels_, dtype=bool)
core_samples_mask[db.core_sample_indices_] = True
cluster_labels=db.labels_
A=set(cluster_labels)
A=list(A)
print('Estimated number of clusters: %d'%(len(set(cluster_labels)) - (1 if -1 in cluster_labels else 0)))
n_clusters= len(set(cluster_labels))

label_list=[[] for i in xrange(n_clusters)]
for k in A:
    for i,j in enumerate(cluster_labels):
        if j==k:
            label_list[j].append(i)

YN=np.empty((0,3))
TE=0
for k in xrange(n_clusters):
    no=0
    yes=0
    t=len(label_list[k])
    for e in label_list[k]:
        if numeric_labels[e]==0:
            no+=1;
        else:
            yes+=1
    py=float(yes)/t
    pn=float(no)/t
    entropy= scipy.stats.entropy([py, pn],base=2) 
    TE+= t*entropy/41187
    YN=np.vstack((YN,[yes,no,entropy]))   
yn=list(YN)

c=0
for i,j in enumerate(cluster_labels):
        if j==-1:
            c+=1

			
#Measure of Clustering			
print('Estimated number of clusters: %d' % n_clusters)
print("Homogeneity: %0.3f" % metrics.homogeneity_score(numeric_labels, cluster_labels))
print("Completeness: %0.3f" % metrics.completeness_score(numeric_labels, cluster_labels))
print("V-measure: %0.3f" % metrics.v_measure_score(numeric_labels, cluster_labels))
print("Adjusted Rand Index: %0.3f"
      % metrics.adjusted_rand_score(numeric_labels, cluster_labels))
print("Adjusted Mutual Information: %0.3f"
      % metrics.adjusted_mutual_info_score(numeric_labels, cluster_labels))
#print("Silhouette Coefficient: %0.3f"
 #     % metrics.silhouette_score(X, labels))


unique_labels = set(cluster_labels)
colors = plt.cm.Spectral(np.linspace(0, 1, len(unique_labels)))
for k, col in zip(unique_labels, colors):
    if k == -1:
        # Black used for noise.
        col = 'k'

    class_member_mask = (cluster_labels == k)

    xy = X[class_member_mask & core_samples_mask]
    plt.plot(xy[:, 0], xy[:, 1], 'o', markerfacecolor=col,
             markeredgecolor='k', markersize=14)

    xy = X[class_member_mask & ~core_samples_mask]
    plt.plot(xy[:, 0], xy[:, 1], 'o', markerfacecolor=col,
             markeredgecolor='k', markersize=6)

plt.title('Estimated number of clusters: %d' % n_clusters)
plt.show()



csvfile = "YN.csv"
with open(csvfile, "w") as output:
    writer = csv.writer(output, lineterminator='\n')
    writer.writerows(yn)
csvfile = "cluster_list.csv"
with open(csvfile, "w") as output:
    writer = csv.writer(output, lineterminator='\n')
    writer.writerows(label_list)

scipy.spatial.distance.euclidean(X[10,],X[34030,])
labels_true = [0, 0, 1, 1]
labels_pred = [0, 0, 2, 3]
metrics.homogeneity_score(labels_true, labels_pred)


pca = PCA(n_components=2).fit(X)
pca_2d = pca.transform(X)
for i in range(0, pca_2d.shape[0]):
    if cluster_labels[i] == 0:
        c1 = plt.scatter(pca_2d[i,0],pca_2d[i,1],c='r',marker='+')
    elif cluster_labels[i] == 1:
        c2 = plt.scatter(pca_2d[i,0],pca_2d[i,1],c='g',marker='o')
    elif cluster_labels[i] == -1:
        c3 = plt.scatter(pca_2d[i,0],pca_2d[i,1],c='b',marker='*')
plt.legend([c1, c2, c3], ['Cluster 1', 'Cluster 2','Noise'])
plt.title('DBSCAN: clusters and noise')
plt.show()


contact_data=numpy.loadtxt(open("C:\Users\Jayanta\Documents\contact_data.csv","rb"),delimiter=",",skiprows=1)
Y= contact_data
db=sklearn.cluster.DBSCAN(metric='euclidean')
db.fit(Y)

##gmm
metric =np.zeros((0,3))
for j in xrange(50,60):
    i=10*j
    g = mixture.GMM(n_components=i)
    g.fit(X) 
    metric=np.vstack((metric, [i,g.bic(X),g.aic(X)]))
cluster_labels_gmm=g.predict(X)
A=set(cluster_labels_gmm)
A=list(A)
print('Estimated number of clusters: %d'%(len(set(cluster_labels_gmm)) ))
n_clusters_gmm= len(A)

label_list_gmm=[[] for i in xrange(max(A)+1)]
for k in A:
    for i,j in enumerate(cluster_labels_gmm):
        if j==k:
            label_list_gmm[j].append(i)
YN_gmm=np.empty((0,5))
TE=0
for k in xrange(max(A)+1):
    no=0
    yes=0
    t=len(label_list_gmm[k])
    if(t>0):
        for e in label_list_gmm[k]:
            if numeric_labels[e]==0:
                no+=1;
            else:
                yes+=1
        py=float(yes)/t
        pn=float(no)/t
        entropy= scipy.stats.entropy([py, pn],base=2) 
        TE+= t*entropy/41188
        YN_gmm=np.vstack((YN_gmm,[k,yes,no,entropy,py/pn]))   
yn_gmm=list(YN_gmm)

np.where((YN_gmm[:,4]>=1))&(YN_gmm[:,1]+YN_gmm[:,2])>100 )
YN_gmm[484,1]+YN_gmm[484,2]
#450 484
YN_gmm[484,]
YN_gmm[450,]
label_list_gmm[583]
np.mean( X[label_list_gmm[583],0])

nbrs = NearestNeighbors(n_neighbors=5000, algorithm='ball_tree').fit(X)
distances, indices = nbrs.kneighbors(X)
plt.hist(distances[:,4999],bins=[0,1,2,3,4,5,6,7,8,9,10,20,40,50])

Z =np.column_stack((X,Y))

####minmax
min_max_scaler = preprocessing.MinMaxScaler()
X_minmax = min_max_scaler.fit_transform(X)
Dist=np.array(())
for j in xrange(1,11):
    i = 50*j
    km = sklearn.cluster.MiniBatchKMeans(i)
    km.fit(X_minmax) 
    cluster_labels=km.labels_
    centers= km.cluster_centers_
    n_clusters= len(set(cluster_labels))
    label_list=[[] for t in xrange(i)]
    A=set(cluster_labels)
    A=list(A)
    for k in A:
        for u,v in enumerate(cluster_labels):
            if v==k:
                label_list[v].append(u)
    D = 0 
    for u in xrange(n_clusters):
        for v in xrange(len(label_list[u])):
            D += distance.euclidean(X[label_list[u][v]],centers[u])
    Dist= np.append(Dist,D)

km = sklearn.cluster.MiniBatchKMeans(100)
km.fit(X_minmax)
cluster_labels=km.labels_
centers= km.cluster_centers_
n_clusters= len(set(cluster_labels))
A=set(cluster_labels)
A=list(A)
label_list=[[] for t in xrange(100)]
for k in A:
        for u,v in enumerate(cluster_labels):
            if v==k:
                label_list[v].append(u)
YN=np.empty((0,3))
TE=0

for k in xrange(100):
    no=0
    yes=0
    t=len(label_list[k])
    if t>0:
        for e in label_list[k]:
            if numeric_labels[e]==0:
                no+=1;
            else:
                yes+=1
        py=float(yes)/t
        pn=float(no)/t
        entropy= scipy.stats.entropy([py, pn],base=2) 
        TE+= t*entropy/41187
        YN=np.vstack((YN,[yes,no,entropy]))   
yn=list(YN)




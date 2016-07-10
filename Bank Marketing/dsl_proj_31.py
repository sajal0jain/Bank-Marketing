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
from sklearn.cluster import MeanShift, estimate_bandwidth
from sklearn.cluster import AffinityPropagation
from sklearn.datasets.samples_generator import make_blobs

full_data=pd.read_csv('C:\Users\Jayanta\Documents\\Bank_date_with_date.csv', sep=',',header=0)#bank _dat with_date.csv
true_labels=full_data.ix[:,20]
def label_conv(x):
    if(x=='yes'):
        return 1
    else:
        return 0
numeric_labels=map(label_conv,true_labels)
client_data=numpy.loadtxt(open("C:\Users\Jayanta\Documents\client_data.csv","rb"),delimiter=",",skiprows=1)
X=client_data
min_max_scaler = preprocessing.MinMaxScaler()
X_minmax = min_max_scaler.fit_transform(X)

contact_data=numpy.loadtxt(open("C:\Users\Jayanta\Documents\contact_data.csv","rb"),delimiter=",",skiprows=1)
Y= contact_data
min_max_scaler = preprocessing.MinMaxScaler()
Y_minmax = min_max_scaler.fit_transform(Y)


Z =np.column_stack((X,Y))
min_max_scaler = preprocessing.MinMaxScaler()
Z_minmax = min_max_scaler.fit_transform(Z)

##k-means
Dist=np.array(())
for j in xrange(1,16):
    i = 50*j
    km = sklearn.cluster.MiniBatchKMeans(i)
    km.fit(Z_minmax) 
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
            D += distance.euclidean(Z_minmax[label_list[u][v]],centers[u])
    Dist= np.append(Dist,D)
plt.plot([100*i for i in xrange(1,16)],Dist)
fig = plt.figure()
plt.plot([100*i for i in xrange(1,16)],Dist)
fig.suptitle('Within cluster sum of squares', fontsize=20)
plt.xlabel('Number of Cluster', fontsize=16)
plt.ylabel('Cluster Withinness', fontsize=16)
fig.savefig('cluster_lose_function.jpg')

km = sklearn.cluster.MiniBatchKMeans(350)
km.fit(Z_minmax)
cluster_labels=km.labels_
centers= km.cluster_centers_
label_list_km=[[] for t in xrange(350)]
A=set(cluster_labels)
A=list(A)
for k in A:
        for u,v in enumerate(cluster_labels):
            if v==k:
                label_list_km[v].append(u)

YN_km=np.empty((0,4))
TE=0

for k in xrange(350):
    no=0
    yes=0
    t=len(label_list_km[k])
    if t>0:
        for e in label_list_km[k]:
            if numeric_labels[e]==0:
                no+=1;
            else:
                yes+=1
        py=float(yes)/t
        pn=float(no)/t
        entropy= scipy.stats.entropy([py, pn],base=2) 
        TE+= t*entropy/41187
        YN_km=np.vstack((YN_km,[k,t,(py/pn if pn!=0 else 999),entropy]))   
yn_km=list(YN_km)
np.where(YN_km[:,2]>=1)
[YN_km[np.where(YN_km[:,2]>=1),0],YN_km[np.where(YN_km[:,2]>=1),1]]
#cluster nuber 28,113,233,259
full_data.ix[np.where(YN_km[:,2]>=1),:]


s_cust1=full_data.ix[np.array(label_list_km[29]),:]
s_cust2=full_data.ix[np.array(label_list_km[55]),:]
s_cust3=full_data.ix[np.array(label_list_km[62]),:]
s_cust4=full_data.ix[np.array(label_list_km[70]),:]
s_cust5=full_data.ix[np.array(label_list_km[154]),:]
s_cust6=full_data.ix[np.array(label_list_km[164]),:]
s_cust7=full_data.ix[np.array(label_list_km[187]),:]
s_cust8=full_data.ix[np.array(label_list_km[235]),:]
s_cust9=full_data.ix[np.array(label_list_km[280]),:]
s_cust10=full_data.ix[np.array(label_list_km[294]),:]
s_cust11=full_data.ix[np.array(label_list_km[321]),:]
s_cust12=full_data.ix[np.array(label_list_km[243]),:]
s_cust13=full_data.ix[np.array(label_list_km[102]),:]
succ_clust = pd.concat([s_cust1,s_cust2,s_cust3,s_cust5,s_cust6,s_cust7,s_cust8,s_cust9,s_cust10,s_cust11,s_cust12,s_cust13])

succ_clust.to_csv('SUCCESS.csv')
s_cust1.to_csv('success_cluster_1.csv')
s_cust2.to_csv('success_cluster_2.csv')
s_cust3.to_csv('success_cluster_3.csv')
s_cust4.to_csv('success_cluster_4.csv')
####dbscan
nbrs = NearestNeighbors(n_neighbors=5000, algorithm='ball_tree').fit(Z_minmax)
distances, indices = nbrs.kneighbors(Z_minmax)
###economic variables
W= full_data.ix[:,15:20] 
W= np.matrix(W)
min_max_scaler = preprocessing.MinMaxScaler()
W_minmax= min_max_scaler.fit_transform(W)

km = sklearn.cluster.MiniBatchKMeans(3)
km.fit(W_minmax)
wlabels= km.labels_

label_list_km=[[] for t in xrange(3)]
A=set(wlabels)
A=list(A)
for k in A:
        for u,v in enumerate(wlabels):
            if v==k:
                label_list_km[v].append(u)
WN_km=np.empty((0,3))
for k in xrange(3):
    no=0
    yes=0
    t=len(label_list_km[k])
    if t>0:
        for e in label_list_km[k]:
            if numeric_labels[e]==0:
                no+=1;
            else:
                yes+=1
        py=float(yes)/t
        pn=float(no)/t
        WN_km=np.vstack((WN_km,[k,yes,no]))
econ_suc= full_data.ix[label_list_km[0],:]
econ_suc.to_csv('economics_1.csv')

### 16.4
full_data.ix[:,22]= pd.to_datetime( full_data.ix[:,22])
before= full_data[full_data['Date']<= '2009-07-01']
after= full_data[full_data['Date']> '2009-07-01']
before.Date.count()
after.Date.count()
full_data.ix.shape()

z1= Z_minmax[0:37107,:]
z2= Z_minmax[37107:,:]

km1 = sklearn.cluster.MiniBatchKMeans(700)
km1.fit(z1)
label1 = km1.labels_
list1=[[] for t in xrange(700)]
A=set(label1)
A=list(A)
for k in A:
        for u,v in enumerate(label1):
            if v==k:
                list1[v].append(u)
rslt1 =np.empty((0,4))
for k in xrange(700):
    no=0
    yes=0
    scr=0
    py=0
    t=len(list1[k])
    if t>0:
        for e in list1[k]:
            if numeric_labels[e]==0:
                no+=1;
            else:
                yes+=1
        py=float(yes)/t
        pn=float(no)/t
        scr = py/pn if pn>0 else 9999
    rslt1=np.vstack((rslt1,[k,t,scr,py]))

rslt1[np.where(rslt1[:,2]>0.75),1]
index_n = & (rslt1[:,1]>100))
rslt1[np.where((rslt1[:,2]>=0.5)),0:1]
suceess_1= full_data.ix[ list1[61]+list1[233]+list1[312]+list1[316]+list1[341]+list1[355]+list1[427]+list1[483]+list1[666]+list1[436]+list1[514]+list1[719]+list1[964],:]
suceess_1.to_csv('suuccessbefore2009.csv')
#
af = AffinityPropagation(preference=-50).fit(z1)
cluster_centers_indices = af.cluster_centers_indices_
labels = af.labels_

n_clusters_ = len(cluster_centers_indices)
list1=[[] for t in xrange(n_clusters)]
A=set(label1)
A=list(A)
for k in A:
        for u,v in enumerate(label1):
            if v==k:
                list1[v].append(u)
rslt1 =np.empty((0,4))
for k in xrange(n_clusters):
    no=0
    yes=0
    scr=0
    py=0
    t=len(list1[k])
    if t>0:
        for e in list1[k]:
            if numeric_labels[e]==0:
                no+=1;
            else:
                yes+=1
        py=float(yes)/t
        pn=float(no)/t
        scr = py/pn if pn>0 else 9999
    rslt1=np.vstack((rslt1,[k,t,scr,py]))

np.where((rslt1[:,2]>=1) & (rslt1[:,1]>50))

.labels_
A=set(cluster_labels)



km2 = sklearn.cluster.MiniBatchKMeans(50)
km2.fit(z2)
label2 = km2.labels_
list2=[[] for t in xrange(50)]
A2=set(label2)
A2=list(A2)
for k in A2:
        for u,v in enumerate(label2):
            if v==k:
                list2[v].append(u+37107)
rslt2 =np.empty((0,4))
for k in xrange(50):
    no=0
    yes=0
    scr=0
    py=0
    t=len(list2[k])
    if t>0:
        for e in list2[k]:
            if numeric_labels[e]==0:
                no+=1;
            else:
                yes+=1
        py=float(yes)/t
        pn=float(no)/t
        scr = py/pn
    rslt2=np.vstack((rslt2,[k,t,scr,py]))
rslt2[np.where(rslt2[:,2]>=1),0]
index_n =np.where((rslt2[:,2]>=1) & (rslt2[:,1]>100))

suceess_2= full_data.ix[ list2[17]+list2[18]+list2[24]+list2[29]+list2[30]+list2[49],:]
suceess_2.to_csv('suuccessafter2009.csv')

bank <- read.csv("C:/Users/Razer/Desktop/Chandra/bank-additional.csv")
View(bank)

library(randomForest)

for(i in 1:(ncol(bank)-1)){
  plot(bank[,i],bank[,ncol(bank)])
}

# previously successful campaigns
prev.success = subset(bank,bank$poutcome=='success')
prev.failure = subset(bank,bank$poutcome!='success')
View(prev.success)

for(i in 1:(ncol(prev.success)-1)){
  plot(prev.success[,i],prev.success[,ncol(prev.success)],main = 'prev.success')
}

'deposits positively influenced by:
  1. divorced 
  2. professional course
  3. march month'

# training with oversampled successful data and undersampled failure data
oversample = ifelse(runif(nrow(prev.success))<0.9,T,F)
undersample = ifelse(runif(nrow(prev.success))<0.1,T,F)

train = rbind(prev.failure[oversample,],prev.success[undersample,])
test = rbind(prev.failure[!oversample,],prev.success[!undersample,])

fit <- randomForest(y~.-duration,data=train, importance=TRUE, ntree=1000)
varImpPlot(fit,main='Variable Importance')
prediction = predict(fit,test)
result=table(prediction,test$y)

accuracy = (result[1]+result[4])/sum(result)
accuracy

# Clustering
library(cluster)

# take only those variables that seem important
train = ifelse(runif(nrow(bank))<0.2,T,F)
sample = bank[train,c('job','education','age','day_of_week','euribor3m','marital','nr.employed','month')]
dsy= daisy(x = sample[,c('job','education','age','day_of_week','euribor3m','marital','nr.employed','month')])


hcl = hclust(dsy)
plot(as.dendrogram(hcl))


' analysing the clusters'
cut5 = cutree(hcl, k = 1:5) #k = 1 is trivial
cutree(hcl, h = 250)

## Compare the 2 and 4 grouping:
g24 <- cutree(hcl, k = c(1,2))
table(grp2 = g24[,"1"], grp4 = g24[,"2"])




write.csv(bank.mod,'bank.mod.csv')

# create numerical variables
bank.mod = bank
names = colnames(bank)
counter= 1
for(name in names){
  for(level in levels(bank[,name])){
    bank.mod[,toString(level)]=ifelse((bank.mod[,name]==level),1,0)
    counter = counter + 1
  }
  bank.mod[,name]=NULL
}


# library(kmeans)
bank.mod=scale(bank.mod)

fit=kmeans(bank.mod,5)
View(aggregate(bank.mod,by=list(fit$cluster),FUN=mean))

write.csv(aggregate(bank.mod,by=list(fit$cluster),FUN=mean),'cluster_data.csv')
#length(levels(bank[,colnames(bank)[1]]))

cluster1 = bank.mod[fit$cluster==1,]
cluster2 = bank.mod[fit$cluster==2,]
cluster3 = bank.mod[fit$cluster==3,]
cluster4 = bank.mod[fit$cluster==4,]
cluster5 = bank.mod[fit$cluster==5,]

bank.mod$y = bank$y

bank_data <- read.csv("~/bank-additional-full.csv", header=TRUE)#read bank_additional_full
require(zoo)
month_list=unique(bank_data$month)
as.Date(month_list)
for( i in 1:12){print (i)}
year=rep(0,41188)
for(i in 1:41188)
{
  if(i<27681){year[i]=2008}
  else if(i<39131) {year[i]=2009}
  else year[i]=2010
}
t_date={}
for (i in 1:41188)
{
  t_date<- append(t_date,as.yearmon(paste(bank_data$month[i],year[i]),"%b%Y"))
  
}
bank_data[22]=t_date
names(bank_data)[22]="Date"
ggplot(aes(x =as.Date.yearmon(bank_data$Date), y =bank_data$emp.var.rate,group=1), data = bank_data) + geom_line()
ggplot(aes(x =as.Date.yearmon(bank_data$Date), y =bank_data$cons.price.idx,group=1), data = bank_data) + geom_line()
ggplot(aes(x =as.Date.yearmon(bank_data$Date), y =bank_data$cons.conf.idx,group=1), data = bank_data) + geom_line()
ggplot(aes(x =as.Date.yearmon(bank_data$Date), y =bank_data$nr.employed,group=1), data = bank_data) + geom_line()




contacted_before=bank_data[which(bank_data$pdays!=999),]
not_contacted_before=bank_data[which(bank_data$pdays==999),]
require(zoo)
month_list=unique(bank_data$month)
as.Date(month_list)
for( i in 1:12){print (i)}
year=rep(0,41188)
for(i in 1:41188)
{
  if(i<27681){year[i]=2008}
  else if(i<39131) {year[i]=2009}
  else year[i]=2010
}
t_date={}
for (i in 1:41188)
{
  t_date<- append(t_date,as.yearmon(paste(bank_data$month[i],year[i]),"%b%Y"))

}
bank_data[22]=t_date
names(bank_data)[22]="Date"

client_info=bank_data[,1:7]
#write.csv(client_info,'client_data.csv',sep=',')
factor_column={}
for(i in 1:ncol(bank_data) )
{
  if(class(bank_data[,i])=="factor"){
    factor_column=append(factor_column,i)}
}
bank_duplicate<-bank_data
for(i in 1:ncol(bank_data) )
{
 if(class(bank_data[,i])=="factor"){
   factor_types<-unique(bank_data[,i])
   for ( j in 1:length(factor_types))
   {
     #n<-ncol(bank_duplicate)+1
     bank_duplicate<-cbind(bank_duplicate,sapply(bank_duplicate[,i],function(x) ifelse(x==factor_types[j],1,0)))
   }
 }
}
View(bank_duplicate[,c(2,seq(23,34,1))])
names(bank_duplicate)[23]="job:housemaid"
names(bank_duplicate)[24]="job:services"
names(bank_duplicate)[25]="job:admin"
names(bank_duplicate)[26]="job:bluecollar"
names(bank_duplicate)[27]="job:technician"
names(bank_duplicate)[28]="job:retired"
names(bank_duplicate)[29]="job:management"
names(bank_duplicate)[30]="job:unemployed"
names(bank_duplicate)[31]="job:selfemployed"
names(bank_duplicate)[32]="job:unknown"
names(bank_duplicate)[33]="job:entrepreneur"
names(bank_duplicate)[34]="job:student"
View(bank_duplicate[,c(3,seq(35,38,1))])
names(bank_duplicate)[35]="married"
names(bank_duplicate)[36]="single"
names(bank_duplicate)[37]="divorced"
names(bank_duplicate)[38]="marrittal:unknown"
View(bank_duplicate[,c(4,seq(39,46,1))])
names(bank_duplicate)[39]="education:basic4y"
names(bank_duplicate)[40]="education:highschool"
names(bank_duplicate)[41]="education:basic6y"
names(bank_duplicate)[42]="education:basic9y"
names(bank_duplicate)[43]="education:professional_course"
names(bank_duplicate)[44]="education:unknown"
names(bank_duplicate)[45]="education:university_deg"
names(bank_duplicate)[46]="education:illiterate"
View(bank_duplicate[,c(5,seq(47,49,1))])
names(bank_duplicate)[47]="default:no"
names(bank_duplicate)[48]="default:unknown"
names(bank_duplicate)[49]="default:yes"
View(bank_duplicate[,c(6,seq(50,52,1))])
names(bank_duplicate)[50]="housing:no"
names(bank_duplicate)[51]="housing:yes"
names(bank_duplicate)[52]="housing:unknown"
View(bank_duplicate[,c(7,seq(53,55,1))])
names(bank_duplicate)[53]="loan:no"
names(bank_duplicate)[54]="loan:yes"
names(bank_duplicate)[55]="loan:unknown"
View(bank_duplicate[,c(8,seq(56,57,1))])
names(bank_duplicate)[56]="contact:telephone"
names(bank_duplicate)[57]="contact:cellular"  
View(bank_duplicate[,c(15,seq(58,60,1))])
names(bank_duplicate)[58]="pout:nonexistant"
names(bank_duplicate)[59]="pout:failure"
names(bank_duplicate)[60]="pout:success"


bank_duplicate<-bank_duplicate[,-factor_column]
write.csv(bank_duplicate,'numeric_bank_data.csv',sep=',')
job_types<-unique(bank_data$job)
job_list<-list()
for ( i in 1:length(job_types))
{
  n<-ncol(client_info)+1
  client_info[,n]<-sapply(bank_data$job,function(x) ifelse(x==job_types[i],1,0))
  names(client_info)[n]<-job_types[i]
}

numeric_bank_data<-numeric_bank_data[,-1]
fit=kmeans(bank_data,10,nstart=100)


(bank_data[12,22]-bank_data[41188,22])*12


client_info= bank_duplicate[,c(1,seq(12,44,1))]
write.csv(client_info,"client_data.csv",sep ="," )
client_info=as.matrix(client_info)
d1<-dist(client_info)
hclust(d1,method="complete")

contact_data<- numeric_bank_data[,c(3,4,5,47,48,49)]
write.csv(contact_data,"contatct_data.csv",sep ="," )

c_n<- length(which(cluster_1$y=="yes"))
length(which(cluster_3$y=="yes"))
length(which(cluster_4$y=="yes"))
length(which(success_cluster_2$y=="yes"))

succ_cluster<- rbind(success_cluster_2,cluster_1,cluster_3,cluster_4)

require(ggplot2)

n_y <- length(which(succ_cluster$y=="yes"))
n_n <- length(which(succ_cluster$y=="no"))

t_y<- length(which(full_data$y=="yes"))
t_n<- length(which(full_data$y=="no"))


a = c(1,1,1,1,1,1,1,2,2,2,2,2,2,2)
b = c("A","A","A","B","B","B","B","C","C","D","D","D","D","D")
c = c(60,20,20,80,5,5,5,50,50,25,25,25,20,5)
dat = data.frame(Group=a, Member=b, Percentage=c)
ggplot(dat, aes(x=Member, y=Percentage, fill=Percentage)) + geom_bar(stat="identity", colour="white")


DF <- data.frame(rbind(c(n_y,t_y),c(n_n,t_n)))
rownames(DF)[1]<- 'Yes'
rownames(DF)[2]<- 'No'
colnames(DF)[1]<- 'Cluster Data'
colnames(DF)[2]<- 'Total Data'
DF$Yes= as.numeric(DF$Yes)
DF$No= as.numeric(DF$No)
library(reshape2)
DF1 <- melt(DF, id.var="Rank")

datm<-melt(cbind(DF, ind = rownames(DF)), id.vars = c('ind'))

ggplot(datm,aes(x = variable, y = value,fill = ind)) + 
  geom_bar(position = "fill",stat = "identity") +xlab('Data')+ylab('Y/N')
  scale_y_continuous(labels = percent_format())
  
write.csv(succ_cluster,'Clusters.csv')  

hist(succ_cluster$age)
hist(full_data$age)

bar(succ_cluster$job)

write.csv(summary(full_data[,16:20]),'full_cluster_economic_condition.csv')
write.csv(summary(succ_cluster[,17:21]),'success_cluster_economic_condition.csv')
write.csv(summary(e_success[,17:21]),'economic_cluster.csv')



df1<- data.frame(full_data$Date,full_data$cons.conf.idx)
p1 <- ggplot(df1, aes(x = full_data$Date, y = full_data$cons.conf.idx))
p2 <- p1 + geom_point(color="blue") + geom_line() 

plot(full_data[e_success$X +1,]$Date,e_success$cons.conf.idx,type='b')
plot(full_data$Date, full_data$cons.conf.idx,main="Consumer Confidence Index for Whole data",xlab="Time",ylab="CCF")

plot(full_data[e_success$X +1,]$Date,e_success$euribor3m,type='l')
plot(full_data$Date, full_data$euribor3m,main="Euribor for The Whole Data",xlab='Time',ylab="Euribor")

plot(full_data[succ_cluster$X +1,]$Date,succ_cluster$euribor3m,main='Euribor for the Success Clusters',xlab='Time',ylab="Euribor")
plot(full_data[succ_cluster$X +1,]$Date,succ_cluster$cons.conf.idx,main='Consumer Confidence Index for success clusters',xlab="Time",ylab="CCF")


ggplot(data.frame(full_data[succ_cluster$X +1,]$Date,succ_cluster$euribor3m),aes(full_data[succ_cluster$X +1,]$Date,succ_cluster$euribor3m))



#age
g <- ggplot(full_data, aes(age))
g + geom_bar()
g <- ggplot(succ_cluster, aes(age))
g + geom_bar()
#job*
g<- ggplot(full_data, aes(job))
g + geom_bar()+ labs(title='Job_profile in the entire data set')
g<- ggplot(succ_cluster, aes(job))
g + geom_bar()+ labs(title='Job_profile in success clusters')
#marital
g<- ggplot(full_data, aes(marital))
g + geom_bar()+ labs(title='Marital status entire data set')
g<- ggplot(succ_cluster, aes(marital))
g + geom_bar()+ labs(title='Marital Status in success clusters')
#education*
g<- ggplot(full_data, aes(education))
g + geom_bar()+ labs(title='Education in entire data set')
g<- ggplot(succ_cluster, aes(education))
g + geom_bar()+ labs(title='Education in success clusters')
##default
g<- ggplot(full_data, aes(default))
g + geom_bar()+ labs(title='Default in entire data set')
g<- ggplot(succ_cluster, aes(default))
g + geom_bar()+ labs(title='Default in success clusters')
#housing loan
g<- ggplot(full_data, aes(housing))
g + geom_bar()+ labs(title='Default in entire data set')
g<- ggplot(succ_cluster, aes(housing))
g + geom_bar()+ labs(title='Default in success clusters')
#contact
g<- ggplot(full_data, aes(contact))
g + geom_bar()+ labs(title='Contact Type in entire data set')
g<- ggplot(succ_cluster, aes(contact))
g + geom_bar()+ labs(title='Contact Type  in success clusters')
#contact data
#poutcome*
g<- ggplot(full_data, aes(poutcome))
g + geom_bar()+ labs(title='Result of previous campaign for the entire data set')
g<- ggplot(succ_cluster, aes(poutcome))
g + geom_bar()+ labs(title='Result of Previous campaign for success clusters')
#
g<- ggplot(full_data, aes(previous))
g + geom_bar()+ labs(title='Previous campaign for the entire data set')
g<- ggplot(succ_cluster, aes(previous))
g + geom_bar()+ labs(title='Previous campaign for success clusters')

bank_data[,22]<- as.Date(bank_data[,22])

econ<- bank_data[,c(16:20,22)]
months<- unique(econ$Date)
ueq<- {}
for ( i in 1:length(months)){
ueq<- rbind(ueq,econ[match(months[i],econ$Date),])
}

qplot(ueq$Date, ueq$emp.var.rate,data=ueq,main="Employment variation Rate")+xlab("TimeLine")+ylab("Emp. variation rate")+geom_line()
qplot(ueq$Date, ueq$cons.price.idx,data=ueq,main="CPI")+geom_line()
qplot(ueq$Date, ueq$cons.conf.idx,data=ueq,main="CCF")+geom_line()+xlab("TimeLine")+ylab("")
qplot(ueq$Date, ueq$euribor3m,data=ueq,main="Euribor")+geom_line()+xlab("TimeLine")+ylab("Euribor")
qplot(ueq$Date, ueq$nr.employed,data=ueq,main="Number of Employed ")+geom_line()
qplot(ueq$Date, ueq$delccf,data=ueq,main="Change in CCF")+geom_line()+xlab("TimeLine")+ylab("Change in CCF")
qplot(ueq$Date, ueq$delcpi,data=ueq,main="CPI Inflation ")+geom_line()+xlab("TimeLine")+ylab("CPI Inflation")
qplot(ueq$Date, ueq$delnemp,data=ueq,main="Employement Change ")+geom_line()

cor(ueq[,c(1,4,7,8,9)])

a={}
for ( i  in 1:24){
  a=append(a,100*(ueq$cons.price.idx[i+1]-ueq$cons.price.idx[1])/ueq$cons.price.idx[1])
}
ueq$delcpi<- c(0,a)
b={}
for ( i  in 1:24){
  b=append(b,100*(ueq$nr.employed[i+1]-ueq$nr.employed[i])/ueq$nr.employed[i])
}
ueq$delnemp<- c(0,b)

c={}
for ( i  in 1:24){
  c=append(c,100*(ueq$cons.conf.idx[i+1]-ueq$cons.conf.idx[1])/ueq$cons.conf.idx[1])
}
ueq$delccf<- c(0,c)


#Rebound Year 2009-10 
full<- cbind(client_data,contact_data)
scaled.full<- scale(full)
dd.plot(scaled.full, alpha=0.1)
answer<- outlier(scaled.full,logical = TRUE)

ggplot(b4, aes(b4$education,fill=..count..))+  
  geom_bar()+ labs(title='Education :Before 2010') +
  labs(x="Education", y="Count")
ggplot(after, aes(after$education,fill=..count..))+  
  geom_bar()+ labs(title='Since 2010') +
  labs(x="Education", y="Count")

ggplot(b4, aes(b4$marital,fill=..count..))+  
  geom_bar()+ labs(title='Marital status:Before 2010') +
  labs(x="Marital Status", y="Count")

ggplot(after, aes(after$marital,fill=..count..))+  
  geom_bar()+ labs(title='Marital status:Since 2010') +
  labs(x="Marital Status", y="Count")

ggplot(b4, aes(b4$housing,fill=..count..))+  
  geom_bar()+ labs(title='Housing Loan:Before 2010') +
  labs(x="Housing Loan", y="Count")

ggplot(after, aes(after$housing,fill=..count..))+  
  geom_bar()+ labs(title='Hosing laon: Since 2010') +
  labs(x="Housing Loan", y="Count")

ggplot(b4, aes(b4$loan,fill=..count..))+  
  geom_bar()+ labs(title='Before 2010') +
  labs(x="Loan", y="Count")

ggplot(after, aes(after$housing,fill=..count..))+  
  geom_bar()+ labs(title='Sincee 2010') +
  labs(x=" Loan", y="Count")

ggplot(b4, aes(b4$job,fill=..count..))+  
  geom_bar()+ labs(title='Job profiile:Before 2010') +
  labs(x="Job ", y="Count")

ggplot(after, aes(after$job,fill=..count..))+  
  geom_bar()+ labs(title='Job profiile:Since 2010') +
  labs(x="Job", y="Count")

ggplot(b4, aes(b4$poutcome,fill=..count..))+  
  geom_bar()+ labs(title='Before 2010') +
  labs(x="Previous Result ", y="Count")

ggplot(after, aes(after$poutcome,fill=..count..))+  
  geom_bar()+ labs(title='Since 2010') +
  labs(x="Previous Result ", y="Count")

y1=3130/38144
y2= 1510/ 3044
df<- data.frame(c('Before 2010','Since 2010'),c(y1,y2))
colnames(df)<- c('Timeline','Yes_Proportion')
p2<-ggplot(df,aes(x=factor(Timeline),y=Yes_Proportion), color=factor(Timeline)) +  
  stat_summary(fun.y=mean,position="stack",geom="bar")+xlab("Timeline")+ylab("Proportion of Yes")+ggtitle("Change in Yes Proportion")
plot(p2)


pie <- ggplot(bank_data, aes(x = factor(1), fill = factor(job))) +
  geom_bar(width = 1)
pie + coord_polar(theta = "y")+ylab("Job Profile")


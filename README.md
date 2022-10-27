*                       R-Lab-SLIIT-2Y2S

# display 1 to 200 values

print(100:200)

# get cunt working dir

getwd()

# change curent working d

setwd("path")

# help ?solve

help("data.frame")

# install package

install.packages("ggplot2")

# vector

w<-c(1,2,3,4,5)
w
class(w)

# factor

gender<-c(0,1,0,1,0,0,1)
gender

Gender <-factor(gender,c(0,1),c("male","female"))
Gender

# male   female male   female male   male   female

# Levels: male female

# list

p<-c(1,2,3)
q<-"green"
r<-21
p
q
r
Data<-list(p,q,r)
Data

# metrix

matris1<- matrix(c(1,2,3,4),nrow = 2,ncol = 2,byrow = True)
matris1

# data frame

height<-c(67,89,43,90)
weigth<-c(56,34,49,67)
data_set<- data.frame(height,weigth)
data_set

setwd("C:\\Users\\ASUS\\Downloads")
getwd()
data<- read.csv("DATA 3.csv",header = TRUE)

names(data)<-c("Age","Gender","Accomodation")
data$Gender <- factor(data$Gender,c(1,2),c("Male","female"))
fix(data0)
data$Accomodation <- factor(data$Accomodation,c(1,2,3),c("aaa","bbb","ccc"))
fix(data)

# attach modification to file

attach(data)

# frequency table

gender.freq<-table(Gender)
acc.freq<-table(Accomodation)
gender.freq
acc.freq

# create pie chart

pie(gender.freq,"Pie chart for Gender")
pie(acc.freq,"Pie chart acc")

# bar chart

barplot(gender.freq,main = "Gender",ylab = "freq")
abline(h=0)

# boxplot

boxplot(Age,main="boxplot for age",ylab="Age",outpch=8)

# two way freq table

gender_acc.freq<-table(Gender , Accomodation)
gender_acc.freq
barplot(gender_acc.freq,main = "gender acc ",legend=row.names(gender_acc.freq))
abline(h=0)

# clusted bar chart

barplot(gender_acc.freq,beside = True,main = "Gender & Accomadation",legend=row.names(gender_acc.freq))
abline(h=0)

# side by side boxplot                                                                            outpch using outline pattern= any number

boxplot( AGe~Gender,main = "Gender & Accomadation",xlab="Gender",ylab="Age")
boxplot(AGe~Accomodation,main="Boxplot for Age by Accmodation",xlab = "Accmodation",ylab = "Age",outpch=2)

# get mean two variable categorical

xtabs(AGe~Gender+Accomodation)/gender_acc.freq

# --------------------------------------------------------------------------------------------

# x <- seq(-pi, pi, 0.1)

# plot(x, cos(x))

setwd("C:\\Users\\User\\Downloads")
data<-read.table("sample.text",header=True,sep=",")

# editor mode

fix(data)

# rename col haders

names(data)<-c("Team","Attendance","salary","years")
data

# give access varible coloum

attach(data)

boxplot(Attendance)
#not using attach
boxplot(data$Attendance)

# boxplot

boxplot(Attendance,main = "boxplot for Attendance",outline = TRUE,xlab="Attendance",horizontal =TRUE)

# histrogram

hist(Attendance,main ="His for Attendance",ylab ="Freq")

# set x=0

abline(h=0)

# stem -leaf

stem(Attendance)

# mean

mean(Attendance)

# median

median(Attendance)

# standard deviation

sd(Attendance)

# get all values mean mediam etc..

summary(Attendance)

# quantile values

quantile(Attendance)

# get quantile by index  Q1,Q2,Q3 .. etc

quantile(Attendance)[2]
quantile(Attendance)[4]

# IQR

IQR(Attendance)

# function acept args year give the mode

get.modes<-function(y){
counts<-table(y)
names(counts)[counts==max(counts)]
}

# call function

get.modes(years)

# get outliers (min= q1-1.5*IQR/ max = Q1+1.5*IQR)

get.outliers<- function(y){
q1<- quantile(y)[2]
q2<- quantile(y)[4]
iqr<-q3-q1
ub<- q3 -1.5*iqr
lb<- q1-1.5*iqr
print(paste("Upper",ub))
print(paste("Lower",ub))
print(paste("outliers",paste(sort(y[y<lb[y>ub]]),collapse = ",")))
}
get.outliers(years)

# concadinate vector

paste()

# --------------------------------------------------------------------------------------------

getwd()

data1<-read.table("Data.text",header=TRUE,sep=",")

# change table haders

names(data1)<-c("x1",x2)

attach(data1)

# histrogram

hist(x2,main="histrogram title")

# 7 clss and histrogram width limitation

hist(x2,main="histrogram title",breaks=seq(130,270,length=8))

# number of class

# length = n+1

# freuency distribution

    # 1 ) identify break point

histrogram<- hist(x2,main="histrogram title",breaks=seq(130,270,length=8))
breaks<- round(histrogram)
breaks

# 2) identify freuency of each class

freuency<-histrogram$counts
freuency
#3) identify tha mid point each class
mid<-histrogram$mids
mid

# create vector

class<-c()

# repeate until break points

for(i in 1: length(breaks)-1){

# concadinate string (paste0 usin can get without spaces)

    class[i]<-paste0("(",breaks[i],",",breaks[i+1],")")

}

# distribution freuency table display

cbind(class=class,Freuency=freuency)

# fraw int plot same freuency table

lines(histrogram$mids,freuency)

# draw new plot type = l, o,p

plot(mid.freuency,type="l",main="freuency polygon for number of sharholders",xlab="sharholders",ylab="freuency",ylim=c(
0,max(freuency)))

# cumalative freuency

cum.freq<- cumsum(freuency)

new <- c()
for(i in 1: length(breaks)){
if(i==1){
new[i]=0
}else{
new[i] =cum.freq[i-1]
}
}

plot(breaks,new,type="o",main="freuency polygon for number of sharholders",xlab="sharholders",ylab=" cumalative
freuency",ylim=c(0,max(cum.freq)))

# display cumalative polygon

cbind(Upper=breaks,CumFreq=new)

# -----------------------------------------------------------

# get summary of the data sets

str(data)

# obsavation ex-:517 obs.

# find max and min

max(wind)
min(wind)

# get 5 number summary

summary(temprager)

# find outliers (pch is outliers icons 0-25)

boxplot(wind,horizontal=TRUE,outliers=TRUE,pch=16)

# median

median(temp)

# mean and sd

mean(wind)
sd(wind)

# IQR

IQR(wind)

# find obsavation friday and sunday

# 2 way freuency table for day and month

freq<-table(day,month)
#answer-: get in the freq table cross valye fridayand september

# find avg= mean temprager september

mean(temp(month=="sep")

# get max count day in jul month

count<-table(day(month=="jul"))
names(count==max(count))

# -----------------------------------------------------------

# change data stur

# horizontal way

datastr<-data[[1]]

# find varince

var(data)

# using sample

s<- sample(data,5)

# get 1:30 samples

samples<-c()
n<-c()
for(i in 1:30){
s<-sample(data,5)
samples<-cbind(samples,s)
n<-c(n.paste("S",i))
}
colnames(samples)<-n
samples

# find samples mean

s.mean<-colMeans(samples)
s.mean

# find samples varince

s.vars<- apply(samples,2,var)
s.vars

# sample mean

mean(s.mean)

# find sample mean varince

var(s.mean)

# compaire values

    # population mean
    mean(data)
    # sample mean
    mean(s.mean)

# answer- values are approximatly equal

# population varince

va(data)
var(s.mean)

# 2 value are not equal becourse sample size is too small












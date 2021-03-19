install.packages("readxl")
install.packages("table1")
library(finalfit)
library(table1)
library(readxl)
library(uwIntroStats)
data1<-read_excel("/Users/why/Desktop/uw/course/2020 spring/BIOST 579/final project/DataFile1.xls",sheet=1,col_names=TRUE)
data2<-read_excel("/Users/why/Desktop/uw/course/2020 spring/BIOST 579/final project/DataFile2.xls",sheet=1,col_names=TRUE)
data3<-read_excel("/Users/why/Desktop/uw/course/2020 spring/BIOST 579/final project/DataFile3.xls",sheet=1,col_names=TRUE)
data<-merge(data1,data2,by="id")
datafull<-merge(data,data3,by="id")
data$rec_or_dth<-with(data,ifelse(recvte==1|death==1,1,0))
data3$rec_or_dth<-with(data3,ifelse(recvte2==1|death2==1,1,0))
datafull$rec_or_dth<-with(datafull,ifelse(recvte2==1|death2==1,1,0))
data_woman<-subset(data,sex==0)


##problem 1

model1<-regress("odds",rec_or_dth~age+as.factor(hrt)+as.factor(oc)+bmi+as.factor(act)+as.factor(pstmp2)*as.factor(sex),data=data)

##problem2
data_woman<-replace(data_woman,'.',NA)
data_woman$pstmp2<-factor(data_woman$pstmp,levels=c('0','1','2'))
model2<-regress("odds",rec_or_dth~age+as.factor(hrt)+as.factor(oc)+bmi+as.factor(act)+as.factor(priorcvd)+as.factor(pstmp2),data=data_woman)



#descriptive
#table1
data$vte_type<-factor(data$vte_type,levels=1:3,labels=c("DVT","PE","both"))
data$sex<-factor(data$sex,levels=0:1,labels=c("Female","Male"))
data$race<-factor(data$race,levels=c(0,1,2,9))
data$pstmp<-factor(data$pstmp,levels=0:2,labels=c("No","Post","Peri"))
data$act<-factor(data$act,levels=0:1,labels=c("No","Yes"))
data$smoker<-factor(data$smoker,levels=c("never smoker","current smoker","former smoker"),labels=c("0","1","2"))
data$priorcvd<-factor(data$priorcvd,levels=0:1,labels=c("No","Yes"))
data$hrt<-factor(data$hrt,levels=0:1,labels=c("No","Yes"))
data$oc<-factor(data$oc,levels=0:1,labels=c("No","Yes"))

label(data$vte_type)<-"Type of incident VTE"
label(data$age)<-"Age at incident VTE (years)"
label(data$pstmp)<-"Post-menopausal status"
label(data$bmi)<-"Body mass index (kg/m^2)"
label(data$act)<-"Anticoagulation therapy at baseline "
label(data$smoker)<-"Smoking status"
label(data$priorcvd)<-"Previous cardiovascular disease"
label(data$hrt)<-"Hormone replacement therapy use at baseline"
label(data$oc)<-"Oral contraceptive use at baseline"
data$rec_or_dth<-factor(data$rec_or_dth,levels=0:1,labels=c("No recurrence or death","recurrence or death"))
table1(~vte_type+age+sex+race+pstmp+bmi+act+smoker+priorcvd+hrt+oc | rec_or_dth,data=data)
library(ggplot2)
library(reshape2)
#histogram
sub <- data[,c(3,7)]
data_hist<- melt(sub)
ggplot(data =data_hist, aes(x = value)) + 
  geom_histogram() + theme_bw()+
  facet_wrap(~variable, scales = "free")+theme(strip.text = element_text(size=30),axis.title=element_text(size=30,face="bold") )


##missing data

library(VIM)
## This will give you a histogram of missing data (left panel) and
## the pattern of missingness (right panel)
aggr_plot <- aggr(data[,-c(1,17)], col=c('navyblue','red'), numbers=TRUE,prop=TRUE,
                  sortVars=TRUE, labels=names(data[,-c(1,17)]),cex.lab = 2,cex.axis = 1.3, gap=1, ylab=c("Histogram of missing data","Pattern"),combined=TRUE)
## This will provide a spinogram, displaying the proportion of missing (red) ## versus not (light blue) by group
data$sex<-factor(data$sex,levels=c("0","1"))

spineMiss(data[, c("sex", "pstmp")])
spineMiss(data[, c("sex", "smoker")])
spineMiss(data[,c("sex","bmi")])
spineMiss(data[,c("sex","oc")])
data$rec_or_dth<-factor(data$rec_or_dth,levels=0:1)
explanatory = c("vte_type","age","sex","race","pstmp","bmi","act","smoker","priorcvd","hrt","oc" )
dependent = "rec_or_dth"
data %>% 
  missing_pairs(dependent, explanatory)

##line chart
png("/Users/why/Desktop/uw/course/2020 spring/BIOST 579/final project/linechart.jpg")
# whole data
x<-c(1,2)
plot(x,c(0.1259105,0.09674235),type = "o",col = "black", xlab = "follow-up years", ylab = "rate of recurrence or death",xaxt="n")
axis(1,at=c(1,2),labels=c("1","2"))
lines(x,c(0.1254789,0.09649123),type = "o",col = "red")
lines(x,c(0.1264237, 0.09706546),type = "o",col = "blue")
# Save the file.
dev.off()

data_fem<-subset(data,sex=="0")
data3_fem<-subset(datafull,sex=="0")
m0.1<-sum(data_fem$pstmp=="0"&data_fem$rec_or_dth==1,na.rm=TRUE)/sum(data_fem$pstmp=="0",na.rm=TRUE)
m1.1<-sum(data_fem$pstmp=="1"&data_fem$rec_or_dth==1,na.rm=TRUE)/sum(data_fem$pstmp=="1",na.rm=TRUE)
m2.1<-sum(data_fem$pstmp=="2"&data_fem$rec_or_dth==1,na.rm=TRUE)/sum(data_fem$pstmp=="2",na.rm=TRUE)
m0.2<-sum(data3_fem$pstmp=="0"&data3_fem$rec_or_dth==1,na.rm=TRUE)/sum(data3_fem$pstmp=="0",na.rm=TRUE)
m1.2<-sum(data3_fem$pstmp=="1"&data3_fem$rec_or_dth==1,na.rm=TRUE)/sum(data3_fem$pstmp=="1",na.rm=TRUE)
m2.2<-sum(data3_fem$pstmp=="2"&data3_fem$rec_or_dth==1,na.rm=TRUE)/sum(data3_fem$pstmp=="2",na.rm=TRUE)

##predict model
library(pROC)
basic<-glm(rec_or_dth~age+act+priorcvd,family=binomial,data=data)
pred_baisc<-predict(basic,newdata=datafull,type="response")
roc_basic<-plot.roc(datafull$rec_or_dth,pred_baisc,col="red")

data-replace(data,'.',NA)
data$smoker<-factor(data$smoker,levels=c('0','1','2'))
model1<-glm(rec_or_dth~age+act+priorcvd+smoker+vte_type,family=binomial,data=data)
pred_model1<-predict(model1,newdata=datafull,type="response")
roc1<-plot.roc(datafull$rec_or_dth,pred_model1,col="blue",add=TRUE)

model2<-glm(rec_or_dth~act+priorcvd+pstmp2+hrt,family=binomial,data=data)
pred_model2<-predict(model2,newdata=datafull,type="response")
roc2<-plot.roc(datafull$rec_or_dth,pred_model2,col="green",add=TRUE)

model3<-glm(rec_or_dth~age*priorcvd+age*act+pstmp2,family=binomial,data=data)
pred_model3<-predict(model3,newdata=datafull,type="response")
roc3<-plot.roc(datafull$rec_or_dth,pred_model3,col="black",add=TRUE)

legend("bottomright",legend=c("basic model AUC=0.629","model1 AUC=0.620","model2 AUC=0.592","model3=0.632"),bty="n",lwd=2, cex=1,col=c("red","blue","green","black"))






-----------------------------------------------------------------------------------------------------
# The code has been written for the winter project guided by Latenview Analytics as a part of       #
# data preparation by Group 4                                                                       #
# Co-author: Bodhisattwa, Somya and Faichali                                                        #
-----------------------------------------------------------------------------------------------------
library(ggplot2)
library(stringr)

setwd("C:/Users/Bodhisattya/Desktop/PGDBA/Latentview Winter/codes")
data=read.csv("CAX_Startup_Data - Copy(2).csv")
data_old=data
# "no info" and "unknown amount" were replaced with nothing in the excel itself
# values which could be filled from the variable "last round of funding received in million USD"
# into "last funding amount" were filled
# similarly values from "year of founding" and "established founding date" were filled into "age of 
# company" wherever possible

#str(data)

#for loop to find out number of missing values in each column
missing_values=c()
for(i in 1:ncol(data)){
  if(class(data[,i])=="integer"|class(data[,i])=="numeric"){
    missing_values=append(missing_values,length(which(is.na(data[,i]==TRUE))))
  }
  if(class(data[,i])=="factor"){
    missing_values=append(missing_values,length(which(data[,i]=="")))
  }
}
write.csv(cbind(colnames(data),missing_values),"missing_values.csv")

#find which variables have less than 15% missingness
var=which(missing_values/nrow(data)<.15)
write.csv(cbind(colnames(data)[var],var),"var.csv")

data_new=data[,var]
#str(data_new)

#also remove the variable "year of founding"
a=which(colnames(data_new)=="year.of.founding")
data_new=data_new[,-a]

# new features added into this new data frame by feature engineering,we merge the new features 
# with this new data frame after imputing the missing values

# since variable "focus function of company" have values almost similar to industry of the variable
# we drop the variable "Focus functions of company
b=which(colnames(data_new)=="Focus.functions.of.company")
data_new=data_new[,-b]
#str(data_new)
write.csv(data_new,"data_new.csv")

numeric=c()
categorical=c()
for(i in 1:ncol(data_new)){
  if(class(data_new[,i])=="integer"|class(data_new[,i])=="numeric"){
  numeric=append(numeric,i)  
  }
  if(class(data_new[,i])=="factor"|class(data_new[,i])=="character"){
  categorical=append(categorical,i)
  }
}

# impute categorical variables
for(i in categorical){
  data_new[,i]=tolower(data_new[,i])
  data_new[,i]=as.factor(data_new[,i])
  if(length(which(data_new[,i]==""))==0)
    next
  d=subset(data_new[,c(2,i)],data_new[,i]!="")
  d[,2]=droplevels(d[,2])
  t=table(d[,1],d[,2])
  t=as.matrix(t)
  f_impute=colnames(t)[which(t[1,]==max(t[1,]))] 
  s_impute=colnames(t)[which(t[2,]==max(t[2,]))]
  for(j in 1:nrow(data_new)){
    if(data_new[j,i]==""){
      if(data_new[j,2]=="Failed")
        data_new[j,i]=f_impute
      else
        data_new[j,i]=s_impute
    }
  }
}

# impute numeric variables
for(i in numeric){
  data_new[,i]=tolower(data_new[,i])
  data_new[,i]=as.numeric(data_new[,i])
  if(length(which(is.na(data_new[,i])==TRUE))==0)
    next
  t=tapply(data_new[,i],data_new[,2],median,na.rm=TRUE)
  t=as.matrix(t)
  f_impute=t[1,1]
  s_impute=t[2,1]
  for(j in 1:nrow(data_new)){
    if(is.na(data_new[j,i])==TRUE){
      if(data_new[j,2]=="Failed")
        data_new[j,i]=f_impute
      else
        data_new[j,i]=s_impute
    }
  }
}

------------------------------------------------------------------------------------------------------------------------------------------
# Feature Engineering
# tokenization, removing comma, spelling correction on the text data has been done in Excel
# creating dummy variables for Industry of company
data_new$Industry.of.company <- lapply(data$Industry.of.company, FUN=tolower)
data_new$Is_marketing <- sapply(data$Industry.of.company, FUN = function(x) max(as.numeric(str_detect(x,c("marketing","market_research")))))
data_new$Is_analytics <- sapply(data$Industry.of.company, FUN = function(x) max(as.numeric(str_detect(x,c("analytics")))))
data_new$Is_e_commerce <- sapply(data$Industry.of.company, FUN = function(x) as.numeric(str_detect(x,"e-commerce")))
data_new$Is_telco <- sapply(data$Industry.of.company, FUN = function(x) max(as.numeric(str_detect(x,c("mobile","network_hosting_infrastructure","social_networking","telecommunications")))))
data_new$Is_software <- sapply(data$Industry.of.company, FUN = function(x) max(as.numeric(str_detect(x,c("enterprise_software","cloud_computing","software_development","search","email","security","cleantech")))))
data_new$Is_advertising <- sapply(data$Industry.of.company, FUN = function(x) max(as.numeric(str_detect(x,c("advertising","classifieds")))))
data_new$Is_entertainment <- sapply(data$Industry.of.company, FUN = function(x) max(as.numeric(str_detect(x,c("media","entertainment","music","gaming")))))
data_new$Is_retail <- sapply(data$Industry.of.company, FUN = function(x) max(as.numeric(str_detect(x,c("retail","food_&_beverages")))))
data_new$Is_energy <- sapply(data$Industry.of.company, FUN = function(x) max(as.numeric(str_detect(x,c("energy")))))
data_new$Is_healthcare <- sapply(data$Industry.of.company, FUN = function(x) max(as.numeric(str_detect(x,c("healthcare","hospitality","pharmaceuticals")))))
data_new$Is_education <- sapply(data$Industry.of.company, FUN = function(x) max(as.numeric(str_detect(x,c("education","publishing")))))
data_new$Is_HR <- sapply(data$Industry.of.company, FUN = function(x) max(as.numeric(str_detect(x,c("career_job_search","human_resources_")))))
data_new$Is_finance <- sapply(data$Industry.of.company, FUN = function(x) max(as.numeric(str_detect(x,c("finance","crowdfunding")))))
data_new$Is_transport <- sapply(data$Industry.of.company, FUN = function(x) max(as.numeric(str_detect(x,c("transportation","travel","space_travel")))))
data_new$Is_real_estate <- sapply(data$Industry.of.company, FUN = function(x) max(as.numeric(str_detect(x,c("real_estate","deals")))))
data_new$Is_other <-  sapply(data$Industry.of.company, FUN = function(x) max(as.numeric(str_detect(x,c("government","")))))


# new feature for list of investors
# list of important investors has been created separately
# Presence of Important (top 5) Investors as a feature
# tokenization, comma removal, spelling correction has been done in Excel

data$Investors <- lapply(data$Investors, FUN=tolower)
data_new$important_investors <- sapply(data$Investors, FUN = function(x) max(as.numeric(str_detect(x,c("sv_angel","500_startups","y_combinator","data_collective","andreessen_horowitz")))))


------------------------------------------------------------------------------------------------------------------------------------------------------------
#chisquare test of the categorical variables and bar-plot visualizations

chi_square=data.frame()
var_to_keep=c()
for(i in categorical){
  
  #chi-square test
  y1=data_new[,2]
  y2=data_new[,i]
  c=chisq.test(y1,y2)#is the distribution of y1(response) statistically dependent on y2(predictor)
  c$p.value#if p value <0.05, then the variable y2 is significant
  
  #visualization
  ggplot(data_new,aes(x= as.factor(data_new[,30]), fill = Dependent.Company.Status)) + geom_bar(position = "fill") + labs(x = colnames(data_new)[30], y = "proportion")
}
colnames(chi_square)=c("sl.no","variable","p-value")
write.csv(chi_square,"chi_square.csv")

---------------------------------------------------------------------------------------------------------------------------------------
# test for numerical variables
  
data_success=subset(data_new,tolower(data_new$Dependent.Company.Status)=="success")
data_failed=subset(data_new,tolower(data_new$Dependent.Company.Status)=="failed")

# check for 95% ci for mean and median
# this is obtained by generating bootstrapped samples from the data

library("boot", lib.loc="~/R/win-library/3.2")

mean=c()
median=c()

for(k in numeric){
  
  # CI for mean
  b_success=boot(data_success[,k],function(u,i) mean(u[i]),R=1000)
  b=boot.ci(b_success)#cofidence interval for success rows
  x1=rbind(b$normal[,2:3],b$basic[,4:5],b$percent[,4:5],b$bca[,4:5])
  
  b_failed=boot(data_failed[,k],function(u,i) mean(u[i]),R=1000)
  b=boot.ci(b_failed)#confidence interval for failed rows
  y1=rbind(b$normal[,2:3],b$basic[,4:5],b$percent[,4:5],b$bca[,4:5])
  
  mean=rbind(mean,cbind(rbind(colnames(data_new)[k],"","",""),x1,y1))
  
  # CI for median  
  b_success=boot(data_success[,k],function(u,i) median(u[i]),R=1000)
  b=boot.ci(b_success)#cofidence interval for success rows
  x2=rbind(b$normal[,2:3],b$basic[,4:5],b$percent[,4:5],b$bca[,4:5])
  
  b_failed=boot(data_failed[,k],function(u,i) median(u[i]),R=1000)
  b=boot.ci(b_failed)#confidence interval for failed rows
  y2=rbind(b$normal[,2:3],b$basic[,4:5],b$percent[,4:5],b$bca[,4:5])
  
  median=rbind(median,cbind(rbind(colnames(data_new)[k],"","",""),x2,y2))
}
colnames(mean)=c("variable","lower interval success","upper interval success","lower interval failure","upper interval failure")
write.csv(mean,"mean_ci.csv")

colnames(median)=c("variable","lower interval success","upper interval success","lower interval failure","upper interval failure")
write.csv(median,"median_ci.csv")

# check the csv files to see if intervals for success and failure are overlapping
# if they overlap then we can say that mean/median are same for both success and failure and
# the particular variable can be deemed insignificant

# the CIs for median for all variables could not be calculated. So, for those variables we have 
# to visually see the histograms and the summary to comment upon the significance of the variables

smry=c()
for(i in numeric){
  d_f=data_failed[,i]
  d_s=data_success[,i]
  smry=rbind(smry,cbind(rbind(colnames(data_new)[i],"","","","",""),summary(data_new[,i]),summary(d_f),summary(d_s))) 
  hist(d_f,breaks=20,main=paste(colnames(data_new)[i],"failed"))
  hist(d_s,breaks=20,main=paste(colnames(data_new)[i],"success"))
  hist(data_new[,i],main=colnames(data_new)[i],breaks=20)
  
}
write.csv(smry,"summary.csv")
# check the summary along with the histograms generated

write.csv(data_new,"data_new.csv")



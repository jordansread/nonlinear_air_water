##11-10-2015
#predict future walleye recruitment success based on air temperature metrics
#classify as "fail", "success" for use in tier system to guide stocking - predict probability of success
#defined as greater or less than 10 per mile

library(ggplot2)
library(dplyr)
library(gridExtra)
library(scales)
library(randomForest)
library(reshape2)

#data processing
##################################################
#treaty
mydata<-read.csv("C:/Users/hanseg/Documents/BassWalleye/trends/final.yoy.2014.csv", header=TRUE, sep=",")
summary(mydata)
mydata$WBIC<-factor(mydata$WBIC)

#additional data from statewide database
mydata1<-read.csv("C:/Users/hanseg/Documents/BassWalleye/trends/additional_yoy_fixed.csv", header=TRUE, sep=",")
summary(mydata1)
mydata1$WBIC<-factor(mydata1$WBIC)

#remove duplicates
toadd<-mydata1[!mydata1$id%in%mydata$id,]

mydata1.1<-rbind(mydata, toadd)

mydata2<-subset(mydata1.1, age0cpe.mile!="NA" & Year>1988)
mydata2$year.factor<-factor(mydata2$Year)
mydata2$log.cpe<-log(mydata2$age0cpe.mile+0.015)

#make recruitment categorical
mydata2$recruitment<-"poor"
#mydata2$recruitment[mydata2$age0cpe.mile<=10]<-"fail"
#mydata2$recruitment[mydata2$age0cpe.mile>=10&mydata2$age0cpe.mile<30]<-"good"
mydata2$recruitment[mydata2$age0cpe.mile>=10]<-"good"
mydata2$recruitment<-factor(mydata2$recruitment)



#read in Alex's data
lake<-read.csv("C:/Users/hanseg/Documents/BassWalleye/trends/AL_WIlakecharacteristics.csv", header=TRUE, sep=",")
#take out alex's lat long
lake<-lake[,-c(3,4,5)]
#merge
mydata3.1<-merge(mydata2, lake, by="WBIC")

#other location data
location<-read.csv("C:/Users/hanseg/Documents/BassWalleye/trends/all_lakes_climate_division.csv", sep=",", header=TRUE)
location<-location[,2:4]

mydata3<-merge(mydata3.1, location, by="WBIC")

summary(mydata3)
mydata3$Landlock<-factor(mydata3$Landlock)
mydata3$Impound<-factor(mydata3$Impound)
#mydata3$landscapeposition<-factor(mydata3$landscapeposition)
#mydata3$log.area.acres<-log(mydata3$Area*.000247105)
mydata3$area.hectares<-mydata3$Area*.0001

mydata3$watershedarea<-mydata3$HUC10Area*.0001
mydata3$SDF<-mydata3$Length/(2*(pi*(mydata3$Area))^.5)
mydata3$Length<-log(mydata3$Length*.00062)
mydata3$accessscore<-factor(mydata3$accessscore)
mydata3$publicaccess<-factor(mydata3$publicaccess)
mydata3$mroad_dist<-mydata3$mroad_dist/1000
mydata3$Road_dist<-mydata3$Road_dist/1000


############################################################################################
#read in temp data
temp<-read.delim("C:/Users/hanseg/Documents/BassWalleye/air vs water fish models/hab_metrics_AIR.tsv", sep="\t", header=TRUE)
summary(temp)


#only use temp metrics likely related to walleye
#read in which columns to exclude
exclude.temps<-c("dateOver18","dateOver20", "dateOver21", "GDD_air_10c", "days_above_8.9" ,  "days_above_18",  "days_above_20" , "days_above_21")

temp3<-temp[,!colnames(temp)%in%exclude.temps]


#caluclate mean temp metrics but don't use 1979-1989 to be consistent with water temperature analysis and data availability
temp3<-temp3[temp3$year>1988,]

temp.means<-aggregate(temp3[,c(3:ncol(temp3))], by=list(temp3$lake.id), FUN="mean", na.rm=T)
#ensure that metrics measured in days are whole days - getting weird results based on difference in means of .1 days
temp.means[,c(3:5,14)]=round(temp.means[,c(3:5,14)], 0)
#round warming rates
temp.means[,c(6:11)]=round(temp.means[,c(6:11)], 3)


mydata3.3<-merge(mydata3, temp.means, by.x=c("WBIC"), by.y=c("Group.1"))


#####################################################################################
#use secchi from temp data - hierarchical approach. Use in situ mean if available, then satellite mean
secchi<-read.csv("C:/Users/hanseg/Documents/BassWalleye/ModeledTemperatureData/secchi_hierarchical.txt", sep="\t", header=TRUE)
secchi.means=subset(secchi, secchi_source=="in-situ mean" | secchi_source=="satellite mean")
secchi.lake.means=summarise(group_by(secchi.means, lakeid, secchi_source), secchi.m=mean(secchi.m))
#make wide
secchi.wide=dcast(secchi.lake.means, lakeid~secchi_source, value.var="secchi.m")

mydata3.4<-merge(mydata3.3, secchi.wide, by.y="lakeid", by.x= "WBIC", all.x=T)
mydata3.4$secchi.m=mydata3.4$"in-situ mean"
mydata3.4$secchi.m[is.na(mydata3.4$secchi.m)]=mydata3.4$"satellite mean"[is.na(mydata3.4$secchi.m)]

#if secchi missing from Luke's data check alex's data

mydata3.4$secchi.m[is.na(mydata3.4$secchi.m)]=mydata3.4$Secchi[is.na(mydata3.4$secchi.m)]
mydata3.4$secchi.m[is.na(mydata3.4$secchi.m)]=mydata3.4$Secchi_satellite[is.na(mydata3.4$secchi.m)]

################################################################################################

#remove landcover variables  - for climate projection - and proximate variables like lat and long

#list variables of interest for prediction
predictor.variables<-c("recruitment", "publicaccess","MaxDepth",  "Impound", "Conductance","landscapeposition",
                       "area.hectares", "watershedarea" ,"SDF" ,
                       "peak_temp" , "dateOver5", "dateOver8.9" , "coef_var_May" ,  "coef_var_April" , "coef_var_June"  ,
                       "warm_rate_April" ,         "warm_rate_May","warm_rate_June",
                       "mean_jul", "mean_JAS" ,"spring_days_in_10.5_15.5", "GDD_air_5c" ,
                        "secchi.m","WBIC")


mydata3.5<-mydata3.4[,colnames(mydata3.4)%in%predictor.variables]


######################################################################
#terrible non-automated code here.

#check for correlations and remove variables if |r|>=0.8


#remove factors and NA
corrdata<-mydata3.5[,-c(1,2,3,5)]
corrdata<-na.omit(corrdata)
cortable<-cor(corrdata)
write.table(cortable, "C:/Users/hanseg/Documents/BassWalleye/air vs water fish models/correlations_walleye_recruitment_analysis_airtemps.csv", sep=",", row.names=T)


#I identified these by looking at cortable
remove.variables<-c("dateOver5","mean_jul", "mean_JAS" , "spring_days_in_10.5_15.5","peak_temp", "coef_var_June","coef_var_May", "warm_rate_June"  )

#new variables to remove based on r>.8 
mydata3.6<-mydata3.5[,!colnames(mydata3.5)%in%remove.variables]



#code for dropping variables from RF models for model selection


#########################################################
#You will need to have your data frame with your response variable, WBIC, and all potential predictors, with na values removed
#At this stage I have removed predictors with |r|>0.8
#then I drop WBIC from the dataframe for running the looping code
#run random forest model for prediction with all variables
preds<-na.omit(mydata3.6)
preds2<-select(preds, -WBIC)

##DATA FORMATTING NOTES####
#this is set up for my walleye recruitment models, where the response (recruitment) is binary (good/poor). You will need to change this to reflect your 
#variable classes, and if there are more than 2 or it is a continuous variable, it will be different.
#Also note that in my case, "good" is the first level,
#"poor" is the second level. So make sure your classes correspond to the correct position in the matrices.

#also change "recruitment" in the RF model to whatever your response variable is called

##############################################

#variable selection, given new data

#for each iteration, run 50 random forest models, compute mean error rate (overall and individual classes) and variable importance

#change number of models here
nforest=50
errors<-c(0,0,0)
var.importance<-data.frame("variable"=0,"good"=0, "poor"=0, "MeanDecreaseAccuracy"=0, "MeanDecreaseGini"=0)


# Random Forest prediction of recruitment success
set.seed(17)
for(i in 1:nforest){
  fit <- randomForest(recruitment ~., data=preds2, importance=TRUE, ntree=1000, mtry=(ncol(preds2)/2), nodesize=5)
  
  ## The out of bag error
  ooo <- fit$confusion[, -dim(fit$confusion)[2]]
  s.ooo <- sum(ooo)
  diag(ooo) <- 0
  overall.error1<-sum(ooo)/s.ooo
  good.error1<-fit$confusion[1,3]
  poor.error1<-fit$confusion[2,3]
  
  oob.errors<-c(overall.error1, good.error1, poor.error1)
  
  errors=rbind(errors, oob.errors)
  
  add.importance<-data.frame("variable"=row.names(fit$importance), "good"=fit$importance[,1]/fit$importanceSD[,1],
                             "poor"=fit$importance[,2]/fit$importanceSD[,2],
                             "MeanDecreaseAccuracy"=fit$importance[,3]/fit$importanceSD[,3], 
                             "MeanDecreaseGini"=fit$importance[,4])
  var.importance=rbind(var.importance, add.importance)
}

overall.error<-mean(errors[2:nrow(errors),1])
overall.error.sd<-sd(errors[2:nrow(errors),1])
#calculate as simple formula (this is how Diaz Uriarte did it)
overall.error.sd2<-sqrt(overall.error*(1-overall.error)*(1/nrow(preds)))

good.error<-mean(errors[2:nrow(errors),2])
good.error.sd<-sd(errors[2:nrow(errors),2])
good.error.sd2<-sqrt(good.error*(1-good.error)*(1/nrow(preds)))

poor.error<-mean(errors[2:nrow(errors),3])
poor.error.sd<-sd(errors[2:nrow(errors),3])
poor.error.sd2<-sqrt(poor.error*(1-poor.error)*(1/nrow(preds)))



#calculate mean importance - all metrics
#this will be the importance used in all subsequent drops, as recalculating variable importance can result in severe overfitting
#as cited in Dias_Uriarte and Alvares de Andres 2006 paper
mean.importance<-aggregate(var.importance[2:nrow(var.importance),c(2:5)], by=list(var.importance$variable[2:nrow(var.importance)]), FUN="mean")
sd.importance<-aggregate(var.importance[2:nrow(var.importance),c(2:5)], by=list(var.importance$variable[2:nrow(var.importance)]), FUN="sd")
colnames(sd.importance)<-c("Group.1", "good.sd", "poor.sd", "MeanDecreaseAccuracy.sd", "Gini.sd")
mean.importance2<-merge(mean.importance, sd.importance, by="Group.1")
impvar <- mean.importance$Group.1[order(mean.importance$MeanDecreaseAccuracy, decreasing=TRUE)]
#write table of variable importance
write.table(mean.importance2, "C:/Users/hanseg/Documents/BassWalleye/trends/Future projections fish/Tables/variable_importance_yoy_newNLDAS.csv", sep=",", row.names=F)



#run new models, each time dropping 1 predictor. do not recalculate variable importance
# Random Forest prediction of recruitment success

for(j in 1:(length(impvar)-1)){
  
  #remove one variable at a time. how much does this affect the OOB error rate?
  
  #here, replace recruitment with your variable name
  
  keep<-c("WBIC", "recruitment",impvar[1:(length(impvar)-j)])
  
  mydata.model1<-mydata3.6[,(names(mydata3.6) %in% keep)]
  
  preds<-na.omit(mydata.model1)
  preds$WBIC<-factor(preds$WBIC)
  preds2<-select(preds, -WBIC)
  
  set.seed(17)
  errors<-c(0,0,0)
  
  for(i in 1:nforest){
    fit <- randomForest(recruitment ~., data=preds2, importance=TRUE, ntree=1000, mtry=(ncol(preds2)/2), nodesize=5)
    ## The out of bag error
    ooo <- fit$confusion[, -dim(fit$confusion)[2]]
    s.ooo <- sum(ooo)
    diag(ooo) <- 0
    overall.error1<-sum(ooo)/s.ooo
    good.error1<-fit$confusion[1,3]
    poor.error1<-fit$confusion[2,3]
    
    oob.errors<-c(overall.error1, good.error1, poor.error1)
    
    errors=rbind(errors, oob.errors)
    
  }
  print(j)
  
  overall.error<-c(overall.error,mean(errors[2:nrow(errors),1]))
  good.error<-c(good.error, mean(errors[2:nrow(errors),2]))
  poor.error<-c(poor.error, mean(errors[2:nrow(errors),3]))
  
}
print(overall.error)
print(good.error)
print(poor.error)




error.table<-data.frame("Model"=seq(1:length(impvar)), "Overall error"<-overall.error, "Good error"<-good.error, "Poor error"<-poor.error)
colnames(error.table)<-c("Model", "Overall error", "Good error", "Poor error")

write.table(error.table, "C:/Users/hanseg/Documents/BassWalleye/trends/Future projections fish/Tables/recruitment_model_errors_variable_selection_newNLDAS.csv", row.names=F, sep=",")


###############################################
#another terrible non-automated process here#
############################################

#Find the simplest model where the error rate is less than or equal to this value:
#maximum error allowed: 
min(overall.error)+overall.error.sd


#identify which model (which j value) corresponds to the simplest (fewest predictor variables) where the overall error rate is less than or equal to
#the error rate identified above. Set that j value below to view your final model


#Once you have identified the j value for model you will use, run this code to get your best model
###########################################################################################
#model 1
#within 1 SD

#set the j value here
j=18

keep<-c("WBIC", "recruitment",impvar[1:(length(impvar)-j)])

mydata.model2<-mydata3.6[,(names(mydata3.6) %in% keep)]


preds<-na.omit(mydata.model2)
preds2<-select(preds, -WBIC)

set.seed(17)
fit <- randomForest(recruitment ~., data=preds2, importance=TRUE, ntree=1000, mtry=(ncol(preds2)/2), nodesize=5)
print(fit)

windows()
varImpPlot(fit)



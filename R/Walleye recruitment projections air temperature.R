##7-1-2015
#predict future walleye recruitment success based on previous model and new temperature data
#classify as "fail", "success" for use in tier system to guide stocking - predict probability of success
#defined as greater or less than 10 per mile

library(ggplot2)
library(dplyr)
library(gridExtra)
library(lme4)
library(scales)
library(languageR)
library(corrgram)
library(randomForest)
library(reshape2)
library(rpart)      	        # Popular decision tree algorithm
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree
library(caret)					# Just a data source for this script
# but probably one of the best R packages ever. 

#build model based on historical data (model selection happened elsewhere)

#best model for classification includes
#[1] "area.hectares"  "SDF"            "conductance" "GDD_air_5c"


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

mydata2<-subset(mydata1.1, age0cpe.mile!="NA" & Year>1988 )
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
exclude.temps<-c("dateOver18","dateOver20", "dateOver21", "GDD_air_10c"   , "days_above_8.9"    ,       "days_above_18"     ,       "days_above_20"     ,       "days_above_21"  )

temp3<-temp[,!colnames(temp)%in%exclude.temps]




#caluclate mean temp metrics but don't use 1979-1989 a
temp3<-temp3[temp3$year>1988,]

temp.means<-aggregate(temp3[,c(3:ncol(temp3))], by=list(temp3$lake.id), FUN="mean", na.rm=T)

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


#list variables from contemporary model for prediction (selected elsewhere)
predictor.variables<-c("recruitment",  "Conductance" , "SDF", "GDD_air_5c" ,  "area.hectares", "MaxDepth", "WBIC")


mydata3.6<-mydata3.4[,colnames(mydata3.4)%in%predictor.variables]



################################################################################################

#run random forest model for prediction
preds<-na.omit(mydata3.6)
preds$WBIC<-factor(preds$WBIC)
preds2<-preds[,c(2:ncol(preds))]
set.seed=17

fit <- randomForest(recruitment ~., data=preds2, importance=TRUE, strata=preds$WBIC, 
                    sampsize=rep(1,nlevels(preds$WBIC)),ntree=1000, mtry=(ncol(preds2)/2), nodesize=5)

print(fit)


############################
#GENMOM
future.temp<-read.delim("C:/Users/hanseg/Documents/BassWalleye/air vs water fish models/hab_metrics_GENMOM_AIR.tsv", sep="\t", header=TRUE)


##################################################
#ECHAM5
future.temp2<-read.delim("C:/Users/hanseg/Documents/BassWalleye/air vs water fish models/hab_metrics_ECHAM5_AIR.tsv", sep="\t", header=TRUE)



############################
#CM2.0
future.temp3<-read.delim("C:/Users/hanseg/Documents/BassWalleye/air vs water fish models/hab_metrics_CM2.0_AIR.tsv", sep="\t", header=TRUE)


######################################################
#really only need GDD from these.
genmom=select(future.temp, year, lake.id, GDD_air_5c)
echam=select(future.temp2, year, lake.id, GDD_air_5c)
cm2=select(future.temp3, year, lake.id, GDD_air_5c)

###########################
#projections will focus on two time periods
#midcentury (2040-2064) - all three models
genmom.mid=subset(genmom, year>2039&year<2065)
echam.mid=subset(echam, year>2039&year<2065)
cm2.mid=subset(cm2, year>2039&year<2065)

genmom.mid.mean=aggregate(genmom.mid$GDD_air_5c, by=list(genmom.mid$lake.id), FUN="mean")
colnames(genmom.mid.mean)=c("lakeid", "GDD_air_5c")
echam.mid.mean=aggregate(echam.mid$GDD_air_5c, by=list(echam.mid$lake.id), FUN="mean")
colnames(echam.mid.mean)=c("lakeid", "GDD_air_5c")
cm2.mid.mean=aggregate(cm2.mid$GDD_air_5c, by=list(cm2.mid$lake.id), FUN="mean")
colnames(cm2.mid.mean)=c("lakeid", "GDD_air_5c")

#late(r) century (2070-2089)
genmom.late=subset(genmom, year>2064&year<2090)
echam.late=subset(echam, year>2064&year<2090)

genmom.late.mean=aggregate(genmom.late$GDD_air_5c, by=list(genmom.late$lake.id), FUN="mean")
colnames(genmom.late.mean)=c("lakeid", "GDD_air_5c")
echam.late.mean=aggregate(echam.late$GDD_air_5c, by=list(echam.late$lake.id), FUN="mean")
colnames(echam.late.mean)=c("lakeid", "GDD_air_5c")

#check distribution of GDD in contemporary vs. mid vs late - all lakes and walleye lakes
contemporary.means=select(temp.means, Group.1, GDD_air_5c)
colnames(contemporary.means)[1]="lakeid"
contemporary.means$Model="NLDAS"
contemporary.means$Time="1989-2014"

genmom.mid.mean$Model="GENMOM"
genmom.mid.mean$Time="2040-2064"
echam.mid.mean$Model="ECHAM"
echam.mid.mean$Time="2040-2064"
cm2.mid.mean$Model="CM2"
cm2.mid.mean$Time="2040-2064"

genmom.late.mean$Model="GENMOM"
genmom.late.mean$Time="2065-2089"
echam.late.mean$Model="ECHAM"
echam.late.mean$Time="2065-2089"

plot.gdd=rbind(contemporary.means, genmom.mid.mean, echam.mid.mean, cm2.mid.mean, genmom.late.mean, echam.late.mean)
plot.gdd$Model=factor(plot.gdd$Model, levels=c("NLDAS", "CM2", "ECHAM", "GENMOM"))
plot.gdd$Time=factor(plot.gdd$Time)


#link to lake data
lake.predictors<-select(lake, Area, Length, MaxDepth, Conductance, WBIC)
lake.predictors$area.hectares<-lake.predictors$Area*.0001
lake.predictors$SDF<-lake.predictors$Length/(2*(pi*(lake.predictors$Area))^.5)

future.gdd=merge(lake.predictors, plot.gdd, by.x="WBIC", by.y="lakeid")


#predict probability of success of all lakes based on both time periods, all models temperature data
future.gdd$probability.good<-predict(fit, future.gdd, type="prob")[,1]

#make table based on threshold gain, lose, maintain
threshold=0.47

future.gdd$success=0
future.gdd$success[future.gdd$probability.good>=threshold]=1

write.csv(future.gdd,"~/BassWalleye/air vs water fish models/future_wae_projected_probability_airtemp.csv", row.names=F )




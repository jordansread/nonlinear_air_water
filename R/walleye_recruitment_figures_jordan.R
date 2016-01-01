#random forest figures

#best model identified elsewhere and implemented here
#treaty
mydata<-read.csv("data/final.yoy.2014.csv", header=TRUE, sep=",")
summary(mydata)
mydata$WBIC<-factor(mydata$WBIC)

#additional data from statewide database
mydata1<-read.csv("data/additional_yoy_fixed.csv", header=TRUE, sep=",")
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
lake<-read.csv("data/AL_WIlakecharacteristics.csv", header=TRUE, sep=",")
#take out alex's lat long
lake<-lake[,-c(3,4,5)]
#merge
mydata3.1<-merge(mydata2, lake, by="WBIC")

#other location data
location<-read.csv("data/all_lakes_climate_division.csv", sep=",", header=TRUE)
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
temp<-read.delim("data/hab_metrics_AIR.tsv", sep="\t", header=TRUE)
summary(temp)


#only use temp metrics likely related to walleye
#read in which columns to exclude
exclude.temps<-c("dateOver18","dateOver20", "dateOver21", "GDD_air_10c"   , "days_above_8.9"    ,       "days_above_18"     ,       "days_above_20"     ,       "days_above_21"  )

temp3<-temp[,!colnames(temp)%in%exclude.temps]



#caluclate mean temp metrics but don't use 1979-1989 
temp3<-temp3[temp3$year>1988,]

temp.means<-aggregate(temp3[,c(3:ncol(temp3))], by=list(temp3$lake.id), FUN="mean", na.rm=T)
#ensure that metrics measured in days are whole days - getting weird results based on difference in means of .1 days
temp.means[,c(3:5,14)]=round(temp.means[,c(3:5,14)], 0)
#round warming rates
temp.means[,c(6:11)]=round(temp.means[,c(6:11)], 3)


mydata3.3<-merge(mydata3, temp.means, by.x=c("WBIC"), by.y=c("Group.1"))


#####################################################################################
#use secchi from temp data - hierarchical approach. Use in situ mean if available, then satellite mean
secchi<-read.csv("data/secchi_hierarchical.txt", sep="\t", header=TRUE)
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

#list variables from other model for prediction
predictor.variables<-c("recruitment",  "Conductance" , "SDF", "GDD_air_5c" ,  "area.hectares", "WBIC", "MaxDepth")


mydata3.6<-mydata3.4[,colnames(mydata3.4)%in%predictor.variables]



################################################################################################

#run random forest model for prediction
preds<-na.omit(mydata3.6)
preds$WBIC<-factor(preds$WBIC)
preds2<-select(preds, -WBIC)
set.seed=17

fit <- randomForest(recruitment ~., data=preds2, importance=TRUE, strata=preds$WBIC, 
                    sampsize=rep(1,nlevels(preds$WBIC)),ntree=1000, mtry=(ncol(preds2)/2), nodesize=5)

print(fit)


####################
#Here is some code I have used for figures - it is unmodified and kind of a mess, so you will have to adapt it. but use it if you want!



###########################################
#variable importance plot
#############################################
windows()
imp1 <- importance(fit, scale=TRUE)
impvar1 <- rownames(imp1)[order(imp1[, 3], decreasing=TRUE)]
op <- par(mfrow=c(2, 2))
for (i in 1:4) {
  partialPlot(fit, preds2, impvar1[i], xlab=impvar1[i],which.class="good",
              main=paste(impvar1[i]) )
}
par(op)

#save partial importance in datafile for easier plotting 
imp1 <- importance(fit, scale=TRUE)
impvar1 <- rownames(imp1)[order(imp1[, 3], decreasing=TRUE)]
partialplot.data<-c(0,0,0)
for (i in 1:4) {
  temp=partialPlot(fit, preds2, impvar1[i], xlab=impvar1[i],which.class="good", n.pt=1000)
  temp.data<-data.frame("x"=temp$x, "logit"=temp$y, "variable"=impvar1[i] )
  partialplot.data<-rbind(partialplot.data, temp.data)
}
partialplot.data<-partialplot.data[2:nrow(partialplot.data),]
partialplot.data$probability<-exp(partialplot.data$logit)/(1+exp(partialplot.data$logit))
write.table(partialplot.data, "data/partial_importance_yoy_predict_2015_newNLDAS.csv", sep=",", row.names=FALSE)


#make a nicer partial importance plot with probability instead of logit
windows()
z<-ggplot(partialplot.data, aes(x, probability))+geom_path(lwd=1.2)+ylab("Probability of walleye recruitment")
z=z+facet_wrap(~variable, scale="free_x", ncol=2)+theme_bw()
z=z+theme( axis.title.x=element_blank(),
           axis.title.y=element_text(size=14, face="bold", vjust=0.25),
           axis.text.x=element_text(size=12),
           axis.text.y=element_text(size=12),
           legend.title=element_text(size=14, face="bold"),
           legend.text=element_text(size=12),
           strip.text.x=element_text(size=14, face="bold"))
z=z+theme(
  panel.grid.major = element_line(colour=NA),
  panel.grid.minor = element_line(colour = NA),
  panel.background = element_rect(colour = NA),
  legend.position="bottom",
  strip.background=element_blank() 
)
print(z)
ggsave("figures/partial_effects_wae_yoy_2015_newNLDAS.tiff", height=6, width=6, units="in", dpi=300, compression="lzw")




########################################
#median and quartile partial plot
##########################################
source("/Users/hanseg/Documents/BassWalleye/trends/partialPlot_median_quartile.R")


z=partialPlot_range_withpoints(fit, preds2, "area.hectares", y="Probability of recruitment", which.class="good", xlab="Surface area (ha)", ylab="Probability of recruitment")
z2=partialPlot_range_withpoints(fit, preds2, "Conductance", y="Probability of recruitment", which.class="good", xlab="Conductivity", ylab="Probability of recruitment")
z3=partialPlot_range_withpoints(fit, preds2, "SDF", y="Probability of recruitment", which.class="good", xlab="SDF", ylab="Probability of recruitment")
z4=partialPlot_range_withpoints(fit, preds2, "GDD_wtr_5c", y="Probability of recruitment", which.class="good", xlab="Degree days", ylab="Probability of recruitment")


#make a nicer partial importance plot with probability instead of logit, with appropriate x axis labels
#must subset and combine separate plots.
area<-z$y
cond<-z2$y
sdf<-z3$y
gdd<-z4$y




#windows()
z<-ggplot(area)
z=z+geom_ribbon(aes(x, ymin=twentyfifth, ymax=seventyfifth),  fill="#dadaeb", aes=.2)
z=z+geom_path(aes(x, median), lwd=2)
z=z+scale_y_continuous(lim=c(0,.8), breaks=c(0,.2,.4,.8))
z=z+scale_x_log10()
z=z+xlab("Lake area (ha)")+ylab("Probability of recruitment success")+theme_bw()
z=z+theme( axis.title.x=element_text(size=12, face="bold"),
           axis.title.y=element_text(size=12, face="bold", vjust=0.25),
           axis.text.x=element_text(size=12),
           axis.text.y=element_text(size=12),        
           panel.grid.major = element_line(colour=NA),
           panel.grid.minor = element_line(colour = NA),plot.margin = unit(c(1,0,1.6,0.5), "lines"),
           panel.background = element_rect(colour = NA)
)
#z=z+geom_text(data=NULL, aes(x=10, y=.56), label="A", size=4, vjust=-0.4)
print(z)
#ggsave("partial_effects_wae_yoy_predictivemodel_alldata_m1.tiff", height=6, width=6, units="in", dpi=300, compression="lzw")


#windows()
z2<-ggplot(cond)
z2=z2+geom_ribbon(aes(x, ymin=twentyfifth, ymax=seventyfifth),  fill="#dadaeb", aes=.2)
z2=z2+geom_path(aes(x, median), lwd=2)
z2=z2+scale_y_continuous(lim=c(0,.8), breaks=c(0,.2,.4,.8))
z2=z2+xlab(expression(bold(atop("Conductance",(mu*S%.%cm^-1)))))+ylab("Probability of success")+theme_bw()
z2=z2+scale_x_continuous(breaks=seq(0,600,200))
z2=z2+theme( axis.title.x=element_text(size=12, face="bold"),
             axis.title.y=element_blank(),
             axis.text.x=element_text(size=12),
             axis.text.y=element_blank(),        
             panel.grid.major = element_line(colour=NA),
             panel.grid.minor = element_line(colour = NA), plot.margin = unit(c(1,0,.0,0), "lines"),
             panel.background = element_rect(colour = NA)
)
#z2=z2+geom_text(data=NULL, x=.1375, y=.56, label="C", size=4,vjust=-.4)

print(z2)
#ggsave("partial_effects_wae_yoy_predictivemodel_alldata_m1.tiff", height=6, width=6, units="in", dpi=300, compression="lzw")
#windows()
z3<-ggplot(sdf)
z3=z3+geom_ribbon(aes(x, ymin=twentyfifth, ymax=seventyfifth),  fill="#dadaeb", aes=.2)
z3=z3+geom_path(aes(x, median), lwd=2)
z3=z3+scale_y_continuous(lim=c(0,.8), breaks=c(0,.2,.4,.8))
z3=z3+xlab(expression(bold("SDF")))+ylab("Probability of success")+theme_bw()
z3=z3+theme( axis.title.x=element_text(size=12, face="bold"),
             axis.title.y=element_blank(),
             axis.text.x=element_text(size=12),
             axis.text.y=element_blank(),        
             panel.grid.major = element_line(colour=NA),
             panel.grid.minor = element_line(colour = NA),plot.margin = unit(c(1,0,1.6,0), "lines"),
             panel.background = element_rect(colour = NA)
)

#z3=z3+geom_text(data=NULL, x=1.2, y=.56, label="D", size=4,vjust=-.4)
print(z3)
#ggsave("partial_effects_wae_yoy_predictivemodel_alldata_m1.tiff", height=6, width=6, units="in", dpi=300, compression="lzw")




#windows()
z4<-ggplot(gdd)
z4=z4+geom_ribbon(aes(x, ymin=twentyfifth, ymax=seventyfifth),  fill="#dadaeb", aes=.2)
z4=z4+geom_path(aes(x, median), lwd=2)
z4=z4+scale_y_continuous(lim=c(0,.8), breaks=c(0,.2,.4,.8))
z4=z4+scale_x_continuous(breaks=c(2200,2600,3000))
z4=z4+xlab(expression(bold(atop("Degree days",(~degree~C%.%days)))))+ylab("Probability of recruitment success")+theme_bw()
z4=z4+theme( axis.title.x=element_text(size=12, face="bold"),
             axis.title.y=element_blank(),
             axis.text.x=element_text(size=12),
             axis.text.y=element_blank(),        
             panel.grid.major = element_line(colour=NA),
             panel.grid.minor = element_line(colour = NA),plot.margin = unit(c(1,0.2,0.0,0), "lines"),
             panel.background = element_rect(colour = NA)
)
#z4=z4+geom_text(data=NULL, x=2150, y=.56, label="B", size=4, hjust=.8, vjust=-.4)

print(z4)
#ggsave("partial_effects_wae_yoy_predictivemodel_alldata_m1.tiff", height=6, width=6, units="in", dpi=300, compression="lzw")

grid.arrange(arrangeGrob(z,z2,z3, z4, nrow=1, heights=c(1, 1,1,1)))
save.plot<-arrangeGrob(z,z2,z3,z4, nrow=1, heights=c(1, 1,1,1), widths=c(.28,.24,.24,.24))

ggsave('figures/partial_plots_wae_recruitment_newNLDAS.tiff',save.plot, height=6, width=10, units="in", dpi=300, compression="lzw")






########################################################
#contour plots#
#####################################################
#new function for 2 continuous variable partial plot
#backtransform option allows real probabilities instead of logit
source("C:/Users/hanseg/Documents/BassWalleye/trends/partialPlot2_probs.R")


windows()
tiff(filename = "figures/contour_GDD_area.tiff",
     width = 12, height = 12, units = "in", pointsize=20,
     compression = "lzw",
     bg = "white", res = 300)
partialPlot2(fit, preds2,  "GDD_wtr_5c","area.hectares", "Probability success", which.class="good", log.transform.x2=T)
dev.off()

windows()
tiff(filename = "figures/contour_GDD_conductivity.tiff",
     width = 12, height = 12, units = "in", pointsize=20,
     compression = "lzw",
     bg = "white", res = 300)
partialPlot2(fit, preds2,  "GDD_wtr_5c","Conductance", "Probability success", which.class="good")
dev.off()

windows()
tiff(filename = "figures/contour_area_conductance.tiff",
     width = 12, height = 12, units = "in", pointsize=20,
     compression = "lzw",
     bg = "white", res = 300)
partialPlot2(fit, preds2,  "area.hectares","Conductance", "Probability success", which.class="good")
dev.off()



windows()
tiff(filename = "figures/contour_area_sdf.tiff",
     width = 12, height = 12, units = "in", pointsize=20,
     compression = "lzw",
     bg = "white", res = 300)
partialPlot2(fit, preds2,  "area.hectares","SDF", "Probability success", which.class="good")
dev.off()

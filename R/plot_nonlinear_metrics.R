library(dplyr)
library(readr)
library(gsplot)



bin.w <- 100
air.bins <- seq(1200, 4200, by=bin.w)
min.n <- 100 # min obs within bin

GCMs = c("MIROC5",'IPSL', 'MRI', 'GFDL', 'CNRM', 'ACCESS')

data.early <- data.frame(site_id=c(), GDD_air_5=c(), GDD_wtr_5=c())
data.late <- data.early
for (GCM in GCMs){
  water.hab = read_delim(sprintf('data/hab_metrics/%s_thermal_metrics.tsv',GCM), delim = '\t')
  air.hab = read_delim(sprintf('data/hab_metrics/%s_air_metrics.tsv',GCM), delim = '\t')
  #end of century
  #GDD.water <- group_by(water.hab, site_id, year) %>% filter(year >2079) %>% summarize(GDD_wtr_5 = mean(as.numeric(gdd_wtr_5c)))  %>% filter(!is.na(GDD_wtr_5))
  #GDD.air <- rename(air.hab, site_id=lake.id) %>% group_by(site_id, year) %>% filter(year >2079, GDD_air_5c > 800) %>% # empty years are GDD zero - remove them
  #  summarize(GDD_air_5 = mean(GDD_air_5c)) %>% filter(!is.na(GDD_air_5))
  
  GDD.water <- water.hab %>% rename(GDD_wtr_5 = gdd_wtr_5c) %>% 
    filter(year > 2079, !is.na(GDD_wtr_5)) %>% 
    select(site_id, year, GDD_wtr_5)
  GDD.air <- air.hab %>% rename(site_id=lake.id, GDD_air_5 = GDD_air_5c) %>% 
    filter(year > 2079, !is.na(GDD_air_5), GDD_air_5 > 800) %>% 
    select(site_id, year, GDD_air_5)
  data.late = left_join(GDD.air, GDD.water, by=c('site_id', 'year')) %>% rbind(data.late)
  # 
  # GDD.water <- group_by(water.hab, site_id, year) %>% filter(year < 2001) %>% summarize(GDD_wtr_5 = mean(as.numeric(gdd_wtr_5c)))  %>% filter(!is.na(GDD_wtr_5))
  # GDD.air <- rename(air.hab, site_id=lake.id) %>% group_by(site_id, year) %>% filter(year < 2001, GDD_air_5c > 800) %>% # empty years are GDD zero - remove them
  #   summarize(GDD_air_5 = mean(GDD_air_5c)) %>% filter(!is.na(GDD_air_5))
  # data.early = left_join(GDD.air, GDD.water, by='site_id') %>% rbind(data.early)
  
  GDD.water <- water.hab %>% rename(GDD_wtr_5 = gdd_wtr_5c) %>% 
    filter(year < 2001, !is.na(GDD_wtr_5)) %>% 
    select(site_id, year, GDD_wtr_5)
  GDD.air <- air.hab %>% rename(site_id=lake.id, GDD_air_5 = GDD_air_5c) %>% 
    filter(year < 2001, !is.na(GDD_air_5), GDD_air_5 > 800) %>% 
    select(site_id, year, GDD_air_5)
  data.early = left_join(GDD.air, GDD.water, by=c('site_id', 'year')) %>% rbind(data.early)
}

# data.early <- data.early %>% 
#   mutate(rat = GDD_wtr_5/GDD_air_5) # add a ratio to create "expected"
# 
# rat <- data.early %>% group_by(site_id) %>% 
#   summarize(rat = mean(rat, na.rm = TRUE))
# 
# warning('assuming same order')
# if (!data.late$site_id[100] == data.early$site_id[100]) stop('assumption wrong')
# data.late$rat = data.early$rat # assumed same order
# 
# data.late <- data.late %>% 
#   mutate(expected = GDD_air_5*rat)

pl.data <- data.late %>% 
  mutate(bin = cut(GDD_air_5, breaks=air.bins, labels=FALSE)) %>% filter(!is.na(bin)) %>% 
  group_by(bin) %>% summarize(med=median(GDD_wtr_5, na.rm=TRUE), p.75=quantile(GDD_wtr_5, 0.975, na.rm=TRUE), p.25=quantile(GDD_wtr_5, 0.025, na.rm=TRUE), n= length(GDD_wtr_5)) %>% 
  filter(n > min.n)

gs = gsplot() %>% polygon(c(air.bins[pl.data$bin]+bin.w/2, rev(air.bins[pl.data$bin]+bin.w/2)), c(pl.data$p.25, rev(pl.data$p.75)), col=rgb(1,0,0,0.3), border=NA) %>% 
  lines(air.bins[pl.data$bin]+bin.w/2, pl.data$med, col='red', asp=1, lwd=2.5, ylab=' ', xlab='Air GDD', legend.name='2080-2099')
  
# pl.data <- data.late %>% 
#   mutate(bin = cut(GDD_air_5, breaks=air.bins, labels=FALSE)) %>% filter(!is.na(bin)) %>% 
#   group_by(bin) %>% summarize(med=median(expected, na.rm=TRUE), p.75=quantile(expected, 0.975, na.rm=TRUE), p.25=quantile(expected, 0.025, na.rm=TRUE), n= length(expected)) %>% 
#   filter(n > min.n)
# 
# gs <- polygon(gs, c(air.bins[pl.data$bin]+bin.w/2, rev(air.bins[pl.data$bin]+bin.w/2)), c(pl.data$p.25, rev(pl.data$p.75)), col=rgb(.5,.5,.5,0.3), border=NA) %>% 
#   lines(air.bins[pl.data$bin]+bin.w/2, pl.data$med, col='grey20', asp=1, lwd=2.5, legend.name='expected 2080-2099')

pl.data <- data.early %>% 
  mutate(bin = cut(GDD_air_5, breaks=air.bins, labels=FALSE)) %>% filter(!is.na(bin)) %>% 
  group_by(bin) %>% summarize(med=median(GDD_wtr_5, na.rm=TRUE), p.75=quantile(GDD_wtr_5, 0.975, na.rm=TRUE), p.25=quantile(GDD_wtr_5, 0.025, na.rm=TRUE), n= length(GDD_wtr_5)) %>% 
  filter(n > min.n)


gs <- polygon(gs, c(air.bins[pl.data$bin]+bin.w/2, rev(air.bins[pl.data$bin]+bin.w/2)), c(pl.data$p.25, rev(pl.data$p.75)), col=rgb(0,0,1,0.3), border=NA) %>% 
  lines(air.bins[pl.data$bin]+bin.w/2, pl.data$med, col='blue', asp=1, lwd=2.5, ylab=' ', xlab='Air GDD', legend.name='1981-2000') %>% 
  legend(x='topleft', bty='n') %>% 
  view(ylim=c(1200,5500), xlim=c(1200,5500))

gs
graphics::mtext(side=2, 'Water GDD',  las=0, line=3)

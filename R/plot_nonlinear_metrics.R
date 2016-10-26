library(dplyr)
library(readr)
library(gsplot)



bin.w <- 50
air.bins <- seq(1200, 5500, by=bin.w)
min.n <- 100 # min obs within bin

GCMs = c("MIROC5",'IPSL', 'MRI', 'GFDL', 'CNRM', 'ACCESS')

data.early <- data.frame(site_id=c(), GDD_air_5=c(), GDD_wtr_5=c())
data.late <- data.early
for (GCM in GCMs){
  water.hab = read_delim(sprintf('data/hab_metrics/%s_thermal_metrics.tsv',GCM), delim = '\t')
  air.hab = read_delim(sprintf('data/hab_metrics/%s_air_metrics.tsv',GCM), delim = '\t')

  GDD.water <- water.hab %>% rename(GDD_wtr_5 = gdd_wtr_5c) %>% 
    filter(year > 2079, !is.na(GDD_wtr_5)) %>% 
    select(site_id, year, GDD_wtr_5)
  GDD.air <- air.hab %>% rename(site_id=lake.id, GDD_air_5 = GDD_air_5c) %>% 
    filter(year > 2079, !is.na(GDD_air_5), GDD_air_5 > 800) %>% 
    select(site_id, year, GDD_air_5)
  data.late = left_join(GDD.air, GDD.water, by=c('site_id', 'year')) %>% rbind(data.late)
  
  GDD.water <- water.hab %>% rename(GDD_wtr_5 = gdd_wtr_5c) %>% 
    filter(year < 2001, !is.na(GDD_wtr_5)) %>% 
    select(site_id, year, GDD_wtr_5)
  GDD.air <- air.hab %>% rename(site_id=lake.id, GDD_air_5 = GDD_air_5c) %>% 
    filter(year < 2001, !is.na(GDD_air_5), GDD_air_5 > 800) %>% 
    select(site_id, year, GDD_air_5)
  data.early = left_join(GDD.air, GDD.water, by=c('site_id', 'year')) %>% rbind(data.early)
}


rat <- data.early %>% group_by(site_id) %>%
  mutate(rat = GDD_wtr_5/GDD_air_5) %>% 
  summarize(rat = mean(rat, na.rm = TRUE))

data.late <- left_join(data.late, rat) %>% 
  mutate(expected = GDD_air_5*rat)


pl.data <- data.late %>% 
  mutate(bin = cut(GDD_air_5, breaks=air.bins, labels=FALSE)) %>% filter(!is.na(bin)) %>% 
  group_by(bin) %>% summarize(med=median(GDD_wtr_5, na.rm=TRUE), p.75=quantile(GDD_wtr_5, 0.975, na.rm=TRUE), p.25=quantile(GDD_wtr_5, 0.025, na.rm=TRUE), n= length(GDD_wtr_5)) %>% 
  filter(n > min.n)

gs = gsplot(mai=c(.75,1,.1,.1)) %>% polygon(c(air.bins[pl.data$bin]+bin.w/2, rev(air.bins[pl.data$bin]+bin.w/2)), c(pl.data$p.25, rev(pl.data$p.75)), col=rgb(1,0,0,0.3), border=NA) %>% 
  lines(air.bins[pl.data$bin]+bin.w/2, pl.data$med, col='red', asp=1, lwd=3.5, ylab=' ', xlab='Air GDD', legend.name='2080-2099')
  
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
  lines(air.bins[pl.data$bin]+bin.w/2, pl.data$med, col='blue', asp=1, lwd=3.5, ylab=' ', xlab='Air GDD', legend.name='1981-2000') %>% 
  legend(x='topleft', bty='n') %>% 
  abline(0,1, lty = 2, lwd=2, legend.name='1:1', col='black') %>% 
  view(ylim=c(1200,NA), xlim=c(1200,NA))
  #abline(500,1, lty = 1, lwd=2, col='black') %>% 
  #

gs
graphics::mtext(side=2, 'Water GDD',  las=0, line=3)

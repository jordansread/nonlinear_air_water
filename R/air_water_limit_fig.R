SP.wtr.raw <- read.csv('data/north_temperate_lakes_lter__daily_water_temperature_-_sparkling_lake.csv', stringsAsFactors = FALSE, header = TRUE)
SP.air.raw <- read.csv('data/north_temperate_lakes_lter__daily_meteorological_and_dissolved_oxygen_data_-_sparkling_lake_raft.csv', stringsAsFactors = FALSE, header = TRUE)
library(dplyr)

SP.air <- mutate(SP.air.raw, Date = as.Date(sampledate), air = avg_air_temp) %>% select(Date, air)
SP.wtr <- SP.wtr.raw %>% mutate(Date = as.Date(sampledate), wtr=wtemp) %>% select(Date, wtr)

data.joined <- inner_join(SP.wtr, SP.air)
d.2 <- data.joined %>% mutate(week=lubridate::week(Date), year=lubridate::year(Date)) %>% 
  group_by(week, year) %>% summarize(air=mean(air), water=mean(wtr))

layout(matrix(c(1,2), nrow = 2))
par(mai=c(.1,.1,.1,.1), omi=c(1,1, 0,0), mgp=c(2,.5,0))
plot(d.2$air, d.2$water, pch=20, col='grey60',
     ylim=c(0, 30),xlim=c(-10,40), axes=F)
curve = loess.smooth(d.2$air, d.2$water, span = 5/6, degree=2, family="gaussian") %>% data.frame
lines(curve %>% filter(x>0), lwd=3)
axis(1, labels=NA)
axis(2) 
mtext(side=2, 'Water temperature (°C)', line = 2)
box()
# library(dataRetrieval)
site = '07144790'
# new.data = readNWISdv(site, '00010', statCd = '00001') %>% 
#   renameNWISColumns() %>% 
#   rename(Wtemp_Max = Discontinued.March.2015_Wtemp_Max) %>% 
#   select(Date, Wtemp_Max)
# 
# write.table(new.data, file = sprintf('data/nwis_%s_wtr.tsv', site), quote = FALSE, sep = '\t', row.names = FALSE)

# library(geoknife)
# wd <- webdata('daymet', variables = 'tmax', times = c('2001-01-01T00:00:00Z','2016-01-01T00:00:00Z' ))
# job = geoknife(c(-97.79422, 37.72612), fabric = wd, wait=TRUE)
# download(job, sprintf('data/nwis_%s_daymet.csv', site))
new.data <- read.table(sprintf('data/nwis_%s_wtr.tsv', site), sep='\t', stringsAsFactors = FALSE, header=TRUE)
new.data$Date <- as.Date(new.data$Date)
daymet <- geoknife::parseTimeseries(sprintf('data/nwis_%s_daymet.csv', site), delim=',')
daymet$DateTime <- as.Date(daymet$DateTime)
daymet <- daymet %>% rename(Date = DateTime)
data.joined <- inner_join(new.data, daymet)
d.2 <- data.joined %>% mutate(week=lubridate::week(Date), year=lubridate::year(Date)) %>% 
  group_by(week, year) %>% summarize(air=mean(bufferedPoint), water=mean(Wtemp_Max))

plot(d.2$air, d.2$water, pch=20, col='grey60',
     ylim=c(0, 30),xlim=c(-10,40), ylab='Water temperature (°C)', xlab='Air temperature (°C)')
curve = loess.smooth(d.2$air, d.2$water, span = 5/6, degree=2, family="gaussian") %>% data.frame
lines(curve %>% filter(x>0), lwd=3)
mtext(side=2, 'Water temperature (°C)', line = 2)
mtext(side=1, 'Air temperature (°C)', line = 2)
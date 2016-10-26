
library(readr)
library(mda.lakes)
library(dplyr)

empirical.habitat.calc = function(file.name, model, lakeid){
  
  data.url = 'http://cida-test.er.usgs.gov/mda.lakes/drivers_GLM_%s/%s'
  first_doy_above <- function(t, date, threshold, year){
    as.numeric(julian(date[which(t > threshold)[1]], origin = as.Date(paste0(year[1],'-01-01'))))
  }
  
  coef_var_month <- function(t, months, month){
    u.i = months == month
    sd(t[u.i])/mean(t[u.i])
    
  }
  
  warm_rate_month <- function(t, months, month){
    u.i = months == month
    if (sum(u.i) > 15){
      lm(t[u.i]~seq_len(sum(u.i)))$coefficients[2]
    } else {
      NA
    }
  }
  
  mean_months <- function(t, months, month){
    u.i = months %in% month
    mean(t[u.i])
  }
  
  days_between <- function(t, months, month, low, high){
    u.i = months %in% month
    sum(t[u.i] >= low & t[u.i] <= high)
  }
  
  days_above <- function(t, threshold){
    sum(t > threshold)
  }
  
  GDD <- function(t, threshold){
    dd = t - threshold
    sum(dd[dd > 0], na.rm=TRUE)
  }
  # divide up years
  destfile = tempfile(fileext = '.RData')
  download.file(sprintf(data.url, model, file.name), destfile=destfile)
  load(destfile)
  data.site$lake.id = lakeid
  air.data <- data.site %>% 
    rename(time=DateTime) %>% 
    mutate(year=as.numeric(strftime(time, format="%Y"))) %>% 
    mutate(month=months(time)) %>% 
    select(year, month, time, airtemp, lake.id) %>% 
    group_by(year,lake.id) %>% summarize(
      peak_temp=max(airtemp), 
      dateOver5=first_doy_above(airtemp,time, 5, year),
      dateOver6=first_doy_above(airtemp,time, 6, year),
      dateOver8.9=first_doy_above(airtemp,time, 8.9, year),
      dateOver18=first_doy_above(airtemp,time, 18, year),
      dateOver20=first_doy_above(airtemp,time, 20, year),
      dateOver21=first_doy_above(airtemp,time, 21, year),
      coef_var_April = coef_var_month(airtemp, month, 'April'),
      coef_var_May = coef_var_month(airtemp, month, 'May'),
      coef_var_June = coef_var_month(airtemp, month, 'June'),
      warm_rate_April = warm_rate_month(airtemp, month, 'April'),
      warm_rate_May = warm_rate_month(airtemp, month, 'May'),
      warm_rate_June = warm_rate_month(airtemp, month, 'June'),
      mean_jul = mean_months(airtemp, month, 'July'),
      mean_JAS = mean_months(airtemp, month, c('July','August','September')),
      spring_days_in_10.5_15.5 = days_between(airtemp, month, c('January','February','March','April','May'), 10.5, 15.5),
      GDD_air_5c = GDD(airtemp, 5),
      GDD_air_10c = GDD(airtemp, 10),
      days_above_8.9 = days_above(airtemp, 8.9),
      days_above_18 = days_above(airtemp, 18),
      days_above_20 = days_above(airtemp, 20),
      days_above_21 = days_above(airtemp, 21),
      count = length(airtemp)) %>% 
    filter(count >= 364) %>% 
    select(-count) %>% data.frame
  
  saveRDS(air.data, file = sprintf('%s/hab_metrics/%s_%s_empirical_hab.rds', tempdir(), lakeid, model))   
  message('done with ',lakeid,'***\n')
}

if (!dir.exists(file.path(tempdir(),'hab_metrics'))){
  dir.create(path=file.path(tempdir(),'hab_metrics'))
}

models = c('GFDL', 'IPSL', 'MRI', 'MIROC5', 'CNRM', 'ACCESS')
for (model in models){
  
  driver.index <- read.table(sprintf('http://cida-test.er.usgs.gov/mda.lakes/drivers_GLM_%s/driver_index.tsv',model), sep = '\t', header=TRUE, stringsAsFactors = FALSE)
  lake.ids <- unique(driver.index$id)
  for (i in 1:length(lake.ids)){
    
    
    tryCatch({
      file.name <- driver.index$file.name[driver.index$id == lake.ids[i] & driver.index$variable == 'airtemp']
      empirical.habitat.calc(file.name, model, lakeid = lake.ids[i])
    },  error = function(e) { # try again
      message('failed on ',lake.ids[i],'...')
      #empirical.habitat.calc(lake.ids[i], model)
    })
    
  }
  
  # now load in RData and combine all the files
  files <- list.files(file.path(tempdir(), 'hab_metrics'),pattern = sprintf("\\%s_empirical_hab.rds$", model))
  air.data = readRDS(file.path(tempdir(), 'hab_metrics',files[1]))
  data.out = air.data
  for (i in 2:length(files)){
    air.data = readRDS(file.path(tempdir(), 'hab_metrics',files[i]))
    data.out = rbind(data.out, air.data)
  }
  
  write.table(data.out, file=sprintf('data/hab_metrics/%s_air_metrics.tsv',model),sep='\t',row.names=F)
}




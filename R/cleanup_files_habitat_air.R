GCM = 'ACCESS'
water.hab = readr::read_delim(sprintf('data/hab_metrics/%s_thermal_metrics.tsv',GCM), delim = '\t')

lakes <- unique(water.hab$site_id)

models = c('GFDL', 'IPSL', 'MRI', 'MIROC5', 'CNRM', 'ACCESS')
for (model in models){
  
  existing.hab <- readr::read_delim(sprintf('data/hab_metrics/%s_air_metrics.tsv',model), delim = '\t')

  missing.lakes <- setdiff(lakes, unique(existing.hab$lake.id))
  driver.index <- read.table(sprintf('http://cida-test.er.usgs.gov/mda.lakes/drivers_GLM_%s/driver_index.tsv',model), sep = '\t', header=TRUE, stringsAsFactors = FALSE)
  for (i in seq_len(length(missing.lakes))){
    
    tryCatch({
      file.name <- driver.index$file.name[driver.index$id == missing.lakes[i] & driver.index$variable == 'airtemp']
      empirical.habitat.calc(file.name, model, lakeid = missing.lakes[i])
    },  error = function(e) { # try again
      message('failed on ',missing.lakes[i],'...')
      Sys.sleep(5)
      empirical.habitat.calc(missing.lakes[i], model)
    })
    
  }
  if (length(missing.lakes) == 0){
    message('nothing to do for ', model)
  } else {
    # now load in RData and combine all the files
    files <- list.files(file.path(tempdir(), 'hab_metrics'),pattern = sprintf("\\%s_empirical_hab.rds$", model))
    air.data = existing.hab
    data.out = air.data
    for (i in 1:length(files)){
      air.data = readRDS(file.path(tempdir(), 'hab_metrics',files[i]))
      data.out = rbind(data.out, air.data)
    }
    
    write.table(data.out, file=sprintf('data/hab_metrics/%s_air_metrics.tsv',model),sep='\t',row.names=F)
    message('wrote out ', sprintf('data/hab_metrics/%s_air_metrics.tsv',model))
  }
  
}




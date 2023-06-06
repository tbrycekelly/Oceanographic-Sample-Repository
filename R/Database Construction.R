library(openxlsx)


init.db = function(path, name) {
  
  db = list(
    db = list(
      dir = path,
      file = paste0(path, '/', name, '.db'),
      size = NA
    )
  )
  
  saveRDS(db, db$db$file)
  
  db
}

backup = function(db) {
  if (!dir.exists(db$db$dir)) {
    dir.create(db$db$dir)
  }
  
  if (file.exists(db$db$file)) {
    file.copy(db$db$file, to = gsub('.db', paste0(' ', gsub(':', '', Sys.time()), '.dbbackup'), db$db$file))
  }
  
  saveRDS(db, db$db$file)
}


init.sample = function() {
  list(
    id = NA,
    
    collection = list(
      cruise = NA,
      deployment = NA,
      tube = NA,
      depth = NA,
      station = NA,
      longitude = NA,
      latitude = NA,
      deployment.time = NA,
      recovery.time = NA
    ),
    
    preparation = list(
      min.size = NA,
      max.size = NA,
      tube.fraction = NA,
      volume = NA,
      analysis.type = NA,
      filter.type = NA,
      filter.size = NA
    ),
    
    analysis = list(
      
    ),
    storage = list(
      location = NA,
      box = NA,
      position = NA
    ),
    notes = list(),
    history = list(
    )
  )
}


update.history = function(sample, message) {
  sample$history = c(sample$history, paste0(Sys.time(), ': ', message))
  sample
}


update.notes = function(sample, message) {
  sample$notes = c(sample$notes, paste0(Sys.time(), ': ', message))
  sample
}


update.inventory = function(inventory, db) {
  
  ## Preprocess inventory
  inventory = inventory[!is.na(inventory$id),]
  l = which(is.na(inventory$cruise) | is.na(inventory$deployment))
  if (length(l) > 0) {
    message('Some inventory items (', length(l),') have missing cruise and/or deployment info, removing them.')
    inventory = inventory[-l,]
  }
  
  current.samples = names(db)
  
  for (i in 1:nrow(inventory)) {
    
    ## Check if sample is already in the database
    if (inventory$id[i] %in% names(db)) {
      
      ## Check if the data in the database is up to date
      k = which(names(db) == inventory$id[i])
      k = names(db)[k]
      
      if (length(k) > 1) {
        stop('More than one sample with ID ', inventory$id[i])
      } else if (length(k) == 0) {
        stop('Apparently non-existant duplicate?')
      }
      message('Checked metdata for sample ID ', inventory$id[i])
      msg = 'Checked metdata.'
    } else {
      
      ## This is a new sample:
      k = inventory$id[i]
      db[[k]] = init.sample()
      db[[k]]$id = inventory$id[i]
      
      message('Added sample ID ', db[[k]]$id, ' to database.')
      msg = paste0('Initialized new sample ', db[[k]]$id)
    }
    
    ## Check metadata
    db[[k]]$collection$cruise = inventory$cruise[i]
    db[[k]]$collection$deployment = inventory$deployment[i]
    db[[k]]$collection$tube = inventory$tube.id[i]
    db[[k]]$collection$depth = inventory$depth[i]
    db[[k]]$collection$station = inventory$station[i]
    db[[k]]$collection$longitude = inventory$longitude[i]
    db[[k]]$collection$latitude = inventory$latitude[i]
    db[[k]]$collection$deployment.time = inventory$deployment.time[i]
    db[[k]]$collection$recovery.time = inventory$recovery.time[i]
    
    db[[k]]$preparation$min.size = inventory$min.size[i]
    db[[k]]$preparation$max.size = inventory$max.size[i]
    db[[k]]$preparation$tube.fraction = inventory$tube.fraction[i]
    db[[k]]$preparation$volume = inventory$volume[i]
    db[[k]]$preparation$analysis.type = inventory$analysis.type[i]
    db[[k]]$preparation$filter.type = inventory$filter.type[i]
    db[[k]]$preparation$filter.size = inventory$filter.size[i]
    
    db[[k]]$storage$box = inventory$storage.box[i]
    db[[k]]$storage$storage.position = inventory$storage.position[i]
    
    db[[k]]$notes = list(inventory$notes[i])
    
    db[[k]] = update.history(db[[k]], msg)
  }
  
  message('Done.')
  db
}


summarize = function(db) {
  sample = db[[names(db)[1]]]
  sample$id = NULL
  sample$notes = NULL
  sample$history = NULL
  
  ## Itemize collection entries
  for (i in 1:length(sample$collection)) {
    for (k in 2:length(db)) {
      if (!db[[names(db)[k]]]$collection[[i]] %in% sample$collection[[i]]) {
        sample$collection[[i]] = c(sample$collection[[i]], db[[names(db)[k]]]$collection[[i]])
      }
    }
  }
  
  ## Itemize collection entries
  for (i in 1:length(sample$preparation)) {
    for (k in 2:length(db)) {
      if (!db[[names(db)[k]]]$preparation[[i]] %in% sample$preparation[[i]]) {
        sample$preparation[[i]] = c(sample$preparation[[i]], db[[names(db)[k]]]$preparation[[i]])
      }
    }
  }
  
  ## Itemize storage entries
  for (i in 1:length(sample$storage)) {
    for (k in 2:length(db)) {
      if (!db[[names(db)[k]]]$storage[[i]] %in% sample$storage[[i]]) {
        sample$storage[[i]] = c(sample$storage[[i]], db[[names(db)[k]]]$storage[[i]])
      }
    }
  }
  
  sample
}


retreive = function(db, key, value) {
  res = list()
  
  for (i in 1:length(db)) {
    if (value %in% eval(parse(text = paste0('db[[', i, ']]$', key)))) {
      res[[db[[i]]$id]] = db[[i]]
    }
  }
  
  res
}


export = function(db) {
  res = data.frame(id = rep(NA, length(db)), cruise = NA, deployment = NA,
                   tube = NA, station = NA, min.size = NA, max.size = NA,
                   tube.fraction = NA, analysis.type = NA, filter.type = NA,
                   filter.size = NA, storage.location = NA, storage.box = NA, storage.position = NA)
  
  for (i in 1:length(db)) {
    res$id[i] = db[[i]]$id
    
    res$cruise[i] = db[[i]]$collection$cruise
    res$deployment[i] = db[[i]]$collection$deployment
    res$tube[i] = db[[i]]$collection$tube
    
    res$min.size[i] = db[[i]]$preparation$min.size
    res$max.size[i] = db[[i]]$preparation$max.size
    res$tube.fraction[i] = db[[i]]$preparation$tube.fraction
    res$analysis.type[i] = db[[i]]$preparation$analysis.type
    res$filter.type[i] = db[[i]]$preparation$filter.type
    res$filter.size[i] = db[[i]]$preparation$filter.size
    
    res$storage.location[i] = db[[i]]$storage$location
    res$storage.box[i] = db[[i]]$storage$box
    res$storage.position[i] = db[[i]]$storage$position
    
  }
  
  res
}


calc.flux = function(db) {
  index = summarize(db)
  flux = data.frame(cruise = rep(NA, length(db)),
                    deployment = NA,
                    tube = NA,
                    depth = NA,
                    station = NA,
                    longitude = NA,
                    latitude = NA,
                    deployment.time = NA,
                    recovery.time = NA,
                    min.size = NA,
                    max.size = NA,
                    analysis.type = NA,
                    filter.type = NA,
                    filter.size = NA
                    )
  
  for (i in 1:length(db)) {
    flux$cruise[i] = db[[i]]$collection$cruise
    flux$deployment[i] = db[[i]]$collection$deployment
    flux$tube[i] = db[[i]]$collection$tube
    flux$depth[i] = db[[i]]$collection$depth
    flux$station[i] = db[[i]]$collection$station
    flux$longitude[i] = db[[i]]$collection$longitude
    flux$latitude[i] = db[[i]]$collection$latitude
    flux$deployment.time[i] = db[[i]]$collection$deployment.time
    flux$recovery.time[i] = db[[i]]$collection$recovery.time
    
    flux$min.size[i] = db[[i]]$preparation$min.size
    flux$max.size[i] = db[[i]]$preparation$max.size
    flux$analysis.type[i] = db[[i]]$preparation$analysis.type
    flux$filter.type[i] = db[[i]]$preparation$filter.type
    flux$filter.size[i] = db[[i]]$preparation$filter.size
  }
  duplicated(flux)
  
  aggregated.flux = unique(flux)
  
}



library(png)
library(base64enc)
library(jsonlite)
library(GEOquery)
source('./analysis.R')

# setwd('~/Desktop/practice/geo3R/server/')

#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")  # For development, allow all origins
  
  if(req$REQUEST_METHOD == "OPTIONS"){
    res$setHeader("Access-Control-Allow-Methods", "GET, POST")
    res$setHeader("Access-Control-Allow-Headers", "Content-Type")
    res$status <- 200
    return()
  }
  
  plumber::forward()
}


#* @get echo
function(){
  return(list(msg='hello world!'))
}



#* @post getGSE
function (id){
  
  print(id)
  
  gse <- readRDS(paste0('../db/', id, '.rds'))
  
  pdata <- pData(gse)
  
  result <- pdata[, grepl('.*:ch1$', colnames(pdata))] %>% 
    tibble::rownames_to_column('PID') %>% as.list()
  
  result$title <- gse@experimentData@title
  
  return( result )
}



#* @post runDEG
function(id='', control='a', case='b'){
  gse <- readRDS(paste0('../db/', id, '.rds'))
  
  if (length(control) == 1) { control = trimws( strsplit(control, ',')[[1]] ) }
  if (length(case) == 1 ) { case = trimws( strsplit(case, ',')[[1]] ) }
  
  print( paste('ID :', id) )
  print( paste('Control :', paste0(control, collapse = ', ') ) )
  print( paste('Case :', paste0(case, collapse = ', ') ) )
  
  # gse <- readRDS('../db/GSE54495.rds')
  edata <- exprs(gse)
  fdata <- fData(gse)
  pdata <- pData(gse)
  
  #case <- c('GSM1316832', 'GSM1316833')
  #control <- c('GSM1316829', 'GSM1316830')
  
  metadata <- pdata[, grepl('.*:ch1$', colnames(pdata))] %>% 
    tibble::rownames_to_column('PID') %>%
    filter(PID %in% c(case, control)) %>% 
    mutate(
      group = ifelse(PID %in% control, 'control', 'case')
    )

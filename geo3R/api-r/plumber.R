library(plumber)
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
    res$setHeader("Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
    res$status <- 200
    return()
  }
  
  plumber::forward()
}


#* @get echo
function(){
  return(list(msg='hello world!'))
}



#* @get getGSE
function (id){
  
  print(id)
  
  gse <- readRDS(paste0('./db/', id, '.rds'))
  
  # gse <- GEOquery::getGEO(id)[[1]]
  
  pdata <- pData(gse)
  edata <- exprs(gse) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column('SPOT_ID')
  
  fdata <- fData(gse) %>% 
    select(ID, ENTREZ_GENE_ID) %>% 
    tibble::remove_rownames()
  
  pdata <- pdata[, grepl('.*:ch1$', colnames(pdata))] %>% 
    tibble::rownames_to_column('PID') 
  
  return( list(
    title = gse@experimentData@title,
    data_processing = gse@phenoData@data$data_processing[1],
    metadata=pdata,
    edata = edata,
    fdata = fdata
  ) )
}



#* @post runDEG
function(id, edata, fdata, metadata, control, case){
  # gse <- readRDS(paste0('../db/', id, '.rds'))
  
  options(scipen = 999)
  if (length(control) == 1) { control = trimws( strsplit(control, ',')[[1]] ) }
  if (length(case) == 1 ) { case = trimws( strsplit(case, ',')[[1]] ) }
  
  print( paste('ID :', id) )
  print( paste('Control :', paste0(control, collapse = ', ') ) )
  print( paste('Case :', paste0(case, collapse = ', ') ) )
  
  # gse <- readRDS('../db/GSE54495.rds')
  # edata <- exprs(gse)
  # fdata <- fData(gse)
  # pdata <- pData(gse)
  
  #case <- c('GSM4504101', 'GSM4504103', 'GSM4504105')
  #control <- c('GSM4504102', 'GSM4504114', 'GSM4504116', 'GSM4504118')
  
  metadata <- metadata %>% 
    filter(PID %in% c(control, case)) %>% 
    mutate( group = ifelse(PID %in% control, 'control', 'case') )
  
  
  edata <- edata[,c('SPOT_ID', control, case)] %>% 
    tibble::column_to_rownames('SPOT_ID') %>% 
    as.matrix()
  
  metadata <- metadata[match(colnames(edata), metadata$PID) , ]
  
  
  
  spotID_mapper <- spot_mapper(fdata)
  edata <- edata[spotID_mapper$SPOT_ID, ]
  edata <- na.omit(edata)
  if(max(edata) > 16 & min(edata) > 0) { edata <- log2(edata) }
  if(max(edata) > 16 & min(edata) == 0) { edata <- log2(edata+1) }
  
  new_edata <- handle_duplicate(edata, control, case, spotID_mapper)
  

  print(dim(edata))
  print(dim(new_edata))
  
  # differential expression
  dfe_results <- run_dfe(new_edata, metadata)
  
  dfe_results <- format(dfe_results, scientific=T, digits = 5)
  
  print(head(dfe_results))
  print(str(dfe_results))
  
  png('temp.png')
  v <- plot_pca(new_edata, metadata)
  dev.off()
  imgPCA <- base64encode( readBin('temp.png', what = 'raw',
                                  n = file.size('temp.png')) )
  
  
  png('temp.png')
  plot_pca_var(new_edata, metadata)
  dev.off()
  imgPCAvar <- base64encode( readBin('temp.png', what = 'raw',
                                     n = file.size('temp.png')) )
  
  
  png('temp.png')
  hcluster_samples(new_edata, metadata)
  dev.off()
  imgHClust <- base64encode( readBin('temp.png', what = 'raw',
                                     n = file.size('temp.png')) )
  
  fig <- plot_heatmap(new_edata, metadata)
  ggsave(filename = 'temp.png', plot = fig, dpi = 600)
  imgHeatmap <- base64encode( readBin('temp.png', what = 'raw',
                                     n = file.size('temp.png')) )
  
  fig <- plot_volcano(dfe_results, alpha=0.1, lab = T)
  ggsave(filename = 'temp.png', plot = fig,
         dpi = 400, height = 6, width = 8)
  imgVolcano <- base64encode( readBin('temp.png', what = 'raw',
                                      n = file.size('temp.png')) )
  
  # remove temp picture
  file.remove('./temp.png')
  

  return(list(
    pca = list(
      varE = v,
      plot = imgPCA,
      var_plot = imgPCAvar
    ),
    dendogram = imgHClust,
    heatmap = imgHeatmap,
    volcano = imgVolcano,
    deg = dfe_results
  ))
}


#* @post volcano
function(results, alpha, minFC, maxFC, by){
  alpha <- as.numeric(alpha)
  minFC <- as.numeric(minFC)
  maxFC <- as.numeric(maxFC)
  
  print(head(results))
  print(str(results))
  
  fig <- plot_volcano(results, minFC, maxFC, alpha, by)
  
  
  ggsave(filename = 'temp.png', plot = fig,
         dpi = 400, height = 6, width = 8)
  imgVolcano <- base64encode( readBin('temp.png', what = 'raw',
                                      n = file.size('temp.png')) )
  
  # remove temp picture
  file.remove('./temp.png')
  
  return(list(
    volcano = imgVolcano
  ))
  
}

#* @post volcano/mark_gene
function(results, genes, minFC, maxFC, alpha, by){
  
  genes_new <- genes[genes %in% results$Gene]
  
  plot_data <- results %>%
    select(Gene, logFC, !!sym(by)) %>%
    mutate(logFC := as.numeric(logFC), !!sym(by) := as.numeric(!!sym(by))) %>% 
    mutate(
      state = case_when(
        Gene %in% genes_new ~ 'mark',
        logFC >= maxFC & !!sym(by) <= alpha ~ 'up',
        logFC <= minFC & !!sym(by) <= alpha ~ 'down',
        TRUE ~ 'same'
      )
    )
    
    fig <- ggplot(plot_data, aes(
      x = logFC, 
      y = -log10( !!sym(by) ), 
      color = as.factor(state))) +
    geom_hline(yintercept = -log10(alpha), linetype = "dashed", color = "red") +
    geom_vline(xintercept = c(minFC, maxFC), linetype = "dashed", color = "red") +
    geom_point(data = subset(plot_data, state!='mark'), alpha=0.25) + 
    geom_point(data = subset(plot_data, state=='mark'), size=2) + 
    scale_color_manual(values = c(
      'up'= 'dodgerblue',
      'down' = 'hotpink',
      'same' = 'gray',
      'mark' = 'red'
    )) +
    geom_text_repel(data = subset(plot_data, state=='mark'),
                    aes(label=Gene),
                    size = 2.5,
                    colour = 'black',
                    fontface = 'bold',
                    max.overlaps = ceiling(length(genes) / 2)
    ) +
    theme_bw() +
    theme(legend.position = 'none')
    
    
  ggsave(filename = 'temp.png', plot = fig,
           dpi = 400, height = 6, width = 8)
  imgVolcano <- base64encode( readBin('temp.png', what = 'raw',
                                        n = file.size('temp.png')) )
  
  # remove temp picture
  file.remove('./temp.png')
  
  return(list(
    volcano = imgVolcano,
    not_found = setdiff(genes, genes_new)
  ))
}










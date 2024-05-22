library(dplyr)
library(limma)
library(RColorBrewer)
library(ggrepel)
library(ggplot2)
library(pheatmap)
library(rafalib)


## ------------ functions ------------
fumeric <- function(vect) { as.numeric( as.factor(vect) ) }
flevels <- function(vect) { levels( as.factor(vect) ) }
outlierFilter <- function(v){
  Q1 <- quantile(v, 0.25)
  Q3 <- quantile(v, 0.75)
  
  minValue <- Q1 - (Q3-Q1)
  maxValue <- Q3 + (Q3-Q1)
  
  return(setNames(c(minValue, maxValue), c('min', 'max')))
}


## ----------------- retrieve gene list -------------------
query_genes = read.table('/Users/ridwan204/Reuben/practice/geo3R/api-r/db/all_genes.txt', sep='\t', header=T)

query_genes = list(
  metabolism = query_genes$xenobiotics_metabolism[query_genes$xenobiotics_metabolism != ""],
  stress = query_genes$oxidative_stress[ query_genes$oxidative_stress != "" ],
  transporter = query_genes$xenobiotics_transporter[ query_genes$xenobiotics_transporter != "" ]
)
query_genes$all <- unlist(query_genes) |> unique()

entrez_mapper <- read.table('./db/entrez_gene_annonation.tsv', sep='\t',
                            col.names = c('ENTREZ_GENE_ID', 'Gene'), quote = "")


## --------------- map spot id ---------------
spot_mapper <- function(fdata){
  
  mapper <- fdata %>% 
    rename(SPOT_ID=ID) %>%
    filter(ENTREZ_GENE_ID != '' & 
             !is.na(ENTREZ_GENE_ID) & 
             !grepl('^.+///.+$', ENTREZ_GENE_ID)) %>%
    mutate( ENTREZ_GENE_ID := as.integer(ENTREZ_GENE_ID) ) %>%  
    inner_join(entrez_mapper, by='ENTREZ_GENE_ID') %>% 
    filter(Gene %in% query_genes$all) %>% 
    select(SPOT_ID, Gene)
  
  return(mapper)
}


## --------------- Handle duplicate ----------------
handle_duplicate <- function(edata, control, case, spotID_mapper){
  
  new_edata <- edata %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column('SPOT_ID') %>% 
    inner_join(spotID_mapper, by='SPOT_ID') %>% 
    select(SPOT_ID, Gene, everything()) %>% 
    mutate(
      logFC = rowMeans(.[ , control ]) - 
        rowMeans(.[ , case ])
    ) %>%
    group_by(Gene) %>% 
    filter(
      if( n() == 2 ){
        if ( sum(grepl('^\\d+_at$', SPOT_ID)) == 1 ) { grepl('^\\d+_at$', SPOT_ID) }
        else{ c(T,T) }
      } 
      else if ( n() > 2 ){
        ( logFC > outlierFilter(logFC)['min'] & logFC < outlierFilter(logFC)['max'] )
      }
      else {
        TRUE
      }
    ) %>% 
    summarize( across(-SPOT_ID, mean) ) %>% 
    select(everything(), -logFC) %>% 
    tibble::column_to_rownames('Gene') %>% 
    as.matrix()
  
  return(new_edata)
}

## ----------------------- draw pca --------------------------
plot_pca <- function (edata, metadata){
  pca <- prcomp(t(edata))
  
  s <- (pca$sdev^2*100 / sum(pca$sdev^2))[1:2]

  color <- c('red', 'green')
  
  plot(pca$x[,1], pca$x[,2], pch = 19, col = color[fumeric(metadata$group)], 
       xlab = paste0('PC1 (', round(s[1], 3), '%)'), 
       ylab = paste0('PC2 (', round(s[2], 3), '%)')
       )
  grid()
  abline(h=0, v=0)
  legend('topright', flevels(metadata$group), col=color, pch=19)
  
  return(sum(s))
}

plot_pca_var <- function (edata, metadata){
  pca <- prcomp(t(edata))

  plot(pca$sdev^2*100 / sum(pca$sdev^2),
       xlab = 'Principle Component',
       ylab = '% Varience',
       xaxt = 'n'
  )
  grid()
  points(pca$sdev^2*100 / sum(pca$sdev^2), pch = 19, col='tomato')
  axis(1, at=seq(length(pca$sdev)), labels = seq(length(pca$sdev)))
}

## ----------------- draw dendogram -------------------------
hcluster_samples <- function(edata, metadata){
  D <- dist(t(edata))
  dend <- hclust(D)
  myplclust(dend, hang=0, labels = metadata$PID,
            lab.col = fumeric(metadata$group)+1, cex=1,
            main='Heirarchical Clustering')
  legend('topright', flevels(metadata$group), col=2:5, pch=19)
}


## -------------- heatmap ------------------------
plot_heatmap <- function (edata, metadata){
  # heatmap 
  ann_df <- data.frame(condition=metadata$group,
                       row.names = metadata$PID )
  ann_color <- list(
    condition = c("case"="red", 'control'='green')
  )
  
  fig <- pheatmap(edata, annotation_col = ann_df, annotation_colors = ann_color,
           show_rownames = F, 
           color = colorRampPalette(c("red", "black", "green"))(100))
  
  return(fig)
}


## --------------- differential expression -------------------
run_dfe <- function(edata, metadata){
  design <- model.matrix(~ 0 + metadata$group)
  colnames(design) <- c("case","control")
  
  fit <- lmFit(edata, design)
  head(fit$coefficients)
  
  contrasts <- makeContrasts( 'case - control', levels=design )
  fit2 <- contrasts.fit(fit, contrasts)
  fit2 <- eBayes(fit2)
  
  results <- topTable(fit2, number = Inf, adjust.method = 'fdr') %>% 
    tibble::rownames_to_column('Gene')
  
  return(results)
}


# ----------------- volcano plot --------------------
plot_volcano <- function(results, minFC=-1, maxFC=1, alpha=0.05, by="P.Value", lab=T){
  # by = "P.Value"
  # alpha = 0.01
  
  plot_data <- results %>%
    select(Gene, logFC, !!sym(by)) %>%
    mutate(logFC := as.numeric(logFC), !!sym(by) := as.numeric(!!sym(by))) %>% 
    mutate(
      state = case_when(
        logFC >= maxFC & !!sym(by) <= alpha ~ 'up',
        logFC <= minFC & !!sym(by) <= alpha ~ 'down',
        TRUE ~ 'same'
      )
    ) %>%
    ggplot(aes(
      x = logFC, 
      y = -log10( !!sym(by) ), 
      color = as.factor(state))) +
    geom_hline(yintercept = -log10(alpha), linetype = "dashed", color = "red") +
    geom_vline(xintercept = c(minFC, maxFC), linetype = "dashed", color = "red") +
    geom_point() +
    scale_color_manual(values = c(
      'up'= 'blue',
      'down' = 'red',
      'same' = 'gray'
    ))
  
    
    if (lab){
      plot_data <- plot_data + geom_text_repel(
        data = . %>% filter(state !=  'same'),
        aes(label = Gene),
        size = 2.5,
        fontface = 'bold',
        max.overlaps = 20,
        colour = 'black'
      )
    }
    
    plot_data <- plot_data +
      labs(x = "logFC", y = paste0("-log10(", by, ")", collapse = '')) +
      theme_bw() +
      theme(legend.position = 'none')
  
  return(plot_data)
}


## -------------------- mark gene in volcano plot -----------------------
mark_gene_in_volcano <- function(results, genes, minFC, maxFC, alpha, by){
  
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
      geom_point(data = subset(plot_data, state!='mark'), alpha=0.50) + 
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
                      max.overlaps = 10
      ) +
      theme_bw() +
      theme(legend.position = 'none')
  
    return(fig)
}























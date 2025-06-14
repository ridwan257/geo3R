library(dplyr)
library(tibble)
library(readr)
library(tidyr)
library(ggplot2)
library(Biobase)
library(GEOquery)

source('./session.R')
source('./util_func.R')
source('./analysis_functions.R')
source('./plot_func.R')

#* @filter cors
function(req, res) {
	res$setHeader("Access-Control-Allow-Origin", "*")
	
	if(req$REQUEST_METHOD == "OPTIONS"){
		res$setHeader("Access-Control-Allow-Methods", "GET, POST")
		res$setHeader("Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
		res$status <- 200
		return()
	}
	
	plumber::forward()
}


## ===========================================================================
## ===========================================================================
#* @get /echo
function(){
	print("hello world!")
	return(list(msg='hello world!'))
}

## ===========================================================================
## ===========================================================================
#* List all active session environments and their contents
#* @get /list_sessions
function() {
	session_ids <- ls(envir = SESSION_ENV)
	
	session_info <- lapply(session_ids, function(id) {
		env <- SESSION_ENV[[id]]
		vars <- ls(env)
		list(
			session_id = id,
			variables = vars
		)
	})
	
	return(session_info)
}

## ===========================================================================
## ===========================================================================
#* Load GEO data for a user
#* @post /kill_session
#* @param session_id The session ID
function(session_id){
	print( paste("Killing request -->", session_id) )
	kill_session(session_id)
	cat('\n')
}

## ===========================================================================
## ===========================================================================
#* Load GEO data for a user
#* @post /load_geo
#* @param session_id The session ID
#* @param gse_id The GEO ID (e.g., GSE1234)
function(session_id, gse_id){
	print(paste(session_id, gse_id))
	
	env <- get_session_env(session_id)
	env$gse_id <- gse_id
	
	if( !look_in_db(gse_id) ){
		
		res_data <- download_geo_data(gse_id)
		
		if(!res_data$ok)
		{
			return(list( data_found = FALSE ))
		}
		env$platform_len <- res_data$platform_len
		push_in_db(gse_id, res_data$gse)
	}
	
	
	pdata <- pData( get_db()[[gse_id]] )
	fdata <- head( fData( get_db()[[gse_id]] ), 50)
	
	pdata <-  tibble::rownames_to_column(pdata, 'PID') 
	
	return( list(
		data_found = TRUE,
		total_platform = env$platform_len,
		title = get_db()[[gse_id]]@experimentData@title,
		data_processing = get_db()[[gse_id]]@phenoData@data$data_processing[1],
		annotation = annotation( get_db()[[gse_id]] ),
		n_samples = nrow(pdata),
		metadata=pdata,
		fdata = fdata
	) )
}


## ===========================================================================
## ===========================================================================
#* Load GEO data for a user
#* @post /run_analysis
#* @param session_id The session ID
#* @param analysis_options A dictionary type, options for analysis
#* @param sample_list A list of control and Case samples
function(session_id, analysis_options, sample_list)
{
	cat("\n")
	print( paste(session_id, "--> requesting analysis") )
	
	env <- get_session_env(session_id)
	## ------------ SETTING PARAMETERS
	is_opt_chagned <- set_analysis_options(env, analysis_options)
	print( paste("is_opt_chagned", is_opt_chagned) )
	
	is_sample_changed <- create_mdata(env, sample_list)
	print( paste("is_sample_changed", is_sample_changed) )
	print(env$mdata)
	## -------------- subset pData
	env$pdata <- pData( get_db()[[env$gse_id]] )
	env$pdata <- env$pdata[rownames(env$mdata), ]
	env$pdata$condition <- env$mdata$condition
	
	## ------------ Preprocess the data
	print("")
	preprocess_data(env)

	## ------------ basic distribution plot
	plot_name <- get_plot_name(session_id, "dist1.png")
	png(plot_name, width = 800, height = 600, res = 100)
	boxplot(env$edata, main = "Boxplot")
	dev.off()
	
	plot_name <- get_plot_name(session_id, "dist2.png")
	png(plot_name, width = 800, height = 600, res = 100)
	limma::plotDensities(env$edata, group = env$mdata$condition, legend = 'topright')
	dev.off()
	
	## ------------ plot PCA and hierarchical clustering
	print("DOING PCA")
	# print( t(env$edata) %>% is.na() %>% rowSums() )
	print( scale(t(env$edata)) %>% is.na() %>% rowSums() )
	env$pca_data <- prcomp(scale(t(env$edata)))
	print("DONE PCA")
	env$clust_data <- hclust( dist(scale(t(env$edata))), method = "average" ) %>% 
		as.dendrogram(hang=0.2)
	
	
	
	plot_name <- get_plot_name(session_id, "pca1.png")
	png(plot_name, width = 700, height = 500, res = 100)
	plot_pca(env, env$mdata$condition, lpos='topright')
	dev.off()
	print("done pca")
	
	plot_name <- get_plot_name(session_id, "hclust1.png")
	png(plot_name, width = 700, height = 500, res = 100)
	plot_dendogram(env, env$mdata$condition, lpos='topright')
	dev.off()
	print("done hclust")
	
	## ------------ volcano plot
	model_mat <- model.matrix(~0+condition, data = env$mdata)
	constrast <- makeContrasts('conditioncase-conditioncontrol', levels=model_mat)
	
	fit1 <- lmFit(env$edata, design = model_mat)
	fit1 <- contrasts.fit(fit1, contrasts = constrast)
	fit2 <- eBayes(fit1)
	
	env$deg_result <- topTable(fit2, number = Inf) %>% 
		rownames_to_column('Gene') %>% as_tibble()
	
	plot_name <- get_plot_name(session_id, "volcano1.png")
	p <- plot_volcano(env, highlight = 15, show_line = TRUE)
	ggsave(plot_name, plot = p, width = 4, height = 3, dpi = 300)
	
	## ------------ gene enrichment analysis
	# deg_genes <- env$deg_result %>% 
	# 	filter( abs(logFC) > 1 & !!sym(analysis_options$pval_by) < 0.05 ) %>% 
	# 	pull(Gene)
	# 
	# e_mapper <- get_db()[['entrez_mapper']] %>% 
	# 	filter( gene_name %in% deg_genes)
	# 
	# 
	
	
	return(list(
		status = "done",
		deg_table = env$deg_result
	))
}

## ===========================================================================
## ===========================================================================
#* Load GEO data for a user
#* @post /update_pca_plot
#* @param session_id The session ID
#* @param group_by color group
#* @param legend_pos legend position
function(session_id, group_by, legend_pos){
	cat("\n")
	print( paste(session_id, "--> requesting update pca") )
	
	env <- get_session_env(session_id)
	
	plot_name <- get_plot_name(session_id, "pca1.png")
	png(plot_name, width = 700, height = 500, res = 100)
	plot_pca(env, env$pdata[[group_by]], lpos=legend_pos)
	dev.off()
	print("done pca")
	
}

## ===========================================================================
## ===========================================================================
#* Load GEO data for a user
#* @post /update_hclust_plot
#* @param session_id The session ID
#* @param group_by color group
#* @param legend_pos legend position
function(session_id, group_by, legend_pos){
	cat("\n")
	print( paste(session_id, "--> requesting update clustering plot") )
	
	env <- get_session_env(session_id)
	
	plot_name <- get_plot_name(session_id, "hclust1.png")
	png(plot_name, width = 700, height = 500, res = 100)
	plot_dendogram(env, env$pdata[[group_by]], lpos=legend_pos)
	dev.off()
	print("done hclust")
	
}


## ===========================================================================
## ===========================================================================
#* Load GEO data for a user
#* @post /update_volcano_plot
#* @param session_id The session ID
#* @param plot_params A dictionary type, options for analysis
function(session_id, plot_params){
	cat("\n")
	print( paste(session_id, "--> requesting update volcano") )
	
	env <- get_session_env(session_id)
	# print(plot_params)

	## ------------ volcano plot
	plot_name <- get_plot_name(session_id, "volcano1.png")
	p <- plot_volcano(
		env,
		minFC = as.numeric(plot_params$min_fc),
		maxFC = as.numeric(plot_params$max_fc),
		alpha = as.numeric(plot_params$alpha_val),
		by = plot_params$pval_by,
		lab = plot_params$mark_sig_genes,
		highlight_rule = plot_params$highlight_by, 
		gene_list = if (plot_params$mark_genes) plot_params$gene_names else NA,
		highlight = if (plot_params$highlight) plot_params$highlight_n else NA,
		show_line = TRUE
	)
	ggsave(plot_name, plot = p, width = 4, height = 3, dpi = 300)
}




## ===========================================================================
## ===========================================================================
#* Load GEO data for a user
#* @post /strip_plot_gene
#* @param session_id The session ID
#* @param gene_names A array of genes
#* @param n_col number of col per row
function(session_id, gene_names, n_col){
	cat("\n")
	print( paste(session_id, "--> requesting strip plot") )
	
	env <- get_session_env(session_id)
	
	# --------- STRIP PLOT
	fig <- strip_plot_genes(env, gene_names=gene_names, ncol = n_col)
	
	if(any( class(fig) == 'ggplot' )){
		plot_name <- get_plot_name(session_id, "strip_plot1.png")
		plot_h <- 2.25 * ceiling(env$n_splot_genes/n_col)
		ggsave(plot_name, plot = fig, width = 8, height = plot_h, dpi = 300)
		
		return(list(had_plot=TRUE))
	}
	
	return(list(had_plot=FALSE))
}


































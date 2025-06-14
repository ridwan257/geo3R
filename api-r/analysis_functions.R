library(matrixStats)
library(limma)
library(Biobase)


## ======================================================================
## ======================================================================
download_geo_data <- function(gse_id){
	
	tryCatch({
		cat(paste("Trying to fetch", gse_id, "...\n"))
		
		# -------- from internet
		gse_list <- getGEO(gse_id, GSEMatrix = TRUE, AnnotGPL = TRUE)
		gse <- gse_list[[1]]
		platform_len <-  length(gse_list)
		
		# ------- from local
		# gse <- readRDS(paste("./db/", gse_id, ".rds", sep=""))
		# platform_len <- 1
		
		cat("Dataset downloaded succesfully...\n")
		return(list(
			ok = TRUE,
			gse = gse,
			platform_len = platform_len
		))
	} , error = function(e) {
			cat("Cannot download the dataset...\n")
	
			return(list(
				ok = FALSE,
				error = e$message
			))
	})
}


## ======================================================================
## ======================================================================
collape_to_gene <- function(r_env){
	
	# ----- create mapper vector
	fdata <- fData( get_db()[[r_env$gse_id]] )[, c("ID", r_env$mapping_column), drop=FALSE] %>%
		as_tibble() %>%
		rename(Gene = !!sym(r_env$mapping_column)) %>%
		dplyr::filter(Gene != "" & !is.na(Gene))
	
	print(head(fdata))
	# ------- if convert_from_entrez is TRUE, do mapping
	if (r_env$convert_from_entrez) {
		# entrez_mapper <- read_tsv('./db/entrez_gene_annonation.tsv', show_col_types = FALSE) %>% 
		# 	mutate(EID = as.character(EID))
		
		fdata <- inner_join(
			get_db()[['entrez_mapper']], fdata, by = c('EID' = 'Gene')
		) %>%
			mutate(Gene = gene_name) %>%
			select(ID, Gene)
	}
	r_env$edata <- r_env$edata[fdata$ID, , drop = FALSE]
	
	# common_probes <- intersect(fdata$ID, rownames(r_env$edata))
	# r_env$edata <- r_env$edata[common_probes, , drop = FALSE]
	# fdata <- fdata[fdata$ID %in% common_probes, ]
	
	if (r_env$collapse_method == 'mean') {
		r_env$edata <- limma::avereps(r_env$edata, ID = fdata$Gene)
	} 
	else if (r_env$collapse_method == 'maxvar') {
		variances <- matrixStats::rowVars(r_env$edata)
		probe_indices <- tapply(seq_along(fdata$Gene), fdata$Gene, function(idx) {
			idx[which.max(variances[idx])]
		})
		r_env$edata <- r_env$edata[unlist(probe_indices), , drop = FALSE]
		rownames(r_env$edata) <- names(probe_indices)
	} 
	else if (r_env$collapse_method == 'maxmean') {
		means <- rowMeans(r_env$edata)
		probe_indices <- tapply(seq_along(fdata$Gene), fdata$Gene, function(idx) {
			idx[which.max(means[idx])]
		})
		r_env$edata <- r_env$edata[unlist(probe_indices), , drop = FALSE]
		rownames(r_env$edata) <- names(probe_indices)
	}
	
	print("after merge")
	print( t(r_env$edata) %>% is.na() %>% rowSums() )
}


## ======================================================================
## ======================================================================
preprocess_data <- function(r_env){
	# -------- subsample the data by patient
	r_env$edata <- exprs(get_db()[[r_env$gse_id]])[, rownames(r_env$mdata)]
	print("doing preprocess")
	print( t(r_env$edata) %>% is.na() %>% rowSums() )
	# r_env$edata <- exprs(gse)[, rownames(r_env$mdata)]
	# head(r_env$edata)
	
	# print( is.na(r_env$edata) %>% colSums() )
	# r_env$edata <- na.omit(r_env$edata)
	
	# -------- log transform data
	if(r_env$log_transform){
		r_env$edata <- log2(r_env$edata+1)
	}
	# -------- normalize probes
	if(r_env$normalize){
		r_env$edata <- normalizeBetweenArrays(r_env$edata, method="quantile")
	}
	
	# -------- mapping to gene symbol
	if(r_env$collapse_to_gene == "gene")
	{
		collape_to_gene(r_env)
	}
	
	# -------- filter genes
	if(r_env$filter_gene)
	{
		median_val <- median(r_env$edata)
		keep <- rowSums(r_env$edata > median_val) >= r_env$filter_n
		r_env$edata <- r_env$edata[keep, ]
	}
	
}


# r_env <- .GlobalEnv
# r_env$control <- c("GSM927630", "GSM927631", "GSM927632")
# r_env$case <- c("GSM927639", "GSM927640", "GSM927641")
# env$gse <- readRDS('./db/GSE37768.rds')
# sample_list <- list(
# 	control=c("GSM5411286", "GSM5411287", "GSM5411288", "GSM5411289", "GSM5411290", "GSM5411291", "GSM5411292", "GSM5411293", "GSM5411294"),
# 	case=c("GSM5411277", "GSM5411278", "GSM5411279", "GSM5411280", "GSM5411281", "GSM5411282", "GSM5411283", "GSM5411284", "GSM5411285")
# )









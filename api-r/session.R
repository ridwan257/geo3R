
SESSION_ENV <- new.env()


## =====================================================================
## =====================================================================
SESSION_ENV$CACHE_DATA <- new.env()
SESSION_ENV$CACHE_DATA$entrez_mapper <- read_tsv('./db/entrez_gene_annonation.tsv', show_col_types = FALSE) %>% 
	mutate(EID = as.character(EID))

look_in_db <- function(gse_id){
	return( exists(gse_id, envir = SESSION_ENV$CACHE_DATA, inherits = FALSE) )
}

push_in_db <- function(gse_id, gse_data){
	SESSION_ENV$CACHE_DATA[[gse_id]] <- gse_data
}

get_db <- function(){ return(SESSION_ENV$CACHE_DATA) }


## =====================================================================
## =====================================================================
get_session_env <- function(session_id) {
	if (!exists(session_id, envir = SESSION_ENV)) {
		SESSION_ENV[[session_id]] <- new.env()
		SESSION_ENV[[session_id]]$created_at <- Sys.time()
		
		# ------- setting basic params
		SESSION_ENV[[session_id]]$normalize <- logical(0)
		SESSION_ENV[[session_id]]$log_transform <- logical(0)
		SESSION_ENV[[session_id]]$collapse_to_gene <- character(0)
		SESSION_ENV[[session_id]]$mapping_column <- character(0)
		SESSION_ENV[[session_id]]$convert_from_entrez <- logical(0)
		SESSION_ENV[[session_id]]$filter_gene <- logical(0)
		SESSION_ENV[[session_id]]$filter_n <- numeric(0)
		SESSION_ENV[[session_id]]$alpha_val <- numeric(0)
		SESSION_ENV[[session_id]]$pval_by <- character(0)
		
		SESSION_ENV[[session_id]]$control <- character(0)
		SESSION_ENV[[session_id]]$case <- character(0)
	}
	return( SESSION_ENV[[session_id]] )
}

clear_session <- function(session_id) {
	if (exists(session_id, envir = SESSION_ENV)) {
		rm(list = session_id, envir = SESSION_ENV)
	}
}

kill_session <- function(session_id){
	if (exists(session_id, envir = SESSION_ENV, inherits = FALSE)) {
		rm(list = session_id, envir = SESSION_ENV)
		
		# deleting plots
		file_names <- list.files('../tmp', full.names = TRUE)
		file_names <- file_names[grepl(session_id, file_names)]
		for(fname in file_names){
			unlink( fname, force = TRUE)
		}
		
		message(sprintf("Session '%s' has been killed and tmp plots deleted.", session_id))
	}
}

























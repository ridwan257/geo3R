
get_plot_name <- function(session_id, name){
	return(
		paste("../tmp/", session_id, "_", name, sep="", collapse = "" )
	)	
}



set_analysis_options <- function(r_env, options){
	changed <- FALSE
	
	for (name in names(options)) {
		new_val <- options[[name]]
		print( paste(name, new_val) )
		if (!identical(r_env[[name]], new_val)) {
			r_env[[name]] <- new_val
			changed <- TRUE
		}
	}
	return(changed)
}


create_mdata <- function(r_env, sample_list){
	changed <- FALSE
	
	new_samples <- c(sample_list$control, sample_list$case)
	
	if(
		!exists("mdata", envir = r_env, inherits = FALSE) ||
		!identical( new_samples, rownames(r_env$mdata) )
	){
		r_env$mdata <- data.frame(
			row.names = new_samples,
			condition = rep(c('control', 'case'), c(length(sample_list$control), length(sample_list$case)))
		)
		changed <- TRUE
	}
	
	return(changed)
}


# create_mdata(env, sample_list)







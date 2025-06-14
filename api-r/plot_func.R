library(ggrepel)
library(dendextend)
# library(clusterProfiler)
# library(enrichplot)


## ================================================================================
## --------------------------------------- PCA Genes ------------------------------
## ================================================================================
plot_pca <- function(r_env, groups, main="PCA of Samples", cex=1, lpos="NA", ...){
	e_var <- summary(r_env$pca_data)$importance[2, ]
	
	
	plot(r_env$pca_data$x[, 1], r_env$pca_data$x[, 2], 
		 # col = as.numeric(as.factor(m_mdata_filt$condition)), 
		 pch = "",
		 xlab = paste("PC1 (", round(e_var[1] * 100, 2), "%)", sep = ""),
		 ylab = paste("PC2 (", round(e_var[2] * 100, 2), "%)", sep = ""), 
		 main=main,
		 ...
	)
	
	# Adding the sample names instead of points
	text(r_env$pca_data$x[, 1], r_env$pca_data$x[, 2], 
		 labels = colnames(r_env$edata), 
		 cex = cex,  # Adjust text size as needed
		 col = as.numeric(as.factor(groups))+1)
	
	if(lpos != "NA"){
		legend(
			lpos, levels(as.factor(groups)), col=2:(length(unique(groups))+1), pch=19,
			cex=cex
		)
	}
}



plot_dendogram <- function(r_env, groups, main="Heirarchical Clustering", lpos="NA"){
	
	groups <- factor(groups)
	label_colors <- as.numeric(groups) + 1
	
	dend <- color_labels(r_env$clust_data, col=label_colors[order.dendrogram(r_env$clust_data)])
	
	plot(dend, main=main)
	if(lpos != "NA"){
		legend(lpos, levels(groups), col = 2:(length(unique(groups))+1), pch=19)
	}
	
}


## ================================================================================
## ----------------------------------- Volcano plots ------------------------------
## ================================================================================
plot_volcano <- function(
		r_env, minFC=-1, maxFC=1, alpha=0.05, by="P.Value", lab=FALSE,
		highlight = NA, show_line = FALSE, highlight_rule = "Sig", a=0.75, 
		text_size = 2.5, gene_list = NA, show_grid=TRUE, msize=1.1, psize = 1,
		mcolor='black', legend=FALSE, n_overlap = 20
){
	# by = "P.Value"
	# alpha = 0.01
	# highlight_rule = Sig | Bvalue | Pvalue
	
	min_fc_value <- min(r_env$deg_result$logFC)
	max_fc_value <- max(r_env$deg_result$logFC)
	max_log_pval <- max(-log10(r_env$deg_result[[by]]))
	
	plot_data <- r_env$deg_result %>%
		dplyr::select(Gene, logFC, !!sym(by), B) %>%
		dplyr::mutate(logFC := as.numeric(logFC), !!sym(by) := as.numeric(!!sym(by))) %>% 
		dplyr::mutate(
			state = case_when(
				logFC >= maxFC & !!sym(by) <= alpha ~ 'up',
				logFC <= minFC & !!sym(by) <= alpha ~ 'down',
				TRUE ~ 'same'
			)
		)
	
	figure <- ggplot(plot_data, aes(
		x = logFC, 
		y = -log10( !!sym(by) ), 
		color = state)) +
		geom_hline(yintercept = 0, color = "black", linewidth=0.2)
	
	if(show_line){
		figure <- figure +
			geom_hline(yintercept = -log10(alpha), linetype = "dashed", color = "red", linewidth=0.3) +
			geom_vline(xintercept = c(minFC, maxFC), linetype = "dashed", color = "red", linewidth=0.3)
	}
	
	figure <- figure +
		geom_point(show.legend = legend, alpha=a, size=psize) +
		scale_color_manual(values = c(
			'up'= 'blue',
			'down' = 'red',
			'same' = 'gray'),
			breaks = c("down", "same", "up"),
			labels = c("Down-regulated", "Not-Significant", "Up-regulated")
		)
	
	
	if(!is.na(highlight)){
		if(highlight_rule=='Bvalue') { plot_data <- arrange(plot_data, desc(B)) }
		else if(highlight_rule=='Pvalue') { plot_data <- arrange( plot_data, !!sym(by), desc(abs(logFC)) ) }
		else if(highlight_rule=='Sig') { plot_data <- arrange( plot_data, abs(logFC) * log10(!!sym(by)) ) }
		else { stop("Invalid highlight_rule") }
		
		figure <- figure + 
			# geom_point(
			# 	data = plot_data[1:highlight, ],
			# 	# color = mcolor,
			# 	# size = msize
			# ) +
			geom_text_repel(
				data = plot_data[1:highlight, ],
				aes(label = Gene),
				size = text_size,
				fontface = 'bold',
				max.overlaps = n_overlap,
				colour = 'black',
				show.legend = NA
			)
	}
	
	if (!all(is.na(gene_list))){
		mark_gene_df <- dplyr::filter(plot_data, Gene %in% gene_list)
		
		figure <- figure + 
			geom_point(
				data = mark_gene_df,
				color = mcolor,
				size = msize
			) +
			geom_text_repel(
				data = mark_gene_df,
				aes(label = Gene),
				size = text_size,
				fontface = 'bold',
				max.overlaps = n_overlap,
				colour = 'black',
				show.legend = FALSE
			)
	}
	
	if (lab){
		figure <- figure + geom_text_repel(
			data = . %>% dplyr::filter(state !=  'same'),
			aes(label = Gene),
			size = text_size,
			fontface = 'bold',
			max.overlaps = n_overlap,
			colour = 'black',
			show.legend = FALSE
		)
	}
	
	
	figure <- figure +
		labs(x = "logFC", y = paste0("-log10(", by, ")", collapse = '')) +
		scale_y_continuous(limits = c(0, NA), breaks = seq(0, ceiling(max_log_pval), 1)) +  # Step of 1 for y-axis
		scale_x_continuous(breaks = seq(floor(min_fc_value), ceiling(max_fc_value), 1)) +  # Step of 1 for x-axis
		theme_bw(base_size = 8) +
		theme(
			legend.position = "top",
			legend.title = element_blank(),
			legend.text = element_text(size = 12)
			# panel.border = element_rect(color = 'black', fill = NA, linewidth = 1.5)
		)
	
	if(!show_grid){
		figure <- figure +
			theme(
				panel.grid.major = element_blank(),
				panel.grid.minor = element_blank()
			)
	}
	
	return(figure)
}



## ================================================================================
## -------------------------------- Strip Plot Genes ------------------------------
## ================================================================================
strip_plot_genes <- function(
		r_env, gene_names, title="", ncol=5,
		cmap = NULL
){
	
	plot_data <- r_env$edata %>%
		as.data.frame() %>% 
		tibble::rownames_to_column("Gene") %>% 
		dplyr::filter(Gene %in% gene_names)
	
	r_env$n_splot_genes <- nrow(plot_data)
	
	print(nrow(plot_data))
	if(nrow(plot_data) == 0) { print("no row in strip-plot!"); return(0) }
	
	fig <- plot_data %>% 
		tidyr::pivot_longer(
			cols = -'Gene',
			names_to = 'Sample_ID',
			values_to = 'Expression'
		) %>% 
		dplyr::left_join(
			rownames_to_column(r_env$mdata, 'Sample_ID'),
			by='Sample_ID'
		) %>% 
		ggplot(
			aes(x = condition, y=Expression, color=condition)
		) +
		geom_boxplot() +
		geom_jitter(width=0.2, alpha=0.6) +
		facet_wrap(
			"Gene", 
			scales = "free_y",
			ncol = ncol
		) +
		ggtitle(title) +
		theme_bw() +
		theme(
			plot.title = element_text(size=16, hjust=0.5, margin = margin(b = 10)),
			axis.title.x = element_blank(),
			axis.text.x = element_text(angle = 45, vjust = 0.5)
		)
	
	if(!is.null(cmap)){
		fig <- fig + 
			scale_color_manual(
				values = cmap
			)
	}
	
	return(fig)
}





## ================================================================================
## -------------------------------- Strip Plot Genes ------------------------------
## ================================================================================
# e_mapper <- sample(entrez_mapper$EID, 10)
# 
# 
# go_result_bp <- enrichGO(
# 	gene         = e_mapper,
# 	OrgDb        = org.Hs.eg.db::org.Hs.eg.db,
# 	keyType      = "ENTREZID",
# 	ont          = "BP",
# 	pAdjustMethod= "BH",
# 	pvalueCutoff = 1,
# 	qvalueCutoff = 1
# )
# 
# go_result_mf <- enrichGO(
# 	gene         = e_mapper,
# 	OrgDb        = org.Hs.eg.db::org.Hs.eg.db,
# 	keyType      = "ENTREZID",
# 	ont          = "MF",
# 	pAdjustMethod= "BH",
# 	pvalueCutoff = 1,
# 	qvalueCutoff = 1
# )
# 
# go_result_cc <- enrichGO(
# 	gene         = e_mapper,
# 	OrgDb        = org.Hs.eg.db::org.Hs.eg.db,
# 	keyType      = "ENTREZID",
# 	ont          = "CC",
# 	pAdjustMethod= "BH",
# 	pvalueCutoff = 1,
# 	qvalueCutoff = 1
# )
# 
# kegg_result <- enrichKEGG(
# 	gene         = e_mapper,
# 	organism     = "hsa",
# 	pvalueCutoff = 1
# )

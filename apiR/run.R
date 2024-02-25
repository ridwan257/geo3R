install.packages("base64enc")
install.packages("jsonlite")
install.packages("png")
install.packages("pheatmap")
install.packages("RColorBrewer")
install.packages("devtools")
install.packages("rafalib")
install.packages("plumber")
install.packages("BiocManager")
install.packages('ggplot2')
install.packages('dplyr')
install.packages('ggrepel')
install.packages('limma')


library(BiocManager)

BiocManager::install("BiocGenerics")
BiocManager::install("GEOquery")
BiocManager::install("Biobase")


library(plumber)

plumb('./plumber.R')$run(port=10000)

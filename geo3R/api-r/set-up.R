r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)

path <- './rlib'

.libPaths(c('./rlib', .libPaths()))

install.packages("base64enc", lib=path)
install.packages("jsonlite",  lib=path)
install.packages("png",  lib=path)
install.packages("pheatmap", lib=path)
install.packages("RColorBrewer", lib=path)
# install.packages("devtools")
install.packages("rafalib", lib=path)
install.packages("plumber", lib=path)
install.packages("BiocManager", lib=path)
install.packages('ggplot2', lib=path)
install.packages('dplyr', lib=path)
install.packages('ggrepel', lib=path)
install.packages('limma', lib=path)


library(BiocManager)
BiocManager::install("BiocGenerics", lib=path)
BiocManager::install("GEOquery", lib=path)
BiocManager::install("Biobase", lib=path)



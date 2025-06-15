if (!requireNamespace("renv", quietly = TRUE)) 
	install.packages("renv")

if (!dir.exists("../tmp")) dir.create("../tmp")

renv::restore()

library(plumber)
plumb('./plumber_api.R')$run(host='127.0.0.1', port=8080)






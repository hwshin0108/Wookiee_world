# One click 

cat("\033[32m", "Choose your Directory for raw data", "\033[39m", "\n")
full_path <- tcltk::tk_choose.dir()

source(file = paste0(full_path, "/", '0. bulk_pre-process.R'))
source(file = paste0(full_path, "/", '1. Normalization_edgeR.R'))
source(file = paste0(full_path, "/", '2. Analysis_edgeR.R'))
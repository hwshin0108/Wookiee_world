##### # To using tcltk, if u r mac user -> download XQuartz

#@ install pakage
# install.packages("BiocManager")
# 
# lapply(c("AnnotationDbi", "org.Hs.eg.db", "org.Mm.eg.db", "edgeR", "enrichplot", "clusterProfiler"), BiocManager::install, character.only = T)
# lapply(c("data.table"), install.packages, character.only = T)

#@ load packages
lapply(c("tidyverse", "data.table", "AnnotationDbi", "org.Hs.eg.db", "org.Mm.eg.db", "edgeR", "tcltk", "enrichplot",
         "clusterProfiler"), library, character.only = T)

#####
#@ Data loading, pre-processing
cat("\033[32m", "Choose your raw data", "\033[39m", "\n")
data <- fread(tcltk::tk_choose.files())
data <- as.data.frame(data)
rownames(data) <- data[,1]
data <- data[,-1]

cat("\033[32m", "Labeling\n", "Check your Group:", "\033[39m", colnames(data), "\n") # \n -> 개행문자 = like enter
repeat { # to avoid ERROR, using repeat (= while)
  group_name <- as.character(readline(prompt = "Enter group name (ex) Con, Exp1, Exp2) -> "))
  group_name <- trimws(as.vector(strsplit(group_name, ",")[[1]]))
  
  if (length(group_name) > 0) {
    cat("Done\n")
    break
  } else {
    cat("\033[34m", "Write your group names.\n", "\033[39m")
  }
}

cat("\033[32m", "Labeling\n", "Replicate number by group:", "\033[39m", colnames(data), "\n") # \n -> 개행문자 = like enter
repeat { # to avoid ERROR, using repeat (= while)
  group_num <- as.character(readline(prompt = "Enter replicate number (ex) 3, 3, 4) -> "))
  group_num <- as.numeric(trimws(as.vector(strsplit(group_num, ",")[[1]])))
  
  if (length(group_name) == length(group_num) && sum(group_num) == ncol(data)) {
    cat("Done\n")
    break
  } else if (length(group_num) == 0) {
    cat("\033[34m", "Write your replicate number.\n", "\033[39m")
  } else {
    cat("\033[34m", "Wrong value! plz check labeling!\n", "\033[39m")
  }
}

group_colname <- c()
condition <- c()

for (i in seq_along(group_num)) {
  for (j in 1:group_num[i]) {
    group_colname <- c(group_colname, paste0(group_name[i], "_", j))
    condition <- c(condition, group_name[i])
  }
}

colnames(data)[1:ncol(data)] <- group_colname

temp <- rowSums(data)        # In Count number typ data: remove all row value = 0
data_ft <- data[temp != 0,]

#### preparing count data
cat("\033[32m", "Choose your directory to save \n", "\033[39m")
cat("\033[32m", "This dataset has been renamed and filtered by removing entries with zero values.\n", "\033[39m")
save_path <- tcltk::tk_choose.dir()
write.csv(data_ft, file = paste0(save_path, "/", readline(prompt = "Give your file name: ") ,".csv"), row.names = T) 

warning("Please check your groups labeling sequence and error") #####################
## using upper csv file, preparing metadata in excel (Sample Name, condition need)

rm(data, condition, group_colname, group_name, group_num, i, j, save_path, temp)

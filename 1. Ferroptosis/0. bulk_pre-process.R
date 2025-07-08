##### 
#@ install pakage
lapply(c("DESeq2", "AnnotationDbi", "org.Hs.eg.db", "edgeR"), BiocManager::install, character.only = T)
lapply(c("data.table"), install.packages, character.only = T)

#@ load packages
lapply(c("DESeq2", "tidyverse", "data.table", "AnnotationDbi", "org.Hs.eg.db", "edgeR"), library, character.only = T)

#####
#@ Data loading, pre-processing

data <- fread('/Users/wookiee/R_local repository/wookiee/1. Ferroptosis/Data_csv/0. Counts_from_fasta_2024-11-17 (raw data).csv')

group_name <- c("CON", "Era", "OA", "RSL")
group_num <- c(4, 4, 4, 4)
group_colname <- c()
condition <- c()

for (i in seq_along(group_num)) {
  for (j in 1:group_num[i]) {
    group_colname <- c(group_colname, paste0(group_name[i], "_", j))
    condition <- c(condition, group_name[i])
  }
}

colnames(data)[2:ncol(data)] <- group_colname
colnames(data)[ncol(data)] <- "RSL_4"
condition <- condition[-length(condition)]

temp <- rowSums(data[,-1])        # In Count number typ data: remove all row value = 0
data_ft <- data[temp != 0,]
temp_ft <- data_ft[,-1]
rownames(temp_ft) <- as.character(unlist(data_ft[,1]))

#### preparing count data
write.csv(temp_ft, file = "/Users/wookiee/R_local repository/wookiee/1. Ferroptosis/Data_csv/1. filtered_data.csv", row.names = T) 
## using upper csv file, preparing metadata in excel (Sample Name, condition need)

##### 
#@ install pakage
lapply(c("DESeq2", "AnnotationDbi", "org.Hs.eg.db"), BiocManager::install, character.only = T)
lapply(c("data.table"), install.packages, character.only = T)

#@ load packages
lapply(c("DESeq2", "tidyverse", "data.table", "AnnotationDbi", "org.Hs.eg.db"), library, character.only = T)

#####
#@ Data loading, pre-processing

data <- fread("/Users/wookiee/R_local repository/wookiee/1. Ferroptosis/Counts_from_fasta_2024-11-17 (raw data).csv")

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

data$gene <- as.character(mapIds(org.Hs.eg.db, rownames(data), keytype = "ENTREZID", "SYMBOL")) 
data <- na.omit(data)
data$gene <- make.unique(data$gene)
temp <- data$gene
data <- data[,-c(1, 17)]
rownames(data) <- temp
rm(temp)
condition <- factor(condition)
data_mat <- as.matrix(data)

dds <- DESeqDataSetFromMatrix(countData = round(data_mat), colData = DataFrame(condition), design = ~0 + condition)
dds <- DESeq(dds)
resultsNames(dds)

#####
#@


















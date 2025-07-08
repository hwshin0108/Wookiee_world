# DESeq2 running
countData <- read.csv('/Users/wookiee/R_local repository/wookiee/1. Ferroptosis/Data_csv/1. filtered_data.csv', row.names = 1)
colData <- read.csv('/Users/wookiee/R_local repository/wookiee/1. Ferroptosis/Data_csv/1. metadata.csv', row.names = 1)
all(colnames(countData) == rownames(colData))
colData$condition <- factor(colData$condition)

dds <- DESeqDataSetFromMatrix(countData = round(countData),
                              colData = colData,
                              design = ~ condition)
dds <- DESeq(dds)
resultsNames(dds)

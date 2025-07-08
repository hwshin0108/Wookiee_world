# code
lapply(c("DESeq2", "tidyverse", "data.table", "AnnotationDbi", "org.Hs.eg.db", "edgeR"), library, character.only = T)

# edgeR running
counts <- read.csv('/Users/wookiee/R_local repository/wookiee/1. Ferroptosis/Data_csv/1. filtered_data.csv', row.names = 1)
group <- factor(unlist(read.csv('/Users/wookiee/R_local repository/wookiee/1. Ferroptosis/Data_csv/1. metadata.csv', row.names = 1)))

# TMM normalization
y <- DGEList(counts = counts,
             group = group)
keep <- filterByExpr(y)

y <- y[keep, , keep.lib.sizes = F]
y <- calcNormFactors(y)

# Becareful and comapring with single group value

expt <- unique(as.vector(group))[-1]
temp_ls <- list()

for (i in seq_along(expt)) {
  # list 초기화 단계
  if (i == 1) {
    temp_ls <- list()
  }
  
  design <- model.matrix(~group)
  y <- estimateDisp(y, design)
  fit <- glmQLFit(y, design)
  temp_ls[[expt[i]]] <- glmQLFTest(fit, coef = i + 1)
  saveRDS(temp_ls[i], file = paste0('/Users/wookiee/R_local repository/wookiee/1. Ferroptosis/Data_rds/1. edgeR_normalized_', expt[i] ,'.rds'))
  write.csv(temp_ls[[i]], file = paste0('/Users/wookiee/R_local repository/wookiee/1. Ferroptosis/Data_csv/1. edgeR_normalized_', expt[i] ,'.csv'), row.names = T)
  
  temp_edg <- temp_ls[[expt[i]]]$table
  temp_edg <- temp_edg[temp_edg$PValue < 0.05 & abs(temp_edg$logFC) > 1.5,]
  write.csv(temp_edg, file = paste0('/Users/wookiee/R_local repository/wookiee/1. Ferroptosis/Data_rnk/1. edgeR_filtered_', expt[i] ,'.csv'), row.names = T)
}

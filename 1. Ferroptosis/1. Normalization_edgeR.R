# code
lapply(c("tidyverse", "data.table", "AnnotationDbi", "org.Hs.eg.db", "edgeR"), library, character.only = T)

# edgeR running
if (exists("data_ft")) {
  counts <- data_ft
  group <- factor(sub("_.*", "", colnames(counts)))
} else {
  cat("\033[32m", "Choose your filtered data", "\033[39m", "\n")
  counts <- read.csv(tcltk::tk_choose.files(), row.names = 1)
  group <- factor(sub("_.*", "", colnames(counts)))
}

# TMM normalization
y <- DGEList(counts = counts,
             group = group)
keep <- filterByExpr(y)

y <- y[keep, , keep.lib.sizes = F]
y <- calcNormFactors(y, method = "TMM") # trimmed mean, 절사평균, remove effect of outlier.

# Becareful and comapring with single group value
expt <- unique(as.vector(group))
temp_ls <- list()

for (i in seq_along(expt)) {
  # list 초기화 단계
  if (i == 1) {
    temp_ls <- list()
    cat("\033[32m", "Choose your Directory to save rds and csv files (normalized by edgeR)", "\033[39m", "\n")
    save_path <- tcltk::tk_choose.dir()
    
    if (!dir.exists(paste0(save_path, "/rds"))) {
      dir.create(paste0(save_path, "/rds"), recursive = TRUE)
    }
    if (!dir.exists(paste0(save_path, "/csv"))) {
      dir.create(paste0(save_path, "/csv"), recursive = TRUE)
    }
    
  } else if (i == length(expt)) {
    break
  }
  
  design <- model.matrix(~group)
  y <- estimateDisp(y, design)
  fit <- glmQLFit(y, design)
  temp_ls[[expt[i]]] <- glmQLFTest(fit, coef = i + 1)
  
  saveRDS(temp_ls[i], file = paste0(save_path, "/rds/", expt[i+1], "_vs_Con", '.rds'))
  write.csv(temp_ls[[i]], file = paste0(save_path, "/csv/", expt[i+1], "_vs_Con", '.csv'), row.names = T)
  # temp_edg <- temp_edg[temp_edg$PValue < 0.05 & abs(temp_edg$logFC) > 1,]
}
cat("\033[32m", "Done", "\033[39m", "\n")

if (exists("data_ft")) {
  rm(counts, design, fit, temp_ls, y, expt, group, i, keep, save_path, data_ft)  
} else {
  rm(counts, design, fit, temp_ls, y, expt, group, i, keep, save_path)
}


## Analyze_edgeR.ver
# GO enrichment analysis 
# To using tcltk, if u r mac user -> download XQuartz
lapply(c("clusterProfiler", "AnnotationDbi", "org.Hs.eg.db", "org.Mm.eg.db", "dplyr", 
         "tcltk", "enrichplot"), library, character.only = T)

# Folder path where I call the files
cat("\033[32m", "Choose your Directory for analysis (.csv folder_normalized by edgeR)", "\033[39m", "\n")
full_path <- tcltk::tk_choose.dir()
ls_data <- list()
repeat {
  if (length(ls_data) == 0) {
    ls_data <- list.files(full_path, pattern = "\\.csv$", full.names = T)# \\ - previous path, $ - means end 
  } else {
    break
  }
}
lab_data <- list.files(full_path, pattern = "\\.csv$", full.names = F) # only file name, .csv가 붙음
ls_gene <- list()
# From filtered data, take only logFC value

for (i in seq_along(ls_data)) {
  temp <- read.csv(ls_data[i], row.names = 1)
  temp <- temp[temp$PValue < 0.05,]
  row_names <- rownames(temp)
  temp <- temp$logFC
  names(temp) <- row_names
  temp = sort(temp, decreasing = T)
  ls_gene[[i]] <- temp
  names(ls_gene)[i] <- substr(lab_data[i], 1, nchar(lab_data[i]) - 4) ## remove format of file (group name only)
}

# Notice 
## choice the organism for GSE
ls_org <- list("1" = "org.Hs.eg.db",
               "2" = "org.Mm.eg.db")
ls_kegg <- list("1" = "hsa",
                "2" = "mmu")

cat("\033[32m", "Select orgaism", "\033[39m\n", "Human(hsa) = 1, Mouse(mmu) = 2") # \n -> 개행문자 = like enter
repeat { # to avoid ERROR, using repeat (= while)
  temp_org <- as.numeric(readline(prompt = "Organism -> "))
  if (temp_org %in% c(1, 2)) {
    break
  } else {
    cat("\033[34m", "Wrong value! Plz select b/w 1 ~ 2.\n", "\033[39m")
  }
}
user_org <- ls_org[[temp_org]]
user_kegg <- ls_kegg[[temp_org]]

ls_key <- c("ENTREZID", "ENSEMBL", "SYMBOL")

cat("\033[32m", "Select keyType\n", "Your keyType:", "\033[39m", head(row_names), "\n",
    "ENTREZID = 1, ENSEMBL = 2, SYMBOL = 3") # \n -> 개행문자 = like enter
repeat { # to avoid ERROR, using repeat (= while)
  temp_key <- as.numeric(readline(prompt = "keyType -> "))
  if (temp_key %in% c(1, 2, 3)) {
    break
  } else {
    cat("\033[34m", "Wrong value! Plz select b/w 1 ~ 3.\n", "\033[39m")
  }
}

user_key <- ls_key[temp_key]

if (user_key != "ENTREZID") {
  for (i in seq_along(ls_gene)) {
      names(ls_gene[[i]]) <- as.character(mapIds(x = user_org,
                                                 names(ls_gene[[i]]),
                                                 user_key,
                                                 "ENTREZID"))
      ls_gene[[i]] <- na.omit(ls_gene[[i]])
  }
}

cat("\033[32m", "Analyzing GO and KEGG", "\033[39m", "\n") # \n -> 개행문자 = like enter
for (i in seq_along(ls_gene)) {
  gse_bp <- gseGO(geneList = ls_gene[[i]], 
               ont = "BP", keyType = "ENTREZID", minGSSize = 5, maxGSSize = 5000,  ## 5-5000 possible
               pvalueCutoff = 1, verbose = TRUE, OrgDb = user_org, pAdjustMethod = 'BH') # method (fdr or BH)
  gse_bp <- setReadable(gse_bp, user_org, keyType = "ENTREZID") ## core enrichment -> as a SYMBOL (from ENTREZID)
  gse_bp@result$ont <- rep("BP", nrow(gse_bp@result))
  
  gse_cc <- gseGO(geneList = ls_gene[[i]], 
                  ont = "CC", keyType = "ENTREZID", minGSSize = 5, maxGSSize = 5000,  ## 5-5000 possible
                  pvalueCutoff = 1, verbose = TRUE, OrgDb = user_org, pAdjustMethod = 'BH') # method (fdr or BH)
  gse_cc <- setReadable(gse_cc, user_org, keyType = "ENTREZID") ## core enrichment -> as a SYMBOL (from ENTREZID)
  gse_cc@result$ont <- rep("CC", nrow(gse_cc@result))
  
  gse_mf <- gseGO(geneList = ls_gene[[i]], 
                  ont = "MF", keyType = "ENTREZID", minGSSize = 5, maxGSSize = 5000,  ## 5-5000 possible
                  pvalueCutoff = 1, verbose = TRUE, OrgDb = user_org, pAdjustMethod = 'BH') # method (fdr or BH)
  gse_mf <- setReadable(gse_mf, user_org, keyType = "ENTREZID") ## core enrichment -> as a SYMBOL (from ENTREZID)
  gse_mf@result$ont <- rep("MF", nrow(gse_mf@result))
  
  gse_kegg <- gseKEGG(geneList = ls_gene[[i]], organism = user_kegg, keyType = "ncbi-geneid", 
                  minGSSize = 5, maxGSSize = 5000, pvalueCutoff = 1, pAdjustMethod = "BH", 
                  verbose = TRUE, use_internal_data = FALSE, seed = FALSE)
  gse_kegg <- setReadable(gse_kegg, user_org, keyType = "ENTREZID")
  gse_kegg@result$ont <- rep("KEGG", nrow(gse_kegg@result))
  
  assign(paste0(names(ls_gene[i]), "_GOBP"), gse_bp)
  assign(paste0(names(ls_gene[i]), "_GOCC"), gse_cc)
  assign(paste0(names(ls_gene[i]), "_GOMF"), gse_mf)
  assign(paste0(names(ls_gene[i]), "_KEGG"), gse_kegg)
  
  cat("\033[32m", "Integrate all of data", "\033[39m\n")
  gse_temp <- gse_bp
  gse_temp@result <- rbind(gse_bp@result, gse_cc@result, gse_mf@result, gse_kegg@result)
  assign(paste0(names(ls_gene[i]), "_merge"), gse_temp)
  
  if (i == 1) {
    cat("\033[32m", "Do you want to save merge GSEA results?", "\033[39m", "\n")
    temp_choice <- toupper(readline(prompt = "Y/N: "))
    if (temp_choice == "Y") {
      cat("\033[32m", "Select your directory", "\033[39m", "\n")
      temp_path <- tcltk::tk_choose.dir()
      write.csv(gse_temp@result, file = paste0(temp_path, "/", names(ls_gene[i]), ".csv"))
      cat("\033[32m", "Saving...", "\033[39m", "\n")
    }
  }
  
  if (i != length(ls_gene)) {
    cat("\n\033[32m", "Analyzing next group...", "\033[39m", "\n")
  }
}

rm(list = ls(pattern = paste0(c("temp", "user", "ls", "lab", "gse"), collapse = "|")))
rm(i, full_path, row_names)

# 
# RSL_kegg2 <- RSL_kegg
# RSL_kegg2@result <-a
# a <- subset(RSL_kegg, RSL_kegg@result$Description %in% interest)
# gseaplot2(RSL_kegg2, geneSetID = 1:4)
# 
# interest = c("Fatty acid metabolism", "Oxidative phosphorylation", "Neutrophil extracellular trap formation", "Ferroptosis")
# dotplot(Era, x="NES",color = "pvalue")
# dotplot(RSL_kegg, x = "NES", showCategory = interest, color = "pvalue")

## Analyze_edgeR.ver
# GO enrichment analysis 
# To using tcltk, if u r mac user -> download XQuartz
lapply(c("clusterProfiler", "AnnotationDbi", "org.Hs.eg.db", "org.Mm.eg.db", "dplyr", "tcltk"), library, character.only = T)

# Folder path where I call the files
full_path <- tcltk::tk_choose.dir()
ls_data <- list.files(full_path, pattern = "\\.csv$", full.names = T) # \\ - previous path, $ - means end 
lab_data <- list.files(full_path, pattern = "\\.csv$", full.names = F) # only file name, .csv가 붙음
ls_gene <- list()

# From filtered data, take only logFC value

for (i in seq_along(ls_data)) {
  temp <- read.csv(ls_data[i], row.names = 1)
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

cat("\033[32m", "Select orgaism\n", "Your organism:", "\033[39m\n",
    "Human = 1, Mouse = 2: ") # \n -> 개행문자 = like enter
repeat { # to avoid ERROR, using repeat (= while)
  temp_org <- as.numeric(readline(prompt = "Organism -> "))
  if (temp_org %in% c(1, 2)) {
    break
  } else {
    cat("\033[34m", "Wrong value! Plz select b/w 1 ~ 2.\n", "\033[39m")
  }
}
user_org <- ls_org[[temp_org]]


ls_key <- c("ENTREZID", "ENSEMBL", "SYMBOL")

cat("\033[32m", "Select keyType\n", "Your keyType:", "\033[39m", head(row_names), "\n",
    "ENTREZID = 1, ENSEMBL = 2, SYMBOL = 3: ") # \n -> 개행문자 = like enter
repeat { # to avoid ERROR, using repeat (= while)
  temp_key <- as.numeric(readline(prompt = "keyType -> "))
  if (temp_key %in% c(1, 2, 3)) {
    break
  } else {
    cat("\033[34m", "Wrong value! Plz select b/w 1 ~ 3.\n", "\033[39m")
  }
}
user_key <- ls_key[temp_key]


ls_ont <- c("BP", "CC", "MF", "ALL")

cat("\033[32m", "Select Ontology\n", "\033[39m", "BP = 1, CC = 2, MF = 3, ALL = 4: ") # \n -> 개행문자 = like enter
repeat {
  temp_ont <- as.numeric(readline(prompt = "ont -> "))
  if (temp_ont %in% c(1, 2, 3, 4)) {
    break
  } else {
    cat("\033[34m", "Wrong value! Plz select b/w 1 ~ 4.\n", "\033[39m")
  }
}
user_ont <- ls_ont[temp_ont]


for (i in seq_along(ls_gene)) {
  gse <- gseGO(geneList = ls_gene[[i]], 
               ont = user_ont, keyType = user_key, nPerm = 1000, minGSSize = 3, maxGSSize = 100,
               pvalueCutoff = 0.05, verbose = TRUE, OrgDb = user_org, pAdjustMethod = 'none')
  assign(names(ls_gene)[i], gse)
}

## remove all valuse what I want
rm(list = ls(pattern = paste0(c("temp", "user", "ls", "lab"), collapse = "|")))



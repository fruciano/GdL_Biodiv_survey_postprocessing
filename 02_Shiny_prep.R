library(xlsx)

data_new=readRDS("data_post_cleanup.RDS")
# Import data


col_exclude_names=c(colnames(data_new)[1:(grep("Cognome", colnames(data_new))-1)],
                      "Newsletter")
data_for_shiny=data_new[,which((colnames(data_new) %in% col_exclude_names)==FALSE)]
# Keep only the columns relevant for the public


#####################
### Italian cols ####
#####################

col_exclude_it=c(grep("Research", colnames(data_for_shiny)),
                 grep("Techniques", colnames(data_for_shiny))
              )
data_for_shiny_IT=data_for_shiny[,-col_exclude_it]
# Remove columns present in both the Italian and English version

biogeography_italian_translation=read.xlsx(file="biogeography_italian_translation.xlsx",
                                           sheetIndex = 1)
biogeography_italian=data_for_shiny_IT$Biogeographic_area
for (i in seq(nrow(biogeography_italian_translation))){
  biogeography_italian=gsub(biogeography_italian_translation$English[i],
                            biogeography_italian_translation$Italian[i],
                            biogeography_italian)
}
data_for_shiny_IT$Biogeographic_area=biogeography_italian
# Replace biogeographical areas by their Italian translations



matrice=data_for_shiny_IT$Matrice
matrice_split=strsplit(matrice, ",")
matrice_split_ITonly=lapply(matrice_split, function(X){
  X2=lapply(X, function(x) strsplit(x, "/"))
  X3=paste(unlist(lapply(X2, function(Y) Y[[1]][1])), collapse=",")
})
matrice_split_ITonly=do.call("rbind", matrice_split_ITonly)
data_for_shiny_IT$Matrice=matrice_split_ITonly
# Remove the English version from the matrix

colnames(data_for_shiny_IT)=gsub("_", " ", colnames(data_for_shiny_IT))
colnames(data_for_shiny_IT)=gsub("Gruppo", "Gruppo di organismi", colnames(data_for_shiny_IT))
colnames(data_for_shiny_IT)=gsub("Biogeographic area", "Area biogeografica", colnames(data_for_shiny_IT))
colnames(data_for_shiny_IT)=gsub("Sito", "Sito personale", colnames(data_for_shiny_IT))
colnames(data_for_shiny_IT)=gsub("Scholar", "Google Scholar", colnames(data_for_shiny_IT))

write.xlsx(data_for_shiny_IT, file="data_for_shiny_IT.xlsx",
           row.names = FALSE)
saveRDS(data_for_shiny_IT, file="data_for_shiny_IT.RDS")



#####################
### English cols ####
#####################



col_exclude_en=c(grep("Area_di_ricerca", colnames(data_for_shiny)),
                 grep("Tecniche", colnames(data_for_shiny)),
                 grep("Incarichi", colnames(data_for_shiny))
)
data_for_shiny_EN=data_for_shiny[,-col_exclude_en]
# Remove columns present in both the Italian and English version

matrice=data_for_shiny_EN$Matrice
matrice_split=strsplit(matrice, ",")
matrice_split_ENonly=lapply(matrice_split, function(X){
  X2=lapply(X, function(x) strsplit(x, "/"))
  X3=paste(unlist(lapply(X2, function(Y) Y[[1]][2])), collapse=",")
})
matrice_split_ENonly=do.call("rbind", matrice_split_ENonly)
data_for_shiny_EN$Matrice=matrice_split_ENonly
# Remove the Italian version from the matrix

settore_english_translation=read.xlsx(file="settore_disciplinare_translation.xlsx",
                                           sheetIndex = 1)
settore_english=data_for_shiny_EN$Settore_disciplinare
for (i in seq(nrow(settore_english_translation))){
  settore_english=gsub(settore_english_translation$Italian[i],
                            settore_english_translation$English[i],
                       settore_english)
}
data_for_shiny_EN$Settore_disciplinare=settore_english
# Replace biogeographical areas by their Italian translations


colnames(data_for_shiny_EN)=c("Surname", "First name", "CNR Institute",
                              "General area", "Research area", "Group of organisms",
                              "Biogeographic realm", "Matrix",
                              "Website", "Google Scholar",
                              "ORCID", "ResearchGate"
                              )
write.xlsx(data_for_shiny_EN, file="data_for_shiny_EN.xlsx",
           row.names = FALSE)
saveRDS(data_for_shiny_EN, file="data_for_shiny_EN.RDS")

library(googlesheets4)

gs4_deauth()
# No need for authorization

google_sheet_url=""

data=read_sheet(google_sheet_url)
# Read in Google sheet data

data=data[order(data$Timestamp, decreasing = TRUE),]
# Reoder by timestamp (so that in the case of duplicates older values are removed)


#######################
### Column renaming ###
#######################

library(xlsx)

ren_patterns=xlsx::read.xlsx("renaming_patterns.xlsx", 1)

data_new=data
for (i in seq(nrow(ren_patterns))){
    sel_col=grep(ren_patterns[i,1], colnames(data), ignore.case = FALSE)
   colnames(data_new)[sel_col]=ren_patterns[i,2]
}
# Rename updating data sequentially


data=data_new

#########################
### Duplicate removal ###
#########################

data=data[!duplicated(data$Email),]
data=data[!duplicated(paste0(data$Cognome, data$Nome)),]
# Remove duplicates based on mail and/or full name

###############################
### Remove quotes from text ###
###############################

data=as.data.frame(sapply(data, function(x) gsub("\"", "", x)))

############################
### Remove entries with ####
###   blacklisted terms ####
############################

black_list=unlist(read.table(file="https://raw.githubusercontent.com/napolux/paroleitaliane/master/paroleitaliane/lista_badwords.txt",
                      header = FALSE, stringsAsFactors = FALSE))
# Import black list from repository
black_list=gsub("fica", " fica", black_list)
# Don't consider "fica" if not preceded by space, as it's often in relevant names (es., "geografica")
black_list=gsub("tette", " tette", black_list)
# Same for "tette" (es., "Aree protette")
black_list=gsub("mona", " mona", black_list)
# Same for "mona" (es., "Simona")


NSFW_results=data.frame(flagged_excluded=NULL)

flag_NSFW=apply(data, 1, FUN = function(x){
  x2=is.na(unlist(lapply(black_list, function(bdwrd){
    grep(bdwrd, x, ignore.case = TRUE)[1]
  })))==FALSE
})
flag_NSFW_bdwrd=apply(flag_NSFW, 2, which)

if (length(flag_NSFW_bdwrd)>0){
  
  
flag_NSFW_case=which(unlist(lapply(flag_NSFW_bdwrd, function(x) {
  k=NA
  if(length(x)>0){k=TRUE} else {k=FALSE}
  return(k)
})))
flag_NSFW_bdwrd=flag_NSFW_bdwrd[flag_NSFW_case]
# Flag those instances where a bad word is found

flag_NSFW2=flag_NSFW_case
for (i in seq(length(flag_NSFW_case))){
  tmp_wrd=black_list[names(flag_NSFW_bdwrd[[i]])]
  in_name=length(grep(tmp_wrd, data[flag_NSFW_case[i],"Cognome"], ignore.case = TRUE))>0
  in_mail=length(grep(tmp_wrd, data[flag_NSFW_case[i],"Email"], ignore.case = TRUE))>0
  if((in_name && in_mail)==FALSE){
    flag_NSFW2=flag_NSFW2[-i]
  }
}
# Don't consider bad words the ones that are contained both in the email and
# in the name (to allow for surnames such as "Troia")

NSFW_results=list(flagged_excluded=flag_NSFW2,
                  flagged_not_excluded=setdiff(flag_NSFW_case, flag_NSFW2)
                  )
saveRDS(NSFW_results, file = "NSFW_results.RDS")
# Save results of flagging for further investigation
}

if (length(NSFW_results$flagged_excluded)>0) {
data=data[-flag_NSFW2,]
}
# Actual removal of flagged items from dataset

###################################
###   Modifying the institute   ###
### to have a consistent style  ###
###################################

sigle_istituti=read.xlsx("sigle_istituti.xlsx", sheetIndex = 1)

istituti_new=unlist(lapply(data$Istituto, function(x){
  best_match=which(unlist(lapply(sigle_istituti$Sigla, function(y){
    length(grep(y, x, ignore.case = TRUE))>0
  })))[1]
  sigle_istituti$Sigla[best_match]
}))

data$Istituto=istituti_new

#######################
### Change all-caps ###
#######################

### THIS IS TO DO - SIGNPOST FOR LATER


##########################
#### Final reordering ####
##########################

data=data[order(data$Cognome, decreasing = FALSE),]
# For now reordering in alphabetical order, can choose different criterion
saveRDS(data, file="data_post_cleanup.RDS")
write.xlsx(data[grep("Email", colnames(data)):ncol(data)],
           file="data_post_cleanup.xlsx", sheetName = "Data",
            row.names = FALSE, showNA = FALSE)

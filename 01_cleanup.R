library(googlesheets4)

gs4_deauth()
# No need for authorization

google_sheet_url="https://docs.google.com/spreadsheets/d/1YJ9dcTKNkQBSGoBdUDru2kwxBr5s_Klz-F4nFSJcieQ/"
data=read_sheet(google_sheet_url)
# Read in Google sheet data

data=data[order(data$Timestamp, decreasing = TRUE),]
# Reoder by timestamp (so that in the case of duplicates older values are removed)



#######################
### Column renaming ###
#######################


ren_patterns=data.frame(old_colname=c("Nome", "Tax", "Tecn", "Tech", "Area di", "Research area"),
                new_colname=c("Name", "Taxa", "Tecniche", "Techniques", "Ricerca", "Research"))
# [TO DO] Change into the final useful renaming pattern
# (can also be imported from external file)

data_new=data
for (i in seq(nrow(ren_patterns))){
    sel_col=grep(ren_patterns[i,1], colnames(data), ignore.case = TRUE)
   colnames(data_new)[sel_col]=ren_patterns[i,2]
}
# Rename updating data sequentially

data=data_new

#########################
### Duplicate removal ###
#########################

data=data[!duplicated(data$Email),]
data=data[!duplicated(data$Name),]
# Remove duplicates based on mail and/or full name

############################
### Remove entries with ####
###   blacklisted terms ####
############################

black_list=unlist(read.table(file="https://raw.githubusercontent.com/napolux/paroleitaliane/master/paroleitaliane/lista_badwords.txt",
                      header = FALSE, stringsAsFactors = FALSE))
# Import black list from repository

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
  in_name=length(grep(tmp_wrd, data[flag_NSFW_case[i],"Name"], ignore.case = TRUE))>0
  in_mail=length(grep(tmp_wrd, data[flag_NSFW_case[i],"Email"], ignore.case = TRUE))>0
  if((in_name && in_mail)==FALSE){
    flag_NSFW2=flag_NSFW2[-i]
  }
}
# Don't consider bad words the ones that are contained both in the email and
# in the name (to allow for surnames such as "Troia")

NSFW_results=list(flagged_excluded=flag_NSFW2,
                  flagged_not_excluded=setdiff(flag_NSFW_case, flag_NSFW2))
saveRDS(NSFW_results, file = "NSFW_results.RDS")
# Save results of flagging for further investigation

data=data[-flag_NSFW2,]
# Actual removal of flagged items from dataset
}

#############################
### Keyword splitting and ###
###       recombining     ###
#############################

# Will use for now the one in "Techniques"
# Can be applied later to other fields with keywords

# This is to avoid the inclusion of more than 5 keywords
# and also very long keywords with too many words

kwd_split_comb=function(kwd_txt, sep=",", max_kwd_n=5, max_words_per_kwd=4, dbl_space_rem=TRUE){
  text_split=strsplit(kwd_txt, split = sep)[[1]]
  if (length(text_split)>max_kwd_n){
    text_split[seq(max_kwd_n)]
  }
  # Keep only the first bunch of keywords
  text_split=trimws(text_split, which = "both")
  # Remove leading/trailing white spaces
  if (dbl_space_rem==TRUE){
  text_split=unlist(lapply(text_split, function(x) gsub("  ", " ", x)))
  }
  # Remove double spaces
  kwd_to_keep=unlist(lapply(text_split, function(x){
    length(strsplit(x, split = " ")[[1]])<=max_words_per_kwd
    }))
  text_split=text_split[kwd_to_keep]
  # Remove keywords longer than a certain number of words
  if (length(text_split)>1){
    text_comb=paste(text_split, collapse=",")
  } else if (length(text_split)==1){
    text_comb=text_split
  } else {
    text_comb=NA
  }
return(text_comb)
}


data$Techniques=unlist(lapply(data$Techniques, kwd_split_comb))


##########################
#### Final reordering ####
##########################

data=data[order(data$Name, decreasing = FALSE),]
# For now reordering in alphabetical order, can choose different criterion
saveRDS(data, file="data_post_cleanup.RDS")

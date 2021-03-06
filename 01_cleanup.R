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

##############################
### Remove space from URLs ###
##############################

url_fields=which(colnames(data) %in%
                         c("Sito", "Scholar", "ORCID", "ResearchGate"))

for (j in seq(length(url_fields))){
  data[,url_fields[j]]=gsub(" ", "", data[,url_fields[j]])
}

##########################################
### Remove links without http or https ###
##########################################

for (fld in url_fields){
  tmp_url_fld=data[,fld]
  tmp_url_fld_valid_idx=union(grep("http://", tmp_url_fld),
                              grep("https://", tmp_url_fld))
  tmp_url_fld_to_NA=setdiff(seq(length(tmp_url_fld)), tmp_url_fld_valid_idx)
  tmp_url_fld[tmp_url_fld_to_NA]=NA
  data[,fld]=tmp_url_fld
}

############################
### Fix malformed ORCIDs ###
############################

data$ORCID=gsub("my-orcid\\?orcid=", "", data$ORCID)
# Some people by mistake put their login page

orcid_w_search_idx=grep("search", data$ORCID)
if (length(orcid_w_search_idx)>0){
  for (i in seq(length(orcid_w_search_idx))){
    tmp_orcid_str=data$ORCID[orcid_w_search_idx[i]]
    if (length(grep("-", tmp_orcid_str)>0)){
    tmp_orcid_str=strsplit(tmp_orcid_str, "=")[[1]]
    tmp_orcid_str=tmp_orcid_str[length(tmp_orcid_str)]
    new_tmp_orcid_str=paste0("https://orcid.org/", tmp_orcid_str)
    data$ORCID[orcid_w_search_idx[i]]=new_tmp_orcid_str
    }
  }
}
# Some people insert the search for their own ORCID ID

######################################
### Remove Scholar from websites   ###
######################################

data$Sito[grep("scholar", data$Sito)]=NA

#########################
### Fix Scholar links ###
#########################

non_scholar_scholar=setdiff(seq(length(data$Scholar)),
                            grep("scholar", data$Scholar))
data$Scholar[non_scholar_scholar]=NA
# Remove links to pages which are not Scholar profiles

non_profile_scholar=setdiff(seq(length(data$Scholar)),
                            grep("citations", data$Scholar))
data$Scholar[non_profile_scholar]=NA
# Remove links which do not contain "citations" in the URL
# (usually these are simply Scholar searches)

##############################
### Fix ResearchGate links ###
##############################

non_RG_RG=setdiff(seq(length(data$ResearchGate)),
                            grep("researchgate", data$ResearchGate))
data$ResearchGate[non_RG_RG]=NA
# Remove links to pages which are not ResearchGate profiles



#################################
###     Final "last resort"   ###
### check that URL is working ###
#################################

library(httr)
for (fld in url_fields){
  for (i in seq(nrow(data))){
  if (is.na(data[i,fld])==FALSE){
    
    tryCatch({
      if(http_status(GET(data[i,fld],config = httr::config(connecttimeout = 20)
      ))$category!="Success"){
        data[i,fld]=NA 
      } 
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
 }  
}
### Remember to use this only with good connection (lots of timeouts may
### happen otherwise)


#######################
### Change all-caps ###
#######################

library(tools)
free_text_fields=which(colnames(data) %in%
                         c("Email","Cognome", "Nome",
                           "Area_di_ricerca", "Research_area",
                           "Tecniche", "Techniques",
                           "Sito", "Scholar", "ORCID", "ResearchGate"))

for (j in seq(length(free_text_fields))){
  tmp_all_caps_idx=which(unlist(lapply(data[,free_text_fields[j]], function(s)
    s==toupper(s))))
  if (length(tmp_all_caps_idx)>0){
  data[tmp_all_caps_idx,free_text_fields[j]]=toTitleCase(
    tolower(data[tmp_all_caps_idx,free_text_fields[j]])
  )
  }
}

data[,free_text_fields]

#############################################
### Change name and surname all lowercase ###
#############################################

nome_cognome_cols=which(colnames(data) %in% c("Cognome", "Nome"))

for (j in seq(length(nome_cognome_cols))){
  tmp_lw_caps_idx=which(unlist(lapply(data[,nome_cognome_cols[j]], function(s)
    s==tolower(s))))
  if (length(tmp_lw_caps_idx)>0){
    data[tmp_lw_caps_idx,nome_cognome_cols[j]]=toTitleCase(
      data[tmp_lw_caps_idx,nome_cognome_cols[j]]
    )
  }
}

#######################################
### Small changes to research areas ###
###          and techniques         ###
#######################################

res_tech_fields=which(colnames(data) %in%
                        c("Area_di_ricerca", "Research_area",
                          "Tecniche", "Techniques"))
for (fld in res_tech_fields){
  data[,fld]=unlist(lapply(data[,fld], function(x){
    paste(toupper(substring(x, 1, 1)),
          tolower(substring(x, 2, nchar(x))),
          sep = "")
  }))
}
# Make sure that all the research areas and techniques fields
# always start with an uppercase letter

for (fld in res_tech_fields){
  data[,fld]=gsub(";", ",", data[,fld])
}
# Change the ";" into "," so that when describing several fields,
# always the same separator is used

##########################
#### Final reordering ####
##########################

data=data[order(data$Cognome, decreasing = FALSE),]
# For now reordering in alphabetical order, can choose different criterion
saveRDS(data, file="data_post_cleanup.RDS")
write.xlsx(data[grep("Email", colnames(data)):ncol(data)],
           file="data_post_cleanup.xlsx", sheetName = "Data",
            row.names = FALSE, showNA = FALSE)

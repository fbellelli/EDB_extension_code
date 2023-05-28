


extract_implementation_years <- function(data){
  
  #EXECUTION INFO:------------------------------------------------------
  
  original_number_of_columns <- ncol(data)
  this_year <- lubridate::year(Sys.Date())
  #_______________________________________________________________________________
  
  #IDENTIFY TYPE OF MEASURE:------------------------------------------------------
  
  #measures are split in two groups. for the first one (regulation_standards) we only look for the starting year of the measure. For the second group (subsidies) we look for both the starting and ending year.
  
  #list of harmonised measure types
  regulations_standards<-c("Technical regulation or specifications",
                           "Conformity assessment procedures",
                           "Import licences",
                           "Ban/Prohibition",
                           "Export licences",
                           "Risk assessment",
                           "Countervailing measure / investigation",
                           "Regulation affecting movement or transit",
                           "Environmental provisions in trade agreements",
                           "Other environmental requirements",
                           "Import quotas",
                           "Intellectual property measures",
                           "Quarantine requirements",
                           "Safeguard measure / investigation",
                           "Export quotas",
                           "Investment measures",
                           "Anti-dumping measure / investigation",
                           "Services requirements",
                           "Internal taxes",
                           "Export tariffs") 
  subsidies<-  c("Grants and direct payments",
                 "Non-monetary support",
                 "Tax concessions",
                 "Loans and financing",
                 "Public procurement",
                 "Other support measures",
                 "Income or price support",
                 "Import tariffs",
                 "Other price and market based measures")
  
  #classify measures
  data$is_subsidy<-NA
  data$is_regulation<-NA
  for (i in 1:nrow(data)){
    data$is_subsidy[i]<-any(subsidies %fin% str_split(data$`Harmonized types of measures`[i],";")[[1]])
    data$is_regulation[i]<-any(regulations_standards %fin% str_split(data$`Harmonized types of measures`[i],";")[[1]])
  }
  #_______________________________________________________________________________
  
  
  
  
  #EXTRACT IMPLEMENTATION PERIOD FOR SUBSIDIES:-----------------------------------
  
  #extract unique implementation period descriptions (reduces number of iterations and speeds execution)
  text_subsidy<-unique(data[is_subsidy==TRUE,]$`Implementation period`)
  text_subsidy<-data.table(original=text_subsidy,text=text_subsidy,subsidy_start=NA,subsidy_end=NA)
  for (i in 1:nrow(text_subsidy)){
    
    #Whenever possible subset string from "Duration of the measure/subsidy"
    #  text_subsidy$text[i]<-gsub(".*((?:Duration of the measure|Duration of the subsidy).*)", replacement="\\1", x=text_subsidy$text[i], perl = TRUE)
    
    #remove reporting years at start of string
    text_subsidy$text[i]<-gsub("^(?:(?:\\d{1:2} )?(?:January|February|March|April|May|June|July|August|September|October|November|December) )?\\d{4} - (?:(?:\\d{1:2} )?(?:January|February|March|April|May|June|July|August|September|October|November|December) )?\\d{4}(\\[A-z]+.*)", replacement="\\1", x=text_subsidy$text[i], perl = TRUE)
    
    #From ... to. Find explicit date ranges
    temp<-"(?:[Ff]rom|[Ss]ince|[Bb]etween)?(?:(?: \\d{1,2}[stndrh]{0,2})?\\s?(?:of)?[a-zA-Z]{0,9})?\\s?(\\d{4}) (?:to|until|up to|-|till|and)(?:(?: \\d{1,2}[stndrh]{0,2})?\\s?(?:of)?[a-zA-Z]{0,9})? (\\d{4})\\b"
    if (grepl(pattern=temp ,x=text_subsidy$text[i], perl = TRUE)) {
      temp <- str_match_all(text_subsidy$text[i],temp)
      text_subsidy$subsidy_start[i]<-min(temp[[1]][,2])
      text_subsidy$subsidy_end[i]<-max(temp[[1]][,3])
    }
    
    #detect strings specifying an end date explicitly
    temp<-"(?:Ends(?: on| in)?|[Ee]nded(?: on| in)?|[Ee]nding(?: on|in)?|[Ee]xpire[sd](?: on| in)?|Terminated(?: on| in)?|available until|[Uu]ntil(?: end| end of)?|[Ll]atest|Prolonged until|[Uu]p to the end(?: of)?|Project completed after|On-going until|until and including|Currently to|will expire on|not be applied after the year|available till|repealed\\s?for facilities placed in service after|continue provisionally until|Phase-out from|produced before|On-going [[:punct:]]sunset|[Ss]unset[[:punct:]]|Last date for application[[:punct:]]|[Ss]unsets(?: in| on)?|[Ee]xpiration of the [Ll]aw(?: on| in)) (?:(?:\\d{1,2}[stndrh]{0,2})?\\s?(?:of )?[a-zA-Z]{0,9}\\s?)?(?:\\d{1,2}/\\d{1,2}/)?(\\d{4})\\b"
    if(is.na(text_subsidy$subsidy_end[i])&grepl(pattern=temp ,x=text_subsidy$text[i], perl = TRUE)){ 
      text_subsidy$subsidy_end[i]<-max(str_match_all(text_subsidy$text[i],temp)[[1]][,2])
      temp<-min(str_match_all(text_subsidy$original[i],"(?:[[:punct:]]|\\b)(\\d{4})(?:[[:punct:]]|\\b|Period of application|Duration of the)")[[1]][,2])
      text_subsidy$subsidy_start[i]<-ifelse(temp<text_subsidy$subsidy_end[i],temp,"-")
    }
    
    
    if(is.na(text_subsidy$subsidy_start[i])&is.na(text_subsidy$subsidy_end[i])){
      #single year measures
      temp <- "(?:(?:Calendar|Fiscal|Marketing|Financial) year|FY) (?:(?:\\d{1,2}[stndrh]{0,2})?\\s?(?:of )?[a-zA-Z]{0,9}\\s?)?(?:\\d{1,2}/\\d{1,2}/)?(\\d{4})$|^(?:[Dd]uration of the (?:subsidy|measure|policy):(?: [Tt]he [Yy]ear)?\\s)?(\\d{4})$"
      text_subsidy$subsidy_start[i] <- ifelse(grepl(pattern=temp ,x=text_subsidy$text[i], perl = TRUE),min(str_match_all(text_subsidy$text[i],temp)[[1]][,2:3],na.rm = TRUE),NA)
      text_subsidy$subsidy_end[i] <- text_subsidy$subsidy_start[i]
    }
    
    
    if(is.na(text_subsidy$subsidy_start[i])){
      #Find smallest year in string
      temp<-str_match_all(text_subsidy$original[i],"(?:[[:punct:]]|\\b)(\\d{4})(?:[[:punct:]]|\\b|Period of application|Duration of the)")[[1]][,2]
      temp<-temp[temp>1950] #discard values before 1950
      text_subsidy$subsidy_start[i]<-ifelse(length(temp)==0,NA,min(temp))
    }
    
    if(is.na(text_subsidy$subsidy_end[i])){
      #Find largest year in string. If only one date found in string then it is assumed that no end date exist
      temp<-str_match_all(text_subsidy$text[i],"(?:\\b|[[:punct:]])(\\d{4})(?:\\b|[[:punct:]])")[[1]][,2]
      temp<-temp[(temp>=1990)&(temp<2100)]   #dropping extreme dates
      text_subsidy$subsidy_end[i]<-ifelse(ifelse(length(temp)>1,!is.na(temp),FALSE),
                                          max(temp),
                                          "-")
    }
  }
  rm(i,temp)
  text_subsidy$text<-NULL
  
  #import results into data table
  data<-left_join(data,text_subsidy,by=c("Implementation period"="original"))
  
  #If no start date was found, use the notification year
  data$subsidy_end <- suppressWarnings(as.integer(data$subsidy_end))
  data$subsidy_start <- suppressWarnings(as.integer(data$subsidy_start))
  data$subsidy_start<-ifelse((data$is_subsidy==TRUE)&is.na(data$subsidy_start),
                             ifelse((!is.na(data$subsidy_end)) & (data$Year > data$subsidy_end), data$subsidy_end, data$Year),
                             data$subsidy_start)
  #_______________________________________________________________________________
  
  
  
  
  #EXTRACT ENTRY INTO FORCE DATE FOR OTHER TYPES OF MEASURES:---------------------
  
  #for regulation, taxes measures we only look for the starting year
  
  #extract unique implementation period descriptions (reduces number of iterations and speeds execution)
  text_regulation<-unique(data[(is_regulation==TRUE),]$`Implementation period`)
  text_regulation<-data.table(original=text_regulation,text=text_regulation,regulation_start=NA)
  for (i in 1:nrow(text_regulation)){
    
    #Whenever possible subset string from "Entry into force"
    #text_regulation$text[i]<-gsub(".*([Ee]ntry into [Ff]orce.*)", replacement="\\1", x=text_regulation$text[i], perl = TRUE)
    
    if(is.na(text_regulation$regulation_start[i])){
      #Find smallest year in string
      temp<-str_match_all(text_regulation$original[i],"(?:[[:punct:]]|\\b)(\\d{4})(?:[[:punct:]]|\\b)")[[1]][,2]
      temp<-temp[(temp>=1950)&(temp<=this_year+1)]
      text_regulation$regulation_start[i]<-ifelse(length(temp)>0,
                                                  min(temp),
                                                  NA)
    }
  }
  rm(i,temp)
  text_regulation$text<-NULL
  
  #import results into data table
  data<-left_join(data,text_regulation,by=c("Implementation period"="original"))
  
  #impute notification date as starting date when missing
  data$regulation_start <- suppressWarnings(as.integer(data$regulation_start))
  data$regulation_start<-ifelse((data$is_regulation==TRUE)&is.na(data$regulation_start),
                                           data$Year,
                                           data$regulation_start)
  #_______________________________________________________________________________
  
  
  #COMBINE IMPLEMENTATION INFO ---------------------------------------------------
  
  data$START <- ifelse(is.na(data$subsidy_start), ifelse(is.na(data$regulation_start), data$Year, data$regulation_start), data$subsidy_start)
  data$END <- data$subsidy_end
  
  #check for unreasonable dates
  data$START <- ifelse(data$START<1945,data$Year,data$START)
  data$END <- ifelse(data$END>2100,NA,data$END)
  #_______________________________________________________________________________
  
  
  #OUTPUT DATA
  return(data[ ,c(1:original_number_of_columns, ncol(data)-1, ncol(data)), with=FALSE])
}


extract_countries <- function(data,
                              country_column = "Notifying Member",      #this is the name of the column to filter 
                              reference_list_file = "./programme files/country_reference_list_FULL.csv",     #file path to reference table for country names
                              brexit_year = 2020,         #from this year UK is not listed among countries associated with EU measures 
                              output_name = "WTO_name",   #select naming convention for output: "WTO_name" (WTO members), "ISO.3" (M49 ISO-3 codes), "M49_code" (M49 codes) 
                              keep_unnamed = "no"){       #if "no" the data of countries with no name in output list will be deleted. If "yes" (or any other value) the names will be left unchanged.
  
  
  
  #READ REFERENCES FILE:----------------------------------------------------------
  
  table_references<-read.csv(reference_list_file,header=TRUE,colClasses = "character")
  #_______________________________________________________________________________
  
  
  #FUZZY MATCH COUNTRY COLUMN NAME:----------------------------------------------
  
  if(!(country_column %in% colnames(data))){
    cat(paste("No exact match was found for column:",country_column,"\n"))
    temp<-stringdist(tolower(country_column),tolower(str_trim(colnames(data))))
    country_column<-colnames(data)[match(min(temp),temp)]
    cat(paste("The programme will use the closest match found:", country_column))
  }
  #_______________________________________________________________________________
  
  
  
  #CREATE A CONVERSION TABLE FOR COUNTRIES IN FILE:-------------------------------
  
  #extract list of countries from file
  list_countries <- sort(unique(data[,get(country_column)]))
  
  #create table with unique id
  conversion_table <- data.frame(id=1:length(list_countries), list_countries)
  
  #split up entries with multiple notifying members
  conversion_table <- separate_rows(conversion_table,"list_countries",sep=";")
  
  #create table
  conversion_table$simplified <- str_trim(tolower(conversion_table$list_countries), side = "both")
  conversion_table$conversion <- NA
  
  #clean entries for single EU nations
  conversion_table$simplified <- gsub("european union: (.*)\\b","\\1",conversion_table$simplified, perl = TRUE)
  
  
  #check all countries. If no exact match is found use fuzzy match.
  table_references_lower <- table_references %>% mutate_all(.funs=tolower)
  for (i in 1:nrow(conversion_table)){
    row_number <- which(table_references_lower == conversion_table$simplified[i], arr.ind=TRUE)[1]
    conversion_table$conversion[i] <- ifelse(is.null(row_number)|is.na(row_number), 
                                             table_references_lower$WTO_name[table_references_lower$WTO_name!=""][which.min(stringdist(conversion_table$simplified[i],table_references_lower$WTO_name[table_references_lower$WTO_name!=""]))],
                                             table_references[row_number, output_name]) 
  }
  rm(i, row_number)
  #_______________________________________________________________________________
  
  
  
  
  #LIST UNMATCHED COUNTRIES:------------------------------------------------------
  
  unmatched <- conversion_table$list_countries[is.na(conversion_table$conversion)]
  if (length(unmatched)>0){
    cat(paste("No match was found for the followng", length(unmatched),"countries:\n - "))
    cat(paste(unmatched, collapse = "\n - "))
    cat("\n")
    stop("ADD THESE COUNTRIES TO THE LIST")
  }
  #_______________________________________________________________________________
  
  
  
  
  #DELETE UNNAMED COUNTRIES:------------------------------------------------------
  
  filtered_data <- data
  unnamed <- conversion_table$list_countries[conversion_table$conversion==""]
  if (length(unnamed)>0){
    cat(paste("The following", length(unnamed),"countries were recognised but have no name in the output list:\n - "))
    cat(paste(unnamed, collapse = "\n - "))
    cat("\n\n")
    if (keep_unnamed == "no"){
      print("-> Data for the countries above has been omitted")
      filtered_data <- filtered_data %>% filter(!(get(country_column) %fin% unnamed))
    } else {
      print("-> The name of the countries above will be left unchanged")
      conversion_table <- conversion_table %>% mutate(ifelse(conversion == "", list_countries, conversion))
    }
  }
  #_______________________________________________________________________________
  
  
  #COLLAPSE CONVERSION TABLE:-----------------------------------------------------
  
  #collapse data
  conversion_table <- conversion_table %>%
    group_by(id) %>%
    summarise(list_countries = paste0(list_countries, collapse=";"),
              COUNTRIES = paste0(conversion, collapse=";"), .groups="drop") %>% as.data.frame()
  #_______________________________________________________________________________
  
  
  
  #IMPORT CONVERTED NAMES IN ORIGINAL TABLE:--------------------------------------
  
  temp <- "list_countries"
  names(temp) <- country_column
  
  data <- suppressWarnings(left_join(data, conversion_table[,2:3], by=temp))
  #_______________________________________________________________________________
  
  
  
  #SPLIT EU COUNTRIES:------------------------------------------------------------
  
  EU_countries <- "Austria;Belgium;Bulgaria;Croatia;Cyprus;Czech Republic;Denmark;Finland;France;Germany;Greece;Hungary;Ireland;Italy;Latvia;Lithuania;Luxembourg;Malta;Netherlands;Poland;Portugal;Romania;Slovak Republic;Slovenia;Spain;Sweden;Estonia"
  data$COUNTRIES <- ifelse(data$Year>=2020,
                           gsub("European Union \\(formerly EC\\)", EU_countries, data$COUNTRIES),
                           gsub("European Union \\(formerly EC\\)", paste0(EU_countries,";United Kingdom"), data$COUNTRIES))
  
  
  #_______________________________________________________________________________
  
  
  
  #OUTPUT DATA
  return(data)
}

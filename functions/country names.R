
extract_countries <- function(data,
                              country_column = "Notifying Member",      #this is the name of the column to filter 
                              brexit_year = 2020,         #from this year UK is not listed among countries associated with EU measures 
                              output_name = "WTO_en"){   #select naming convention for output: "WTO_name" (WTO members), "ISO.3" (M49 ISO-3 codes), "M49_code" (M49 codes) 

  
  flog.info("Cleaning country names")

  #FUZZY MATCH COUNTRY COLUMN NAME:----------------------------------------------
  
  if(!(country_column %in% colnames(data))){
    flog.info(paste("No exact match was found for column:",country_column))
    temp<-stringdist(tolower(country_column),tolower(str_trim(colnames(data))))
    country_column<-colnames(data)[match(min(temp),temp)]
    flog.info(paste("The programme will use the closest match found:", country_column))
  }
  #_______________________________________________________________________________
  
  
  
  #CREATE A CONVERSION TABLE FOR COUNTRIES IN FILE:-------------------------------
  
  #extract list of countries from file
  list_countries <- sort(unique(data[,get(country_column)]))
  
  #create table with unique id
  conversion_table <- data.frame(id=1:length(list_countries), list_countries)
  
  #split up entries with multiple notifying members
  conversion_table <- as.data.frame(separate_rows(conversion_table,"list_countries",sep=";"))
  
  #clean entries for single EU nations
  conversion_table$simplified <- sub("[Ee]uropean [Uu]nion: (.*)\\b","\\1", conversion_table$list_countries, perl = TRUE)
  
  # convert to WTO name with fuzzy matching
  conversion_table$conversion <- country_name(conversion_table$simplified, to = output_name, verbose = T)
  
  # delete simplified column
  conversion_table$simplified <- NULL
  #_______________________________________________________________________________
  
  
  
  #COLLAPSE CONVERSION TABLE:-----------------------------------------------------
  
  # remove any country name that is not recognised
  conversion_table <- conversion_table[!(conversion_table$conversion == ""|is.na(conversion_table$conversion)),]
  
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

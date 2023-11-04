
check_colnames <- function(data){
  
  flog.info("Checking EDB columns")
  
  essential_columns <- c("Nr",
                         "Notifying Member",
                         "Year",
                         "Measure description",
                         "Type of measure",
                         "Coverage of the measure",
                         "ICS - HS code",
                         "Implementation period",
                         "Environment-related objective",
                         "Keywords",
                         "Harmonized types of environment-related objectives",
                         "Harmonized types of measures",
                         "Harmonized types of sectors subject to the measure")
  
  #loop over essential column
  for (column in essential_columns){
    
    #check if essential columns are present in the database with the same name
    if(!(column %in% colnames(data))){
      cat(paste("No exact match was found for column:",column,"\n"))
      
      #if not, find the closest match
      temp<-stringdist(tolower(column),tolower(str_trim(colnames(data))))
      
      #if the closest match is too different, then five an error, otherwise adjust column name accordingly
      if (min(temp) > nchar(column)/2){
        stop("No close match could be found, please add it to the dataset or adjust the column names to allow a correct execution of the script")
      } else {
        country_column<-colnames(data)[match(min(temp),temp)]
        cat(paste("Renaming the closest match found:", country_column,"\n-----------------\n"))
      }
    }
    
  }
  
  #check data format
  data$Year <- as.integer(data$Year)
  data$Nr <- as.integer(data$Nr)
  
  return(data)
}
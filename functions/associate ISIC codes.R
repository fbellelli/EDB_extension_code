
associate_ISIC <- function(data){
  
  #here I mapped the correspondence between ISIC codes and EDB sectors
  chp_agriculture<-c("01")
  chp_chemicals<-c("20-21")
  chp_energy<-c("35")
  chp_forestry<-c("02")
  chp_fisheries<-c("03")
  chp_manufacturing<-c("10-19;22-33")
  chp_mining<-c("05-09")
  chp_all<-c("01-99")
  chp_other<-c("36-43")
  chp_services<-c("44-99") 
  
  #match codes to sectors
  data$ISIC <- paste(
    ifelse(grepl("Agriculture",data$`Harmonized types of sectors subject to the measure`,ignore.case=TRUE), chp_agriculture,""),
    ifelse(grepl("Forestry",data$`Harmonized types of sectors subject to the measure`,ignore.case=TRUE), chp_forestry,""),
    ifelse(grepl("Fisheries",data$`Harmonized types of sectors subject to the measure`,ignore.case=TRUE), chp_fisheries,""),
    ifelse(grepl("Mining",data$`Harmonized types of sectors subject to the measure`,ignore.case=TRUE), chp_mining,""),
    ifelse(grepl("Chemicals",data$`Harmonized types of sectors subject to the measure`,ignore.case=TRUE), chp_chemicals,""),
    ifelse(grepl("Manufacturing",data$`Harmonized types of sectors subject to the measure`,ignore.case=TRUE), chp_manufacturing,""),
    ifelse(grepl("Energy",data$`Harmonized types of sectors subject to the measure`,ignore.case=TRUE), chp_energy,""),
    ifelse(grepl("Other",data$`Harmonized types of sectors subject to the measure`,ignore.case=TRUE), chp_other,""),
    ifelse(grepl("services",data$`Harmonized types of sectors subject to the measure`,ignore.case=TRUE), chp_services,""),
    ifelse(grepl("All products/economic activities",data$`Harmonized types of sectors subject to the measure`,ignore.case=TRUE), chp_all,""),
    sep=";")
  
  #clean strings
  for(i in 1:10){
    data$ISIC <- gsub("^;|;$","", data$ISIC, perl=TRUE) 
    data$ISIC <- gsub(";;",";", data$ISIC, perl=TRUE) 
  }
  
  
  return(data)
}

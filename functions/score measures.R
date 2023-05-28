
score_measures <- function(measures_data,
                           measures_keywords,
                           path_sector_data= "./programme files/sector_data_final.csv", #path to data on share of sectors on GDP
                           path_verbs_list = "./programme files/verb_list.csv",   #path to ranked verb list
                           share_of_other= 3){ #Assumed percentage size of the harmonised sector "other" over GDP. Specify a number between 0 and 100.
  
  
  
  #--------------IMPORT DATA------------------------
  #import data
  sector_data<-read.csv(path_sector_data, stringsAsFactors = FALSE)
  verb_data<-read.csv(path_verbs_list,stringsAsFactors = FALSE)
  
  
  
  
  #BREADTH ===========================================================================
  
  cat("Calculating BREADTH components \n")
  
  #1)----------------------ECONOMIC SECTORS ------------------------------------------
  
  #SPLIT EDB DATA BY COUNTRY
  working_data<-measures_data %>% separate_rows(COUNTRIES, sep = ";")
  
  #CLEAN TABLE CONTAINING THE ECONOMIC SHARE OF EACH SECTORS
  #set negative values to 0% and values higher than 100% as NA:
  for (i in 3:ncol(sector_data)){
    sector_data[!is.na(sector_data[i])&sector_data[i]<=0,i]<-NaN
    sector_data[!is.na(sector_data[i])&sector_data[i]>=100,i]<-NaN
  }
  rm(i)
  
  #calculate average share of value added by each sector:
  share_sector_original<-sector_data %>% filter(Years>=2000) %>% group_by(Countries) %>% 
    summarise(agriculture = mean(agriculture_forestry_fishing*3/6, na.rm=TRUE),
              forestry = mean(agriculture_forestry_fishing*1/6, na.rm=TRUE),
              fishing = mean(agriculture_forestry_fishing*2/6, na.rm=TRUE),
              mining = ifelse(mean(industry-manufacturing, na.rm = TRUE)<0,0, mean(industry-manufacturing, na.rm = TRUE)*2/3),
              chemicals = mean(share_chemicals_manufacturing*manufacturing/100, na.rm =TRUE),
              energy = ifelse(mean(industry-manufacturing, na.rm = TRUE)<0,0, mean(industry-manufacturing, na.rm = TRUE)*1/3),
              manufacturing = mean(manufacturing-share_chemicals_manufacturing*manufacturing/100, na.rm=TRUE),
              services = mean(services, na.rm=TRUE),
              all_industry = mean(industry,na.rm =TRUE), .groups="drop")
  
  # whenever possible, fill in missing value by regressing on the other sectors
  temp<-share_sector_original
  fit_agriculture<-glm(agriculture/100 ~ all_industry + services, data = share_sector_original, family = quasibinomial(link=logit))
  fit_services<-glm(services/100 ~ agriculture + all_industry, data = share_sector_original, family = quasibinomial(link=logit))
  fit_manufacturing<-glm(manufacturing/100 ~ agriculture + all_industry + services, data = share_sector_original, family = quasibinomial(link=logit))
  fit_industry<-glm(all_industry/100 ~ agriculture + manufacturing + services, data = share_sector_original, family = quasibinomial(link=logit))
  fit_chemicals<-glm(chemicals/100 ~ agriculture + all_industry + services, data = share_sector_original, family = quasibinomial(link=logit))
  
  #fill in iteratively using the predicted values of missing sectors:
  for (iteration in 1:6){
    temp<-temp %>% mutate(pred = predict.glm(fit_agriculture,.,type = "response")*100,
                          agriculture = ifelse(is.na(agriculture),pred,agriculture))
    temp<-temp %>% mutate(pred = predict.glm(fit_services,.,type = "response")*100,
                          services = ifelse(is.na(services),pred,services))
    temp<-temp %>% mutate(pred = predict.glm(fit_manufacturing,.,type = "response")*100,
                          manufacturing = ifelse(is.na(manufacturing),pred,manufacturing))
    temp<-temp %>% mutate(pred = predict.glm(fit_industry,.,type = "response")*100,
                          all_industry = ifelse(is.na(all_industry),pred,all_industry))
    temp<-temp %>% mutate(pred = predict.glm(fit_chemicals,.,type = "response")*100,
                          chemicals = ifelse(is.na(chemicals),pred,chemicals))
  }
  rm(iteration, fit_agriculture, fit_chemicals, fit_industry, fit_manufacturing, fit_services)
  
  #calculate the remaining sectors based on predicted values:
  temp<- temp %>% mutate(forestry = agriculture/3,
                         fishing = 2*agriculture/3,
                         energy = ifelse(all_industry-manufacturing<0,0, all_industry-manufacturing)*1/3,
                         mining = ifelse(all_industry-manufacturing<0,0, all_industry-manufacturing)*2/3,
                         tot = agriculture + forestry + fishing + mining + energy + services + manufacturing + chemicals)
  
  #re-equilibrate and calculate the "other" harmonised sector as residual
  share_sector_final<- temp %>% mutate(agriculture = (agriculture/tot)*(100-share_of_other),
                                       forestry = (forestry/tot)*(100-share_of_other),
                                       fishing = (fishing/tot)*(100-share_of_other),
                                       chemicals = (chemicals/tot)*(100-share_of_other),
                                       energy = (energy/tot)*(100-share_of_other),
                                       manufacturing = (manufacturing/tot)*(100-share_of_other),
                                       services = (services/tot)*(100-share_of_other),
                                       all_industry = (all_industry/tot)*(100-share_of_other),
                                       other = share_of_other) %>%
    select(Countries,agriculture, forestry, fishing, chemicals, mining, energy, manufacturing, services, other)
  rm(temp)
  
  #assign to Chinese Taipei the same coefficients as South Korea. (Chinese Taipei submitted more than 200 measures, but no sector share data is available - therefore I assume the sectoral shares are the same as South Korea)
  share_sector_final[share_sector_final$Countries=="Chinese Taipei",-1]<-share_sector_final[share_sector_final$Countries=="Korea, Republic of",-1]
  
  #For any other country for which data is missing, use sample average
  share_sector_final[,2:ncol(share_sector_final)] <- sapply(share_sector_final[,2:ncol(share_sector_final)], function(x) as.numeric(ifelse(is.na(x),mean(x,na.rm=TRUE),x)))
  
  
  #FIND THE COUNTRY NAME CORRESPONDING TO EACH ENTRY IN THE EDB
  list_countries<-unique(working_data$COUNTRIES)
  dest_countries<-unique(sector_data$Countries)
  working_data$match_countries<-as.character(NA)
  for (i in 1:length(list_countries)){
    temp<- list_countries[i]
    working_data$match_countries[working_data$COUNTRIES==temp]<-dest_countries[which.min(stringdist(tolower(list_countries[i]),tolower(dest_countries)))][1]
  }
  rm(i,list_countries, dest_countries, temp)
  
  #CALCULATE INDEX AFFECTED SECTORS:
  working_data <- working_data %>% rowwise() %>% mutate(BREADTH_economic_sectors=log(1+ifelse(grepl("All products/economic activities",`Harmonized types of sectors subject to the measure`,ignore.case=TRUE),
                                                                                              100,
                                                                                              share_sector_final$agriculture[match(match_countries,share_sector_final$Countries)]*as.integer(grepl("Agriculture",`Harmonized types of sectors subject to the measure`,ignore.case=TRUE))+
                                                                                                share_sector_final$chemicals[match(match_countries,share_sector_final$Countries)]*as.integer(grepl("Chemicals",`Harmonized types of sectors subject to the measure`,ignore.case=TRUE))+
                                                                                                share_sector_final$energy[match(match_countries,share_sector_final$Countries)]*as.integer(grepl("Energy",`Harmonized types of sectors subject to the measure`,ignore.case=TRUE))+
                                                                                                share_sector_final$forestry[match(match_countries,share_sector_final$Countries)]*as.integer(grepl("Forestry",`Harmonized types of sectors subject to the measure`,ignore.case=TRUE))+
                                                                                                share_sector_final$fishing[match(match_countries,share_sector_final$Countries)]*as.integer(grepl("Fisheries",`Harmonized types of sectors subject to the measure`,ignore.case=TRUE))+
                                                                                                share_sector_final$manufacturing[match(match_countries,share_sector_final$Countries)]*as.integer(grepl("Manufacturing",`Harmonized types of sectors subject to the measure`,ignore.case=TRUE))+
                                                                                                share_sector_final$mining[match(match_countries,share_sector_final$Countries)]*as.integer(grepl("Mining",`Harmonized types of sectors subject to the measure`,ignore.case=TRUE))+
                                                                                                share_sector_final$services[match(match_countries,share_sector_final$Countries)]*as.integer(grepl("Services",`Harmonized types of sectors subject to the measure`,ignore.case=TRUE))+
                                                                                                share_sector_final$other[match(match_countries,share_sector_final$Countries)]*as.integer(grepl("Other",`Harmonized types of sectors subject to the measure`,ignore.case=TRUE))+
                                                                                                0*as.integer(grepl("Not specified",`Harmonized types of sectors subject to the measure`,ignore.case=TRUE))
  ))/log(101))
  
  #For multi-country measures take average
  working_data <- working_data %>%
    group_by(Nr) %>%
    summarise(BREADTH_economic_sectors = round(mean(BREADTH_economic_sectors, na.rm=TRUE),3), .groups="drop")
  
  #import in final data
  measures_data <- left_join(measures_data,working_data, by=c("Nr"="Nr"))
  
  
  
  
  #2)----------------------NUMBER OF ENVIRONMENTAL OBJECTIVES ------------------------------------------
  measures_data <- measures_data %>% mutate(temp = 1+str_count(`Harmonized types of environment-related objectives`,";"))
  max_temp<-max(measures_data$temp)
  measures_data<- measures_data %>% mutate(BREADTH_env_objectives = round(log(1+temp)/log(1+max_temp),3))
  
  measures_data$temp<-NULL
  
  
  #3)----------------------NUMBER OF ENVIRONMENTAL KEYWORDS ------------------------------------------
  measures_data <- measures_data %>% mutate(temp = 1+str_count(Keywords,";"))
  max_temp<-max(measures_data$temp)
  measures_data<- measures_data %>% mutate(BREADTH_env_keywords = round(log(1+temp)/log(1+max_temp),3))
  
  measures_data$temp<-NULL
  
  
  #---------------------- FINAL BREADTH SCORE ------------------------------------------
  measures_data$BREADTH <- round(1.5*measures_data$BREADTH_economic_sectors + 0.75*measures_data$BREADTH_env_objectives + 0.75*measures_data$BREADTH_env_keywords,3)
  
  
  
  
  
  
  
  #DEPTH==========================================================================
  
  cat("Calculating DEPTH components \n")
  
  #1)----------------------WORDING INTENSITY--------------------------------
  
  #clean keywords, transform to lowercase and detect stop words
  measures_keywords$keywords<-tolower(measures_keywords$lemma) 
  stopw<-stopwords("en")
  measures_keywords$stopw<-measures_keywords$keywords %in% stopw
  rm(stopw)
  
  #exclude stop words and keep only verbs
  measures_verbs<-measures_keywords%>%filter(stopw==FALSE, upos %in% c("VERB")) %>% select(measure_nr,keywords)
  
  #calculate word frequencies and import intensity definition from verb list
  temp<-verb_data %>% select(VERBS,Intensity)
  measures_verbs<-left_join(measures_verbs,temp, by=c("keywords"="VERBS"))
  rm(temp)
  
  #calculate wording score
  temp<-measures_verbs %>% group_by(measure_nr,Intensity) %>% summarise(tot = n(), .groups = "drop") %>% ungroup() %>%
    mutate(score_int = log(1+tot)*ifelse(Intensity=="w",1,
                                         ifelse(Intensity=="a",2,
                                                ifelse(Intensity=="s",3,0)))) %>%
    group_by(measure_nr) %>%
    summarise(DEPTH_wording = sum(score_int), .groups="drop")
  max_temp<-max(temp$DEPTH_wording, na.rm = TRUE)
  temp<-temp %>% mutate(DEPTH_wording = ifelse(is.na(DEPTH_wording),0,DEPTH_wording))
  temp$DEPTH_wording<-log(1+temp$DEPTH_wording)/log(1+max_temp)
  
  #create a reference table for verbs in measures
  measures_verbs<- measures_verbs %>% group_by(measure_nr,keywords) %>% summarise(freq = n(), .groups = "drop")                                    
  
  #import wording score into working table
  measures_data<-left_join(measures_data,temp, by=c("Nr"="measure_nr"))
  measures_data<-measures_data %>% mutate(DEPTH_wording = ifelse(is.na(DEPTH_wording),0,DEPTH_wording))
  measures_data$DEPTH_wording <- round(measures_data$DEPTH_wording,3)
  rm(temp)
  
  
  #2)----------------------NUMBER OF MEASURE TYPES--------------------------------
  measures_data <- measures_data %>% mutate(temp = 1+str_count(`Harmonized types of measures`,";"))
  max_temp<-max(measures_data$temp)
  measures_data<- measures_data %>% mutate(DEPTH_measures_variety = round(log(1+temp)/log(1+max_temp),3))
  
  measures_data$temp<-NULL
  
  
  #3)----------------------POLICY TOOLS--------------------------------
  #IDENTIFY MEASURES CONTAINING SUBSIDIES / STANDARD AND REGULATIONS or BOTH
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
  
  measures_data$measure_tool_subsidy<-NA
  measures_data$measure_tool_sandard_regulation<-NA
  for (i in 1:nrow(measures_data)){
    measures_data$measure_tool_subsidy[i]<-any(subsidies %fin% str_split(measures_data$`Harmonized types of measures`[i],";")[[1]])
    measures_data$measure_tool_sandard_regulation[i]<-any(regulations_standards %fin% str_split(measures_data$`Harmonized types of measures`[i],";")[[1]])
  }
  
  #score based on ranking of type of harmonised measures.
  tools_group1<-c("Ban/Prohibition","Internal taxes","Grants and direct payments","Income or price support")
  tools_group2<-c("Import quotas","Export quotas","Export tariffs","Non-monetary support","Tax concessions","Loans and financing","Import tariffs","Other price and market based measures","Public procurement")
  tools_group3<-c("Export licences","Import licences","Technical regulation or specifications","Conformity assessment procedures","Regulation affecting movement or transit","Environmental provisions in trade agreements","Other environmental requirements","Quarantine requirements","Services requirements","Other support measures")
  tools_group4<-c("Investment measures","Intellectual property measures","Risk assessment","Countervailing measure / investigation","Safeguard measure / investigation","Anti-dumping measure / investigation","Other measures")  
  tools_group5<-c("Not specified")
  
  #calculate component score
  measures_data$DEPTH_measures_instrument <- as.numeric()
  for (i in 1:nrow(measures_data)){
    temp<-str_split(measures_data$`Harmonized types of measures`[i],";")
    measures_data$DEPTH_measures_instrument[i]<-ifelse(any(tools_group1 %fin% temp[[1]]),1,
                                                       ifelse(any(tools_group2 %fin% temp[[1]]),0.75,
                                                              ifelse(any(tools_group3 %fin% temp[[1]]),0.5,
                                                                     ifelse(any(tools_group4 %fin% temp[[1]]),0.25,
                                                                            ifelse(any(tools_group5 %fin% temp[[1]]),0,NA)))))
  }
  measures_data$measure_tool_subsidy <- NULL
  measures_data$measure_tool_sandard_regulation <- NULL
  
  
  #---------------------- FINAL DEPTH SCORE ------------------------------------------
  measures_data$DEPTH<- round(measures_data$DEPTH_wording + measures_data$DEPTH_measures_variety + measures_data$DEPTH_measures_instrument,3)
  
  
  
  
  #FINAL MEASURE STRENGTH INDEX==========================================================================
  measures_data$MEASURE_SCORE<-round(measures_data$DEPTH*measures_data$BREADTH, 3)
  
  
  
  return(measures_data)
}


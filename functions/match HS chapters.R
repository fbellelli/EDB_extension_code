#function to extract mode, this will be used to summarise information
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}




match_HS_chapters <- function(measures_data,
                              measures_keywords,
                              udpipe_model_path = "programme files/english-ewt-ud-2.4-190531.udpipe",   #path to the model containing ML model for lemmatisation and annotation of keywords
                              path_HS = "programme files/HS_cleaned_description.csv",     #path to the file containing HS code descriptions
                              path_ICS_conversion_table = "programme files/ICS_HS_conc.csv",   #path to conversion table HS-ICS codes (Razi's table)
                              path_OECD_env_goods = "programme files/table_OECD_env_goals_chapters.csv",   #path to the correspondence table between harmonised objectives and environmental chapters (based on OECD environmental goods list)
                              keyword_threshold = 20, #size-reduction parameters (see methodology note for details) - all keywords that are found in more than this specified number of chapters will be ignored for matching
                              cut_off_absolute = 0.25, #size-reduction parameters (see methodology note for details) -  between 0 and 1: percentile of the absolute link strength (L-tilde) distribution which is set as cut-off value. For instance, 0.1 means that the weakest 10% of the links are eliminated. 
                              cut_off_relative = 0.8, #size-reduction parameters (see methodology note for details) - between 0 and 1: FOR EACH MEASURE, only the links with relative strength (L-bar) above this value are retained. 
                              weight_sector_yes = 1 ,  #Step 3 weights (see methodology note) - weight to be applied to sectors which match with the HS code. (Notice that measures labelled as "Service" are applied a weight of 0, no matter what)
                              weight_sector_no = 0.5,  #Step 3 weights (see methodology note) - weight to be applied to sectors which DO NOT match with the HS code.
                              weight_env_obj_yes = 1,  #Step 3 weights (see methodology note) - weight applied to links with consistent environmental objective
                              weight_env_obj_no = 0.9){  
  
  flog.info("Matching HS codes to measures")
  
  # MAPPING HS-SECTORS (redefine as appropriate) --------------------
  
  #here I mapped the correspondence between HS chapters and harmonised sectors as outlined in table 1 of the methodology note. Adjust as desired.
  chp_agriculture<-c(6:14)
  chp_chemicals<-c(28:40)
  chp_energy<-c(84:85)
  chp_forestry<-c(44:48)
  chp_fisheries<-c(3)
  chp_manufacturing<-c(15:24, 50:70, 84:96)
  chp_mining<-c(25:27, 71:83)
  chp_all<-c(1:97)
  chp_notspecified<-c(0)
  chp_other<-c(1:2, 4:5, 41:43, 49, 97:99)
  chp_services<-c(0) #no chapter because HS refers to goods
  
  
  
  
  #--------------IMPORT DATA------------------------
  #import data
  HS_data<-read.csv(path_HS)
  OECD_env_goods_matches<-read.csv(path_OECD_env_goods,stringsAsFactors = FALSE)
  conversion_table<-read.csv(path_ICS_conversion_table,colClasses = "character")
  
  #adjusting format of variables
  HS_data$HS_code<-as.character(HS_data$HS_code)
  HS_data$Description<-as.character(HS_data$Description)
  
  
  
  #--------------EXTRACT KEYWORDS & ANNOTATE------------------------
  
  if (file.exists("cache/HS_keywords.csv")){
    HS_keywords <- read.csv("cache/HS_keywords.csv")
  } else {
    
    #load language model from udpipe
    ud_model <- udpipe_load_model(udpipe_model_path)
    
    #tokenise and annotate words in HS code description (!this step takes time!)
    flog.info("extracting, annotating and cleaning keywords of HS code descriptions")
    HS_data$Description <- enc2utf8(HS_data$Description)
    Encoding(HS_data$Description) <- "UTF-8"
    HS_data$Description <- iconv(HS_data$Description, "UTF-8", "UTF-8",sub='') 
    
    HS_keywords <- udpipe_annotate(ud_model, HS_data$Description)
    HS_keywords <- as.data.frame(HS_keywords)
    
    #create variable with row number of original HS table 
    HS_keywords$HS_entry_ref <- as.integer(substr(HS_keywords$doc_id,4,10))
    
    # keep only necessary columns to save memory
    HS_keywords <- HS_keywords[,c("doc_id", "lemma", "upos", "HS_entry_ref")]
    
    # cache extracted data for future runs
    fwrite(HS_keywords, file = "cache/HS_keywords.csv")
  }
  
  #merge data information in keywords table:
  HS_keywords<-cbind(HS_keywords, HS_data[HS_keywords$HS_entry_ref,])
  measures_keywords<- left_join(measures_keywords, measures_data, by=c("measure_nr"="Nr"))
  
  
  
  
  #--------------CLEAN KEYWORDS------------------------
  #take lemmas and transform to lowercase
  HS_keywords$keywords<-tolower(HS_keywords$lemma)
  measures_keywords$keywords<-tolower(measures_keywords$lemma) 
  
  #exclude stopwords and keep only nouns, verbs, proper nouns and adjectives
  stopw<-stopwords("en")
  stopw<-c(stopw,"trade","certain","new","state","protection","provide","chapter","system","person","better","method","individual","group","particular","level","programme","protect","project","public","small","low","medium","large","high","result","commercial","producer","condition","technical","order","control","reduce","regulate","development","implement","specify","support","include","define","business","standard","facility","develop","value","year","specific","operation","issue","register","property","plan","framework","objective","area","number","apply","current","aid","payment","intend","act","address","draft","basic","maximum","active","country")
  HS_keywords$stopw<-HS_keywords$keywords %in% stopw
  measures_keywords$stopw<-measures_keywords$keywords %in% stopw
  HS_keywords<-HS_keywords%>%filter(stopw==FALSE, upos %in% c("NOUN","PROPN","VERB","ADJ"), is_section!=1)
  measures_keywords<-measures_keywords%>%filter(stopw==FALSE, upos %in% c("NOUN","PROPN","VERB","ADJ"))
  rm(stopw)
  
  
  
  #--------------CALCULATE FREQUENCIES AND TD-IDF WEIGHTS OF HS KEYWORDS (omega)------------------------
  
  flog.info("Calculate weights (omega) of keywords")
  
  # count in how many chapters keywords appear
  keywords_chp_tally <-as.data.table(HS_keywords)[,.(key_in_n_chapters = length(unique(chapter))), by = "keywords"]
  HS_keywords  <- left_join(HS_keywords, keywords_chp_tally, by = "keywords")
  rm(keywords_chp_tally)
  
  #Calculating omega  
  tot_chapters<-length(unique(HS_keywords$chapter))
  HS_keywords$key_omega <- ifelse(1+log((1+tot_chapters)/(1+HS_keywords$key_in_n_chapters)) >= 1+log((1+tot_chapters)/(1+keyword_threshold)),
                                  1+log((1+min(tot_chapters, keyword_threshold))/(1+HS_keywords$key_in_n_chapters)),
                                  0)

  #adjust importance of some frequent keywords for specific chapters
  HS_keywords$key_omega <- ifelse((HS_keywords$chapter %in% c(84))&(HS_keywords$lemma %in% c("water","agricultural")),
                                  HS_keywords$key_omega * 0.2, HS_keywords$key_omega)
  HS_keywords$key_omega <- ifelse((HS_keywords$chapter %in% c(84))&(HS_keywords$lemma %in% c("gas","air")),
                                  HS_keywords$key_omega * 0.3, HS_keywords$key_omega)
  HS_keywords$key_omega <- ifelse((HS_keywords$chapter %in% c(3,69,7))&(HS_keywords$lemma %in% c("water")),
                                  HS_keywords$key_omega * 0.3, HS_keywords$key_omega)
  HS_keywords$key_omega <- ifelse((HS_keywords$chapter %in% c(7,85))&(HS_keywords$lemma %in% c("gas")),
                                  HS_keywords$key_omega * 0.3, HS_keywords$key_omega)
  HS_keywords$key_omega <- ifelse((HS_keywords$chapter %in% c(87))&(HS_keywords$lemma %in% c("special","design","agricultural")),
                                  HS_keywords$key_omega * 0.5, HS_keywords$key_omega)
  HS_keywords$key_omega <- ifelse((HS_keywords$chapter %in% c(85))&(HS_keywords$lemma %in% c("oil")),
                                  HS_keywords$key_omega * 0.3, HS_keywords$key_omega)
  HS_keywords$key_omega <- ifelse((HS_keywords$chapter %in% c(84))&(HS_keywords$lemma %in% c("plant")),
                                  HS_keywords$key_omega * 0.3, HS_keywords$key_omega)
  HS_keywords$key_omega <- ifelse((HS_keywords$chapter %in% c(90))&(HS_keywords$lemma %in% c("production")),
                                  HS_keywords$key_omega * 0.3, HS_keywords$key_omega)
  HS_keywords$key_omega <- ifelse((HS_keywords$chapter %in% c(70))&(HS_keywords$lemma %in% c("safety")),
                                  HS_keywords$key_omega * 0.5, HS_keywords$key_omega)
  HS_keywords$key_omega <- ifelse((HS_keywords$chapter %in% c(3))&(HS_keywords$lemma %in% c("consumption")),
                                  HS_keywords$key_omega * 0.5, HS_keywords$key_omega)
  
  
  
  #omit HS keywords that fall above the threshold
  HS_keywords <- HS_keywords[HS_keywords$key_omega!=0,]
  
  #calculate count, frequency and weighted count for each HS keywords
  HS_keywords <- HS_keywords %>%
    group_by(chapter,keywords)%>%
    mutate(n_HS = n()) %>% 
    ungroup() %>%
    group_by(chapter) %>%
    mutate(freq_HS = n_HS / n(),
           adj_n_HS = n_HS*key_omega)
  
  
  
  #--------------ESTABLISH KEYWORD MATCHES------------------------
  
  flog.info("Linking measures to HS chapters")
  
  #preparing index for looping over measures
  list_measures<-unique(measures_keywords$measure_nr)
  
  #initialise empty data frame to save results
  match_table<-data.frame()
  
  #establish keyword matches (this step takes long)
  for (measure in list_measures){
    temp_measures<-measures_keywords[measures_keywords$measure_nr==measure]
    
    #MEASURES: get the frequency of each unique keyword in the measure description
    temp_freq<-txt_freq(temp_measures$keywords)
    colnames(temp_freq)<-c("keywords","n_measures","freq_measures")
    temp_sector<-measures_data$`Harmonized types of sectors subject to the measure`[measure]
    temp_env_obj<-measures_data$`Harmonized types of environment-related objectives`[measure]
    temp_freq<-temp_freq %>% mutate(freq_measures = freq_measures/100, 
                                    measure_nr = measure,
                                    harmonised_sector = temp_sector,
                                    harmonised_env_objective = temp_env_obj)
    
    #get a table for of all keywords match in the HS descriptions
    temp_match<-HS_keywords[HS_keywords$keywords %fin% temp_freq$key,]
    
    #HS: get the frequency of each unique keyword in the HS description by HS 2-digits category
    temp_match_freq <- temp_match %>%
      group_by(chapter,keywords) %>%
      summarise(n_HS = getmode(n_HS), 
                omega = getmode(key_omega),
                freq_HS = getmode(freq_HS),
                adj_n_HS = getmode(adj_n_HS), .groups="drop") #notice that all the values are identical, the mode is only used to reduce a vector to a single value
    
    #create a list of matches in a unique table
    temp_match_table<-inner_join(temp_freq,temp_match_freq, by="keywords")
    
    #compile match table
    match_table<-bind_rows(match_table, temp_match_table)
  }
  rm(measure,temp_measures,temp_freq,temp_match_freq, list_measures,temp_match,temp_match_table,tot_chapters,temp_sector,temp_env_obj)
  
  
  #--------------CALCULATE ABSOLUTE LINK STRENGTH------------------------
  
  #reorder match table
  match_table<-match_table %>% 
    select(measure_nr,harmonised_sector, harmonised_env_objective, chapter,keywords,n_measures,n_HS,freq_measures,freq_HS,omega) %>%
    arrange(measure_nr,chapter)
  
  #calculate link strength: L, L-tilde
  link_table<-match_table %>% 
    mutate(word_L= n_measures * n_HS * omega , 
           word_W = case_when(grepl("Agriculture",harmonised_sector,ignore.case=TRUE) & (chapter %in% chp_agriculture) ~ weight_sector_yes,#the following lines check that the chapter number is consistent with the harmonised sector of the measure.
                              grepl("Chemicals",harmonised_sector,ignore.case=TRUE) & (chapter %in% chp_chemicals) ~ weight_sector_yes,
                              grepl("Energy",harmonised_sector,ignore.case=TRUE) & (chapter %in% chp_energy) ~ weight_sector_yes,
                              grepl("Forestry",harmonised_sector,ignore.case=TRUE) & (chapter %in% chp_forestry) ~ weight_sector_yes,
                              grepl("Fisheries",harmonised_sector,ignore.case=TRUE) & (chapter %in% chp_fisheries) ~ weight_sector_yes,
                              grepl("Manufacturing",harmonised_sector,ignore.case=TRUE) & (chapter %in% chp_manufacturing) ~ weight_sector_yes,
                              grepl("Mining",harmonised_sector,ignore.case=TRUE) & (chapter %in% chp_mining) ~ weight_sector_yes,
                              grepl("All products/economic activities",harmonised_sector,ignore.case=TRUE) & (chapter %in% chp_all) ~ weight_sector_yes,
                              grepl("Not specified",harmonised_sector,ignore.case=TRUE) & (chapter %in% chp_notspecified) ~ weight_sector_yes ,
                              grepl("Other",harmonised_sector,ignore.case=TRUE) & (chapter %in% chp_other) ~ weight_sector_yes ,
                              grepl("services",harmonised_sector,ignore.case=TRUE) ~ 0 ,
                              TRUE ~ weight_sector_no)*ifelse(is.na(match(chapter,OECD_env_goods_matches$chapters,nomatch = NA)), #is the measure linked to a chapter that has environmental goods (OECD list)?
                                                              weight_env_obj_no, #if not, apply this weight
                                                              ifelse(grepl(paste(str_split(harmonised_env_objective,";")[[1]], collapse = "|"), # if yes, look at the harmonised environmental objectives of the measure ...
                                                                           OECD_env_goods_matches[match(chapter,OECD_env_goods_matches$chapters,nomatch = NA), "env_obj"],ignore.case = TRUE), # ... and compare them with the environemntal objectives addressed by the environmental good of this chapter
                                                                     weight_env_obj_yes,#if the objective of the measure matches with the objective of the env.goods to which the measure was link, then apply this weight
                                                                     weight_env_obj_no)#otherwise apply this weight
                              ),
           word_LW = word_L * word_W) %>%
    group_by(measure_nr,chapter) %>%
    summarise(L = sum(word_L),
              L_tilde = sum(word_LW), .groups = "drop") 
  
  #penalise chapters most easily matched
  link_table$L_tilde <- ifelse(link_table$chapter==84, link_table$L_tilde*0.4, link_table$L_tilde)
  link_table$L_tilde <- ifelse(link_table$chapter %in% c(85,70), link_table$L_tilde*0.55, link_table$L_tilde)
  link_table$L_tilde <- ifelse(link_table$chapter %in% c(90,3,69,7,87), link_table$L_tilde*0.7, link_table$L_tilde)
  
  
  
  #-------------CHECK HS/ICS CODES REPORTED BY MEMBERS---------------------
  
  
  flog.info("Checking and converting HS/ICS codes reported by Members")
  
  #import ICS and HS into the link table 
  working_data<-left_join(link_table,measures_data[,c("Nr","ICS - HS code")], by= c("measure_nr"="Nr")) 
  working_data <- measures_data[,c("Nr","ICS - HS code")]
  
  #count number of codes provided
  working_data <- working_data %>% mutate(nr_codes_provided = ifelse(`ICS - HS code`=="",0,1+str_count(`ICS - HS code`,";")))
  
  #split line for each HS/ICS code provided
  working_data <- working_data %>% separate_rows(`ICS - HS code`,sep=";") %>% rbind(.,working_data[working_data$nr_codes_provided==0,])
  
  #Identify HS codes, ICS codes and ambiguous codes
  working_data<-working_data %>% ungroup() %>%
    mutate(is_HS=ifelse(nr_codes_provided>1,
                        grepl(pattern="^(\\d\\d\\.?){2,6}$|^\\d\\.(\\d\\d\\.?){1,5}$|^\\d{3,4}\\..*$|^\\d{3,4}$", x=`ICS - HS code` , perl=TRUE),
                        grepl(pattern="^\\d?\\d\\.\\d{4}.*$|^\\d{3,4}\\..*$|^\\d{3,4}|^\\d?\\d\\.\\d{2}\\..*$", x=`ICS - HS code`, perl = TRUE)), #|^\\d{1,2}\\.\\d{4}$
           is_ICS=ifelse(nr_codes_provided>1,
                         grepl(pattern="^(\\d?\\d)\\.\\d{3}(\\..*)?$|^\\d{5}(\\d\\d){0,2}$|(\\d?\\d)\\.\\d{5}$",x = `ICS - HS code`, perl=TRUE),
                         grepl(pattern="^(\\d?\\d)\\.\\d{3}\\..*?$",x = `ICS - HS code`, perl=TRUE)),                      
           is_ambiguous=ifelse((is_HS==FALSE)&(is_ICS==FALSE)&(`ICS - HS code`!=""),TRUE,FALSE))
  
  #convert ICS codes using the conversion table
  check_ICS<-unique(working_data$`ICS - HS code`[working_data$is_ICS]) #extract all unique codes to minimise the number of operations
  check_ICS<-unique(gsub(pattern="^(\\d\\d)\\.?(\\d\\d\\d).*$",replacement= "\\1.\\2" , x=check_ICS, perl=TRUE)) #standardise format of ICS codes (the table is at the ICS 5 digits level)
  
  converted_table<-data.frame(check_ICS=check_ICS)
  converted_table<-left_join(converted_table,conversion_table, by=c("check_ICS"="ICS_code"))
  converted_table <-converted_table %>%
    mutate(HS_chp = str_replace(HS_code, "^(\\d\\d)\\d\\d$","\\1"))%>%
    group_by(check_ICS) %>% 
    summarise(HS_conversion = paste0(unique(HS_code), collapse = ";"),
              possible_chp = paste0(unique(HS_chp), collapse = ";"), .groups="drop")
  converted_table$HS_conversion[converted_table$HS_conversion=="NA"] <- NA
  converted_table$possible_chp[converted_table$possible_chp=="NA"] <- NA
  
  working_data$HS_conversion<-NA
  working_data$possible_chp<-NA
  for (i in 1:nrow(converted_table)){
    working_data$HS_conversion[working_data$`ICS - HS code`==converted_table$check_ICS[i]]<-converted_table$HS_conversion[i]
    working_data$possible_chp[working_data$`ICS - HS code`==converted_table$check_ICS[i]]<-converted_table$possible_chp[i]
  }
  rm(i, check_ICS)
  fc()
  
  #evaluate the possible matches for ambiguous codes
  check_ambiguous<-unique(working_data$`ICS - HS code`[working_data$is_ambiguous==TRUE])
  ambiguous_table<-data.frame(check_ambiguous=check_ambiguous)
  ambiguous_table<-ambiguous_table %>% mutate(match_as_HS = ifelse(grepl("^\\d?\\d\\.?$",check_ambiguous,perl=TRUE), str_pad(str_replace(check_ambiguous,"^(\\d?\\d)\\.?$","\\1"), width = 2, pad="0"),
                                                                   ifelse(grepl("^(\\d?\\d)\\.(\\d\\d)$",check_ambiguous,perl=TRUE), str_pad(str_replace(check_ambiguous,"^(\\d?\\d)\\.(\\d\\d)$","\\1\\2"), width=4, pad ="0"),
                                                                          ifelse(grepl("^(\\d?\\d)\\.(\\d)$",check_ambiguous,perl=TRUE), str_pad(str_replace(check_ambiguous,"^(\\d?\\d)\\.(\\d)$","\\1\\20"), width = 4, pad ="0"),
                                                                                 ifelse(grepl("^(\\d\\d)\\.(\\d{3})$",check_ambiguous,perl=TRUE), str_replace(check_ambiguous,"^(\\d\\d)\\.(\\d{3})$","\\1\\20"),NA)
                                                                          ))),
                                              match_as_ICS = ifelse(grepl("^\\d?\\d\\.?$",check_ambiguous,perl=TRUE), str_pad(str_replace(check_ambiguous,"^(\\d?\\d)\\.?$","\\1"), width = 2, pad="0"),
                                                                    ifelse(grepl("^(\\d?\\d)\\.(\\d\\d)$",check_ambiguous,perl=TRUE), str_pad(str_replace(check_ambiguous,"^(\\d?\\d)\\.(\\d\\d)$","\\1.\\20"), width=6, pad ="0"),
                                                                           ifelse(grepl("^(\\d?\\d)\\.(\\d)$",check_ambiguous,perl=TRUE), str_pad(str_replace(check_ambiguous,"^(\\d?\\d)\\.(\\d)$","\\1.\\200"), width = 6, pad ="0"),
                                                                                  ifelse(grepl("^(\\d\\d)\\.(\\d{3})$",check_ambiguous,perl=TRUE), str_replace(check_ambiguous,"^(\\d\\d\\.\\d{3})$","\\1"),NA)
                                                                           ))))
  #create a conversion table with 2 digits ICS
  conversion_table_2digits<-conversion_table %>% mutate(ICS_2digits = str_replace(pattern = "^(\\d\\d)\\.(\\d\\d\\d)$", replacement = "\\1", string=ICS_code),
                                                        HS_2digits = str_replace(pattern = "^(\\d\\d)(\\d\\d)$", replacement = "\\1", string = HS_code)) %>%
    group_by(ICS_2digits) %>% summarise(linked_HS_chapters=paste0(unique(HS_2digits), collapse = ";"), .groups="drop")
  
  temp<-conversion_table %>% mutate(HS_chp=str_replace(HS_code, "^(\\d\\d)\\d\\d$","\\1")) %>%
    group_by(ICS_code) %>% summarise(HS_code = paste0(unique(HS_code), collapse = ";"),
                                     possible_chp = paste0(unique(HS_chp), collapse = ";"), .groups="drop")
  conversion_table_2digits<-conversion_table_2digits %>% mutate(possible_chp = linked_HS_chapters)
  colnames(conversion_table_2digits)<-colnames(temp)
  conversion_table_2digits<-rbind(conversion_table_2digits,temp)
  rm(temp)
  
  #convert ambiguous ICS codes to HS and make the list of possible HS chapters
  ambiguous_table<-left_join(ambiguous_table,conversion_table_2digits, by= c("match_as_ICS"="ICS_code"))
  colnames(ambiguous_table)[4]<-"converted_HS"
  ambiguous_table<-ambiguous_table %>% mutate(converted_HS = ifelse(is.na(converted_HS),paste0(1:99,collapse=";"),converted_HS))
  ambiguous_table<-ambiguous_table %>% mutate(possible_chp= ifelse(is.na(possible_chp),str_replace(match_as_HS,"^(\\d\\d).*$","\\1"),paste(str_replace(match_as_HS,"^(\\d\\d).*$","\\1"),possible_chp,sep=";")),
                                              all_potential_HS=ifelse(is.na(converted_HS),match_as_HS,paste(match_as_HS,converted_HS,sep=";")))
  
  #pull chapter info into the working table
  for (i in 1:nrow(ambiguous_table)){
    working_data$possible_chp[working_data$`ICS - HS code`==ambiguous_table$check_ambiguous[i]]<-ambiguous_table$possible_chp[i]
  }
  rm(i, check_ambiguous)
  
  #update the list of possible chapters with HS codes
  working_data<-working_data %>% ungroup() %>% mutate(reported_HS_chp = ifelse(is_HS==TRUE,ifelse(grepl("^(\\d)(\\d\\d)?\\..*$",`ICS - HS code`,perl = TRUE),str_replace(`ICS - HS code`,"^(\\d)(\\d\\d)?\\..*$","\\1"),
                                                                                                  ifelse(grepl("^(\\d)(\\d\\d)$",`ICS - HS code`,perl = TRUE),str_replace(`ICS - HS code`,"^(\\d)\\d\\d$","\\1"),
                                                                                                         ifelse(grepl("^(\\d\\d).*$",`ICS - HS code`,perl = TRUE),str_replace(`ICS - HS code`,"^(\\d\\d).*$","\\1"),NA))),
                                                                               NA))
  working_data$reported_HS_chp<-suppressWarnings(as.numeric(formatC(as.numeric(working_data$reported_HS_chp), width = 2, flag= "0")))
  working_data<-working_data %>% mutate(possible_chp = ifelse(is_HS==TRUE,reported_HS_chp,possible_chp),
                                        reported_HS = ifelse(is_HS==TRUE,`ICS - HS code`,NA),
                                        reported_ICS = ifelse(is_ICS==TRUE, `ICS - HS code`,NA),
                                        reported_ambiguous = ifelse(is_ambiguous == TRUE, `ICS - HS code`, NA))                                                                   
  
  #allow any chapter for measures with no reported HS/ICS code
  working_data$possible_chp[is.na(working_data$possible_chp)] <- paste0(formatC(1:99, width = 2, flag= "0"),collapse=";") 
  
  #import potential chapters info in links table
  temp <- working_data[,c("Nr","possible_chp")] %>% group_by(Nr) %>% summarise(possible_chp = paste0(possible_chp,collapse=";"), .groups="drop")
  link_table <- left_join(link_table,temp, by=c("measure_nr"="Nr"))
  
  #remove non consistent links from links table
  link_table$filter_in <- NA
  for (i in 1:nrow(link_table)){
    link_table$filter_in[i] <- as.numeric(link_table$chapter[i]) %in% as.numeric(str_split(string = link_table$possible_chp[i], pattern = ";")[[1]]) 
  }
  final_table <- link_table[link_table$filter_in,c("measure_nr","chapter","L_tilde")]
  
  #save data on HS/ICS code identification
  info_reported_codes <-working_data %>% 
    group_by(Nr) %>% 
    summarise(`Reported HS codes` = paste0(na.exclude(reported_HS),collapse = ";"),
              `Reported ICS codes` = paste0(na.exclude(reported_ICS),collapse = ";"),
              `Reported ambiguous codes` = paste0(na.exclude(reported_ambiguous),collapse = ";"), .groups = "drop")
  
  
  #--------------APPLY CUT-OFF VALUES------------------------
  
  flog.info("Reducing number of links by applying cut-offs")
  
  #calculate relative link strength (L-bar)
  temp <- final_table %>% group_by(measure_nr) %>% summarise(tot_L_tilde = sum(L_tilde), .groups="drop")
  final_table <- left_join(final_table,temp, by=c("measure_nr"))
  final_table$L_bar <- final_table$L_tilde / final_table$tot_L_tilde
  final_table$tot_L_tilde <- NULL
  
  #apply cut-off on absolute link strength (L-tilde)
  temp<-quantile(link_table$L_tilde, probs=cut_off_absolute, names= FALSE)
  final_table<- final_table %>% filter(L_tilde >= temp)
  
  #apply cut-off on relative link strength (L-bar)
  final_table <- final_table %>% filter(L_bar >= cut_off_relative)
  
  #round linkage scores
  final_table$L_tilde <- round(final_table$L_tilde, 0)
  final_table$L_bar <- round(final_table$L_bar, 3)
  
  #rename columns
  final_table <- final_table %>% rename("Tentative HS chapters match" = "chapter",
                                        "Absolute link strength (L_tilde)" = "L_tilde",
                                        "Relative link strength (L_bar)" = "L_bar")
  
  
  return(list(HS_matches = final_table, info_reported_codes = info_reported_codes))
}
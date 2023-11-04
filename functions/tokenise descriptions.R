

tokenise_description <- function(measures_data,
                                 cache = TRUE, # Logical indicating wheter to use cached data
                                 udpipe_model_path = "programme files/english-ewt-ud-2.4-190531.udpipe"){ #path to the ML model
  
  flog.info("Tokenising measure descriptions")
  
  if (cache & file.exists("cache/measures_keywords.csv")){
    
    # loading cached data
    flog.info("Loading cached token descriptions")
    cached_data <- fread("cache/measures_keywords.csv")
    
    # make a list of cached measure ids
    cached_measures <- unique(cached_data$measure_nr)
    
    # remove measures for which we already have data
    measures_data <- measures_data[!Nr %fin% cached_measures, ]
    
  } else {
    cached_data <- NULL
  }
  
  if (nrow(measures_data)>0){
    
    #load language model from udpipe
    ud_model <- udpipe_load_model(udpipe_model_path)
    
    #change string encoding
    measures_data$`Coverage of the measure` <- enc2utf8(measures_data$`Coverage of the measure`)
    measures_data$`Environment-related objective` <- enc2utf8(  measures_data$`Environment-related objective`)
    measures_data$`Measure description` <- enc2utf8(  measures_data$`Measure description`)
    
    #tokenise and annotate words in description (!this step takes long!)
    flog.info("Extracting and annotating keywords from - Coverage of the measure")
    measures_keywords1 <- udpipe_annotate(ud_model,measures_data$`Coverage of the measure`) #words in coverage of the measure
    flog.info("Extracting and annotating keywords from - Environment-related objectives \n")
    measures_keywords2 <- udpipe_annotate(ud_model,measures_data$`Environment-related objective`) #words in environment-related objective
    flog.info("Extracting and annotating keywords from - Measure description \n")
    measures_keywords3 <- udpipe_annotate(ud_model,measures_data$`Measure description`)#words in keyword description
    
    #add a column providing info on source of text
    measures_keywords1 <- as.data.frame(measures_keywords1)
    measures_keywords1$source_var <- "Coverage of the measure"
    measures_keywords2 <- as.data.frame(measures_keywords2)
    measures_keywords2$source_var <- "Environment-related objective"
    measures_keywords3 <- as.data.frame(measures_keywords3)
    measures_keywords3$source_var <- "Measure description"
    
    #merge all keywords in a unique table
    measures_keywords <- rbind(measures_keywords1, measures_keywords2, measures_keywords3)
    
    #create variable containing measure number in original table
    measures_keywords$measures_entry_ref <- as.integer(substr(measures_keywords$doc_id,4,10))
    measures_keywords$measure_nr<-measures_data$Nr[measures_keywords$measures_entry_ref]
    measures_keywords$measures_entry_ref<-NULL
    
    # keep only necessary columns to save memory
    measures_keywords <- measures_keywords[,c("lemma", "upos", "source_var", "measure_nr")]
    
  } else {
    flog.info("No new measure from which to extract keywords")
    measures_keywords <- NULL
  }
  
  # join cached and newly extracted data for output
  measures_keywords <- rbind(measures_keywords, cached_data)
  
  # cache extracted data for future runs
  fwrite(measures_keywords, file = "cache/measures_keywords.csv")
  
  return(measures_keywords)
}

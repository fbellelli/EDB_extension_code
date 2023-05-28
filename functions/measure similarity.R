measure_similarity <- function(data = data,
                               measures_keywords = measures_keywords,
                               cut_off=0.5, #Cut-off value for the similarity index (a value between 0 and 1)
                               match_country = FALSE){ #Should similarity be checked only for measures sharing at least one country in common? (execution time does not significantly change if set to FALSE)
  
  
  #COMPUTE SIMILARITY MATRIX ----------------------------------------------------------------
  
  #create tables with unique index for the measures and keywords (these will correspond to the measure-keyword matrix indices)
  table_id_measures <- data.frame(measure_nr=unique(measures_keywords$measure_nr))
  table_id_measures$id_measure <- 1:nrow(table_id_measures)
  table_id_keywords <- data.frame(keyword=unique(measures_keywords$lemma))
  table_id_keywords$id_keyword <- 1:nrow(table_id_keywords)
  
  #import unique indices into measures_keyword table to get combination of measure-keyword occurances in terms of the indices
  measures_keywords <-left_join(measures_keywords,table_id_keywords, by=c("lemma"="keyword"))
  measures_keywords <-left_join(measures_keywords,table_id_measures, by=c("measure_nr"="measure_nr"))
  
  #extract set of coordinates for nonzero values in measure-keyword matrix:
  temp <- !duplicated(paste(measures_keywords$id_measure,measures_keywords$id_keyword))
  id_measures <- measures_keywords$id_measure[temp]
  id_keywords <- measures_keywords$id_keyword[temp]
  rm(temp)
  
  #create matrix
  mk_matrix <- sparseMatrix(i=id_measures, j=id_keywords, x=1L, dims=c(nrow(table_id_measures),nrow(table_id_keywords)))
  
  #get keyword co-occurrence frequency matrix (cardinality of intersection set for measures i-j):
  freq_matrix <- mk_matrix %*% t(mk_matrix)
  rm(mk_matrix,id_measures,id_keywords, table_id_keywords) #free up memory
  
  #get matrix of total unique keywords in i-j combination (cardinality of union set for measures i-j): 
  #in order to increase speed, I take advantage of the fact that the cardinality of the union of set A and B is equal to the cardinality of A + the cardinality of B - cardinality of  their intersection
  tot_matrix <- expand.grid(diag(freq_matrix),diag(freq_matrix))
  tot_matrix <- matrix(tot_matrix[,1] + tot_matrix[,2], nrow(table_id_measures), nrow(table_id_measures))
  tot_matrix <- tot_matrix - freq_matrix
  
  #get similarity index:
  similarity_matrix <- as.matrix(freq_matrix / tot_matrix)
  rm(freq_matrix,tot_matrix) #free-up memory
  #_______________________________________________________________________________________
  
  
  #FILTER RESULTS ------------------------------------------------------------------------
  
  #CLEAN SIMILARITY MATRIX
  #set diagonal to zero (obviously measure i is identical to measure i - but we are not interested in that)
  diag(similarity_matrix) <- 0
  
  #set to one similarity between measures with no description
  similarity_matrix[is.nan(similarity_matrix)] <- 1
  
  # FILTER COUNTRY COMBINATIONS
  if (match_country){
    
    #get list of countries to which each measure applies
    measure_countries <- data[,c("Nr","COUNTRIES")]
    measure_countries <- separate_rows(measure_countries, COUNTRIES, sep=";")
    
    #make a list of all combination of measures for which at least one country is matching
    similarity_table <- measure_countries
    colnames(similarity_table)[1] <- "j"
    similarity_table <- left_join(measure_countries,similarity_table, by="COUNTRIES") %>% rename(i=Nr)  #create all combination of measures for same country
    similarity_table <- similarity_table[!duplicated(paste(similarity_table$i,similarity_table$j)),]  #eliminate multiple rows for same combination of measures (this happens when measures apply to multiple countries)
    similarity_table <- similarity_table[,c("i","j")] #drop list of countries
    similarity_table <- as.data.table(similarity_table)[i != j,] #remove self-referring combinations (diagonal of combination matrix)
    
    #import measure indices for subsetting similarity matrix
    colnames(table_id_measures)[2] <- "id_measure_i"
    similarity_table <- left_join(similarity_table, table_id_measures, by=c("i"="measure_nr"))
    colnames(table_id_measures)[2] <- "id_measure_j"
    similarity_table <- left_join(similarity_table, table_id_measures, by=c("j"="measure_nr"))
    
    #import info from similarity matrix
    similarity_table$similarity <- similarity_matrix[as.matrix(similarity_table[,c("id_measure_i","id_measure_j")])] 
    
    rm(measure_countries, similarity_matrix) #free-up memory
    
  } else {
    #create a list of measure indices
    similarity_table <- expand.grid(id_measure_i = 1:nrow(table_id_measures), id_measure_j = 1:nrow(table_id_measures))
    
    #import similarity index into table
    similarity_table$similarity <- similarity_matrix[as.matrix(similarity_table)]
    
    #free-up memory
    rm(similarity_matrix)
    
    #import original measure Nr
    colnames(table_id_measures) <- c("i","id_measure_i")
    similarity_table <- left_join(similarity_table, table_id_measures, by="id_measure_i")
    colnames(table_id_measures) <- c("j","id_measure_j")
    similarity_table <- left_join(similarity_table, table_id_measures, by="id_measure_j")  
    
  }
  
  # FILTER WITH CUT-OFF VALUE
  similarity_table <- similarity_table[similarity_table$similarity > cut_off, c("i","j","similarity")]
  similarity_table$similarity <- round(similarity_table$similarity, 3)
  #_______________________________________________________________________________________
  
  
  # EXPORT RESULTS TO EDB DATA TABLE -----------------------------------------------------
  
  data <- similarity_table %>% 
    group_by(i) %>% 
    summarise(`Similar measures` = paste0(j, collapse=";"),
              `Similarity index` = paste0(similarity, collapse=";"), .groups="drop") %>%
    left_join(data, . , by=c("Nr"="i"))
  
  return(data)
  #_______________________________________________________________________________________
  
}

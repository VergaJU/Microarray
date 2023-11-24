# This are the microarray processing functions



# Functions

get_gset <- function(gse ,platform_id = "GPL97"){
  # This function will take the gse from getGEO and the platform needed to extract
  # it will return a gset with the data from the platform (the (set))
  if (length(gse) > 1) idx <- grep(platform_id, attr(gse, "names")) else idx <- 1
  gset2 <- gse[[idx]]
  return(gset2)
}

assign_names <- function(c1="Young", c2="Old") {
  # This function will take the name of the conditions 
  # It will return a vector with two names; the one used for the DE genes and 
  # one for the actual lfc and pvalue of all the genes
  doc_name <- paste("DEG_list_", my_id, "_", c1, "_vs_", c2, ".csv", sep = "")
  dds_doc_name <- paste("dds_", my_id, "_", c1[1], c2[1], ".csv", sep = "")
  
  return (c(doc_name, dds_doc_name))
}
map_age <- function(age_category) {
  # Desinged to work on the specific case of age, this will asing numbers 0,1 and 2 as categories for standarization
  # It will return a vector with the age_category as integers
  if (is.numeric(age_category[0])){
    age_vector <- ifelse(age_category < 35, 0, ifelse(age_category >= 65, 2, 1))
  }
  else{
    age_vector <- ifelse(age_category == "Young", 0, ifelse(age_category == "Old", 2, 1))
  }
  
  return (paste(age_vector, collapse = ""))
}

format_expression_values <- function(ex, gset)
{
  qx <- as.numeric(quantile(ex, c(0., 0.25, 0.5, 0.75, 0.99, 1.0), na.rm=T))
  LogC <- (qx[5] > 100) ||
    (qx[6]-qx[1] > 50 && qx[2] > 0)
  if (LogC) 
  {
    ex[which(ex <= 0)] <- NaN
    exprs(gset) <- log2(ex) 
  } # i think this code is trying to determine whether the data is in log2 format yet
  return (gset)
}

get_expression_df <- function(ex, gene_table, column_name="Gene.Symbol")
{
  symbol_column  <- gene_table[[column_name]]
  symbol_df <- data.frame(symbol = symbol_column)
  
  # Combine the extracted "symbol" column with the row index from the AnnotatedDataFrame
  ID_geneSymbol_df <- cbind(symbol_df, row.names = rownames(gene_table))
  
  
  
  expression_df = merge(ex, ID_geneSymbol_df, by="row.names", all.x=TRUE)
  
  return(expression_df)
}

get_categories <- function(sample_data)
{
  gsms <- map_age(sample_data$age)
  gsms #<- "111111111111000000000000"
  sml <- strsplit(gsms, split="")[[1]]
  gs <- factor(sml)
  groups <- make.names(unique(sample_data$age))
  levels(gs) <- groups
  return(gs)
}

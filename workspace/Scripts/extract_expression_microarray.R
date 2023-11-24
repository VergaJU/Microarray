# This script is to run all the microarray experiments and get the Expression 

start_directory <- "~/workspace/"
setwd(file.path(start_directory))

# Libraries
library(illuminaHumanv4.db)
library(ggpubr)
library(GEOquery)
library(limma)
library(gplots)
library(dplyr)
library(tidyr)
source("Scripts/experiment_parsings.R")
source("Scripts/DE_pool.R")
# Global variables

experiment_id <- c( "GSE9676")
experiment_platfomr <- list(c("GPL96", "GPL96"))
PVALUE = 0.1
LOGFOLDCHANGE = 1
y = "Young"
o = "Old"
m = "MiddleAge"

EXPECTED_PROPORTION = 0.01

gene_column ="GB_ACC" # "Gene Symbol"
  
library(readr)

# Read the CSV file
data <- read_csv("Data/Experiments_Platform_format.csv")
data
# Extract data into separate vectors and lists
experiment_id <- data$Experiment
platforms <- strsplit(data$Platforms, ";")
gene_column <- data$Format

# Convert the list of platforms into a list of character vectors
experiment_platfomr <- lapply(platforms, unlist)

# Print or use the extracted data as needed
print(experiment_id)
print(experiment_platfomr)
print(gene_column)
  
  
  
for (experiment_index in seq_along(experiment_id))
  {
    my_id = experiment_id[experiment_index]
    platforms =  experiment_platfomr[[experiment_index]]
    gene = gene_column[experiment_index]
    gse <- getGEO(my_id, GSEMatrix =TRUE)
    for (platform in platforms)
    {
      metadata_doc_name <- paste("Results/samples_", my_id, "_",platform,".csv", sep = "")
      expression_doc_name <- paste("Results/expression_", my_id,"_",platform, ".csv", sep = "")
      gset= get_gset(gse,platform_id = platform)
      sample_data = pData(gset)
      print(paste0("Parsing of ", my_id))
      sample_data = parse_function(input_string = my_id, sample_data =sample_data)
      gs = get_categories(sample_data = sample_data)
      gset$group <- gs
      ex <- exprs(gset) # expression data, what form is this?
      gset <- format_expression_values(ex=ex, gset=gset)
      gene_table = featureData(gset)
      print(paste0("Evaluation of platform ", platform, " On ", my_id, ", Gene Id:", gene))
      expression_df = get_expression_df(ex=ex, gene_table = gene_table, column_name =gene )
      write.table(expression_df, expression_doc_name, row.names=F, sep=",")
      
      write.table(sample_data, metadata_doc_name, row.names=F, sep=",")
      
    }
    
  }


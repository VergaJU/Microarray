# This is the file of the parsing of the different experiments.
# Each function will have an aribitaray name and if the experiment has the same format just copy it


parsing_00 <- function(sample_data){
  return(sample_data)
}

parsing_01 <- function(sample_data){
  data <- separate(sample_data, title, into = c("age_sex", "other"), sep = " yr ")
  data <- separate(data, age_sex, into = c("sex", "age"), sep = " ")
  data <- mutate(data, age_sex = paste(age, sex, sep = "_"))
  return(data)
}

parsing_02 <- function(sample_data){
  data <- separate(sample_data, source_name_ch1, into = c("muscle", "age_sex"), sep = " biopsy ")
  data <- separate(data, age_sex, into = c("age", "sex"), sep = " yr old healthy ")
  data <- mutate(data, age_sex = paste(age, sex, sep = "_"))
  return(data)
}


parsing_03 <- function(sample_data)
{
  data <- separate(sample_data, description, into = c("age", "other"), sep = " ")
  return(data)
  
}

parsing_04 <- function(sample_data)
{
  data <- separate(sample_data, characteristics_ch1.1, into = c("age", "nothing"), sep = " at onset of intervention")
  data <- separate(data, age, into = c("nothing", "age"), sep = "Age: ")
  return(data)
}

parsing_05 <- function(sample_data)
{
  data <- separate(sample_data, title, into = c("nothing", "status"), sep = "Muscle PCOS ")
  data <- separate(data, status, into = c("nothing", "status"), sep = "after ")
  data <- separate(data, status, into = c("status", "id"), sep = " ")
  data$status[data$status=="pioglitazone"] <- "after_pioglitazone"
  data <- separate(data, nothing, into = c("status_2", "id_2"), sep = " ")
  
  data$id <- ifelse(is.na(data$id), data$id_2, data$id)
  data$status <- ifelse(is.na(data$status), data$status_2, data$status)
  data <- mutate(data, age = "Young")
  return(data)
  
}

parsing_06 <- function(sample_data)
{
  data <- separate(sample_data, title, into = c("age", "status"), sep = " ")
  data <- mutate(data, age_class = paste(age, status, sep = "_"))

  return(data)
}


parsing_06 <- function(sample_data)
{
  data <- separate(sample_data, title, into = c("age", "status"), sep = " ")
  data <- mutate(data, age_class = paste(age, status, sep = "_"))
  
  return(data)
}

parsing_07 <- function(sample_data)
{
  data <- mutate(sample_data, age = "MA")
  
  return(data)
}

parsing_08 <- function(sample_data)
{
  sample_data$age = sample_data$`agent:ch1`
  
  
  return(sample_data)
}

parsing_09 <- function(sample_data)
{
  sample_data$age = sample_data$`age:ch1`
  
  
  return(sample_data)
}

parsing_10 <- function(sample_data)
{
  sample_data <- separate(sample_data, title, into = c("age", "sex"), sep = " ")
  sample_data <- separate(sample_data, age, into = c("muscle", "age"), sep = "_")
  sample_data <- separate(sample_data, sex, into = c("sex", "rep"), sep = "_")
  
  
  sample_data <- mutate(sample_data, age_sex = paste(age, sex, sep = "_"))
  return(sample_data)
}

parsing_11 <- function(sample_data)
{
  sample_data$age = sample_data$`treatment:ch2`
  
  
  return(sample_data)
}

parsing_12 <- function(sample_data)
{
  sample_data$age <- sample_data$`age (yr):ch1`
  
  return(sample_data)
}

parsing_13 <- function(sample_data)
{
  sample_data <- separate(sample_data, characteristics_ch1.1, into = c("other", "age"), sep = ": ")
  sample_data <- separate(sample_data, characteristics_ch1.3, into = c("other", "sex"), sep = ":")
  
  
  sample_data <- mutate(sample_data, age_sex = paste(age, sex, sep = "_"))
  
  return(sample_data)
}

parsing_14 <- function(sample_data)
{
  sample_data <- separate(sample_data, characteristics_ch1.1, into = c("other", "age"), sep = ": ")
  
  
  return(sample_data)
}
parsing_15 <- function(sample_data)
{
  data <- mutate(sample_data, age = "25.33")
  
  return(data)
}



function_list <- list(
  "GSE674"=parsing_02,
  "GSE1428"=parsing_03,
  "GSE8157"=parsing_05,
  "GSE9103"=parsing_06,
  "GSE9676"=parsing_01,
  "GSE13070"=parsing_07,
  "GSE38718"=parsing_10,
  "GSE47969"=parsing_00,
  "GSE48278"=parsing_12,
  "GSE59880"=parsing_13,
  "GSE87105"=parsing_09,
  "GSE161643"=parsing_12,
  "GSE161721"=parsing_15,
  "GSE6348"=parsing_04,
  "GSE17992"=parsing_08,
  "GSE40551"=parsing_11,
  "GSE136344"=parsing_14,
  "GSE23527"=parsing_09,
  "GSE155959"=parsing_12
  
)





parse_function <- function(input_string, sample_data) {
  if (input_string %in% names(function_list)) {
    function_to_call <- function_list[[input_string]]
    return (function_to_call(sample_data))
  } else {
    cat("No function associated with", input_string, "\n")
    return (parsing_00(sample_data))
  }
}
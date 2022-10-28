library(tidyverse)
library(lubridate)

textFileDir <- 'C:/Users/daniel.carpenter/Downloads/testData'

# Get List of files
listOfTxtFiles <- list.files(textFileDir,
                             pattern    = 'txt',
                             full.names = TRUE,
                             recursive  = FALSE
                             )


# combined all the text files in listOfTxtFiles and store in dataframe 'df.raw'
for (i in 1:length(listOfTxtFiles)){
  if(i==1){
    assign(paste0("df.raw"), read.table(listOfTxtFiles[i],header = FALSE, sep = ","))
  }
  
  if(!i==1){
    
    assign(paste0("Test",i), read.table(listOfTxtFiles[i],header = FALSE, sep = ","))
    df.raw <- rbind(df.raw,get(paste0("Test",i)))
    rm(list = ls(pattern = "Test"))
  }
}

# Separate into 2 columns and trim whitespace
df <- df.raw %>%
  separate(V1, c('ColumnName', 'ValueField'), 
           sep = ' ', 
           extra = "merge") %>%
  
  # Weather conditions splits into two columns so remove the conditions
  mutate(ValueField = str_remove_all(ValueField, 'conditions') ) %>%
  
  # Trim whitespace
  mutate_at(vars(ColumnName, ValueField), trimws)

# Remove any "" by replacing with NA, then drop those rows
df[df == ""] <- NA
df <- df %>% drop_na()

# Convert "NaN" to NaN
df[df == "NaN"] <- NA


suppressWarnings(
  
  # Clean up names of columns
  df.wide <- df %>%
    mutate(ColumnName = str_remove_all(ColumnName, ':'),
           ColumnName = str_replace_all(ColumnName, '_', ' '),
           ColumnName = str_to_title(ColumnName),
           ColumnName = str_replace_all(ColumnName, ' ', '_'),
           ColumnName = str_replace_all(ColumnName, 'Weather', 'Weather_Conditions')) %>%
    
    
    # Pivot into multiple columns
    pivot_wider(names_from = ColumnName,
                values_from = ValueField) %>%
    unnest()
)  


# CLean up column types
df.clean <- df.wide %>%
  
  # Order date
  mutate(Order_Date = as.Date(Order_Date))


  # Time ordered

  # Time picked up

  # Multiple deliverys as binary













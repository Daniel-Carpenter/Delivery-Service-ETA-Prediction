library(tidyverse)
library(lubridate)


readAndCleanTxtFiles <- function(textFileDirectory, # Directory of the text file
                                 isTrainingData     # TRUE if importing training data. matters because of the target variable
) {
  # Get List of files
  listOfTxtFiles <- list.files(textFileDirectory,
                               pattern    = 'txt', # look for .txt files
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
  df.split <- df.raw %>%
    separate(V1, c('ColumnName', 'ValueField'), 
             sep = ' ', 
             extra = "merge") %>%
    
    # Weather conditions splits into two columns so remove the conditions
    mutate(ValueField = str_remove_all(ValueField, 'conditions') ) %>%
    mutate(ValueField = gsub(r"{\s*\([^\)]+\)}","",as.character(ValueField) ) ) %>%
    
    # Trim whitespace
    mutate_at(vars(ColumnName, ValueField), trimws) %>%
    
    filter(ValueField != 'dtype: object')
  
  # Convert "NaN" to NaN
  df.split[df.split == "NaN"] <- NA
  
  
  suppressWarnings(
    
    # Clean up names of columns
    df.wide <- df.split %>%
      mutate(ColumnName = str_remove_all(ColumnName, ':'),
             ColumnName = str_replace_all(ColumnName, '_', ' '),
             ColumnName = str_to_title(ColumnName),
             ColumnName = str_replace_all(ColumnName, ' ', '_'),
             ColumnName = str_replace_all(ColumnName, 'Weather', 'Weather_Conditions')) %>% 
      
      
      # # Pivot into multiple columns
      pivot_wider(names_from  = ColumnName,
                  values_from = ValueField) %>%
      unnest()
  )  
  
  
  # Function to convert string time (hh:mm) to numeric version (hh + mm / 60)
  cleanHourMinute <- function(stringHM) {
    
    # Split the hour and minute into two indices
    hourAndMin = unlist( strsplit(stringHM, ':')  )
    
    HOUR_IDX   = 1                              # The index for the hour
    MINUTE_IDX = 2                              # The index for the minute
    hourAndMin = lapply(hourAndMin, as.numeric) # Convert to numeric
    
    # Convert to numeric representation
    hourAndMin[MINUTE_IDX] = hourAndMin[[MINUTE_IDX]] / 60
    
    # Sum the hour and numeric minutes to get the partial hour
    hourMinNumericSum = hourAndMin[[HOUR_IDX]] + hourAndMin[[MINUTE_IDX]]
    
    return(hourMinNumericSum)
  }
  
  # CLean up column types
  df.clean <- df.wide %>%
    
    # Order date
    mutate(Order_Date = as.Date(Order_Date, '%d-%m-%Y')) %>%
    
    # Convert Time ordered and Time picked up to numeric representation
    separate(Time_Orderd,       c('hourOrdered',  'minuteOrdered'),  sep = ':') %>%
    separate(Time_Order_Picked, c('hourPickedUp', 'minutePickedUp'), sep = ':') %>%
    
    # Convert to hour and minute to Numeric
    mutate_at(vars(starts_with(c('minute', 'hour'))),   as.numeric) %>%
    
    # Convert to relative time (fraction of 60 minutes)
    mutate_at(vars(starts_with('minute')), ~ . / 60)  %>%
    
    # Add back minute and hours  
    mutate(Time_Ordered      = hourOrdered  + minuteOrdered,
           Time_Order_Picked = hourPickedUp + minutePickedUp)  %>%
    
    # Remove Helper columns 
    select(-c(starts_with(c('minute', 'hour')))) %>%
    
    # COnvert to Factor
    mutate_at(vars(
      Id,
      Delivery_Person_Id,
      Weather_Conditions,
      Road_Traffic_Density,
      Type_Of_Order,
      Type_Of_Vehicle,
      Multiple_Deliveries,
      Festival,                   
      City,
      Name
    ), as.factor ) %>%
    
    # Convert to Numeric
    mutate_at(vars(
      Delivery_Person_Age,
      Delivery_Person_Ratings,
      Restaurant_Latitude,
      Restaurant_Longitude,
      Delivery_Location_Latitude,
      Delivery_Location_Longitude,
      Vehicle_Condition
      # Note TimeTaken not converted due
    ), as.numeric ) 
  
  
  # If you are using training data then clean the target variable
  # Notate it as the target variable
  if (isTrainingData) {
    df.clean <- df.clean %>% 
      mutate(Target_Variable = as.numeric(df.clean$Time_Taken) ) %>%
      select(-Time_Taken)
    
    # Write file for training data
    write.csv(df.clean, file = 'TrainingData.csv',
              row.names = FALSE)
    
  } else {
    
    # Else write for the test data  
    write.csv(df.clean, file = 'TestData.csv',
              row.names = FALSE)
  }
  
  # Return the cleaned dataset
  return(df.clean)
}

# Read in training data
df.train <- readAndCleanTxtFiles(textFileDirectory = 'SampleData/Train/',
                                 isTrainingData    = TRUE)

# Read in test data
df.test <- readAndCleanTxtFiles(textFileDirectory = 'SampleData/Test/',
                                isTrainingData    = FALSE)










# Data Wrangling
library(tidyverse)
library(skimr)
library(lubridate) # dates

# Modeling
library(MASS)
library(caret) # Modeling variants like SVM
library(earth) # Modeling with Mars
library(pls)   # Modeling with PLS
library(glmnet) # Modeling with LASSO

# Aesthetics
library(knitr)
library(cowplot)  # multiple ggplots on one plot with plot_grid()
library(scales)
library(kableExtra)
library(ggplot2)
library(inspectdf)

#Hold-out Validation
library(caTools)

#Data Correlation
library(GGally)
library(regclass)

#RMSE Calculation
library(Metrics)

#p-value for OLS model
library(broom)

#ncvTest
library(car)

library(EnvStats) #Boxcox
library(earth) #MARS
library(VIM)



# setwd("C:/Users/arzackne/Desktop/hw6")

# Convert all character data to factor
df.train.base <- read.csv('Train.csv', stringsAsFactors = TRUE)

hist(df.train.base$revenue)

ggplot(df.train.base, aes(x=revenue)) + geom_histogram() + xlim(c(-10, 200))

min(df.train.base$revenue)

a<-aggr(df.train.base)
summary(a)



# convert the ""'s to NA
df.train.base[df.train.base == ""] <- NA

# Clean data
df.train.base <- df.train.base %>% 
  
  # Ensure boolean variables are numeric
  mutate(adwordsClickInfo.isVideoAd = as.numeric(adwordsClickInfo.isVideoAd) ) %>%
  
  # Make sure dates are dates
  mutate(date = as.Date(date),
         visitStartTime = as_datetime(visitStartTime)
  ) %>%
  
  # Ensure factor are factors
  mutate(custId       = as.factor(custId),
         sessionId    = as.factor(sessionId),
         isTrueDirect = as.factor(isTrueDirect),
         newVisits    = as.factor(if_else(is.na(newVisits), 0, 1) ),
         bounces      = as.factor(if_else(is.na(bounces),   0, 1)   ),
         adwordsClickInfo.page      = as.factor(adwordsClickInfo.page),
         adwordsClickInfo.isVideoAd = as.factor(adwordsClickInfo.isVideoAd)
  ) %>%
  
  dplyr::select(-c(
    isMobile # This is contained in deviceCategory
    
  ))



df.train.base.numeric <- df.train.base %>%
  
  # selecting all the numeric data
  dplyr::select_if(is.numeric) %>%
  
  # converting the data frame to tibble
  as_tibble()






df.train.base.factor <- df.train.base %>%
  
  #selecting all the numeric data
  dplyr::select_if(is.factor) %>%
  
  #converting the data frame to tibble
  as_tibble()





# Function for data report
dataQualityReport <- function(df) {
  
  # Function to remove any columns with NA
  removeColsWithNA <- function(df) {
    return( df[ , colSums(is.na(df)) == 0] )
  }
  
  # Create Comprehensive data report using skimr package
  # This is done a bit piece-wise because PDF latex does not like the skimr package
  # Very much. So Instead of printing `skim(df)`, I have to pull the contents manually
  # Unfortunately. This is not an issue with html typically.
  dataReport <- skim(df) %>%
    rename_all(~str_replace(.,"skim_","")) %>%
    arrange(type, desc(complete_rate) ) # sort data 
  
  # Filter to the class types
  dataReport.numeric <- dataReport %>% filter(type == 'numeric') # numeric data
  dataReport.factor  <- dataReport %>% filter(type == 'factor' ) # factor  data
  
  # Remove columns that do not apply to this type of data -----------------------
  
  ## numeric data
  dataReport.numeric <- removeColsWithNA(dataReport.numeric)  %>%
    
    # Clean column names by removing numeric prefix, 
    rename_all(~str_replace(.,"numeric.","")) 
  
  ## factor  data
  dataReport.factor  <- removeColsWithNA(dataReport.factor ) %>%
    
    # Clean column names by removing factor  prefix
    rename_all(~str_replace(.,"factor.",""))  
  
  
  # Set up options for Display the reports
  options(skimr_strip_metadata = FALSE)
  options(digits=2)
  options(scipen=99)
  
  # Numeric report <- Get summary of data frame --------------------------------
  
  # data frame stats
  dfStats.num <- data.frame(Num_Numeric_Variables = ncol(df %>% select_if(is.numeric)),
                            Total_Observations    = nrow(df) )
  
  # Now see individual column statistics
  dfColStats.num <- dataReport.numeric %>% 
    dplyr::select(-type, -hist)
  
  
  # Factor report <- Get summary of data frame --------------------------------
  
  # Get summary of data frame
  dfStats.factor <- data.frame(Num_Factor_Variables = ncol(df %>% select_if(is.factor)),
                               Total_Observations   = nrow(df) )
  
  # Now see individual column statistics
  dfColStats.factor <- dataReport.factor  %>% 
    dplyr::select(-type, -ordered) 
  
  
  # Return the data frames
  return(list('dfStats.num'       = dfStats.num,    
              'dfColStats.num'    = dfColStats.num,
              'dfStats.factor'    = dfStats.factor, 
              'dfColStats.factor' = dfColStats.factor))
}




# Get the factor and numeric reports
initialReport <- dataQualityReport(df.train.base)

# Numeric data frame stats
initialReport$dfStats.num %>% kable()

# Numeric column stats
initialReport$dfColStats.num %>%
  kable() %>% kable_styling(font_size=7, latex_options = 'HOLD_position') # numeric data





# factor data frame stats
initialReport$dfStats.factor %>% kable()

# factor column stats
initialReport$dfColStats.factor %>%
  kable() %>% kable_styling(font_size=7, latex_options = 'HOLD_position') # numeric data






# Transforming the revenue
transformed_revenue <- df.train.base %>%
  group_by(custId) %>%
  summarise(targetRevenue = log(sum(revenue)) + 1) %>%
  ungroup()

hist(transformed_revenue$targetRevenue,
     col = 'skyblue4',
     main = 'Distribution of Target Revenue for each customer',
     xlab = 'Target Revenue')




df.train.base %>%
  ggplot(aes(x = fct_reorder(channelGrouping, desc(revenue) ),
             y = revenue) ) +
  # Boxplots
  geom_boxplot(aes(color = channelGrouping), fill = 'lightsteelblue1', alpha = 0.7) +
  coord_flip() +
  # Theme, y scale format, and labels
  theme_minimal() + 
  theme(panel.grid.major.x = element_blank()) +
  #scale_y_continuous(labels = comma) +
  labs(title = 'Distribution of Revenue by Different Online Store Channels',
       subtitle = 'Ordered Descending by Revenue Generated by Channels',
       x = 'Channels Used by Customers for Online Store',
       y = 'Revenue Generated')








df.train.base[!complete.cases(df.train.base$continent), ] %>%
  distinct(continent, subContinent, country, region, metro, city)


df.train.base %>%
  filter(region == 'Osaka Prefecture') %>%
  distinct(continent, subContinent, country, metro, city, region)

df.train <- df.train.base

df.train$continent[is.na(df.train$continent) &
                     df.train$region == 'Osaka Prefecture'] <- 'Asia'

df.train %>%
  filter(region == 'Osaka Prefecture' & city == 'Osaka') %>%
  distinct(subContinent)

df.train$subContinent[is.na(df.train$subContinent) &
                        df.train$region == 'Osaka Prefecture' &
                        df.train$city == 'Osaka'] <- 'Eastern Asia'

df.train %>%
  filter(region == 'Osaka Prefecture' & city == 'Osaka') %>%
  distinct(country)

df.train$country[is.na(df.train$country) &
                   df.train$region == 'Osaka Prefecture' &
                   df.train$city == 'Osaka'] <- 'Japan'

df.train %>%
  filter(region == 'Osaka Prefecture' & city == 'Osaka') %>%
  distinct(metro)

df.train %>%
  filter(metro == 'JP_KINKI')

df.train$metro[is.na(df.train$metro) &
                 df.train$region == 'Osaka Prefecture' &
                 df.train$city == 'Osaka'] <- 'JP_KINKI'

df.train %>%
  filter(region == 'Osaka Prefecture' & city == 'Osaka')





df.train[!complete.cases(df.train$continent), ] %>%
  distinct(continent, subContinent, country, region, metro, city)








UNKNOWN_TEXT = 'Unknown'

# If null in location data, then 'Unknown' location
df.train <- df.train %>%
  mutate_at(
    # Only mutate these location variables
    vars(continent:city), 
    
    # Apply function rename null values to Unknown
    list(~ as.factor(ifelse(is.na(.), UNKNOWN_TEXT, .) ) ) 
  )




# Check the data set - see that most of the ad data is now cleaned.
report2 <- dataQualityReport(df.train)
report2$dfColStats.factor %>% kable()







df.train %>%
  distinct(medium)

df.train[!complete.cases(df.train$medium), ] %>%
  distinct(medium, campaign,keyword, referralPath, 
           adContent, adwordsClickInfo.page, adwordsClickInfo.slot, 
           adwordsClickInfo.gclId, adwordsClickInfo.adNetworkType, adwordsClickInfo.isVideoAd)








NO_MEDIUM_TEXT = 'No traffic source '

# Now clean up the data in the main data frame `df.train`
# by setting null values to "No taffic source" if there is no medium
# Applies to "ad*", keyword, and campaign, referralPath, medium variables
df.train <- df.train %>%
  mutate_at(
    # Only mutate the variables starting with ad, THEN the campaign variable
    vars(starts_with('ad'), keyword, campaign, referralPath, medium), 
    
    # Apply function rename set the campaign text if campaign is null
    list(~ as.factor(ifelse(is.na(medium), NO_MEDIUM_TEXT, .) ) ) 
  ) 







# Check the data set - see that most of the ad data is now cleaned.
report3 <- dataQualityReport(df.train)
report3$dfColStats.factor %>% kable()







df.train %>%
  distinct(campaign)

df.train[!complete.cases(df.train$campaign), ] %>%
  distinct(campaign, adwordsClickInfo.page, adwordsClickInfo.slot, 
           adwordsClickInfo.adNetworkType, adwordsClickInfo.isVideoAd)





# Now clean up the data in the main data frame `df.train`
# by setting null values to "None" if there is no campaign.
# Applies to "ad*", keyword, and campaign variables
#df.train <- df.train %>%
#  mutate_at(
    # Only mutate the variables starting with ad, THEN the campaign variable
#    vars(adwordsClickInfo.page, adwordsClickInfo.slot, adwordsClickInfo.adNetworkType, adwordsClickInfo.isVideoAd, campaign), 
    
    # Apply function rename set the campaign text if campaign is null
#    list(~ as.factor(ifelse(is.na(campaign), NO_CAMPAIGN_TEXT, .) ) ) 
#  ) 





# Check the data set - see that most of the ad data is now cleaned.
#report4 <- dataQualityReport(df.train)
#report4$dfColStats.factor %>% kable()






df.train[!complete.cases(df.train$keyword), ] %>%
  distinct(adContent, adwordsClickInfo.adNetworkType, adwordsClickInfo.isVideoAd, keyword)







NO_KEYWORD_TEXT = 'No Keyword'

# Now clean up the data in the main data frame `df.train`
# by setting null values to "No Keyword" if there is no keyword
# Applies to some "ad*", and keyword variables
df.train <- df.train %>%
  mutate_at(
    # Only mutate the variables starting with ad, THEN the keyword variable
    vars(adContent, adwordsClickInfo.adNetworkType, adwordsClickInfo.isVideoAd, keyword), 
    
    # Apply function rename set the campaign text if campaign is null
    list(~ as.factor(ifelse(is.na(keyword), NO_KEYWORD_TEXT, .) ) ) 
  ) 






# Check the data set - see that most of the ad data is now cleaned.
report5 <- dataQualityReport(df.train)
report5$dfColStats.factor %>% kable()





df.train[!complete.cases(df.train$adContent), ] %>%
  distinct(adContent, referralPath)




NONE_TEXT = 'No Ad'

# If the `adContent` is null, label as `None`
df.train <- df.train %>%
  mutate_at(
    # Only mutate the referral path
    vars(referralPath, adContent), 
    
    # Apply function rename set the referral to none
    list(~ as.factor(ifelse(is.na(adContent), NONE_TEXT, .) ) ) 
  )




# Check the data set - see that most of the ad data is now cleaned.
report6 <- dataQualityReport(df.train)
report6$dfColStats.factor %>% kable()




df.train[!complete.cases(df.train$adwordsClickInfo.adNetworkType), ] %>%
  distinct(adwordsClickInfo.adNetworkType,adwordsClickInfo.page, adwordsClickInfo.slot,
           adwordsClickInfo.gclId, adwordsClickInfo.adNetworkType, adwordsClickInfo.isVideoAd)

#referralPath,adwordsClickInfo.page, adwordsClickInfo.slot,  adwordsClickInfo.gclId,adwordsClickInfo.adNetworkType,adwordsClickInfo.isVideoAd





NONE_TEXT = 'No Ad Network'

# If the `adwordsClickInfo.adNetworkType` is null, label as `No Ad Network`
df.train <- df.train %>%
  mutate_at(
    # Only mutate the referral path
    vars(adwordsClickInfo.page, adwordsClickInfo.slot, adwordsClickInfo.gclId,
         adwordsClickInfo.isVideoAd, adwordsClickInfo.adNetworkType), 
    
    # Apply function rename set the referral to none
    list(~ as.factor(ifelse(is.na(adwordsClickInfo.adNetworkType), NONE_TEXT, .) ) ) 
  )


# Check the data set - see that most of the ad data is now cleaned.
report7 <- dataQualityReport(df.train)
report7$dfColStats.factor %>% kable()




df.train[!complete.cases(df.train$adwordsClickInfo.page), ] %>%
  distinct(adwordsClickInfo.page, referralPath, adwordsClickInfo.slot, adwordsClickInfo.gclId)

#adwordsClickInfo.page, referralPath, adwordsClickInfo.slot,  adwordsClickInfo.gclId




NONE_TEXT = 'No Ad Page'

# If the `adwordsClickInfo.page` is null, label as `No Ad Page`
df.train <- df.train %>%
  mutate_at(
    # Only mutate the referral path
    vars(referralPath, adwordsClickInfo.slot, adwordsClickInfo.gclId, adwordsClickInfo.page), 
    
    # Apply function rename set the referral to none
    list(~ as.factor(ifelse(is.na(adwordsClickInfo.page), NONE_TEXT, .) ) ) 
  )





# Check the data set - see that most of the ad data is now cleaned.
report8 <- dataQualityReport(df.train)
report8$dfColStats.factor %>% kable()





df.train[!complete.cases(df.train$networkDomain), ] %>%
  distinct(topLevelDomain)

#topLevelDomain





NONE_TEXT = 'No Domain'

# If the `network domain` is null, label as `No Domain`
df.train <- df.train %>%
  mutate_at(
    # Only mutate the referral path
    vars(networkDomain:topLevelDomain), 
    
    # Apply function rename set the referral to none
    list(~ as.factor(ifelse(is.na(.), NONE_TEXT, .) ) ) 
  )







# Check the data set - see that most of the ad data is now cleaned.
report9 <- dataQualityReport(df.train)
report9$dfColStats.factor %>% kable()




df.train[!complete.cases(df.train$adwordsClickInfo.gclId), ] %>%
  distinct(referralPath, adwordsClickInfo.gclId)



NONE_TEXT = 'No Referrer'

# If the `network domain` is null, label as `No Domain`
df.train <- df.train %>%
  mutate_at(
    # Only mutate the referral path
    vars(referralPath), 
    
    # Apply function rename set the referral to none
    list(~ as.factor(ifelse(is.na(referralPath), NONE_TEXT, .) ) ) 
  )





# Check the data set - see that most of the ad data is now cleaned.
report10 <- dataQualityReport(df.train)
report10$dfColStats.factor %>% kable()





NONE_TEXT = 'No Google Click ID'

# If the `network domain` is null, label as `No Domain`
df.train <- df.train %>%
  mutate_at(
    # Only mutate the referral path
    vars(adwordsClickInfo.gclId), 
    
    # Apply function rename set the referral to none
    list(~ as.factor(ifelse(is.na(adwordsClickInfo.gclId), NONE_TEXT, .) ) ) 
  )


# Check the data set - see that most of the ad data is now cleaned.
report11 <- dataQualityReport(df.train)
report11$dfColStats.factor %>% kable()






#deselecting the browser and source columns
df.train <- df.train %>%
  dplyr::select(!c(browser, operatingSystem))

# Number of rows with any nulls
numRowsWithNulls <- nrow(df.train[!complete.cases(df.train), ])

# Output text
paste('There are', numRowsWithNulls, 'rows with nulls')
paste0('That equates to ', round(numRowsWithNulls / nrow(df.train)* 100, 1), '% rows with nulls')

# Drop the rows
df.train <- df.train %>% drop_na()
paste('Total Rows Remaining:', nrow(df.train))


# Check the data set - see that most of the ad data is now cleaned.
report12 <- dataQualityReport(df.train)
report12$dfColStats.factor %>% kable()







df.train.clean <- df.train

# Make data set of `factor` variables called `df.train.base.factor`
df.train.factor <- df.train %>%
  
  # selecting all the numeric data
  dplyr::select_if(is.factor) %>%
  
  # converting the data frame to tibble
  as_tibble()

# Get list of factors and the number of unique values
factorCols <- as.data.frame(t(df.train.factor %>% summarise_all(n_distinct))) %>%
  kable()

# # We are going to factor collapse factor columns with more than 4 columns
# # So there will be 5 of the original, and 1 containing 'other'
# # This is the threshold
# factorThreshold = 5
# 
# # Get a list of the factors we are going to collapse
# colsWithManyFactors <- rownames(factorCols %>% filter(V1 > factorThreshold))
# 
# # Show a summary of how many factors will be collapsed
# numberOfColsWithManyFactors = length(colsWithManyFactors)
# paste('Before cleaning, there are', numberOfColsWithManyFactors, 'factor columns with more than', 
#       factorThreshold, 'unique values')
# 
# # Collapse the affected factors in the original data (the one that already has imputation)
# ## for each factor column that we are about to collapse
# # The third column is omits the cutstomer ID and session ID
# FIRST_NON_CUST_SESSION_IDX = 3
# for (collapsedColNum in FIRST_NON_CUST_SESSION_IDX:numberOfColsWithManyFactors) {
#   
#   # The name of the column with null values
#   nameOfThisColumn <- colsWithManyFactors[collapsedColNum]
#   
#   # Get the actual data of the column with nulls
#   colWithManyFactors <- df.train[, nameOfThisColumn]
#   
#   # lumps all levels except for the n most frequent 
#   df.train.clean[, nameOfThisColumn] <- fct_lump_n(colWithManyFactors, 
#                                                    n=factorThreshold)
# }
# Check to see if the factor lumping worked
factorColsCleaned <- t(df.train.clean %>%
                         select_if(is.factor) %>%
                         summarise_all(n_distinct))
# # paste('After cleaning, there are', sum(factorColsCleaned > factorThreshold + 1, na.rm = TRUE), 
# #       "columns with more than", factorThreshold + 1, "unique values (omitting NA's)") 




# Get cleaned `numeric` and `factor` `data frames`
# After cleaning, two data sets that contain..

## Numeric data 
df.train.clean.numeric <- df.train.clean %>% select_if(is.numeric)

## Factors 
df.train.clean.factor  <- df.train.clean %>% dplyr::select(where(is.factor))



names(df.train.clean.factor)


# Missing value imps 
df.train.clean <- data.frame(df.train.clean)
names(df.train.clean)

df.train.clean$region <- ifelse(df.train.clean$region == "Unknown", NA, df.train.clean$region)
df.train.clean$metro <- ifelse(df.train.clean$metro == "Unknown", NA, df.train.clean$metro)
df.train.clean$city <- ifelse(df.train.clean$city == "Unknown", NA, df.train.clean$city)
#df.train.clean$networkDomain <- ifelse(df.train.clean$networkDomain == "No Domain", NA, df.train.clean$networkDomain)
#df.train.clean$topLevelDomain <- ifelse(df.train.clean$topLevelDomain == "No Domain", NA, df.train.clean$topLevelDomain)
#df.train.clean$campaign <- ifelse(df.train.clean$campaign == "No traffic source", NA, df.train.clean$campaign)
#df.train.clean$medium <- ifelse(df.train.clean$medium == "No traffic source", NA, df.train.clean$medium)
#df.train.clean$keyword <- ifelse(df.train.clean$keyword == "No Keyword", NA, df.train.clean$keyword)
#df.train.clean$adwordsClickInfo.page <- ifelse(df.train.clean$adwordsClickInfo.page == "No Ad Page", NA, df.train.clean$adwordsClickInfo.page)
#df.train.clean$campaign <- ifelse(df.train.clean$campaign == "No traffic source", NA, df.train.clean$campaign)
#df.train.clean$medium <- ifelse(df.train.clean$medium == "No traffic source", NA, df.train.clean$medium)



missing<-function(x) mean(is.na(x))
apply(df.train.clean,2,missing)

table(df.train.clean$region)
table(df.train.clean$metro)
table(df.train.clean$city)

library(VIM)
library(mice)
dfKNN.imp <- kNN(df.train.clean[,12:14],k=5)

df.train.clean$region <- dfKNN.imp$region
df.train.clean$metro <- dfKNN.imp$metro
df.train.clean$city <- dfKNN.imp$city




dim(df.train.clean)


# Log of revenue
df.train.clean$revenue <- log(df.train.clean$revenue + 1)

abs(min(df.train.clean$timeSinceLastVisit))
df.train.clean$timeSinceLastVisit <- df.train.clean$timeSinceLastVisit + 1
hist(df.train.clean$timeSinceLastVisit)
boxcox(df.train.clean$timeSinceLastVisit, optimize = TRUE, lambda=c(-3,7))
hist((df.train.clean$timeSinceLastVisit**0.11-1)/0.11)
df.train.clean$timeSinceLastVisit <- (df.train.clean$timeSinceLastVisit**0.11-1)/0.11


abs(min(df.train.clean$pageviews))
df.train.clean$pageviews <- df.train.clean$pageviews + 1
hist(df.train.clean$pageviews)
boxcox(df.train.clean$pageviews, optimize = TRUE, lambda=c(-3,7))
hist((df.train.clean$pageviews**-0.16-1)/-0.16)
df.train.clean$pageviews <- (df.train.clean$pageviews**-0.16-1)/-0.16


hist(df.train.clean$visitNumber)
boxcox(df.train.clean$visitNumber, optimize = TRUE, lambda=c(-3,7))
hist((df.train.clean$visitNumber**-0.57-1)/-0.57)
df.train.clean$visitNumber <- (df.train.clean$visitNumber**-0.57-1)/-0.57


plot(df.train.clean$revenue, df.train.clean$pageviews)
plot(df.train.clean$revenue, df.train.clean$visitNumber)
plot(df.train.clean$revenue, df.train.clean$timeSinceLastVisit)


dim(df.train.clean)

# timeSinceLastVisit
#find Q1, Q3, and interquartile range for values in column A
Q1 <- quantile(df.train.clean$timeSinceLastVisit, .25)
Q3 <- quantile(df.train.clean$timeSinceLastVisit, .75)
IQR <- IQR(df.train.clean$timeSinceLastVisit)

#only keep rows in dataframe that have values within 1.5*IQR of Q1 and Q3
no_outliers <- subset(df.train.clean, df.train.clean$timeSinceLastVisit > (Q1 - 1.5*IQR) & df.train.clean$timeSinceLastVisit < (Q3 + 1.5*IQR))

#view row and column count of new data frame
dim(no_outliers) 



# pageviews
#find Q1, Q3, and interquartile range for values in column A
Q1 <- quantile(no_outliers$pageviews, .25)
Q3 <- quantile(no_outliers$pageviews, .75)
IQR <- IQR(no_outliers$pageviews)

#only keep rows in dataframe that have values within 1.5*IQR of Q1 and Q3
no_outliers <- subset(no_outliers, no_outliers$pageviews > (Q1 - 1.5*IQR) & no_outliers$pageviews < (Q3 + 1.5*IQR))

#view row and column count of new data frame
dim(no_outliers) 


# visitNumber
#find Q1, Q3, and interquartile range for values in column A
Q1 <- quantile(no_outliers$visitNumber, .25)
Q1
Q3 <- quantile(no_outliers$visitNumber, .75)
Q3
IQR <- IQR(no_outliers$visitNumber)
IQR

#only keep rows in dataframe that have values within 1.5*IQR of Q1 and Q3
no_outliers <- subset(no_outliers, no_outliers$visitNumber > (Q1 - 1.5*IQR) & no_outliers$visitNumber < (Q3 + 1.5*IQR))

#view row and column count of new data frame
dim(no_outliers) 


# revenue
#find Q1, Q3, and interquartile range for values in column A

Q1 <- quantile(no_outliers$revenue, .005)
Q1
Q3 <- quantile(no_outliers$revenue, .98)
Q3
IQR <- IQR(no_outliers$revenue)
IQR
#only keep rows in dataframe that have values within 1.5*IQR of Q1 and Q3
#no_outliers <- subset(no_outliers, no_outliers$revenue > 0) #(Q3 + 1.5*IQR))
#no_outliers <- subset(no_outliers, no_outliers$revenue > (Q1 - 1.5*IQR) & no_outliers$revenue < (Q3 + 1.5*IQR))


#view row and column count of new data frame
dim(no_outliers) 









# I will be using this df for modeling
df.squeaky.clean <- data.frame(no_outliers)



#ols <- lm(revenue ~ timeSinceLastVisit+pageviews+referralPath+deviceCategory+visitNumber+region+metro+city,data = df.squeaky.clean)
ols <- lm(revenue ~ .,data = df.squeaky.clean)

summary(ols)
plot(ols)




names(df.squeaky.clean)
df.squeaky.clean.numeric <- df.squeaky.clean %>% select_if(is.numeric)

# Perform PCA
# Principal component analysis on numeric data
pc.train <- prcomp(df.squeaky.clean.numeric %>% dplyr::select(-revenue), # do not include response var
                   center = TRUE, # Mean centered
                   scale  = TRUE  # Z-Score standardized
)
# See first 10 cumulative proportions
pc.train.summary <- summary(pc.train)
pc.train.summary



# Now we choose number of PC's that explain 75% of the variation
# Note this threshold is just a judgement call. No significance behind 75%
cumPropThreshold = 0.70 # The threshold
numPCs <- sum(pc.train.summary$importance['Cumulative Proportion', ] < cumPropThreshold)
paste0('There are ', numPCs, ' principal components that explain up to ', cumPropThreshold*100,
       '% of the variation in the data')
chosenPCs <- as.data.frame(pc.train$x[, 1:4])


# Principal components Regression
pcr.df <- data.frame(df.squeaky.clean, chosenPCs)
pcr <- lm(revenue ~ PC1+PC2+PC3+PC4+referralPath+bounces+deviceCategory+visitNumber+region+metro+city, data=pcr.df)
summary(pcr)





# MARS
marsFit <- earth(revenue ~ timeSinceLastVisit+pageviews+referralPath+bounces+deviceCategory+visitNumber+region+metro+city,
                 data=df.squeaky.clean,
                 degree=3,nk=50,pmethod="cv",nfold=5,ncross=5)
summary(marsFit)
plot(marsFit)






df.svm <- cbind(revenue = df.train.clean.numeric$revenue,
                chosenPCs,
                df.train.clean.factor) %>%
  group_by(custId) %>%
  summarise(targetRevenue = log(sum(revenue)) + 1) %>%
  ungroup()




ctrl <- trainControl(method  = "repeatedcv", 
                     number  = 5, # 5 fold cross validation
                     repeats = 1  # 2 repeats
)



# Train and tune the SVM

fit.svm <- train(data = df.svm,#[1:100, ], # TODO - Only showing 1000 records 
                 targetRevenue ~ custId,
                 method     = "svmRadial",         # Radial kernel
                 tuneLength = 9,                   # 9 values of the cost function
                 preProc    = c("center","scale"), # Center and scale data
                 trControl  = ctrl)





# Final model?
fit.svm$finalModel

# How do the predicted vs. Actuals Compare?
# predictedVsObserved(observed  = log(df.svm$SalePrice),
#                     predicted = predict(fit.svm, df.svm),
#                     modelName = 'SVM')

# Gather key diagnostics for summary table
# Get the RMSE and R Squared of the model
hyperparameters.svm = list('C' = fit.svm[["finalModel"]]@param[["C"]],
                           'Epsilon' = fit.svm[["finalModel"]]@param[["epsilon"]])

keyDiagnostics.svm <- data.frame(Model    = 'SVM',
                                 Notes    = 'caret and svmRadial',
                                 Hyperparameters = paste('C =', hyperparameters.svm$C, ',',
                                                         'Epsilon =', hyperparameters.svm$Epsilon)
)
keyDiagnostics.svm <- cbind(keyDiagnostics.svm,
                            fit.svm$results %>% 
                              filter(C == hyperparameters.svm$C) %>%
                              dplyr::select(RMSE, Rsquared)
)
# Show output
keyDiagnostics.svm %>% knitr::kable()








# Add the key diagnostics here
rbind(
  keyDiagnostics.svm
) %>%
  
  # Round to 4 digits across numeric data
  mutate_if(is.numeric, round, digits = 4) %>%
  
  # Spit out kable table
  kable()
















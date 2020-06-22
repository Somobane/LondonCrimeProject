#Memory 
memory.limit()
memory.limit(size=560000)
gc()
rm()


# Data Loading
################################
# Create London Crime Data Set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(splitstackshape)) install.packages("splitstackshape")
if(!require(DT)) install.packages("DT")
if(!require(lubridate)) install.packages("lubridate")
if(!require(ggpubr)) install.packages("ggpubr")
if(!require(patchwork)) install.packages("patchwork")
if(!require(hrbrthemes)) install.packages("hrbrthemes")
if(!require(scales)) install.packages("scales")
if(!require(tidytext)) install.packages("tidytext")
if(!require(ggalt))install.packages("ggalt")
if(!require(purrr))install.packages("purrr")
if(!require(randomForest))install.packages("randomForest")
# Libraries
library(tidyverse)
library(caret)
library(data.table)
library(splitstackshape)
library(DT)
library(lubridate)
library(ggpubr)    ## Extra visualizations and themes
library(patchwork) ## Patch visualizations together
library(hrbrthemes)## extra themes and formatting
library(scales)    ## For formatting numeric variables
library(tidytext)  ## Reordering within facets in ggplot2
library(ggalt)     ## Extra visualizations
library(purrr)
library(randomForest) 

##Data Load
## Source: Kaggels - https://www.kaggle.com/jboysen/london-crime
# Date has been constructed considering the 1 day of the month
london_crimes <- read_csv("C:\\Somosree_BackUp\\Somosree\\DataScience\\Harvard\\Capstone\\LondonCrimeProject\\LondonCrimeProject\\london_crime_by_lsoa.csv",trim_ws = TRUE) 
london_crimes <- london_crimes %>% mutate(Display_Date = as.Date(paste(london_crimes$year, london_crimes$month, 1, sep = "-")))
## Show the first 6 rows
head(london_crimes)

# Validation set will be 10% of data
set.seed(1, sample.kind="Rounding")

test_index <- createDataPartition(y = london_crimes$value, times = 1, p = 0.1, list = FALSE)
edx_london_crimes <- london_crimes[-test_index,]
temp_london_crimes <- london_crimes[test_index,]

# Make sure lsoa_code is avaible in main edx_london_crimes
validation <- temp_london_crimes %>% 
  semi_join(edx_london_crimes, by = "lsoa_code") 

# Add rows removed from validation set back into edx_london_crimes set
removed <- anti_join(temp_london_crimes, validation)
edx_london_crimes <- rbind(edx_london_crimes, removed)

head(edx_london_crimes)


#Data Visualization and Exploration
#Quantitative Analysis: Crime Rate , Borough across 2008-2016
#1. Yearly Crime Count and Percentage Variation
#Tabular Representation
edx_Yearly_Crime_Count <- edx_london_crimes %>% 
  group_by(Year = year) %>%
  summarise(CrimeCount=sum(value,na.rm = TRUE)) %>%   
  ungroup() %>% mutate(Percent_Crime_Variation = (CrimeCount-lag(CrimeCount))/lag(CrimeCount),
                       Percent_Crime_Variation=replace_na(Percent_Crime_Variation,0)) %>% 
  arrange(desc(CrimeCount))

datatable(edx_Yearly_Crime_Count, rownames = FALSE, filter="top", options = list(pageLength = 50, scrollX=T) ) %>%
  formatRound('CrimeCount',digits=0, interval = 3, mark = ",") %>%
  formatRound('Percent_Crime_Variation',digits=3, interval = 3, mark = ",")
#Graphical Representation (Bar Graph)
edx_Yearly_Crime_Count <- edx_london_crimes %>% 
  group_by(Year = floor_date(Display_Date,unit = "year")) %>%
  summarise(CrimeCount=sum(value,na.rm = TRUE)) %>%   
  ungroup() %>% mutate(Percent_Crime_Variation = (CrimeCount-lag(CrimeCount))/lag(CrimeCount),
                       Percent_Crime_Variation=replace_na(Percent_Crime_Variation,0)) 

edx_Yearly_Crime_Count%>%   
  ggplot(aes(Year,CrimeCount))+               
  geom_bar(stat="identity",fill="skyblue",color="black")+        
  geom_line(color="brown",size=1.5,linetype="dashed")+     
  geom_text(aes(label=percent(Percent_Crime_Variation)),vjust=-1,color="black",fontface="bold")+  geom_text(aes(label=comma(CrimeCount)),vjust=1,fontface="bold",color="white")+ 
  scale_y_comma(expand = c(0,0),limits = c(0,800000))+         
  scale_x_date(breaks = "year",date_labels ="%Y")+         
  theme_classic()+                                             
  labs(title = "Total Crime Count in London over the years")
#Conclusion: Crime Count is least in 2014

#2. Crime Analysis borough level
edx_london_crimes_borough <- edx_london_crimes %>%                                     
  group_by(borough) %>% 
  summarise(CrimeCount=sum(value))%>%
  arrange(desc(CrimeCount))%>% 
  ungroup()
#Tabular Representation
datatable(edx_london_crimes_borough, rownames = FALSE, filter="top", options = list(pageLength = 50, scrollX=T) ) %>%
  formatRound('CrimeCount',digits=0, interval = 3, mark = ",") 
#Graphical Representation (Bar Graph)
edx_london_crimes_borough %>% 
  ggplot(aes(reorder(borough,CrimeCount),CrimeCount))+
  geom_bar(stat = "identity",aes(fill=borough),color="black")+
  coord_flip()+
  scale_y_comma()+
  geom_text(aes(label=comma(CrimeCount)),hjust=1)+
  theme_classic()+
  theme(legend.position = "none")+
  labs(x=" ",y=" ",title = "Total Crimes for boroughs from 2008-2016 ")

#Conclusion:WestMinister has the highest crime rate

#3. Crime Rate evolution over the year 2008 -2016 segment by Borough

edx_london_crimes_evolution <-edx_london_crimes %>% 
  group_by(borough) %>% summarise(Crimes_2008=sum(value[year(Display_Date)==2008]),Crimes_2016=sum(value[year(Display_Date)==2016])) %>% 
  ungroup() %>% 
  mutate(CrimeRateIncPct=(Crimes_2016-Crimes_2008)/Crimes_2016) %>%
  arrange(desc(CrimeRateIncPct)) 

#Tabular Representation
datatable(edx_london_crimes_evolution, rownames = FALSE, filter="top", options = list(pageLength = 50, scrollX=T) ) %>%
  formatRound('CrimeRateIncPct',digits=3, interval = 3, mark = ",")

#Graphical Representation (Bar Graph)
edx_london_crimes_evolution%>%      
  ggplot(aes(reorder(borough,CrimeRateIncPct),CrimeRateIncPct))+                       
  geom_bar(stat = "identity",aes(fill=borough),color="black")+               
  coord_flip()+
  scale_y_continuous(labels = percent_format())+                             
  geom_text(aes(label=percent(CrimeRateIncPct)),hjust=1)+
  theme_classic()+
  theme(legend.position = "none")+                                           
  labs(x=" ",y="Percentage Change ",title = "Percentage Change in Crimes from 2008-2016")
#Conclusion: London has highest Crime Percentage incrase from 2008-2016 whereas WestMinister
#which has the highest crimes doesn't shoe an increase in crime rate

#4.  Year wise Highest Crimes nos. segmented across Borough 
edx_Max_Crimes_Borough_Year <- edx_london_crimes %>% 
  group_by(borough,Year = year) %>% 
  summarise(CrimeCount=sum(value)) %>%
  ungroup() %>% 
  group_by(borough) %>%
  filter(CrimeCount==max(CrimeCount)) %>%  
  ungroup() %>% 
  arrange(desc(CrimeCount))

#Tabular Representation
datatable(edx_Max_Crimes_Borough_Year, rownames = FALSE, filter="top", options = list(pageLength = 50, scrollX=T) ) 

#Graphical Representation (Point Graph)
edx_Max_Crimes_Borough_Year %>%mutate(boroughMaxYearCrime = paste0(borough,"-","(",Year,")"))%>%                                                                    
  ggplot(aes(reorder(boroughMaxYearCrime,CrimeCount),CrimeCount))+
  geom_point()+
  scale_y_comma()+
  coord_flip()+
  geom_text(aes(label=comma(CrimeCount)),hjust=1)+
  theme_classic()+
  theme(legend.position = "none")+
  labs(title = "Max Crimes for each Borough",x="Borough and year of max Crimes ",y="Crime Count")

#Tabular Representation to visualize the year 
#which has the maximum no. of Boroughs with highest Crime Count
edx_Boroughs_with_max_incidents <- edx_london_crimes %>% 
  group_by(borough,Year = year) %>% 
  summarise(CrimeCount=sum(value)) %>%
  ungroup() %>% 
  group_by(borough) %>%
  filter(CrimeCount==max(CrimeCount)) %>%  
  ungroup() %>% 
  count(Year,sort = TRUE,name = "Boroughs_with_max_incidents")%>%
  arrange(desc(Boroughs_with_max_incidents))
datatable(edx_Boroughs_with_max_incidents, rownames = FALSE, filter="top", options = list(pageLength = 50, scrollX=T) ) 

#Conclusion: 2016 has maximum number of Boroughs with highest crime count over the 
#span 2008-2016

#5. Contribution of Major Ctaegory in Crimes
#Tabular Representation
head(edx_Maj_Cat_Crimes)
edx_Maj_Cat_Crimes <- edx_london_crimes %>% 
  group_by(major_category) %>%
  summarize(CrimeCount = n()) %>%
  arrange(desc(CrimeCount)) %>%
  ungroup() 

datatable(edx_Maj_Cat_Crimes, rownames = FALSE, filter="top", options = list(pageLength = 50, scrollX=T)) 

#Conclusion: Theft and Handling and Violence Against the Person are major contributor for the Crimes from 2008-2016

#Graphical Representation (Yearly Timeline) (Line Graph)
edx_london_crimes %>% 
  group_by(Yearly=year,major_category) %>% 
  summarise(CrimeCount=sum(value,na.rm = TRUE)) %>% 
  ggplot(aes(Yearly,CrimeCount))+
  geom_line(aes(color=major_category),size=0.75)+
  theme_pubclean()+
  scale_y_comma()+
  expand_limits(y=0)+ 
  facet_wrap(~major_category,scales = "free")+
  labs(y="Crime Count",x=" ")+
  theme(legend.position = "none",strip.background = element_rect(fill="steelblue"),strip.text=element_text(color = "white",face="bold"))

#Conclusion: There is steady decrease of the crimes for all major Category except - "Violence Against the Person" , "Other Noticeable Offences".
#"Theft and Handling" has a steady rate whereas "Sexual Offences" and "Fraud or Forgery" is more or less flattened after 2008.

#Impact of Minor Category in Major Category
edx_london_crimes_Maj_Min_Category <-edx_london_crimes %>% 
  group_by(major_category,minor_category) %>% 
  summarise(CrimeCount=sum(value)) %>%
  arrange(desc(CrimeCount)) %>%
  ungroup() 

#Tabular Representation
datatable(edx_london_crimes_Maj_Min_Category, rownames = FALSE, filter="top", options = list(pageLength = 50, scrollX=T) ) 

#Graphical Representation (Bar Graph)
edx_london_crimes_Maj_Min_Category%>% 
  mutate(minor_category=reorder_within(minor_category,CrimeCount,major_category)) %>%
  ggplot(aes(minor_category,CrimeCount))+
  geom_bar(aes(fill=minor_category),stat = "identity",color="black")+
  coord_flip()+
  scale_x_reordered()+
  scale_y_comma()+
  geom_text(aes(label=comma(CrimeCount)),hjust=1)+
  theme_classic()+
  theme(legend.position = "none")+
  facet_wrap(~major_category,scales ="free")+
  labs(x=" ",y=" ",title = "Count by Minor Category",subtitle = "Seggreggated by Major Category")+
  theme(legend.position = "none",strip.background = element_rect(fill="orange"),strip.text=element_text(color = "white",face="bold"))

#Machine Learning Algorithm
#1. Linear regression

## Major category : "Theft and Handling" & "Violence Against the Person"is one of the major contributor in Crime Count  for London 2008-2016
## Create a linear Regression Model to access the crime count for the Major Category "Theft and Handling","Violence Against the Person"

edx_Maj_Cat <- as.numeric(edx_london_crimes$major_category %in% c("Theft and Handling","Violence Against the Person"))

lm_fit_major_category <- mutate(edx_london_crimes, y = edx_Maj_Cat) %>% lm(y~ value, data = .)
p_hat_major_catagory <- predict(lm_fit_major_category, validation)

summary(lm_fit_major_category)
#Coefficients
coefs <- tidy(lm_fit_major_category, conf.int = TRUE)

#Graphical Representation
edx_london_crimes %>% 
  mutate(x = value) %>%
  group_by(x) %>%
  summarize(y = mean(edx_Maj_Cat)) %>%
  ggplot(aes(x, y)) +
  geom_point() + 
  geom_abline(intercept = lm_fit_major_category$coef[1], slope = lm_fit_major_category$coef[2])

##The relationship is linear

##RMSE Calculation
MSE <- function(true_count, predicted_count){
  sqrt(mean((true_count - predicted_count)^2))
}

#Choose Lambda Values for tuning
lambdas <- seq(0,5,.5)
rmses <- sapply(lambdas, function(l){
  mu <- mean(edx_london_crimes$value)
  
  b_i <- edx_london_crimes %>%
    group_by(lsoa_code) %>%
    summarize(b_i = sum(value - mu)/(n() + l))
  
  b_u <- edx_london_crimes %>%
    left_join(b_i, by='lsoa_code') %>% 
    group_by(major_category) %>%
    summarize(b_u = sum(value - b_i - mu)/(n() +l))
  
  predicted_count <- edx_london_crimes %>%
    left_join(b_i, by = "lsoa_code") %>%
    left_join(b_u, by = "major_category") %>%
    mutate(pred = mu + b_i +  b_u) %>% .$pred
  
  return(RMSE(predicted_count, edx_london_crimes$value))
})

qplot(lambdas, rmses)
lambda <- lambdas[which.min(rmses)]
paste('Optimal RMSE of',min(rmses),'is achieved with Lambda',lambda)
# Predicting the validation set

mu <- mean(validation$value)
l <- lambda
b_i <- validation %>%
  group_by(lsoa_code) %>%
  summarize(b_i = sum(value - mu)/(n() + l))

b_u <- validation %>%
  left_join(b_i, by='lsoa_code') %>% 
  group_by(major_category) %>%
  summarize(b_u = sum(value - b_i - mu)/(n() +l))

predicted_count <- validation %>%
  left_join(b_i, by = "lsoa_code") %>%
  left_join(b_u, by = "major_category") %>%
  mutate(pred = mu + b_i +  b_u) %>% .$pred

RMSE(predicted_count, validation$value)

#RMSE graphical relationship depicts a linear model.

##KNN

#Create Train and Test Data Set
set.seed(1, sample.kind="Rounding")


edx_Knn_london_crimes <- london_crimes %>% 
  filter(major_category %in% c("Theft and Handling","Violence Against the Person")) %>% group_by(year,major_category)%>% 
  summarise(crimeCount = sum(value)) %>% ungroup() 

#Adding noise to handle large data set
x <- edx_Knn_london_crimes$crimeCount
corrupt <- rbinom(length(x),1,0.4)    # choose an average of 40% to corrupt at random
corrupt <- as.logical(corrupt)
noise <- rnorm(sum(corrupt),1000,200) # generate the noise to add
edx_Knn_london_crimes$crimeCount[corrupt] <- x[corrupt] + noise      #


test_index <- createDataPartition(edx_Knn_london_crimes$major_category, times = 1, p = 0.3, list = FALSE)
test_crime_set <- as.data.frame(edx_Knn_london_crimes[test_index, ])
train_crime_set <- as.data.frame(edx_Knn_london_crimes[-test_index, ])

ks <- seq(9, 27,3)
F_1 <- sapply(ks, function(k){
knn_fit <- knn3(as.factor(major_category) ~ as.numeric(crimeCount), data = train_crime_set, k = k, use.all = FALSE)
y_hat <- predict(knn_fit, test_crime_set, type = "class") %>% 
factor(levels = levels(as.factor(train_crime_set$major_category)))
F_meas(data = y_hat, reference = as.factor(test_crime_set$major_category))
})

max(F_1)
ks[which.max(F_1)]

#F1 Score : 1 (Best Fit)

#Random Forest
train_rf <- randomForest(as.factor(major_category) ~ ., data=train_crime_set)
confusionMatrix(predict(train_rf, test_crime_set), as.factor(test_crime_set$major_category))$overall["Accuracy"] 



# Air France Business Case - Team 2

#################################
#Loading Packages
#################################
library(readxl)       # package for loading data
library(data.table)   # package for creating a table
library(dplyr)        # package for data manipulation in tables
library(plotly)       # package for plotting
library(fastDummies)  # package for creating dummies
library(writexl)      # package for saving excel files

#################################
# Step 1 - Understanding the data
#################################

# loading data and mappings
data_AF <- read_excel("Hult/R/Air France/Air France Case Spreadsheet Supplement.xls", 
                      sheet = "DoubleClick")

# loading our Mapping for keywords
keyword_label <- read_excel("Hult/R/Air France/Mapping.xlsx", 
                            sheet = "Keyword_label") 

#loading the destinations Air France flights to
destinations <- read_excel("Hult/R/Air France/Mapping.xlsx",
                           sheet = "Flights Air France")


# Create a new Dataframe with the allocated Budget
publisher <- c("Google - US", "Yahoo - US", "Overture - US", "MSN - US")
budget <- c(0.5, 0.2, 0.2, 0.1)
budget_df <- data.frame(publisher, budget)


# Create a new dataframe with just the US-publisher
US_publishers <- c("Google - US", "MSN - US", "Overture - US", "Yahoo - US")
data_AF_US <- data_AF[data_AF$`Publisher Name`%in% US_publishers,]
data_AF_US$count <- 1


# check if destination in keywords are actually destinations Air France flies to
# --> if not we do not want to show up in the search

key_df <- as.data.table(keyword_label)
key_df$new_label <- c()

# check if the city is in the keyword --> loop over all the cities AF is flying to
for(city in 1:nrow(destinations)){
  
  # primary df saves the rows that have the city in their keyword and only takes the group "Flight to destination"
  primary_df <- key_df[Keyword %ilike% destinations$City[city]] 
  primary_df %>% filter(grepl("Flight to destination", Label))
  
  # if we have a match for that city change the label to "airfrance_destination"
  if(nrow(primary_df > 0)){
    # iterate over keyword and primary dataframe to find matching keys 
    for(i in 1:nrow(key_df)){
      for(j in 1:nrow(primary_df)){
        if(key_df$Label[i] == primary_df$Label[j]){
          key_df$new_label[i] <- "airfrance_destination"
        }else{
          key_df$new_label[i] <- key_df$Label[i]
        } # closing the else
      } # closing the j-loop
    } # closing the for i-loop
  } # closing the if for nrow of primary_df
} # closing the city loop

# check if all the destination changes to airfrance_destination
table(key_df$new_label)

# --> all the "Flight do destination" changed to "airfrance_destination", so we can be sure
# all the cities looked for are actually air france destinations and we do not spend money
# on cities we do not fly to



#################################
# Step 2 - Massaging the data
#################################

# create a column for Net Revenue
data_AF_US$Net_Revenue <- data_AF_US$Amount-data_AF_US$`Total Cost`


# create column for ROA
data_AF_US$ROA <- data_AF_US$Net_Revenue/data_AF_US$`Total Cost`


# create good-bad column based on ROA --> ROA > 0 means we made profit --> used for Regression later on
data_AF_US$ROA_class <- as.integer(data_AF_US$ROA >= 0)

# check if we have a booking for that ad
data_AF_US$booking <- as.integer(data_AF_US$`Total Volume of Bookings` > 0)

# Check if ad has more than 100 impressions
data_AF_US$impressions100 <- as.integer(data_AF_US$Impressions > 100)


# map keywords into keyword categories -->
# 1) All the searches with flight and cities in it --> Flight to destination
# 2) All the searches that include AirFrance or similar --> Air France
# 3) The rest falls into the category --> Flight general
data_AF_US <- merge(x=data_AF_US, y=keyword_label, by="Keyword", all.x = TRUE)


#################################
# Step 3 - Descriptive Statistics
#################################

# create a pivottable by publisher name to get insights about KPI's per publisher
Pivot_table_publisher_US <- data_AF_US %>% group_by(`Publisher Name`) %>% 
  summarise("Sum of Net Revenue" = sum(`Net_Revenue`),
            "Sum of Impressions" = sum(`Impressions`),
            "ROA" = sum(`Net_Revenue`)/sum(`Total Cost`), 
            "Click through rate" = sum(`Clicks`)/sum(`Impressions`),
            "Conversion rate" = sum(`Total Volume of Bookings`)/sum(`Clicks`)
  )


# create a pivottable by publisher name to get insights about KPI's per publisher
Pivot_table_keyword <- data_AF_US %>% group_by(Label) %>% 
  summarise("Booking percentage" = sum(booking)/sum(count)
  )


# filter for the top 5 keywords based on ROA without label Air France and high impressions
data_AF_US_top5 <- data_AF_US[-which(data_AF_US$Label == "Air France"),]
data_AF_US_top5 <- data_AF_US_top5[data_AF_US_top5$impressions100 == "1",]
data_AF_US_top5 <- head(arrange(data_AF_US_top5 ,desc(ROA)), n = 5)


# Generate a comparison of OVerture rates (unassigned) and NYC campaign
data_AF_US_NYC<- data_AF_US[data_AF_US$Campaign %in% c("Geo Targeted New York", "Unassigned") ,]
Pivot_table_NYC <- data_AF_US_NYC %>% group_by(Campaign) %>% 
  summarise("Click through rate" = sum(`Clicks`)/sum(`Impressions`),
            "Conversion rate" = sum(`Total Volume of Bookings`)/sum(`Clicks`)
  )

# translate unassigned into Overture
Pivot_table_NYC$Campaign[Pivot_table_NYC$Campaign == "Unassigned"] <- "Overture - US"


# create a pivottable by publisher name to get insights about KPI's per publisher
pivot_table_flights <- data_AF_US[data_AF_US$Label %in% c("Flight general", "Flight to location") ,]
pivot_table_flights <- pivot_table_flights %>% group_by(Label) %>% 
  summarise("ROA" = sum(`Net_Revenue`)/sum(`Total Cost`), 
            "Click through rate" = sum(`Clicks`)/sum(`Impressions`),
            "Conversion rate" = sum(`Total Volume of Bookings`)/sum(`Clicks`),
            "Booking percentage" = sum(booking)/sum(count),
            "Probability of Booking" = sum(`Total Volume of Bookings`)/sum(`Impressions`)
  )

#write_xlsx(pivot_table_flights,"C:/Users/Tim/Documents/Hult/R/Air France/Charts/flights.xlsx")


#################################
# Step 4 - Predictive Modeling
#################################

# Create dummy variable
data_AF_US <- dummy_cols(
                  data_AF_US,
                  select_columns = c("Publisher Name", "Label", "Match Type")
                  )


# Create regression to predict if ROA will be positive
AF_regression <- glm(ROA_class~`Click Charges`+ `Label_Air France`+`Label_Flight general`,
                     data=data_AF_US, 
                     family="binomial"
                     )

# check for p values  
# summary(AF_regression)

# make predictions to see patterns in positive cases
data_AF_US$predictions <- as.integer(predict(AF_regression, data_AF_US, type="response")>0.5)


#################################
# Step 5 - Dashboarding
#################################


# ROA by Publisher
plot_ly(data = Pivot_table_publisher_US, 
        x = Pivot_table_publisher_US$`Publisher Name`,
        y = Pivot_table_publisher_US$ROA,
        color = "#E04834",
        type = "bar") %>% 
  layout(title = list(text = "ROA per Publisher", 
                      y = 0.95,
                      font = list(family = "Arial Black", 
                                  size = 25
                                )
                      ),
        yaxis = list(title = "ROA", 
                     showgrid = FALSE
                     )
        )

# Conversion rate vs Click through rate (in %) (size = Impressions)
plot_ly(data = Pivot_table_publisher_US, 
        x = Pivot_table_publisher_US$`Click through rate`*100,
        y = Pivot_table_publisher_US$`Conversion rate`*100,
        size = Pivot_table_publisher_US$`Sum of Impressions`,
        mode = "markers",
        marker = list(sizemode = "diameter",
                      color = c("#ff1f00", "#802211", "#f97c66", "#835047"),
                      line = list(width = 0)
                      ),
        type = "scatter") %>% 
  layout(title = list(text = "Impressions per Publisher", 
                      y = 0.97,
                      font = list(family = "Arial Black", size = 25)
                      ),
        xaxis = list(title = "Click through rate %", 
                     showgrid = FALSE, 
                     range = c(0,7)
                     ),
        yaxis = list(title = "Conversion rate %", 
                     showgrid = FALSE, 
                     range = c(0,1.8)
                     ),
        showlegend = FALSE) %>%
  add_text(text = Pivot_table_publisher_US$`Publisher Name`, 
           textfont = list(size = 12), 
           textposition = "top")


# Create a piechart with the allocated budget
plot_ly(data = budget_df, 
        labels =~ publisher, 
        values =~ budget,
        type = "pie", 
        marker = list(colors=c("#ff1f00", "#802211", "#f97c66", "#835047")),
        showlegend=FALSE, 
        textinfo="label+percent") %>% 
  layout(title = list(text="Budget Allocation", 
                      y = 0.99,
                      font = list(family = "Arial Black", 
                                  size = 25)
                      )
         )

# % of booking by Keyword labels
plot_ly(data = Pivot_table_keyword, 
        x = Pivot_table_keyword$`Label`,
        y = Pivot_table_keyword$`Booking percentage`*100,
        color = "#E04834",
        type = "bar") %>% 
  layout(title = list(text = "One or more bookings per keyword",
                      y = 0.95,
                      font = list(family = "Arial Black", 
                                  size = 25
                      )
  ),
  yaxis = list(title = "%", 
               showgrid = FALSE
  )
  )



# ROA of top 5 keywords
plot_ly(data = data_AF_US_top5, 
        x = data_AF_US_top5$Keyword,
        y = data_AF_US_top5$ROA,
        color = "#E04834",
        type = "bar") %>% 
  layout(title = list(text = "ROA per keyword",
                      y = 0.95,
                      font = list(family = "Arial Black", 
                                  size = 25
                      )
  ),
  yaxis = list(title = "ROA", 
               showgrid = FALSE
  )
  )



# Click Charges vs. Net Revenue (Color = Regression model predictions)
plot_ly(data = data_AF_US, 
        x = data_AF_US$`Click Charges`,
        y = data_AF_US$Net_Revenue,
        mode = "markers",
        marker = list(color = data_AF_US$predictions,
                      line = list(width = 0)
        ),
        type = "scatter") %>% 
  layout(title = list(text = "Regression Output", 
                      y = 0.97,
                      font = list(family = "Arial Black", size = 25)
  ),
  xaxis = list(title = "Click Charges", 
               showgrid = FALSE
  ),
  yaxis = list(title = "Net Revenue", 
               showgrid = FALSE
  ),
  showlegend = FALSE)


# Conversion rate vs Click through rate (in %) (comparison NYC campaign and Overture)
plot_ly(data = Pivot_table_NYC, 
        x = Pivot_table_NYC$`Click through rate`*100,
        y = Pivot_table_NYC$`Conversion rate`*100,
        mode = "markers",
        marker = list(size = 15,
                      color = c("#ff1f00", "#802211", "#f97c66", "#835047"),
                      line = list(width = 0)
        ),
        type = "scatter") %>% 
  layout(title = list(text = "NYC Campaign vs. Overture", 
                      y = 0.97,
                      font = list(family = "Arial Black", size = 25)
  ),
  xaxis = list(title = "Click Through Rate %", 
               showgrid = FALSE,
               range = c(0, 6)
  ),
  yaxis = list(title = "Conversion rate %", 
               showgrid = FALSE,
               range = c(0, 1.4)
  ),
  showlegend = FALSE) %>%
  add_text(text = Pivot_table_NYC$Campaign, 
           textfont = list(size = 12), 
           textposition = "top")
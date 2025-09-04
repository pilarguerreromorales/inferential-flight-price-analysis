####DATA Exploration

fda = CLEAN_DATA_FDA

library(psych)
library(lattice)

str(fda)

fda= na.omit(fda)

fda$Airline = factor(fda$Airline)
table(fda$Airline)
options(scipen = 999)
prop.table(table(fda$Airline))
barchart(fda$Airline)


fda$day = factor(fda$day)
table(fda$day)
prop.table(table(fda$day))
barchart(fda$day)

fda$Total_Stops = factor(fda$Total_Stops)
table(fda$Total_Stops)
prop.table(table(fda$Total_Stops))
barchart(fda$Total_Stops)


fda$month = factor(fda$month)
table(fda$month)
prop.table(table(fda$month))
barchart(fda$month)


fda$Time = factor(fda$Time)
table(fda$Time)
prop.table(table(fda$Time))
barchart(fda$Time)

fda$Source = factor(fda$Source )
table(fda$Source)
prop.table(table(fda$Source ))
barchart(fda$Source )

fda$Destination = factor(fda$Destination )
table(fda$Destination)
prop.table(table(fda$Destination ))
barchart(fda$Destination )






describe(fda$Duration)
hist(fda$Duration)
boxplot(fda$Duration)
outliers = boxplot(fda$Duration)$out
outliers
which(fda$Duration %in% outliers)

describe(fda$Price)
boxplot(fda$Price)
hist(fda$Price)


FDA = CLEAN_DATA_FDA
outliers = boxplot(FDA$Price)$out
FDA <- FDA[!FDA$Price %in% outliers, ]
outliers = boxplot(FDA$Duration)$out
FDA <- FDA[!FDA$Duration %in% outliers, ]
str(FDA)



which(is.na(fda$Airline))
which(is.na(fda$day))
which(is.na(fda$month))
which(is.na(fda$Source))
which(is.na(fda$Destination))
which(is.na(fda$Price))
which(is.na(fda$Total_Stops))

################################################################################

######  MANAGING OUTLIERS


df=CLEAN_DATA_FDA_1_
library(ggplot2)

# duration and price with outliers
Duration<- lm(Price ~ Duration, data = df) # fit the model
df$predicted <- predict(Duration)   # Save the predicted values
df$residuals <- residuals(Duration) # Save the residual values
ggplot(df, aes(x = Duration, y = Price)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +     # regression line  
  geom_segment(aes(xend = Duration, yend = predicted), alpha = .01) +      # draw line from point to line
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()

summary(Duration)

# duration within outliers
Q1 <- quantile(df$Duration, 0.25)
Q3 <- quantile(df$Duration, 0.75)
IQR <- Q3 - Q1

upper <- Q3 + 1.5*IQR
lower <- Q1 - 1.5*IQR

df2 <- df[df$Duration >= lower & df$Duration <= upper,]

Q1a <- quantile(df2$Price, 0.25)
Q3a <- quantile(df2$Price, 0.75)
IQR <- Q3a - Q1a

upperp <- Q3a + 1.5*IQR
lowerp <- Q1a - 1.5*IQR

df2 <- df2[df2$Price >= lowerp & df2$Price <= upperp,]

Durationwithinout<- lm(Price ~ Duration, data = df2) # fit the model
df2$predicted <- predict(Durationwithinout)   # Save the predicted values
df2$residuals <- residuals(Durationwithinout) # Save the residual values
ggplot(df2, aes(x = Duration, y = Price)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +     # regression line  
  geom_segment(aes(xend = Duration, yend = predicted), alpha = .01) +      # draw line from point to line
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()

summary(Durationwithinout)
# los outliers no son significant

################################################################################

###### REGRESSION ANALYSIS


#import dataset and remove outliers
#take off outliers
FDA = CLEAN_DATA_FDA
outliers = boxplot(FDA$Price)$out
FDA <- FDA[!FDA$Price %in% outliers, ]
outliers = boxplot(FDA$Duration)$out
FDA <- FDA[!FDA$Duration %in% outliers, ]
str(FDA)

#remove na 
FDA = na.omit(FDA)

#factor appropriate variables
FDA$Airline = factor(FDA$Airline)
FDA$day = factor(FDA$day)
FDA$month = factor(FDA$month)
FDA$Source = factor(FDA$Source)
FDA$Destination = factor(FDA$Destination)
FDA$Time = factor(FDA$Time)
FDA$Total_Stops = factor(FDA$Total_Stops)

#check structure
str(FDA)

#1) Split Dataset - Model Validation - seed 2023

#75 percent for training 
#25 percent for testing 
set.seed(2023)
smp_size = round(0.75 * nrow(FDA))
check = sample(seq_len(nrow(FDA)), size = smp_size)
train = FDA[check, ]
test = FDA[-check, ]


#2) Create the best model with main terms (Simple LR, Multiple LR, Categorical Values)

#refer to column names
colnames(FDA)

#How would the full model be ? 

fullmodel= lm(Price ~ Airline + day + month+ Source + Destination + Time+ Duration + Total_Stops, data = train)
summary(fullmodel)

#not including destinations, time night and stops, which seem to not be significant 

model2= lm(Price ~ Airline + day + month+ Destination + Source + Duration, data = train)
summary(model2)
```
```{r}
model3= lm(Price ~ Airline + day + month+ Destination + Duration, data = train)
summary(model3)

#Model selection
#Test null hypothesis of whether teh full model is better at predicting variables than model3. 

anova(fullmodel, model3)

#We carried out an anova test in which the null hypothesis states that the full model and model2 have the same explainability. Because the p value of this test is very low, 0.2115 we assume that they don’t differ in capturing the variability in price, we cant reject the null hypothesis that they are equally useful . This supports our previous conclusion that we will choose the reduced model 2. 

#Validation 

test$pred = predict(object = model3, newdata = test)
SSE = sum((test$Price-test$pred)^2)
RSE = sqrt(SSE/(nrow(test)-3))
mean_error = RSE/(mean(test$Price))
mean_error

#Our mean error is low in prediction which suggest our model is mostly accurate

modelpred = lm(Price~ pred, test)
summary(modelpred)

#If we only use The preiction variable to predict price  we get a high R suqared of 0.5958.

#Residual Analysis

plot(model3)

#Test for independence of errors

#install.packages("lmtest")
library(lmtest)
dwtest (model3)

#Prediction

model3= lm(Price ~ Airline + day + month + Destination + Duration, data = train)
new = data.frame(Airline = "Air India", day = "9", month = "4", Destination = "Cochin", Duration = 1045)
#mean expected value (mean of price)
predict(object = model3, newdata = new, interval = "confidence", level = 0.95)
#predict specific value of y, more variability. 
predict(object = model3, newdata = new, interval = "prediction", level = 0.95)

#Cross Validation

library(caret)
#Define training control
train = na.omit(train)
trainControl = trainControl(method = "cv", number = 10)
# Train the model
lm = train(Price ~ Airline + day + month + Destination + Duration, data = train, method ="lm", trControl = trainControl)
# Summarize the results
lm

#####################################################################################################

####INFERENTIAL STATISTICS

library(dplyr)

#3.1 Average price for flights per airline and their class:

df %>% group_by(Airline) %>% summarize(a = mean(Price)) %>% arrange(desc(a))

#3.1.5 Understanding the relationship between the Airlines and the price

df$Airline = factor(df$Airline)
anovatest = aov(Price ~ Airline, data = df)
anovasummary = summary (anovatest)  
anovasummary

#3.2. Understanding the relationship between the number of stops and the price

df$Total_Stops = factor(df$Total_Stops)
anovatest = aov(Price ~ Total_Stops, data = df)
anovasummary = summary (anovatest)  
anovasummary

library(DescTools)

tukey = PostHocTest(anovatest, method = "hsd", conf.level = 0.95)
tukey

#3.3. Understanding the relationship of prices for flights at day and night

t.test(Price ~ Time, data = df, var.equal = TRUE)

#3.4. Understanding the effect of Source and Destination on Price

df$Source <- as.factor(df$Source)
df$Destination <- as.factor(df$Destination)
model <- lm(Price ~ Source + Destination, data = df)
summary(model)

library(ggplot2)
library(tidyverse)

#5
df$Duration = factor(df$Duration)
anovatest = aov(Price ~ Duration, data = df)
anovasummary1 = summary (anovatest)  
plot(anovatest, which = 1)

#6


ggplot(df, aes(x = day, y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Day of Month", y = "Price") +
  scale_x_continuous(breaks = 1:31)


#7

ggplot(df, aes(x = month, y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Month", y = "Price") + 
  scale_x_continuous(breaks = 3:6)

#8

df <- df %>% mutate(airline_type = ifelse(Airline %in% 
                                            c("IndiGo", "Air India", "Vistara", "Jet Airways"), "High-Cost", "Low-Cost"))

model <- lm(STOPS ~ Airline + airline_type, data = df)
summary(model)

ggplot(df, aes(x = Airline, y = STOPS, fill = airline_type)) +
  geom_boxplot() +
  labs(x = "Airline", y = "Number of Stops", fill = "Airline Type") +
  theme_bw() +
  theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1))

#9

df %>% 
  group_by(Source, Destination, Airline) %>% 
  summarize(mean_price = mean(Price), n_flights = n()) %>% 
  arrange(Source, Destination, mean_price) %>%
  mutate(acronym = str_extract_all(Airline, "[A-Z]") %>% sapply(paste, collapse = "")) %>% 
  ggplot(aes(x = acronym, y = mean_price, fill = Source)) +
  geom_col() +
  facet_grid(rows = vars(Destination)) +
  labs(x = "Airline", y = "Mean Price", fill = "Source")





















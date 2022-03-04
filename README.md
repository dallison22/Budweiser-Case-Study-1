---
title: "CaseStudy1"
author: "Aundrae Allison"
output: html_notebook
---

```{r}
#reading beer data
beers <- read.csv("https://raw.githubusercontent.com/dallison22/MSDS_6306_Doing-Data-Science/Master/Unit%208%20and%209%20Case%20Study%201/Beers.csv")
head(beers)
```

```{r}
#reading brewries data
breweries <- read.csv("https://raw.githubusercontent.com/dallison22/MSDS_6306_Doing-Data-Science/Master/Unit%208%20and%209%20Case%20Study%201/Breweries.csv")
head(breweries)
```


#### 1.How many breweries are present in each state?

```{r}
library(tidyverse)
breweries%>%
  group_by(State)%>%
  summarise(num_breweries = n())%>%
  arrange(desc(num_breweries))%>%
  ggplot(aes(x = State, y = num_breweries)) +
  geom_col(fill = "steelblue") +
  labs(title = "Number of Breweries in each State")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
```

#### 2.Merge beer data with the breweries data. Print the first 6 observations and the last six observations to check the merged file.  (RMD only, this does not need to be included in the presentation or the deck.)

```{r}
#renaming column name
colnames(beers)[5] <- "Brew_ID"
#merging data
data <- merge(beers, breweries, by = "Brew_ID")
#replacing Name.x with beerName and Name.y with breweryName
colnames(data)[2] <- "beerName"
colnames(data)[8] <- "breweryName"
```

```{r}
#printing first six observation of data
head(data, 6)
```

```{r}
#printing last six observation of data
tail(data, 6)
```

#### 3.Address the missing values in each column.

```{r}
#checking null values in data
colSums(is.na(data))
```
```{r}
#filling null values in ABV column with median value
data[is.na(data$ABV),]$ABV <- median(data$ABV, na.rm = T)
data[is.na(data$IBU),]$IBU <- median(data$IBU, na.rm = T)
```


#### 4.Compute the median alcohol content and international bitterness unit for each state. Plot a bar chart to compare.

```{r}
#calculating median alcohol content by State
median_alcohol <- data%>%
  group_by(State)%>%
  summarise(med_alcohol = median(ABV, na.rm = T))%>%
  arrange(desc(med_alcohol))
median_alcohol
```
```{r}
#plotting median alcohol content by State
 ggplot(median_alcohol, aes(x = State, y = med_alcohol)) +
  geom_col(fill = "steelblue") +
  labs(title = "Median Alcohol Content By State")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
```

```{r}
#calculating median international bitterness unit by State
median_bitterness <- data%>%
  group_by(State)%>%
  summarise(med_bitterness = median(IBU, na.rm = T))%>%
  arrange(desc(med_bitterness))
median_bitterness
```
```{r}
#plotting median international bitterness unit by State
 ggplot(median_bitterness, aes(x = State, y = med_bitterness)) +
  geom_col(fill = "steelblue") +
  labs(title = "Median International Bitterness unit By State")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
```

#### 5.Which state has the maximum alcoholic (ABV) beer? Which state has the most bitter (IBU) beer?
Ans) From above summaries and plots, I observed that State DC (District of Colombia) has maximum alcohol (ABV) beer and State WV (West Virginia) has most bitter (IBU) beer.


#### 6.Comment on the summary statistics and distribution of the ABV variable.

```{r}
summary(data$ABV)
```

```{r}
ggplot(data, aes(x = ABV)) +
  geom_histogram(fill = "steelblue", bins = 30) +
  labs(title = "Distribution of ABV")
```

From above summary and plot, I observed that the distribution of ABV is almost normally distributed with mean = 0.056 and median of 0.05968. The values of ABV range from 0.0010 to 0.12800. There are many outliers with lowst and highest values of ABV.


#### 7.Is there an apparent relationship between the bitterness of the beer and its alcoholic content? Draw a scatter plot.Make your best judgment of a relationship and EXPLAIN your answer.

```{r}
ggplot(data, aes(x = ABV, y = IBU)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", formula = y ~ x) +
  labs(title = "Relation b/w Bear's Bitterness and Alcoholic Content")
```
```{r}
cor(data$ABV, data$IBU)
```

From above plot, I observed that the relation between bittereness of bear and it's alcoholic contnet is positive, as the value of bitterness of bear increase with increase of it's alcoholic content. Since the points are spreaded out and not clustered around the regression line so the relation between these two variables is moderate as evident by the correlation value of 0.514. There are some outliers in the data too.


#### 8.  Budweiser would also like to investigate the difference with respect to IBU and ABV between IPAs (India Pale Ales) and other types of Ale (any beer with “Ale” in its name other than IPA).  You decide to use KNN classification to investigate this relationship.  Provide statistical evidence one way or the other. You can of course assume your audience is comfortable with percentages … KNN is very easy to understand conceptually.


```{r}
#separating data for IPA or Ale only
library(stringr)
data2 <- data[str_detect(data$Style, "IPA|Ale"),]
```

```{r}
#selecting only ABV, IBU, and Style columns
data3 <- data2[, c(4,5,6)]
#converting style values to IPA and ALE only
data3$Style <- ifelse(str_detect(data3$Style, "Ale"), "ALE", "IPA")
table(data3$Style)
```
```{r}
#splitting data into 75% training and 25% testing
set.seed(101) 
sample <- sample.int(n = nrow(data3), size = floor(.75*nrow(data3)), replace = F)
train <- data3[sample, ]
test  <- data3[-sample, ]
```



```{r}
#applying knn classification
library(class)
knn <- knn(train[,c(1,2)], test[,c(1,2)], cl = train$Style, prob = TRUE)
#checking accuracy
sum(knn == test$Style)/length(test$Style)
```

```{r}
#confusion table
table(test$Style, knn)
```

From above outputs, I observed that the accuracy of knn for predicting style is 0.8125, which is pretty good. This means that the model fits and generalize well to the data and can be used for future predictions.


```{r}
data
```



```{r}
table(data$Ounces)
```


#9. Knock their socks off!  Find one other useful inference from the data that you feel Budweiser may be able to find value in.  You must convince them why it is important and back up your conviction with appropriate statistical evidence. 

We can find out which states use larger bottles of beer and also with high value of IBU for marketing purposes.

```{r}
ggplot(data, aes(x = State, y = IBU, color = Ounces)) +
  geom_count() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
```

From above plot, I observed that States, CA and AZ has largest size of bottles with higher value of IBU too. In charts the bubbles of big size represents highest value of IBU and lighter color represent bigger bottle size.

#### Conclusion
To conclude that, US has largest collection of breweries varry by states. Some states have multiple kind of breweries and some have only one kind of brewery. All the analysis and visualizations that are performed in this case study can be used to increase the sale of bear products, and can also be used to find out which state has highest value of IBU and ABV and which states used largest size of bottles as compare to other states. I also find out that most of the bears have ABV value of 0.04 to 0.08. And also that there is a positive relation between IBU and ABV. All these informations can be used to make useful decisions in future. For example states that already have high number of breweries may not be primary candidates for shipments of beer from other States. 

# R project "blinkit"

#loading the csv file

library(tidyverse)
library(dplyr)
library(ggplot2)

blinkit_data<-read.csv("C:/Users/Niki/OneDrive/Desktop/Blinkit .csv.file.csv")
View(blinkit_data)

#view first 10 rows
View(head(blinkit_data,10))

#view last 10 rows
view(tail(blinkit_data,10))

#view structure 
str(blinkit_data)

#summary statistics of dataset
print(summary(blinkit_data))

#check missing values
colSums(is.na(blinkit_data))

#remove duplicates
blinkit_clean<-blinkit_data%>%distinct()
view(blinkit_clean)



# Questions and corresponding R code:

# 1. What is the total sales amount for each Item Type?
ques1<-blinkit_data %>%
  group_by(Item.Type) %>%
  summarise(Total.Sales = sum(Sales))

#view the result
View(ques1)

# 2. What is the average sales amount for each Outlet Type?
ques2<-blinkit_data%>%
  group_by(Outlet.Type) %>%
  summarise(Average.Sales = mean(Sales))

#print the result 
print(ques2)

# 3. Create a bar plot showing total sales for each Item Type.
ggplot(blinkit_data, aes(x = Item.Type, y = Sales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4. What is the distribution of Sales across different Outlet Sizes?
 ques3 <-blinkit_data %>%
  group_by(Outlet.Size) %>%
  summarise(Total.Sales = sum(Sales))

# 5. Create a box plot of Sales by Outlet Size.
ggplot(blinkit_data, aes(x = Outlet.Size, y = Sales)) +
  geom_boxplot(fill = "lightgreen")

# 6. Which month had the highest total sales?
 ques6 <- blinkit_data %>%
  group_by(Month) %>%
  summarise(Total.Sales = sum(Sales)) %>%
  arrange(desc(Total.Sales))
 
 # view the result
 View(ques6)

# 7. Create a line plot showing total sales for each month.
 ques7<-blinkit_data %>%
  group_by(Month) %>%
  summarise(Total.Sales = sum(Sales)) %>%
  ggplot(aes(x = Month, y = Total.Sales)) +
   
  geom_hline(color = "blue") +
  geom_point()
 
 #print the result
 print(ques7)

# 8. What is the relationship between Sales and Ratings?
ggplot(blinkit_data, aes(x = Rating, y = Sales)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm")

# 9. What is the average rating for each Outlet Type?
 ques9<-blinkit_data %>%
  group_by(Outlet.Type) %>%
  summarise(Average.Rating = mean(Rating))
 
 # View the result
 View (ques9)

# 10. Create a bar plot of average rating for each Outlet Type.
 ques10<-blinkit_data %>%
  group_by(Outlet.Type) %>%
  summarise(Average.Rating = mean(Rating)) %>%
  ggplot(aes(x = Outlet.Type, y = Average.Rating)) +
  geom_bar(stat = "identity", fill = "orange")
 
# 11. What is the total sales amount for each Outlet Location Type?
 ques11<-blinkit_data %>%
  group_by(Outlet.Location.Type) %>%
  summarise(Total.Sales = sum(Sales))
 
 #View the result
 View(head(ques11))

# 12. Create a pie chart showing the distribution of total sales 
 #across Outlet Location Types.
 ques12 <-blinkit_data %>%
  group_by(Outlet.Location.Type) %>%
  summarise(Total.Sales = sum(Sales)) %>%
  ggplot(aes(x = "", y = Total.Sales, fill = Outlet.Location.Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y")

# 13. What are the top 5 items with the highest sales?
 ques13<-blinkit_data %>%
  group_by(Item.Type) %>%
  summarise(Total.Sales = sum(Sales)) %>%
  arrange(desc(Total.Sales)) %>%
  head(5)
 
 #view the result
 View(ques13)

# 14. Create a histogram of Sales.
ggplot( blinkit_data, aes(x = Sales)) +
  geom_histogram(bins = 20, fill = "purple", color = "black")

# 15. Which Outlet Size has the highest average rating?
 ques15<-blinkit_data %>%
  group_by(Outlet.Size) %>%
  summarise(Average.Rating = mean(Rating)) %>%
  arrange(desc(Average.Rating))

# 16. Create a density plot of Ratings.
ggplot(blinkit_data, aes(x = Rating)) +
  geom_density(fill = "lightblue")

# 17. Are there any differences in Sales between Low Fat and Regular items?
 ques17<-blinkit_data %>%
  group_by(Item.Fat.Content) %>%
  summarise(Average.Sales = mean(Sales))
 
 # view the result
 View(ques17)

# 18. Create a faceted bar plot of Sales by Item Type for each Outlet Type.
ggplot(blinkit_data, aes(x = Item.Type, y = Sales, fill = Outlet.Type)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Outlet.Type) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 19. Which combination of Outlet Type and Location Type has the highest sales?
 ques19<-blinkit_data %>%
  group_by(Outlet.Type, Outlet.Location.Type) %>%
  summarise(Total.Sales = sum(Sales)) %>%
  arrange(desc(Total.Sales))
 
 # View the result
 View(ques19)

# 20. Create a scatter plot of Sales vs. Ratings, colored by Outlet Size.
ggplot(blinkit_data, aes(x = Rating, y = Sales, color = Outlet.Size)) +
  geom_point() +
  theme_minimal()


	
	
	
		
		
	
	

library(dplyr)
library(ggplot2)
library(tidyr)
library(plotrix)
library(magrittr)



setwd("C:/Users/ACER/Downloads/R/R/R case study 3 (Visualization)")

sales = read.csv("SalesData.csv")

# 1.	Compare Sales by region for 2016 with 2015 using bar chart

data1 = summarise(group_by(sales , Region) , sales_2015 = sum(Sales2015), sales_2016 = sum(Sales2016))

data_plot = gather(data1,key = Year, value = Sales,-Region )
data_plot$Sales = round(data_plot$Sales,1)

ggplot(data_plot,aes(Region,Sales, fill = Year,label = Sales)) + geom_bar(stat = "identity",position = "dodge") + 
  geom_text(size = 4) + xlab('Region') +ylab('Sales') + ggtitle("Comparision of Sales by Region")



# 2.Pie charts for sales for each region in 2016

data_2016 = data1[,c(1,3)]
pie(data_2016$sales_2016 , labels =data_2016$Region )

# 3.Compare sales of 2015 and 2016 with Region and Tiers
df = summarise(group_by(sales , Region , Tier) , totalsales_2015 = sum(Sales2015), totalsales_2016 = sum(Sales2016))
      
data_plot1 = gather(df,key = Year,value = Sales,-c(Region,Tier)) 

ggplot(data_plot1,aes(Tier,Sales,fill = Year)) + geom_bar(stat = "identity", position = "dodge") + facet_wrap(~ Region)+ggtitle("Comparision of Sales by Region and Tiers")



#4.	In East region, which state registered a decline in 
#2016 as compared to 2015?

df_1 = summarise(filter(group_by(sales,State),Region =="East") , totalsales_2015 = sum(Sales2015), totalsales_2016 = sum(Sales2016))

data_plot2 = gather(df_1,key = Year, value = Sales,-State)

ggplot(data_plot2,aes(State,Sales,fill = Year)) + geom_bar(stat = "identity",position = "dodge") +ggtitle("Comparision of Sales by State")

## NY is the only state 

#5.	In all the High tier, which Division saw a decline in number of units sold in 2016 compared to 2015?

df_2 = summarise(filter(group_by(sales,Division),Tier =="High") , totalsales_2015 = sum(Units2015), totalsales_2016 = sum(Units2016))
data_plot3 = gather(df_2 ,key = Year, value = Sales,-Division)

data_plot3_final = ggplot(data_plot3,aes(Division,Sales, fill = Year)) + geom_bar(stat = "identity",position = "dodge")

data_plot3_final + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Comparision of Sales by Division")

# none division see decline 

# 6.	Create a new column Qtr -
#Jan - Mar : Q1
#Apr - Jun : Q2
#Jul - Sep : Q3
#Oct - Dec : Q4

sales$Qtr = if_else(sales$Month == "Jan"|sales$Month == "Feb"|sales$Month == "Mar","Q1",
if_else(sales$Month == "Apr"|sales$Month == "May"|sales$Month == "Jun","Q2",
if_else(sales$Month == "Jul"|sales$Month == "Aug"|sales$Month == "Sep","Q3","Q4")))


# 7.	Compare Qtr wise sales in 2015 and 2016 in a bar plot

df_3 = summarise(group_by(sales,Qtr), totalsales_2015 = sum(Units2015), totalsales_2016 = sum(Units2016))
data_plot4 = gather(df_3,key = Year, value = Sales,-Qtr)

ggplot(data_plot4,aes(Qtr,Sales,fill = Year)) + geom_bar(stat = "identity",position = "dodge") + ggtitle("Comparision of Sales by Quarter")



# 8.	Determine the composition of Qtr wise sales in and 2015 with regards to all the Tiers in a pie chart.
#(Draw 4 pie charts representing a Quarter for each Tier)

q1 = summarise(filter(group_by(sales,Qtr, Tier),Qtr=="Q1" ), totalsales_2015 = sum(Units2015))
q2 = summarise(filter(group_by(sales,Qtr, Tier),Qtr=="Q2" ), totalsales_2015 = sum(Units2015))
q3 = summarise(filter(group_by(sales,Qtr, Tier),Qtr=="Q3" ), totalsales_2015 = sum(Units2015))
q4 = summarise(filter(group_by(sales,Qtr, Tier),Qtr=="Q4" ), totalsales_2015 = sum(Units2015))

pie(q1$totalsales_2015, labels = q1_label, main = "Qtr 1")
q1_label = q1$Tier

q2_label = q2$Tier
pie(q2$totalsales_2015, labels = q2_label, main = "Qtr 2")



q3_label = q3$Tier
pie(q3$totalsales_2015, labels = q3_label, main = "Qtr 3")


q4_label = q4$Tier
pie(q4$totalsales_2015, labels = q4_label, main = "Qtr 4")





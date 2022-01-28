#import relevant libraries 
library(tidyverse)
library(ggplot2)
library(dplyr)
library(corrplot)
library(reshape)
library(factoextra)
library(GGally)

#get data 
df <- read.csv('C:/Users/sbak0/Downloads/marketing_campaign.csv', sep='\t')

head(df)

glimpse(df)

#missing data
sum(is.na(df))

#drop na 
df1 <- na.omit(df)

#Total_spent 
df1$Total_spent <- df1$MntFishProducts + df1$MntFruits + df1$MntGoldProds +
                   df1$MntMeatProducts + df1$MntSweetProducts + df1$MntWines

boxplot(df1$Total_spent, main='Total Spent by Customers', 
       xlab='Total', col = '#adcae6', horizontal=TRUE, notch=TRUE)+
    options(repr.plot.width=5, repr.plot.height=5, repr.plot.res=100)

#Age 
df1$Age <- 2022 - df1$Year_Birth

#Check Age outliers to see if there is any weird data
ggplot(df1, aes(1:length(ID), Age))+
    geom_point(color='#adcae6')+
    labs(x='Number of id', y='Age', title='Age Outlier')

#Three outliers are drop as it is unlikely for someone to be aove 110 
df1 <- df1[!(df1$Age > 100), ]

options(repr.plot.width=10, repr.plot.height=5, repr.plot.res=100)

#Customer Age distribution 
ggplot(df1)+
    geom_histogram(mapping=aes(Age), binwidth=4, fill='white', color='#adcae6')+
    geom_vline(aes(xintercept=mean(Age)), linetype='dashed', color='blue')+
    ggtitle('Customer Age')

#Marital_Status 
ggplot(df1, aes(x=Marital_Status,fill=Marital_Status))+
    geom_bar()+
    ggtitle('Marital Status')

#clean Martial_Status
df1$Marital_Status[df1$Marital_Status == 'Absurd'] = 'Single'
df1$Marital_Status[df1$Marital_Status == 'Alone'] = 'Single'
df1$Marital_Status[df1$Marital_Status == 'Divorced'] = 'Single'
df1$Marital_Status[df1$Marital_Status == 'Together'] = 'Married'
df1$Marital_Status[df1$Marital_Status == 'Widow'] = 'Single'
df1$Marital_Status[df1$Marital_Status == 'YOLO'] = 'Single'

unique(df1$Marital_Status)

options(repr.plot.width=6, repr.plot.height=6, repr.plot.res=100)

ggplot(df1, aes(x=Marital_Status, y=Total_spent, fill=Marital_Status))+
    geom_boxplot()+
    ggtitle('Bax plot of Marital Status VS. Total Spent')

#Total_kids 
df1$Total_kid <- df1$Kidhome + df1$Teenhome
df1$Total_kid <- factor(df1$Total_kid)

#Kids VS. Total 
ggplot(df1, aes(Total_kid, Total_spent))+
    geom_boxplot()

#Income outlier
ggplot(df1, aes(1:length(ID), Income))+
    geom_point(color='#adcae6')+
    labs(x='Number of id', y='Income', title='Income Outlier')

#Income VS. Total spending 
ggplot(df1, aes(Income, Total_spent, color=Marital_Status))+
    geom_point()+
    labs(x='Income', y='Total Spending', title='Income VS. Total Spending')

#Martial Status VS. Wines
ggplot(df1, aes(x=Marital_Status, y=MntWines, fill=Marital_Status))+
    geom_bar(stat='identity')+
    ggtitle('Marital Status VS. Wines')+
    xlab('Marital Status')+
    ylab('Wines')

#Total Kids VS. Wines
ggplot(df1, aes(x=Total_kid, y=MntWines, fill=Total_kid))+
    geom_bar(stat='identity')+
    ggtitle('Total Kids VS. Wines')+
    xlab('Total Kids')+
    ylab('Wines')

#Martial Status VS. Sweet Product
ggplot(df1, aes(x=Marital_Status, y=MntSweetProducts, fill=Marital_Status))+
    geom_bar(stat='identity')+
    ggtitle('Marital Status VS. Sweet Product')+
    xlab('Marital Status')+
    ylab('Sweet Product')

#MTotal Kid VS. Sweet Product
ggplot(df1, aes(x=Total_kid, y=MntSweetProducts, fill=Total_kid))+
    geom_bar(stat='identity')+
    ggtitle('Total Kid VS. Sweet Product')+
    xlab('Total Kid')+
    ylab('Sweet Product')

df_products = df[c('MntWines', 'MntFruits', 'MntMeatProducts', 'MntFishProducts', 'MntSweetProducts', 'MntGoldProds')]
ggpairs(df_products)

colnames(df1)

#remove unnecessary data
df2 <- select_if(df1[c(5, 10:25)], is.numeric)

#correlation map
melt(cor(df2))%>%
  ggplot(aes(X1, X2, fill=value))+
  geom_tile()+
  scale_fill_gradient2(low = "firebrick4", high = "steelblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1),
        axis.title = element_blank())

#find oprimal cluster
fviz_nbclust(df2, kmeans, method = "wss")+
    geom_vline(xintercept = 3, linetype = 2)

#cluster plot
k_model <- kmeans(df2, 3, nstart = 100)
fviz_cluster(k_model, data = df2, geom = c("point"), ellipse.type="ellipse")

#size of each group
k_model$size

df.c<-df1%>%
  mutate(cluster = k_model$cluster)

#Age in each cluster
df.c%>%
  ggplot(aes(x=as_factor(cluster), y=Age))+
  geom_boxplot()

#Income cluster
df.c%>%
    ggplot(aes(x=as_factor(cluster), y=Income))+
    geom_boxplot()

#Product cluster
df.c%>%
  select(cluster, MntWines, MntFruits, MntMeatProducts, MntSweetProducts, MntGoldProds)%>%
  melt(id='cluster')%>%
  ggplot(aes(x=as_factor(cluster), y=value))+
  geom_boxplot()+
  facet_wrap(~variable, ncol=5)

#Channels cluster
df.c%>%
  select(cluster, NumWebPurchases, NumCatalogPurchases, NumStorePurchases, NumWebVisitsMonth)%>%
  melt(id='cluster')%>%
  ggplot(aes(x=as_factor(cluster), y=value))+
  geom_boxplot()+
  facet_wrap(~variable, ncol=4)

#Purchase made with discount
df.c%>%
  ggplot(aes(x=as_factor(cluster), y=NumDealsPurchases))+
  geom_boxplot()

#Campaign cluster
df.camp<-df.c%>%
  group_by(cluster)%>%
  summarise(
    AcceptedCmp1=sum(AcceptedCmp1),
    AcceptedCmp2=sum(AcceptedCmp2),
    AcceptedCmp3=sum(AcceptedCmp3),
    AcceptedCmp4=sum(AcceptedCmp4),
    AcceptedCmp5=sum(AcceptedCmp5),
    Response=sum(Response)
  )%>%
  mutate(
    num = sum(k_model$size),
    AcceptedCmp1=AcceptedCmp1/num,
    AcceptedCmp2=AcceptedCmp2/num,
    AcceptedCmp3=AcceptedCmp3/num,
    AcceptedCmp4=AcceptedCmp4/num,
    AcceptedCmp5=AcceptedCmp5/num,
    Response=Response/num
      )%>%
  select(!num)

#melt wasn't working so an additional step of changing it to a dataframe was added
df.camp <- as.data.frame(df.camp)

df.camp%>%
  melt(id='cluster')%>%
  ggplot(aes(x=as_factor(cluster), y=value))+
  geom_point()+
  facet_wrap(~variable, ncol=6)

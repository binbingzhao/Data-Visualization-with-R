# Data-Visualization-with-R
R Base Plotting with R Markdown (Global Superstore Orders 2016 dataset)
# Import the Global Orders 2016 data set using the read.csv function
df<- read.csv("~/Desktop/Global Superstore Orders 2016.csv")

# Create a horizontal barplot of Total Sales by Market, ordered descending on total sales
options(scipen = 999)
aggdata <- aggregate.data.frame(df$Sales,by=list(df$Market),sum)
names(aggdata)<- c("Market", "Sales")
aggdata.order<- aggdata[order(aggdata$Sales,decreasing=FALSE),]
barplot(aggdata.order$Sales,names.arg=aggdata.order$Market,
        col=c("royalblue","royalblue","royalblue","royalblue","royalblue"),
        horiz=TRUE, border=NA,
        xlim=c(0,5000000),
        las=1, cex.names=0.71)
title("Global Superstore Total Sales by Market",font.main = 4)


# Visually indicate this market as a separate color from the others
barplot(aggdata.order$Sales,names.arg=aggdata.order$Market,
        col=c("Grey","royalblue","royalblue","royalblue","royalblue"),
        horiz=TRUE, border=NA,
        xlim=c(0,5000000),
        las=1, cex.names=0.71)
title("Global Superstore Total Sales by Market",font.main = 4)

#Create a line chart total sales by year for each market
agg2<-aggregate.data.frame(df$Sales,by=list(df$Market,df$Order.Year),sum)
names(agg2) = c("Market","Year","Total_Sales")

agg2.AsiaPacific<- agg2[agg2$Market == "Asia Pacific",]
agg2.Europe<- agg2[agg2$Market == "Europe",]
agg2.USCA<- agg2[agg2$Market == "USCA",]
agg2.LATAM<- agg2[agg2$Market == "LATAM",]
agg2.Africa<- agg2[agg2$Market == "Africa",]

xrange <- range(agg2$Year) 
yrange <- range(agg2$Total_Sales) 

plot(xrange, 
     yrange, 
     xlab="",ylab="",
     xaxt="n",
     las= 1,
     cex.axis= 0.7,
     main = "Total Sales by Year and Market")

axis(1, labels = as.character(agg2$Year), at = as.numeric(agg2$Year))

points(agg2.AsiaPacific$Year, agg2.AsiaPacific$Total_Sales)
points(agg2.Europe$Year, agg2.Europe$Total_Sales)
points(agg2.USCA$Year, agg2.USCA$Total_Sales)
points(agg2.LATAM$Year, agg2.LATAM$Total_Sales)
points(agg2.Africa$Year, agg2.Africa$Total_Sales)


lines(agg2.AsiaPacific$Year, agg2.AsiaPacific$Total_Sales,col = "navyblue")
lines(agg2.Europe$Year, agg2.Europe$Total_Sales,col = "pink")
lines(agg2.USCA$Year, agg2.USCA$Total_Sales,col = "forestgreen")
lines(agg2.LATAM$Year, agg2.LATAM$Total_Sales,col = "gold")
lines(agg2.Africa$Year, agg2.Africa$Total_Sales,col = "darkred")
legend("topleft", legend=c("AsiaPacific","Europe","USCA","LATAM","Africa"), lwd=c(5,5), col=c("navyblue","pink","forestgreen","gold","darkred"))

#Visually indicate this market as a separate color from the other markets in the graph
plot(xrange, 
     yrange, 
     xlab="",ylab="",
     xaxt="n",
     las= 1,
     cex.axis= 0.7,
     main = "Total Sales by Year and Market")

axis(1, labels = as.character(agg2$Year), at = as.numeric(agg2$Year))

points(agg2.AsiaPacific$Year, agg2.AsiaPacific$Total_Sales)
points(agg2.Europe$Year, agg2.Europe$Total_Sales)
points(agg2.USCA$Year, agg2.USCA$Total_Sales)
points(agg2.LATAM$Year, agg2.LATAM$Total_Sales)
points(agg2.Africa$Year, agg2.Africa$Total_Sales)


lines(agg2.AsiaPacific$Year, agg2.AsiaPacific$Total_Sales,col = "royalblue4")
lines(agg2.Europe$Year, agg2.Europe$Total_Sales,col = "royalblue3")
lines(agg2.USCA$Year, agg2.USCA$Total_Sales,col = "royalblue2")
lines(agg2.LATAM$Year, agg2.LATAM$Total_Sales,col = "royalblue1")
lines(agg2.Africa$Year, agg2.Africa$Total_Sales,col = "red")
legend("topleft", legend=c("AsiaPacific","Europe","USCA","LATAM","Africa"), lwd=c(5,5), col=c("royalblue4","royalblue3","royalblue2","royalblue1","red"))

#Create a box plot of total sales by market
boxplot(Sales~Market,
        data=df,
        main="Total Sales by Market",
        ylim = range(0:1000))

#Visually indicate this market as a separate color from the others in the boxplot graph
boxplot(Sales~Market,
        data=df,
        main="Total Sales by Market",
        ylim = range(0:10000),
        col= c("lightblue","lightblue","lightblue","lightblue","red"))

#Generate a different kind of graph other than what was produced
aggdata <- aggregate.data.frame(df$Sales,by=list(df$Market),sum)
names(aggdata)<- c("Market", "Sales")
Sales = c(aggdata$Sales)
Markets = c("Africa", "Asia Pacific", "Europe", "LATAM", "USCA")
percents <- round(Sales/sum(Sales)*100)
Markets <- paste(Markets, percents)
Markets <- paste(Markets,'%',sep='')
pie(Sales,labels = Markets,col = rainbow(length(Markets)), main='Total Sales by Market')

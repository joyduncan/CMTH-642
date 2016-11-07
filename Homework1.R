
## Loading data
url = 'https://raw.githubusercontent.com/ryanarbow/lessonprojects/master/rollingsales_manhattan.csv'
man <- read.csv(url, header = T, sep = ",", skip = 4, strip.white = T)
str(man)
##changing attributes to the correct data types
##Assumption: BOROUGH refers to a location

man$BOROUGH= as.factor(man$BOROUGH)
man$TAX.CLASS.AT.TIME.OF.SALE=as.factor(man$TAX.CLASS.AT.TIME.OF.SALE)

##removing special characters

man$NEIGHBORHOOD = gsub("[[:punct:]]","", man$NEIGHBORHOOD)

##Changing relevant attributes to numeric
man$COMMERCIAL.UNITS=as.numeric(as.character(gsub("[^[:digit:]]","", man$COMMERCIAL.UNITS)))
man$RESIDENTIAL.UNITS=as.numeric(as.character(gsub("[^[:digit:]]","", man$RESIDENTIAL.UNITS)))
man$TOTAL.UNITS=as.numeric(as.character(gsub("[^[:digit:]]","", man$TOTAL.UNITS)))
man$LAND.SQUARE.FEET=as.numeric(as.character(gsub("[^[:digit:]]","", man$LAND.SQUARE.FEET)))
man$GROSS.SQUARE.FEET=as.numeric(as.character(gsub("[^[:digit:]]","", man$GROSS.SQUARE.FEET)))
man$SALE.PRICE=as.numeric(as.character(gsub("[^[:digit:]]","", man$SALE.PRICE)))
man$YEAR.BUILT=as.numeric(man$YEAR.BUILT)


#Changing sale date attribute to date
temp1<-as.Date(man$SALE.DATE, format= "%m/%d/%y")
temp2<-as.Date(man$SALE.DATE, format= "%Y-%m-%d")
temp1[is.na(temp1)]<- temp2[!is.na(temp2)]  
man$SALE.DATE <- temp1

str(man)

na_count<- sapply(man, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

#Although some of the attributes have 0 values I assume this is impossible and that these are equivilent to NA for the following feids 


aa<-sapply(man[,], function(y) sum(length(which(y==0))))
aa <- data.frame(aa)
aa


View(man)

##removing the Borough and Ease.ment attributes as they add no value
man=man[,-(1)]
man=man[,-(6)]
View(man)

##Cleaning up the address collumn for multiple ways to phrase things 
man$ADDRESS <- gsub("AVE| AVEN| AVENU| AVENUE| AVE\\."," AVENUE",man$ADDRESS)
man$ADDRESS <- gsub(" ST| ST\\.| STREET| STREE"," STREET",man$ADDRESS)
man$ADDRESS <- gsub(" PL$| PLA$"," PLACE",man$ADDRESS)
man$ADDRESS <- gsub(" TERR"," TERRACE",man$ADDRESS)
man$ADDRESS <- gsub(" B$| BL$| BLVD| BLVD\\."," BOULEVARD",man$ADDRESS)
man$ADDRESS <- gsub(" DR$", " DRIVE", man$ADDRESS)


##Assign every sale price less than 100 (things that are most likely to be mistakes or passed on to children) to the average in the neighbourhood

for (n in unique(man$NEIGHBORHOOD))
  
{
  mean.sale = mean(man$SALE.PRICE[grepl(n, man$NEIGHBORHOOD) & (man$SALE.PRICE>=100)])
  
  man$SALE.PRICE[grepl(n, man$NEIGHBORHOOD) & (man$SALE.PRICE<100)]=mean.sale
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

##For missing years, assign the mode for the lot number (assuming this will be either the same building or ones built around the same time)
for (o in unique(man$LOT))
{
  mode.age = Mode(man$YEAR.BUILT[grepl(o, man$LOT) & (man$YEAR.BUILT>0)])
  
  man$YEAR.BUILT[grepl(o, man$LOT) & (man$YEAR.BUILT==0)] = mode.age
}


##fill in the gross square feet with mean of each neighborhood
View(man)

for (l in unique(man$NEIGHBORHOOD))
{ mean.gross = mean(man$GROSS.SQUARE.FEET[grepl(l, man$NEIGHBORHOOD) & (man$GROSS.SQUARE.FEET!=0)])
  
  man$GROSS.SQUARE.FEET[grepl(l, man$NEIGHBORHOOD) & (man$GROSS.SQUARE.FEET==0)]= mean.gross
}

summary(man$GROSS.SQUARE.FEET)

##Repeat for land square feet as well

for (z in unique(man$NEIGHBORHOOD))
{ mean.land = mean(man$LAND.SQUARE.FEET[grepl(z, man$NEIGHBORHOOD) & (man$LAND.SQUARE.FEET!=0)])

man$LAND.SQUARE.FEET[grepl(z, man$NEIGHBORHOOD) & (man$LAND.SQUARE.FEET==0)]= mean.land
}


##Replace 0s in total units: Assume there is no empty plots of land being sold, and that lots (even more specific than neighbourhoods are being used for the same purposes)

for (q in unique(man$LOT))
{
  mean.total= mean(man$TOTAL.UNITS[grepl(q, man$LOT) & (man$TOTAL.UNITS>0)])
  man$TOTAL.UNITS[grepl(q, man$LOT) & (man$TOTAL.UNITS==0)] = mean.total
}


comp<-man[which(man$RESIDENTIAL.UNITS > 0 |man$COMMERCIAL.UNITS > 0),]
View(comp)
length(comp$NEIGHBORHOOD)

temp <- man[which(man$RESIDENTIAL.UNITS > 0 & man$COMMERCIAL.UNITS > 0),]
length(temp$NEIGHBORHOOD)/length(comp$NEIGHBORHOOD)
unique(temp$TAX.CLASS.AT.PRESENT)
str(man)

##noticing that only 7%of all units had both residential and commercial units
##also that although there are 51.4% of the rows without either commerical or residential units
temp1 <- man[which(man$RESIDENTIAL.UNITS == 0 & man$COMMERCIAL.UNITS > 0),]
length(temp1$NEIGHBORHOOD)/length(comp$NEIGHBORHOOD)
unique(temp1$TAX.CLASS.AT.PRESENT)

##But it is more common for the unit to be residential 
temp2<- man[which(man$RESIDENTIAL.UNITS>0 & man$COMMERCIAL.UNITS==0),]
length(temp2$NEIGHBORHOOD)/length(comp$NEIGHBORHOOD)
unique(temp2$TAX.CLASS.AT.PRESENT)


##replace those values missing both residential units with the mean of residential units for each lot
for (x in unique(man$LOT))
{
  mean.res = mean(man$RESIDENTIAL.UNITS[grepl(x, man$LOT) & (man$RESIDENTIAL.UNITS>0)])
  man$RESIDENTIAL.UNITS[grepl(x, man$LOT) & (man$COMMERCIAL.UNITS==0) & (man$RESIDENTIAL.UNITS==0)] = mean.res
}


aa<-sapply(man[,], function(y) sum(length(which(y==0))))
aa <- data.frame(aa)
aa
## although there are still 0s left, some are a result of there not being any of that type of unit, there are none left with both 0 residential and commercial units
temp5<- man[which(man$RESIDENTIAL.UNITS == 0 & man$COMMERCIAL.UNITS ==0)]
length(temp5)
write.table(man, "man.txt", quote = FALSE, sep = "\t", row.names = FALSE)

str(man)
## 1. finding interesting trends

##First trend: Total units by year built 

attach(man)

library(ggplot2)

a <- ggplot(man, aes(x= YEAR.BUILT, y= TOTAL.UNITS))

a + geom_point(aes(color = TOTAL.UNITS))

##I now realize there are listings built in 1000 which are highly unlikely especially since one is a commercial garage 

man1 <- man[man$YEAR.BUILT > 1500, ]

detach(man)

attach(man1)

b <- ggplot(man1, aes(x= YEAR.BUILT, y= TOTAL.UNITS))

b + geom_point(aes(color = TOTAL.UNITS))

##as I expected, the number of units is increasing as cities began to build up 

##Second trend: There were more than twice the number of houses sold in 2015 then 2014

man1$year.sold <- format(man1$SALE.DATE, "%Y")

ggplot(data = man1) + geom_bar(aes(x = year.sold)) + xlab("Year Sold") + ylab("Total Count")

##The same by month

man1$month.sold <- format(man1$SALE.DATE, "%m")

ggplot(data = man1) + geom_bar(aes(x = month.sold)) + xlab("Month Sold") + ylab("Total Count")

##Third trend, sales price by year built 

c <- ggplot(man1, aes(x= YEAR.BUILT, y= SALE.PRICE))

c + geom_point(aes(color = SALE.PRICE))

##Fourth trend amount of houses sold at each price

ggplot( data = man1) + geom_histogram(aes(x= SALE.PRICE), binwidth = 10000000)

##Fifth trend sale price by residential units

d <- ggplot(man1, aes(x= RESIDENTIAL.UNITS, y= SALE.PRICE))

d + geom_point(aes(color = SALE.PRICE))

##2. UNIVARIATE REGRESSION MODEL

manplot<- plot(LAND.SQUARE.FEET, SALE.PRICE, xlab="Land Square Feet", ylab="Sale Price")

manline <- lm(SALE.PRICE~LAND.SQUARE.FEET)

abline(manline)

##It looks like the data is being skewed by an outlier, which I will remove for a better fitting line

max(man1$LAND.SQUARE.FEET)

man1<-man1[LAND.SQUARE.FEET < 250000,] 

manplot<- plot(man1$LAND.SQUARE.FEET, man1$SALE.PRICE, xlab="Land Square Feet", ylab="Sale Price")

manline <- lm(man1$SALE.PRICE~man1$LAND.SQUARE.FEET)

abline(manline)

##3. Multivariate linear regression 

multiman <- lm(cbind(GROSS.SQUARE.FEET, TOTAL.UNITS, LOT) ~ SALE.PRICE, data = man1)

multiman

##4. Correlation matrix with four features 

manmatrix <- data.frame(GROSS.SQUARE.FEET, TOTAL.UNITS, SALE.PRICE, LOT)

cor(manmatrix, method="pearson")


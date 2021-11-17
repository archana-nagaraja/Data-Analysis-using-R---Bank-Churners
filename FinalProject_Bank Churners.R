# load the csv file

library(modeest)
options(digits=3)
library(sampling)
library(plotly)

setwd("C:/ARCHANA/Boston University MS Applied Data Analytics/METCS544 - Foundations of Analytics with R - Spring1/Final Project/")

raw.data <- read.csv("BankChurners.csv")

View(raw.data)

# look for any missing values

unique(raw.data$Customer_Age)
unique(raw.data$Gender)
unique(raw.data$Income_Category) # has unknown values
unique(raw.data$Card_Category)
unique(raw.data$Months_on_book)
unique(raw.data$Credit_Limit)

nrow(raw.data[raw.data$Income_Category=="Unknown",]) # 1112 rows have unknown values

#removed rows with unknown income-category
raw.data <- subset(raw.data,Income_Category!="Unknown")

# remove columns not used from raw.data
raw.data <- raw.data[,names(raw.data) %in% c("Attrition_Flag","Customer_Age",
                        "Gender","Income_Category",
                        "Card_Category","Months_on_book","Credit_Limit")]

#split into existing and attrited customers

existing.cust <- raw.data[raw.data$Attrition_Flag=="Existing Customer",names(raw.data) %in%
                        c("Attrition_Flag","Customer_Age","Gender","Income_Category",
                          "Card_Category","Months_on_book","Credit_Limit")]

attrited.cust <- raw.data[raw.data$Attrition_Flag=="Attrited Customer",names(raw.data) %in%
                      c("Attrition_Flag","Customer_Age","Gender","Income_Category",
                        "Card_Category","Months_on_book","Credit_Limit")]

View(attrited.cust)
View(existing.cust)

################################################################################
# Analysis for categorical variable - card_category

# table representation to view frequencies

attr.card <- table(attrited.cust$Card_Category)
attr.card
all.card <- table(raw.data$Card_Category)
all.card

#round((attr.card/all.card)*100,2)

sprintf("Percentage of customers who attrited - by Card Category - %s - %.2f%%",
        rownames(attr.card),round((attr.card/all.card)*100,2))

# Majority of the attrited(1343) customers held a blue category credit card.
# Among the 4 card categories, Platinum has highest attrition - 20%

# Analysis of distribution of card categories using barplot

max(all.card)

all.bp <- barplot(all.card,main="Distribution of Card categories across all customers",
        col="red",ylab="No. of customers",xlab="Card Categories",
        ylim=c(0,1.1*max(all.card)))
text(x=all.bp,y=all.card,labels = all.card,pos=3,col="black",cex=0.8)

attr.bp <- barplot(attr.card,main="Distribution of Card categories attrited customers",
              col="blue",ylab="No. of customers",xlab="Card Categories",
              ylim=c(0,1.1*max(attr.card)))
text(x=attr.bp,y=attr.card,labels = attr.card,pos=3,col="black",cex=0.8)

card.types <- c("Blue","Gold","Platinum","Silver")

plot_ly() %>%
add_trace(x = ~card.types, y = ~all.card, type = 'bar',
            text = all.card, textposition = 'auto',name="All Customers",
            marker = list(color = 'blue',
            line = list(color = 'blue', width = 1.5))) %>%
add_trace(x = ~card.types, y = ~attr.card, type = 'bar',
            text = attr.card, textposition = 'auto',name="Attrited Customers",
            marker = list(color = 'red',
            line = list(color = 'red', width = 1.5))) %>%
layout(title = "Distribution of Card Categories",
                      barmode = 'group',
                      xaxis = list(title = "Card Category"),
                      yaxis = list(title = "No. of Customers"))

# Pie-chart of the distribution of card-category

perc <- round(all.card/sum(all.card),3)*100
perc
labels <- paste(names(all.card),perc)
labels <- paste(labels,"%",sep="")
pie(all.card,col=c("red","blue","green","yellow"),labels=labels,
    main="Distribution of Card categories across all customers",
    radius=1,cex=0.8)

perc <- round(attr.card/sum(attr.card),3)*100
perc
labels <- paste(names(attr.card),perc)
labels <- paste(labels,"%",sep="")
pie(attr.card,col=c("red","blue","green","yellow"),labels=labels,
    main="Distribution of Card categories attrited customers",
    radius=1,cex=0.8)


# The distribution of all the customers across the 4 card-categories is same as 
# the distribution of attrited customers across the 4 card-categories!

################################################################################
# Analysis for numeric variable - months-on-book for attrited customers

# boxplot of months-on-book to analyze the spread 

f <- fivenum(attrited.cust$Months_on_book)
out <- c(f[2]-1.5*(f[4]-f[2]),f[4]+1.5*(f[4]-f[2]))

#boxplot(attrited.cust$Months_on_book,horizontal = TRUE,xaxt='n',col="pink",
#       main="Months-on-book - Attrited Customers")
#axis(1,at=c(out,fivenum(attrited.cust$Months_on_book)))

plot_ly(attrited.cust, y = ~Months_on_book, type="box", 
        name = 'Months-on-book',quartilemethod="exclusive") %>%
    layout(title="Months-on-book - Attrited Customers")

sprintf("The average months-on-book for attrited customers is %g",
        round(mean(attrited.cust$Months_on_book)))

sprintf("Most of the attrited customers were with the company for %s months",
        mfv(attrited.cust$Months_on_book))

# Inferences - 
# The months-on-book for attrited customers ranges between 13 and 56 with about 
# 11 outliers. The interquartile range/middle 50% of the data is between 32 and 
# 40 and the median of 36 separates top 50% of the data from bottom 50%.
# The average is about the same as the median of 36.

x <- seq(0,60,5)
x

hist(attrited.cust$Months_on_book,main="Months-on-book - Attrited 
     Customers",col="light blue",xlab="months-on-book")

plot_ly(attrited.cust,x=~Months_on_book,type = "histogram") %>%
    layout(title="Frequency Distribution - Months-on-book")

# About 600(41%) of the attrited customers were with the company for 35-40 months.

################################################################################
# Analysis for set of 2 or more variables - Customer income-category/card-category
# Customer income-category/card-category

# contingency table of income-category & card-category - attrited customers

sorted.attr.data <- attrited.cust[order(attrited.cust$Income_Category),]

sorted.attr.data$Income_Category <- factor(sorted.attr.data$Income_Category,
                                          levels=c("Less than $40K","$40K - $60K",
                                          "$60K - $80K","$80K - $120K",
                                          "$120K +"))

income.card <- table(sorted.attr.data$Income_Category,
                     sorted.attr.data$Card_Category)

income.card

# We see that for customers with income of 120k+, none of them were holding a 
# platinum card and very few held Gold cards. Platinum/Gold cards with better 
# rewards/perks could have given these customers more interest to stay.

# For all income-levels, we see a big disparity between number of customers holding
# blue vs silver - we could look into opportunities for upgrading/reduce attrition.

# We see about 26 customers with income less than 40k held silver/gold/platinum
# cards. The higher fee associated with these cards could have initiated attrition.

# marginal & conditional distribution of income-category & card-category

addmargins(income.card)

# income-category
round(prop.table(income.card,1),3)

# card-category
round(prop.table(income.card,2),3)

# The total number of platinum/gold customers is much lesser compared to silver
# We should look into offering upgrades to motivate customers to stay.

# mosaic plot - representation of contingency table

income.card

mosaicplot(income.card,color=c("pink","purple"),cex.axis=0.6,las=1,
           xlab="Income Category",ylab="Card Category",
           main="IncomeCategory vs CardCategory")

# barplot of the 2-way table - beside=TRUE

barplot(income.card,beside=TRUE,legend.text=TRUE,col=rainbow(5),
        main="IncomeCategory vs CardCategory",xlab="Card Category",
        ylab="Frequency")

################################################################################
# Analysis of 2 or more variables - Customer income-category/card-category+gender

# Breakdown for attrited customers
aincome.card.gender <- table(sorted.attr.data$Income_Category,
                            sorted.attr.data$Card_Category,
                            sorted.attr.data$Gender)

ftable(aincome.card.gender)

# Breakdown for existing customers

sorted.exis.data <- existing.cust[order(existing.cust$Income_Category),]

sorted.exis.data$Income_Category <- factor(sorted.exis.data$Income_Category,
                                           levels=c("Less than $40K","$40K - $60K",
                                                    "$60K - $80K","$80K - $120K",
                                                    "$120K +"))

eincome.card.gender <- table(sorted.exis.data$Income_Category,
                             sorted.exis.data$Card_Category,
                             sorted.exis.data$Gender)

ftable(eincome.card.gender)

# Bar Plots existing vs attrited - by card-category page#63 of module3

par(mfrow=c(1,2))

male <- raw.data[raw.data$Gender=="M",]
female <- raw.data[raw.data$Gender=="F",]

mpercent <- paste0(round(table(male$Attrition_Flag)/nrow(male),2)*100,"%")
fpercent <- paste0(round(table(female$Attrition_Flag)/nrow(female),2)*100,"%")

table(male$Attrition_Flag)

mplot <- barplot(table(male$Attrition_Flag),col=c("light blue","light green"),
        main=c("Male"))
text(mplot,labels=mpercent)

fplot <- barplot(table(female$Attrition_Flag),col=c("light blue","light green"),
        main=c("Female"))
text(fplot,female$Attrition_Flag,labels=fpercent)

###############################PLOTLY Version######################

plot_ly(x=c("Attrited","Existing"),y=table(male$Attrition_Flag),type="bar",
        name="Male") %>%
    add_text(text=mpercent,textposition = "top")

subplot(
plot_ly(x=c("Attrited","Existing"),y=table(male$Attrition_Flag),type="bar",
        name="Male") %>%
    add_text(text=mpercent,textposition = "top"),
plot_ly(x=c("Attrited","Existing"),y=table(female$Attrition_Flag),type="bar",
        name="Female") %>%
    add_text(text=fpercent,textposition = "top") %>%
layout(title="Attrition by Gender"),
shareY = TRUE)
################################################################################
# Distribution of numeric data - customer's age

# boxplot of customer's age to analyze the spread 

par(mfrow=c(2,1))

age.a <- fivenum(attrited.cust$Customer_Age)

boxplot(attrited.cust$Customer_Age,horizontal = TRUE,xaxt='n',col="red",
        main="Age - Attrited Customers")
axis(1,at=fivenum(attrited.cust$Customer_Age))

# existing customers

age.e <- fivenum(existing.cust$Customer_Age)

boxplot(existing.cust$Customer_Age,horizontal = TRUE,xaxt='n',col="green",
        main="Age - Existing Customers")
axis(1,at=fivenum(existing.cust$Customer_Age))

subplot(
plot_ly(existing.cust, y = ~Customer_Age, type="box", 
            name = 'Existing',quartilemethod="exclusive"),
plot_ly(attrited.cust, y = ~Customer_Age, type="box", 
        name = 'Attrited',quartilemethod="exclusive") %>%
    layout(title="Age Distribution - Existing vs Attrited Customers"),
shareY = TRUE)

sprintf("The average age for attrited customers is %g",
        round(mean(attrited.cust$Customer_Age)))

sprintf("The average age for existing customers is %g",
        round(mean(existing.cust$Customer_Age)))

# histogram to visualize the distribution

par(mfrow=c(1,2))

hist(attrited.cust$Customer_Age,main="Attrited Customers",xlab="Customer's Age",
     xlim=c(20,70),col="red")

hist(existing.cust$Customer_Age,main="Existing Customers",xlab="Customer's Age",
     xlim=c(20,80),col="blue")

# The age distribution for both attrited & existing follows a normal distribution

# PDF 

mean.a <- mean(attrited.cust$Customer_Age)
mean.a
sd.a
sd.a <- sd(attrited.cust$Customer_Age)
data.a <- dnorm(attrited.cust$Customer_Age,mean=mean.a,sd=sd.a)
data.a
x.a <- seq(min(attrited.cust$Customer_Age),max(attrited.cust$Customer_Age),5)

plot(attrited.cust$Customer_Age,data.a,pch=19,main="PDF - Attrited Customers",
     xlab="Customer's Age",ylab="Probability Density Function")

# CDF

data.b <- pnorm(attrited.cust$Customer_Age,mean=mean.a,sd=sd.a)

plot(attrited.cust$Customer_Age,data.b,pch=19,main="CDF - Attrited Customers",
     xlab="Customer's Age",ylab="Cumulative Density Function")

################################################################################
# Central Limit Theorm - Customer's Age

set.seed(2633)

data.mean <- round(mean(raw.data$Customer_Age),2)
data.sd <- round(sd(raw.data$Customer_Age),2)

sample.func <- function(sample.size,sample.count) {
    sample.means <- numeric(sample.count)
    i <- 1
    for (i in 1:sample.count) {
        sample.means[i] <- mean(sample(raw.data$Customer_Age,sample.size,
                                       replace = FALSE))
    }
    return (sample.means)
}

# sample-size: 10
sample.means10 <- sample.func(10,5000)
mean.sm10 <- mean(sample.means10)
sd.sm10 <- sd(sample.means10)

# sample-size: 20
sample.means20 <- sample.func(20,5000)
mean.sm20 <- mean(sample.means20)
sd.sm20 <- sd(sample.means20)

# sample-size: 30
sample.means30 <- sample.func(30,5000)
mean.sm30 <- mean(sample.means30)
sd.sm30 <- sd(sample.means30)

# sample-size: 40
sample.means40 <- sample.func(40,5000)
mean.sm40 <- mean(sample.means40)
sd.sm40 <- sd(sample.means40)

# Plot
sd3 <- c(mean.sm40-3*sd.sm40,mean.sm40+3*sd.sm40)
# Plot density of sample means
hist(sample.means40,main="Sample-means - Sample-size: 40",
                               col="blue",prob=TRUE,
                            xlab="Sample means - Customer's Age")
abline(v=sd3,col="red") # 3-sd from the mean

plot_ly(x=sample.means40)

# From the plot, we see that the distribution of sample means follows a normal
# distribution. Most of the data is within 3 standard deviations of the mean.

sprintf("Data Mean: %.2f; Data Standard Deviation: %.2f",data.mean,data.sd)

sprintf("Sample-size: %g, Mean: %.2f, SD: %.2f",c(10,20,30,40),
        c(mean.sm10,mean.sm20,mean.sm30,mean.sm40),
        c(sd.sm10,sd.sm20,sd.sm30,sd.sm40))

sprintf("Data SD/sqrt(sample.size): %.2f; SD-SampleMeans: %.2f",
        (data.sd/sqrt(40)),sd.sm40)

# Mean of data is about equal to the mean of the sample-means for all sample-sizes.
# SD decreases as the sample-size increases.
# SD of data/sqrt(sample.size) is equal to SD of sample-means.

################################################################################
# Sampling Methods

# Simple random sampling, sample-size: 100

set.seed(2633)

srs <- srswor(n=100,N=nrow(raw.data))
srs.sample <- raw.data[srs!=0,]

# reviewing the fraction of existing vs attrited customers picked by the sample.

paste("Fraction of Attrited vs Existing customers in the data - ")
table(raw.data$Attrition_Flag)/nrow(raw.data)

paste("Fraction of Attrited vs Existing customers picked by SimpleRandomSampling")
table(srs.sample$Attrition_Flag)/nrow(srs.sample)

#attr.srs <- srs.sample[srs.sample$Attrition_Flag=="Attrited Customer",]

table(srs.sample$Card_Category)

# All 14 samples picked are under the Blue category.Unable to validate the 
# card with highest attrition.

hist(attr.srs$Months_on_book,main="Simple Random Sampling",xlab="months on book",labels=TRUE)

# 6/14 = 42% of the attrited customers were with the company - 35-40 months - 
# similar to the findings from the data.

# INFERENCES - SRS - 
# We see that the samples picked through SRSWOR approximately reflects the split
# between attrited/existing customers.

# Systematic sampling - sample-size: 100

grp <- ceiling(nrow(raw.data)/100) # divide into groups
r <- sample(grp,1) # pick first sample
sys.sample <- raw.data[seq(r, by=grp, length=100),]

View(sys.sample)

paste("Fraction of Attrited vs Existing customers picked by Systematic Sampling")
table(sys.sample$Attrition_Flag)/nrow(sys.sample)

# We see that the samples picked through Systematic sampling approximately 
# reflects the split between attrited/existing customers.

attr.sys <- sys.sample[sys.sample$Attrition_Flag=="Attrited Customer",]

table(attr.sys$Card_Category)

# Samples picked by systematic sampling contain 12 blue and 2 silver type cards.

hist(attr.sys$Months_on_book)

# 8/14=57% of attrited customers were with the company for 35-40 months as per
# samples picked from systematic sampling.

# Stratified sampling - using proportional sizes based on the Card-category

sorted.data <- raw.data[order(raw.data$Card_Category),]

size <- round(100*table(raw.data$Card_Category)/nrow(raw.data))
size

size <- c(92,1,1,6)

# t <- round(table(raw.data$Card_Category)/nrow(raw.data),2)*100

# pik <- inclusionprobabilities(table(raw.data$Card_Category)/nrow(raw.data),100)

strat <- strata(sorted.data,stratanames="Card_Category",size=size,
                method="srswor")

strata.sample <- getdata(sorted.data,strat)

paste("Fraction of Attrited vs Existing customers picked by Stratified Sampling")
table(strata.sample$Attrition_Flag)/nrow(strata.sample)

attr.str <- strata.sample[strata.sample$Attrition_Flag=="Attrited Customer",]

table(attr.str$Card_Category)

hist(attr.str$Months_on_book,main="Stratified Sampling",xlab="Months-on-book",
     labels=TRUE)

###############################


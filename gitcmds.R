help()

?install.packages()

help.search("install")

library(base)

data <- read.csv("C:/Users/lalit/Downloads/hotel_bookings.csv")

convert_data_babies = data[["babies"]]

convert_data_agent = data[["agent"]]

convert_data_booking_changes = data[["booking_changes"]]

convert_data_adults = data[["adults"]]

convert_data_lead_time = data[["lead_time"]]

convert_data_adr = data[["adr"]]

convert_data_arrival_date_week_number = data[["arrival_date_week_number"]]

convert_data_is_canceled = data[["is_canceled"]]

convert_data_hotel = data[["hotel"]]

convert_data_meal = data[["meal"]]

convert_data_country = data[["country"]]

convert_data_market_segment = data[["market_segment"]]

summary(data)

library(tableHTML)

tableHTML(head(data))

str(data)

tail(data)

names(data)

dim(data)

class(data)

View(data)

hist(convert_data_lead_time)

barplot(convert_data_lead_time)

boxplot(convert_data_arrival_date_week_number)

plot(convert_data_adr,convert_data_arrival_date_week_number)

max(convert_data_adults)

mean(convert_data_adults)

head(sort(convert_data_lead_time))

sum(convert_data_is_canceled)

plot(convert_data_arrival_date_week_number,convert_data_adr,type="l")

plot(convert_data_arrival_date_week_number,convert_data_booking_changes,type="b")

plot(convert_data_adr,convert_data_booking_changes,type="p")

plot(convert_data_adults,convert_data_booking_changes,type="h")

plot(convert_data_adr,convert_data_adults,type="s")

hist(convert_data_lead_time, col="lightblue")

abline(v = mean(convert_data_lead_time), col="red")

plot(convert_data_adr,convert_data_booking_changes,type="p")

abline(v = mean(convert_data_adr), col="red",h=mean(convert_data_booking_changes))

plot(convert_data_adr,convert_data_adults,type="b",pch = 19, col = "red", xlab = "x", ylab = "y")

length(convert_data_agent)

min(convert_data_adr[convert_data_adr>0])

sd(convert_data_adr)

var(convert_data_adr)

matrix <- matrix(convert_data_adr,nrow=20,ncol=20)

View(matrix)

df <- data.frame(col1=convert_data_adr,col2=convert_data_lead_time,col3=convert_data_hotel,col4=convert_data_arrival_date_week_number)

print(head(df))

colnames(df) <- c("adr","lead_time","hotel")

print(head(df))

colnames(df)[4] <- c("arrival_week")

print(head(df))

df$babies <- convert_data_babies

print(head(df))

df[,c("babies","arrival_week","hotel","lead_time","adr")]

print(head(df))

print(is.factor(convert_data_meal))

factor_df <- factor(convert_data_meal)

levels(factor_df)

library(dplyr)

factor_fd1 <- recode(factor_df,"Undefined"='NAN')

levels(factor_fd1)

testdataframe <- data.frame(booking_changes=convert_data_booking_changes)

transformed <- transform(testdataframe,Additional_charge=booking_changes*50)

print(head(transformed))

print(aggregate(df$adr, list(df$hotel), FUN=sum))

print(aggregate(df$adr, list(df$hotel), FUN=min))

print(aggregate(df$adr, list(df$hotel), FUN=max))

matrix <- matrix(convert_data_adr,nrow=20,ncol=20)

apply( matrix, 1, sum)

apply( matrix, 2, sum)

uppercs <- lapply(df$hotel, toupper)

print(head(uppercs))

sapply(df, max)

tapply(df$adr, df$arrival_week, mean)

df1 <- data.frame(col1=convert_data_adr,col2=convert_data_lead_time,col3=convert_data_hotel)

df2.mat=as.matrix(df1)

print(head(df2.mat))

cmat= cbind(convert_data_adr,convert_data_lead_time)

mat2frame=as.data.frame(cmat)

print(head(mat2frame))

groupby <- by(convert_data_booking_changes,list(convert_data_hotel),mean)

print(groupby)

dflist <- data.frame(adr=convert_data_adr[1:100])

frame.list=as.list(dflist)

print(frame.list)

stackdf <- data.frame(agent=convert_data_agent,lead_time=convert_data_lead_time)

stackdf.stack=stack(stackdf)

print(head(stackdf.stack))

unstack(stackdf.stack)

ls()

order(convert_data_adr[1:1000])

rank(convert_data_adr[1:1000])

which(convert_data_booking_changes>10)

with(df, sum(convert_data_babies))

head(row.names(df))

rownamestest <- data.frame(col1=convert_data_adr[1:4],col2=convert_data_lead_time[1:4])

rownames(rownamestest) <- c("hotel1","hotel2","hotel3","hotel4")

rownames(rownamestest)

row.names(rownamestest) <- LETTERS[1:4]

rownames(rownamestest)

total_people.list=list(convert_data_adults[1:100],convert_data_babies[1:100])

print(total_people.list)

library(dplyr)

library(tidyr)

dfu4 <- data.frame(meal=convert_data_meal,country=convert_data_country,market_segment=convert_data_market_segment)

head(dfu4 %>% mutate(new_col=NA))

head(dfu4 %>% select(-market_segment))

head(dfu4 %>% rename(meal_option=meal))

head(dfu4 %>% filter(country=="GBR"))

dfrecode <- data.frame(agent=convert_data_agent,arrival_week=convert_data_arrival_date_week_number,booking_changes=convert_data_booking_changes)
head(recode(dfrecode$agent,NULL="No Agent"))

head(cut(dfrecode$arrival_week,breaks = c(0,25,40,Inf)))

head(dfrecode %>% mutate(arrival_day=arrival_week*7))

head(dfrecode %>% group_by(arrival_week) %>% mutate(BKCHNGS=sum(booking_changes)))

gather(dfrecode,condition,count,agent,booking_changes)

table(convert_data_country)

table(convert_data_meal)

multiplegrpbydf <- data.frame(country=convert_data_country,lead_time=convert_data_lead_time,hotel=convert_data_hotel)

head(multiplegrpbydf %>% group_by(country,hotel) %>% summarise(avg_lead_time=mean(lead_time)))

egfunc <- function(x) {
  
  x <- x*5-10+20
  
  return(x) }

print(egfunc(4))

typeof(egfunc)

df %>% filter(adr >= 100,hotel == "Resort Hotel",arrival_week == 27)

df.data = table(df$adr)

chisq.test(df.data)

df1.exp <- exp(df$adr)

print(head(df1.exp))

df2.exp <- log(df$adr)

print(head(df2.exp))

matrix <- matrix(convert_data_lead_time[1:100],nrow=10,ncol=10)

scale(matrix)

sweep(matrix,1,1,"+")

ls(pattern = 'c')

library(zoo)

library(dplyr)

rollingdf <- data.frame(adr=convert_data_adr,lead_time=convert_data_lead_time,week=convert_data_arrival_date_week_number)

head(rollingdf %>% mutate(avg_over3 = rollmean(convert_data_adr, k=3, fill=NA, align='right')))

library(zoo)

head(rollingdf %>% mutate(avg_over3 = rollsum(convert_data_adr, k=3, fill=NA, align='right')))

subset(rollingdf,subset = (adr>400))

sweep(matrix,2,2,"/")

nrow(matrix)

ncol(matrix)

head(sort(convert_data_babies, decreasing = TRUE))

newdf = cbind(rollingdf,market=c(convert_data_market_segment))

print(head(newdf))

deletedf <- data.frame(adr=convert_data_adr,lead_time=convert_data_lead_time,week=convert_data_arrival_date_week_number)

rm(list = ls(pattern = 'deletedf'))

ls()

cmat= cbind(convert_data_adr,convert_data_adults)

mat2frame=as.data.frame(cmat)

print(head(mat2frame))

head(which(convert_data_hotel == "Resort Hotel"))


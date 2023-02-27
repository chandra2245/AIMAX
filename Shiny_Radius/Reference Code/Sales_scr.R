library(lubridate)

DF=read.csv("D:\\Shiny_Radius\\Datasets\\Final_Sales_Data.csv")

DF$DAY_DATE<-mdy(DF$DAY_DATE)

DF[is.na(DF)] <- 0

DF2<-DF %>%
  mutate(month = format(DAY_DATE, "%m"), year = format(DAY_DATE, "%Y")) %>%
  group_by(month, year,Brand) %>%
  summarise(total = sum(TOTAL_RX_COUNT))


DF2$Date<-dmy(paste("01",DF2$month,DF2$year, sep="-"))

DF2 <- subset(DF2, select = c(Brand, total,Date))


write.csv(DF2, file = "D:\\Shiny_Radius\\Datasets\\Sales_Summarized.csv",row.names = FALSE)


library("plotly")

DF3<-DF2 %>%
  group_by(Date) %>%
  summarise(sum_total = sum(total))


p <- plot_ly(DF3, x = ~Date, y = ~sum_total, name = 'trace 0', type = 'scatter', mode = 'lines')

p

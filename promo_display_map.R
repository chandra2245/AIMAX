setwd("D:\\Shiny_Radius\\Datasets")

library(readxl)
library(stringr)
library(maps)
library(dplyr)
library(leaflet)
library(htmltools)


c_name=c("BRAND_NAME",
         "SPEND_AMT",
         "PLANNED_SPEND_AMT",
         "ACTIVITY_COUNT",
         "ESTIMATE_SPEND_AMT",
         "MEDIA_TYPE",
         "DATE",
         "GEOGRAPHY")

promo_display_df<-readxl::read_xlsx("All_Data_Final.xlsx",sheet = 5,na = '?',skip = 1,col_names = c_name)
us_lat_long_df<-maps::us.cities


#REMOVING STATES NMAES FROM THE END
us_lat_long_df$name<-str_sub(us_lat_long_df$name,1,-4)


#CONVERTING TO UPPER CASE PROMO DATA
us_lat_long_df$name<-stringr::str_to_upper(us_lat_long_df$name)
promo_display_df$GEOGRAPHY<-stringr::str_to_upper(promo_display_df$GEOGRAPHY)


unique(promo_display_df$GEOGRAPHY)

#CHANGING SOME PREFIX BEFORE JOIN
promo_display_df$GEOGRAPHY<-str_replace(promo_display_df$GEOGRAPHY, "_"," ")
promo_display_df$GEOGRAPHY<-str_replace(promo_display_df$GEOGRAPHY, "-"," ")

promo_display_df$GEOGRAPHY<-str_replace(promo_display_df$GEOGRAPHY, "ST. ","SAINT ")
promo_display_df$GEOGRAPHY<-str_replace(promo_display_df$GEOGRAPHY, "ST ","SAINT ")
promo_display_df$GEOGRAPHY<-str_replace(promo_display_df$GEOGRAPHY, "FT ","FORT ")


#REMOVING THE NORTH EAST SOUTH VALUES OF THE CITIES
us_lat_long_df$name<-str_replace(us_lat_long_df$name, " [ESWN]$","")

promo_display_df$GEOGRAPHY<-str_replace(promo_display_df$GEOGRAPHY, " [ESWN]$","")


#SELECTING ONLY USEFUL COLUMNS
us_lat_long_df<-us_lat_long_df%>%select(name,lat,long)


#REMOVING DUPLICATE CITES
us_lat_long_df<-us_lat_long_df[!duplicated(us_lat_long_df$name),]



#JOINING THE DATAFRAMES
PROMO_DISPLAY_map_df=merge(x = promo_display_df, y =us_lat_long_df, by.x = "GEOGRAPHY",by.y="name")



PROMO_DISPLAY_map_df2<-PROMO_DISPLAY_map_df%>%group_by(GEOGRAPHY,BRAND_NAME,lat,long)%>%summarise(ESTIMATE_SPEND_AMT=mean(ESTIMATE_SPEND_AMT,na.rm=TRUE))%>%filter(BRAND_NAME=="HUMIRA")




leaflet(data = PROMO_DISPLAY_map_df2) %>% addTiles() %>%
  addCircleMarkers(lng =  ~long, lat=~lat, fill =2,
                   label =  paste0("City: =",as.character(PROMO_DISPLAY_map_df2$GEOGRAPHY),"<br/>Mean Estimate Spend Amount",as.character(PROMO_DISPLAY_map_df2$ESTIMATE_SPEND_AMT)))



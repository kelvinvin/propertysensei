
# Load packages
library(dplyr)
library(tidyr)
library(ggplot2)

# Load data
rent_by_town <- read.csv("Data/median-rent-by-town-and-flat-type.csv")
resale_1990_1999 <- read.csv("Data/resale-flat-prices-based-on-approval-date-1990-1999.csv")
resale_2000_2012 <- read.csv("Data/resale-flat-prices-based-on-approval-date-2000-feb-2012.csv")
resale_2012_2014 <- read.csv("Data/resale-flat-prices-based-on-registration-date-from-mar-2012-to-dec-2014.csv")
resale_2015_2016 <- read.csv("Data/resale-flat-prices-based-on-registration-date-from-jan-2015-to-dec-2016.csv")
resale_2017_2022 <- read.csv("Data/resale-flat-prices-based-on-registration-date-from-jan-2017-onwards.csv")


## Clean rent data
rent_by_town <- rent_by_town %>% 
  separate(quarter, c("Year", "Quarter"), sep="-") %>% 
  mutate(Year = as.factor(Year),
         Quarter = as.factor(Quarter),
         flat_type = as.factor(flat_type),
         median_rent = as.numeric(gsub("[^0-9.]", "", median_rent)))


# What flat types do we have data for?
rent_by_town %>% filter(flat_type == "1-RM") %>% select(median_rent) %>% unique() %>% nrow()
rent_by_town %>% filter(flat_type == "2-RM") %>% select(median_rent) %>% unique() %>% nrow()
rent_by_town %>% filter(flat_type == "3-RM") %>% select(median_rent) %>% unique() %>% nrow()
rent_by_town %>% filter(flat_type == "4-RM") %>% select(median_rent) %>% unique() %>% nrow()
rent_by_town %>% filter(flat_type == "5-RM") %>% select(median_rent) %>% unique() %>% nrow()
rent_by_town %>% filter(flat_type == "EXEC") %>% select(median_rent) %>% unique() %>% nrow()

# We don't have data for 1-RM, we exclude it
rent_by_town <- rent_by_town %>% 
  filter(flat_type != "1-RM")

# Appreciation?
rent_by_town %>% filter(flat_type == "2-RM", Year == "2020", !is.na(median_rent)) %>% 
  select(median_rent) %>% summarize(mean = mean(median_rent))

rent_by_town %>% 
  filter(!is.na(median_rent)) %>% 
  group_by(Year, flat_type) %>% 
  summarize(Mean_rent = mean(median_rent)) %>% ungroup() -> Mean_rents


write.csv(rent_by_town, "Data/rent_by_town_cleaned.csv")



## Clean resale data
resale_2015_2016 <- resale_2015_2016[,-10]
resale_2017_2022 <- resale_2017_2022[,-10]

resale_1990_2022 <- rbind.data.frame(resale_1990_1999,
                                     resale_2000_2012,
                                     resale_2012_2014,
                                     resale_2015_2016,
                                     resale_2017_2022)
rm(resale_1990_1999,
   resale_2000_2012,
   resale_2012_2014,
   resale_2015_2016,
   resale_2017_2022)


resale_1990_2022_new <- resale_1990_2022 %>% 
  separate(month, c("Year", "Month"), sep="-") %>% 
  select(Year, Month, town, flat_type, floor_area_sqm, resale_price) %>% 
  mutate(Year = as.factor(Year),
         Month = as.numeric(Month),
         town = as.factor(town))

library(lubridate)
resale_1990_2022_new$Quarter <- quarter(resale_1990_2022_new$Month)
resale_1990_2022_new$Quarter <- paste0("Q", resale_1990_2022_new$Quarter)

resale_1990_2022_new <- resale_1990_2022_new %>% 
  mutate(Month = as.factor(Month),
         Quarter = as.factor(Quarter))


resale_1990_2022_new$flat_type <- gsub(" ROOM", "-RM", resale_1990_2022_new$flat_type)
resale_1990_2022_new$flat_type <- gsub("EXECUTIVE", "EXEC", resale_1990_2022_new$flat_type)

resale_1990_2022_new$flat_type <- as.factor(resale_1990_2022_new$flat_type)

resale_1990_2022_new$flat_type <- droplevels.factor(resale_1990_2022_new$flat_type, 
                                                    exclude = c("MULTI-GENERATION", "MULTI GENERATION"))

write.csv(resale_1990_2022_new, "Data/resale_1990_2022_cleaned.csv")


resale_1990_2022_new <- read.csv("Data/resale_1990_2022_cleaned.csv", stringsAsFactors = T)

# Add districts
districts <- read.csv("Data/Districts_Towns.csv", header = T)
names(districts)[names(districts) == "ï..D1"] <- "D1"

#remove spaces from the districts data
for(i in 1:ncol(districts)){
  for(j in 1:nrow(districts)){
    districts[j,i] <- gsub(" ", "", districts[j,i])
  }
}

#remove spaces from the resale data
resale_1990_2022_new$town <- sapply(resale_1990_2022_new$town, function(x){
  gsub(" ", "", x)
})

districts %>% 
  gather(key = "district", value = "town") -> districts


districts <- districts[districts$town != 0,]
districts$town <- toupper(districts$town)

resale_1990_2022_new <- left_join(resale_1990_2022_new, districts, by = "town")

resale_1990_2022_new$town <- as.factor(resale_1990_2022_new$town)
resale_1990_2022_new$district <- as.factor(resale_1990_2022_new$district)

write.csv(resale_1990_2022_new, "Data/resale_1990_2022_cleaned_v2.csv")

#Plot
resale_1990_2022_new %>% 
  drop_na(flat_type) %>% 
  group_by(Year, flat_type) %>% 
  summarise(avg_price = mean(resale_price), avg_sqm = mean(floor_area_sqm)) %>%
  ggplot(aes(x=Year, y=avg_price, color = flat_type))+
  geom_smooth(se =F)+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

resale_1990_2022_new %>% 
  drop_na(flat_type) %>% 
  group_by(Year, district) %>% 
  summarise(avg_price = mean(resale_price), avg_sqm = mean(floor_area_sqm)) %>%
  ggplot(aes(x=Year, y=avg_price, color = district))+
  geom_smooth(se =F)+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

returns <- resale_1990_2022_new %>% 
  group_by(Year, district, flat_type) %>% 
  summarise(avg_price = mean(resale_price), avg_sqm = mean(floor_area_sqm))

returns %>% 
  group_by(Year, district) %>% 
  summarise(avg_price = mean(avg_price)) %>%
  spread(key = district, avg_price) %>% 
  ggplot(aes(x=Year)) + 
  geom_line(aes(y=D3))+
  geom_line(aes(y=D5))+
  geom_line(aes(y=D12))+
  geom_line(aes(y=D14))+
  geom_line(aes(y=D16))+
  geom_line(aes(y=D18))+
  geom_line(aes(y=D10))+
  geom_line(aes(y=D20))
  
# Returns districts
resale_1990_2022_new %>% 
  drop_na(flat_type) %>% 
  group_by(Year, district, flat_type) %>% 
  summarise(avg_price = mean(resale_price), avg_sqm = mean(floor_area_sqm)) %>% 
  ungroup() %>% 
  group_by(Year, district) %>%
  summarise(avg_price = mean(avg_price)) %>%
  spread(key = district, avg_price) %>% 
  ungroup() %>% 
  mutate(return_D3 = diff(D3) / lag(D3),
         return_D10 = diff(D10) / lag(D10),
         return_D12 = diff(D12) / lag(D12),
         return_D14 = diff(D14) / lag(D14),
         return_D16 = diff(D16) / lag(D16),
         return_D18 = diff(D18) / lag(D18),
         return_D19 = diff(D19) / lag(D19),
         return_D20 = diff(D20) / lag(D20),
         return_D23 = diff(D23) / lag(D23),
         return_D25 = diff(D25) / lag(D25),
         return_D27 = diff(D27) / lag(D27)) -> test

return_districts$return_all_districts <- apply(return_districts[,2:12], 1, mean, na.rm=T)  

avg_return_districts <- apply(return_districts[,2:13], 2, mean, na.rm=T) %>% 
  as.data.frame()

names(avg_return_districts) <- "Annual_Avg_Return"


#Returns flat type
resale_1990_2022_new %>% 
  drop_na(flat_type) %>% 
  group_by(Year, district, flat_type) %>% 
  summarise(avg_price = mean(resale_price), avg_sqm = mean(floor_area_sqm)) %>% 
  ungroup() %>% 
  group_by(Year, flat_type) %>%
  summarise(avg_price = mean(avg_price)) %>%
  spread(key = flat_type, avg_price) %>% 
  ungroup() %>% 
  mutate(return_1RM = diff(`1-RM`) / lag(`1-RM`),
         return_2RM = diff(`2-RM`) / lag(`2-RM`),
         return_3RM = diff(`3-RM`) / lag(`3-RM`),
         return_4RM = diff(`4-RM`) / lag(`4-RM`),
         return_5RM = diff(`5-RM`) / lag(`5-RM`),
         return_EXEC = diff(EXEC) / lag(EXEC)) %>% 
  select(Year, return_1RM:return_EXEC) ->return_flat_type

return_flat_type$return_all_flats <- apply(return_flat_type[,2:7], 1, mean, na.rm=T)  

avg_return_flats <- apply(return_flat_type[,2:8], 2, mean, na.rm=T) %>% 
  as.data.frame()

names(avg_return_flats) <- "Annual_Avg_Return"


write.csv(avg_return_districts, "Data/returns_districts.csv")
write.csv(avg_return_flats, "Data/returns_flats.csv")


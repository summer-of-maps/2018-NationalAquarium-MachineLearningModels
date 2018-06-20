setwd("D:/NA/spreedsheet")

install.packages('devtools')

# install.packages("dplyr")
library(tidyr)
library(dplyr)
library(tidyverse)
library(tidycensus)
library(sf)
library(imputeTS)
library(ggplot2)
library(devtools)

rawDC <- read.csv("OC Marine Debris Data for DC.csv")

locationDC <- separate(rawDC, GPS, c("lat","lon"), sep = ",", remove = FALSE)

locationDC$lat <- as.numeric(locationDC$lat)
locationDC$lon <- as.numeric(locationDC$lon)

write.csv(locationDC, file = "locationDC.csv")

setwd("D:/NA/WP")

DC <- st_read("DC.shp")
Delaware <- st_read("Delaware.shp")
Maryland <- st_read("Maryland.shp")
NC <- st_read("NC.shp")
Virginia <- st_read("Virginia.shp")

dataset <- rbind(Delaware, Maryland, NC, Virginia)

st_write(dataset, "comdata.shp", driver="ESRI Shapefile")

compdata <- st_read("comdata.shp")

# Data cleaning for National Aquarium

#---------------------------------Reading in the 08 data----------------------------------------------

# DC08 (6)
DC08 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2008 DC.xlsx", skip = 1) %>% 
  as.data.frame()
DC08 <- DC08[-seq(nrow(DC08),nrow(DC08)),]

# Delaware08 (32)
Delaware08 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2008 Delaware.xlsx", skip = 1) %>% 
  as.data.frame()
Delaware08 <- Delaware08[-seq(nrow(Delaware08),nrow(Delaware08)),]

# Maryland08 (15)
Maryland08 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2008 Maryland.xlsx", skip = 1) %>% 
  as.data.frame()
Maryland08 <- Maryland08[-seq(nrow(Maryland08),nrow(Maryland08)),]

# NorthCarolina08 (94)
NorthCarolina08 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2008 North Carolina.xlsx", skip = 1) %>% 
  as.data.frame()
NorthCarolina08 <- NorthCarolina08[-seq(nrow(NorthCarolina08),nrow(NorthCarolina08)),]

# Virginia08 (143)
Virginia08 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2008 Virginia.xlsx", skip = 1) %>% 
  as.data.frame()
Virginia08 <- Virginia08[-seq(nrow(Virginia08),nrow(Virginia08)),]

# comb08 (290)
comb08 <- rbind(DC08,Delaware08,Maryland08,NorthCarolina08,Virginia08)

# Replace the NAs and combine the data
comb08$`County/Zone`[is.na(comb08$`County/Zone`)] <- "DC"

na.replace(comb08, 0)

combdata08 <- comb08 %>%
  dplyr::mutate(Latitude = as.numeric(Latitude),
                Longitude = as.numeric(Longitude),
                Adults = as.numeric(Adults),
                Children = as.numeric(Children),
                Adults__1 = as.numeric(Adults__1),
                Children__1 = as.numeric(Children__1),
                Adults__2 = as.numeric(Adults__2),
                Children__2 = as.numeric(Children__2),
                XofBags = as.numeric(`# of Bags`),
                XofBags1 = as.numeric(`# of Bags__1`),
                XofBags2 = as.numeric(`# of Bags__2`)
                ) %>% 
  na.replace(0) %>% 
  dplyr::mutate(Adults = Adults + Adults__1 + Adults__2,
                Children = Children + Children__1 + Children__2,
                People = People + People__1 + People__2,
                Pounds = Pounds + Pounds__1 + Pounds__2,
                Miles = Miles + Miles__1 + Miles__2,
                XofBags = XofBags + XofBags1 + XofBags2) %>% 
  select(-Adults__1,-Children__1,-People__1,-Pounds__1,-Miles__1,`# of Bags__1`,
         -Adults__2,-Children__2,-People__2,-Pounds__2,-Miles__2,`# of Bags__2`,
         -XofBags1,-XofBags2,
         -`# of Bags`,-`# of Bags__1`,-`# of Bags__2`)

#---------------------------------Reading in the 09 data----------------------------------------------

# DC09 (4)
DC09 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2009 DC.xlsx", skip = 1) %>% 
  as.data.frame()
DC09 <- DC09[-seq(nrow(DC09),nrow(DC09)),]

# Delaware09 (35)
Delaware09 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2009 Delaware.xlsx", skip = 1) %>% 
  as.data.frame()
Delaware09 <- Delaware09[-seq(nrow(Delaware09),nrow(Delaware09)),]

# Maryland09 (19)
Maryland09 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2009 Maryland.xlsx", skip = 1) %>% 
  as.data.frame()
Maryland09 <- Maryland09[-seq(nrow(Maryland09),nrow(Maryland09)),]

# NorthCarolina09 (90)
NorthCarolina09 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2009 North Carolina.xlsx", skip = 1) %>% 
  as.data.frame()
NorthCarolina09 <- NorthCarolina09[-seq(nrow(NorthCarolina09),nrow(NorthCarolina09)),]

# Virginia09 (128)
Virginia09 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2009 Virginia.xlsx", skip = 1) %>% 
  as.data.frame()
Virginia09 <- Virginia09[-seq(nrow(Virginia09),nrow(Virginia09)),]

# comb09 (276)
comb09 <- rbind(DC09,Delaware09,Maryland09,NorthCarolina09,Virginia09)

# Replace the NAs and combine the data
comb09$`County/Zone`[is.na(comb09$`County/Zone`)] <- "DC"

na.replace(comb09, 0)

combdata09 <- comb09 %>%
  dplyr::mutate(Latitude = as.numeric(Latitude),
                Longitude = as.numeric(Longitude),
                Adults = as.numeric(Adults),
                Children = as.numeric(Children),
                Adults__1 = as.numeric(Adults__1),
                Children__1 = as.numeric(Children__1),
                Adults__2 = as.numeric(Adults__2),
                Children__2 = as.numeric(Children__2),
                XofBags = as.numeric(`# of Bags`),
                XofBags1 = as.numeric(`# of Bags__1`),
                XofBags2 = as.numeric(`# of Bags__2`)
  ) %>% 
  na.replace(0) %>% 
  dplyr::mutate(Adults = Adults + Adults__1 + Adults__2,
                Children = Children + Children__1 + Children__2,
                People = People + People__1 + People__2,
                Pounds = Pounds + Pounds__1 + Pounds__2,
                Miles = Miles + Miles__1 + Miles__2,
                XofBags = XofBags + XofBags1 + XofBags2) %>% 
  select(-Adults__1,-Children__1,-People__1,-Pounds__1,-Miles__1,`# of Bags__1`,
         -Adults__2,-Children__2,-People__2,-Pounds__2,-Miles__2,`# of Bags__2`,
         -XofBags1,-XofBags2,
         -`# of Bags`,-`# of Bags__1`,-`# of Bags__2`)

#---------------------------------Reading in the 10 data----------------------------------------------

# DC10 (4)
DC10 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2010 DC.xlsx", skip = 1) %>% 
  as.data.frame()
DC10 <- DC10[-seq(nrow(DC10),nrow(DC10)),]

# Delaware10 (36)
Delaware10 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2010 Delaware.xlsx", skip = 1) %>% 
  as.data.frame()
Delaware10 <- Delaware10[-seq(nrow(Delaware10),nrow(Delaware10)),]

# Maryland10 (23)
Maryland10 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2010 Maryland.xlsx", skip = 1) %>% 
  as.data.frame()
Maryland10 <- Maryland10[-seq(nrow(Maryland10),nrow(Maryland10)),]

# NorthCarolina10 (79)
NorthCarolina10 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2010 North Carolina.xlsx", skip = 1) %>% 
  as.data.frame()
NorthCarolina10 <- NorthCarolina10[-seq(nrow(NorthCarolina10),nrow(NorthCarolina10)),]

# Virginia10 (150)
Virginia10 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2010 Virginia.xlsx", skip = 1) %>% 
  as.data.frame()
Virginia10 <- Virginia10[-seq(nrow(Virginia10),nrow(Virginia10)),]

# comb10 (292)
comb10 <- rbind(DC10,Delaware10,Maryland10,NorthCarolina10,Virginia10)

# Replace the NAs and combine the data
comb10$`County/Zone`[is.na(comb10$`County/Zone`)] <- "DC"

na.replace(comb10, 0)

combdata10 <- comb10 %>%
  dplyr::mutate(Latitude = as.numeric(Latitude),
                Longitude = as.numeric(Longitude),
                Adults = as.numeric(Adults),
                Children = as.numeric(Children),
                Adults__1 = as.numeric(Adults__1),
                Children__1 = as.numeric(Children__1),
                Adults__2 = as.numeric(Adults__2),
                Children__2 = as.numeric(Children__2),
                XofBags = as.numeric(`# of Bags`),
                XofBags1 = as.numeric(`# of Bags__1`),
                XofBags2 = as.numeric(`# of Bags__2`)
  ) %>% 
  na.replace(0) %>% 
  dplyr::mutate(Adults = Adults + Adults__1 + Adults__2,
                Children = Children + Children__1 + Children__2,
                People = People + People__1 + People__2,
                Pounds = Pounds + Pounds__1 + Pounds__2,
                Miles = Miles + Miles__1 + Miles__2,
                XofBags = XofBags + XofBags1 + XofBags2) %>% 
  select(-Adults__1,-Children__1,-People__1,-Pounds__1,-Miles__1,`# of Bags__1`,
         -Adults__2,-Children__2,-People__2,-Pounds__2,-Miles__2,`# of Bags__2`,
         -XofBags1,-XofBags2,
         -`# of Bags`,-`# of Bags__1`,-`# of Bags__2`)

#---------------------------------Reading in the 11 data----------------------------------------------

# DC11 (2)
DC11 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2011 DC.xlsx", skip = 1) %>% 
  as.data.frame()
DC11 <- DC11[-seq(nrow(DC11),nrow(DC11)),]

# Delaware11 (38)
Delaware11 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2011 Delaware.xlsx", skip = 1) %>% 
  as.data.frame()
Delaware11 <- Delaware11[-seq(nrow(Delaware11),nrow(Delaware11)),]

# Maryland11 (30)
Maryland11 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2011 Maryland.xlsx", skip = 1) %>% 
  as.data.frame()
Maryland11 <- Maryland11[-seq(nrow(Maryland11),nrow(Maryland11)),]

# NorthCarolina11 (92)
NorthCarolina11 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2011 North Carolina.xlsx", skip = 1) %>% 
  as.data.frame()
NorthCarolina11 <- NorthCarolina11[-seq(nrow(NorthCarolina11),nrow(NorthCarolina11)),]

# Virginia11 (121)
Virginia11 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2011 Virginia.xlsx", skip = 1) %>% 
  as.data.frame()
Virginia11 <- Virginia11[-seq(nrow(Virginia11),nrow(Virginia11)),]

# comb11 (292)
comb11 <- rbind(DC11,Delaware11,Maryland11,NorthCarolina11,Virginia11)

# Replace the NAs and combine the data
comb11$`County/Zone`[is.na(comb11$`County/Zone`)] <- "DC"

na.replace(comb11, 0)

combdata11 <- comb11 %>%
  dplyr::mutate(Latitude = as.numeric(Latitude),
                Longitude = as.numeric(Longitude),
                Adults = as.numeric(Adults),
                Children = as.numeric(Children),
                Adults__1 = as.numeric(Adults__1),
                Children__1 = as.numeric(Children__1),
                Adults__2 = as.numeric(Adults__2),
                Children__2 = as.numeric(Children__2),
                XofBags = as.numeric(`# of Bags`),
                XofBags1 = as.numeric(`# of Bags__1`),
                XofBags2 = as.numeric(`# of Bags__2`)
  ) %>% 
  na.replace(0) %>% 
  dplyr::mutate(Adults = Adults + Adults__1 + Adults__2,
                Children = Children + Children__1 + Children__2,
                People = People + People__1 + People__2,
                Pounds = Pounds + Pounds__1 + Pounds__2,
                Miles = Miles + Miles__1 + Miles__2,
                XofBags = XofBags + XofBags1 + XofBags2) %>% 
  select(-Adults__1,-Children__1,-People__1,-Pounds__1,-Miles__1,`# of Bags__1`,
         -Adults__2,-Children__2,-People__2,-Pounds__2,-Miles__2,`# of Bags__2`,
         -XofBags1,-XofBags2,
         -`# of Bags`,-`# of Bags__1`,-`# of Bags__2`)

#---------------------------------Reading in the 12 data----------------------------------------------

# DC12 (3)
DC12 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2012 DC.xlsx", skip = 1) %>% 
  as.data.frame()
DC12 <- DC12[-seq(nrow(DC12),nrow(DC12)),]

# Delaware12 (38)
Delaware12 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2012 Delaware.xlsx", skip = 1) %>% 
  as.data.frame()
Delaware12 <- Delaware12[-seq(nrow(Delaware12),nrow(Delaware12)),]

# Maryland12 (26)
Maryland12 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2012 Maryland.xlsx", skip = 1) %>% 
  as.data.frame()
Maryland12 <- Maryland12[-seq(nrow(Maryland12),nrow(Maryland12)),]

# NorthCarolina12 (81)
NorthCarolina12 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2012 North Carolina.xlsx", skip = 1) %>% 
  as.data.frame()
NorthCarolina12 <- NorthCarolina12[-seq(nrow(NorthCarolina12),nrow(NorthCarolina12)),]

# Virginia12 (144)
Virginia12 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2012 Virginia.xlsx", skip = 1) %>% 
  as.data.frame()
Virginia12 <- Virginia12[-seq(nrow(Virginia12),nrow(Virginia12)),]

# comb12 (292)
comb12 <- rbind(DC12,Delaware12,Maryland12,NorthCarolina12,Virginia12)

# Replace the NAs and combine the data
comb12$`County/Zone`[is.na(comb12$`County/Zone`)] <- "DC"

na.replace(comb12, 0)

combdata12 <- comb12 %>%
  dplyr::mutate(Latitude = as.numeric(Latitude),
                Longitude = as.numeric(Longitude),
                Adults = as.numeric(Adults),
                Children = as.numeric(Children),
                Adults__1 = as.numeric(Adults__1),
                Children__1 = as.numeric(Children__1),
                Adults__2 = as.numeric(Adults__2),
                Children__2 = as.numeric(Children__2),
                XofBags = as.numeric(`# of Bags`),
                XofBags1 = as.numeric(`# of Bags__1`),
                XofBags2 = as.numeric(`# of Bags__2`)
  ) %>% 
  na.replace(0) %>% 
  dplyr::mutate(Adults = Adults + Adults__1 + Adults__2,
                Children = Children + Children__1 + Children__2,
                People = People + People__1 + People__2,
                Pounds = Pounds + Pounds__1 + Pounds__2,
                Miles = Miles + Miles__1 + Miles__2,
                XofBags = XofBags + XofBags1 + XofBags2) %>% 
  select(-Adults__1,-Children__1,-People__1,-Pounds__1,-Miles__1,`# of Bags__1`,
         -Adults__2,-Children__2,-People__2,-Pounds__2,-Miles__2,`# of Bags__2`,
         -XofBags1,-XofBags2,
         -`# of Bags`,-`# of Bags__1`,-`# of Bags__2`)

#---------------------------------Reading in the 13 data----------------------------------------------

# DC13 (3)
# ******* There's no data for DC 2013
DC13 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2013 DC.xlsx", skip = 1) %>% 
  as.data.frame()
DC13 <- DC13[-seq(nrow(DC13),nrow(DC13)),]

# Delaware13 (42)
Delaware13 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2013 Delaware.xlsx", skip = 1) %>% 
  as.data.frame()
Delaware13 <- Delaware13[-seq(nrow(Delaware13),nrow(Delaware13)),]

# Maryland13 (36)
Maryland13 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2013 Maryland.xlsx", skip = 1) %>% 
  as.data.frame()
Maryland13 <- Maryland13[-seq(nrow(Maryland13),nrow(Maryland13)),]

# NorthCarolina13 (85)
NorthCarolina13 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2013 North Carolina.xlsx", skip = 1) %>% 
  as.data.frame()
NorthCarolina13 <- NorthCarolina13[-seq(nrow(NorthCarolina13),nrow(NorthCarolina13)),]

# Virginia13 (131)
Virginia13 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2013 Virginia.xlsx", skip = 1) %>% 
  as.data.frame()
Virginia13 <- Virginia13[-seq(nrow(Virginia13),nrow(Virginia13)),]

# comb13 (294)
# *****This is important
# *****Remember to add DC 13 data after getting it 
comb13 <- rbind(Delaware13,Maryland13,NorthCarolina13,Virginia13)

# Replace the NAs and combine the data
comb13$`County/Zone`[is.na(comb13$`County/Zone`)] <- "DC"

na.replace(comb13, 0)

combdata13 <- comb13 %>%
  dplyr::mutate(Latitude = as.numeric(Latitude),
                Longitude = as.numeric(Longitude),
                Adults = as.numeric(Adults),
                Children = as.numeric(Children),
                Adults__1 = as.numeric(Adults__1),
                Children__1 = as.numeric(Children__1),
                Adults__2 = as.numeric(Adults__2),
                Children__2 = as.numeric(Children__2),
                XofBags = as.numeric(`# of Bags`),
                XofBags1 = as.numeric(`# of Bags__1`),
                XofBags2 = as.numeric(`# of Bags__2`)
  ) %>% 
  na.replace(0) %>% 
  dplyr::mutate(Adults = Adults + Adults__1 + Adults__2,
                Children = Children + Children__1 + Children__2,
                People = People + People__1 + People__2,
                Pounds = Pounds + Pounds__1 + Pounds__2,
                Miles = Miles + Miles__1 + Miles__2,
                XofBags = XofBags + XofBags1 + XofBags2) %>% 
  select(-Adults__1,-Children__1,-People__1,-Pounds__1,-Miles__1,`# of Bags__1`,
         -Adults__2,-Children__2,-People__2,-Pounds__2,-Miles__2,`# of Bags__2`,
         -XofBags1,-XofBags2,
         -`# of Bags`,-`# of Bags__1`,-`# of Bags__2`)

#---------------------------------Reading in the 14 data----------------------------------------------

# DC14 (5)
DC14 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2014 DC.xlsx", skip = 1) %>% 
  as.data.frame()
DC14 <- DC14[-seq(nrow(DC14),nrow(DC14)),]

# Delaware14 (46)
Delaware14 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2014 Delaware.xlsx", skip = 1) %>% 
  as.data.frame()
Delaware14 <- Delaware14[-seq(nrow(Delaware14),nrow(Delaware14)),]

# Maryland14 (36)
Maryland14 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2014 Maryland.xlsx", skip = 1) %>% 
  as.data.frame()
Maryland14 <- Maryland14[-seq(nrow(Maryland14),nrow(Maryland14)),]

# NorthCarolina14 (83)
NorthCarolina14 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2014 North Carolina.xlsx", skip = 1) %>% 
  as.data.frame()
NorthCarolina14 <- NorthCarolina14[-seq(nrow(NorthCarolina14),nrow(NorthCarolina14)),]

# Virginia14 (128)
Virginia14 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2014 Virginia.xlsx", skip = 1) %>% 
  as.data.frame()
Virginia14 <- Virginia14[-seq(nrow(Virginia14),nrow(Virginia14)),]

# comb14 (298)
comb14 <- rbind(DC14,Delaware14,Maryland14,NorthCarolina14,Virginia14)

# Replace the NAs and combine the data
comb14$`County/Zone`[is.na(comb14$`County/Zone`)] <- "DC"

na.replace(comb14, 0)

combdata14 <- comb14 %>%
  dplyr::mutate(Latitude = as.numeric(Latitude),
                Longitude = as.numeric(Longitude),
                Adults = as.numeric(Adults),
                Children = as.numeric(Children),
                Adults__1 = as.numeric(Adults__1),
                Children__1 = as.numeric(Children__1),
                Adults__2 = as.numeric(Adults__2),
                Children__2 = as.numeric(Children__2),
                XofBags = as.numeric(`# of Bags`),
                XofBags1 = as.numeric(`# of Bags__1`),
                XofBags2 = as.numeric(`# of Bags__2`)
  ) %>% 
  na.replace(0) %>% 
  dplyr::mutate(Adults = Adults + Adults__1 + Adults__2,
                Children = Children + Children__1 + Children__2,
                People = People + People__1 + People__2,
                Pounds = Pounds + Pounds__1 + Pounds__2,
                Miles = Miles + Miles__1 + Miles__2,
                XofBags = XofBags + XofBags1 + XofBags2) %>% 
  select(-Adults__1,-Children__1,-People__1,-Pounds__1,-Miles__1,`# of Bags__1`,
         -Adults__2,-Children__2,-People__2,-Pounds__2,-Miles__2,`# of Bags__2`,
         -XofBags1,-XofBags2,
         -`# of Bags`,-`# of Bags__1`,-`# of Bags__2`)


#---------------------------------Reading in the 15 data----------------------------------------------

# DC15 (6)
DC15 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2015 DC.xlsx", skip = 1) %>% 
  as.data.frame()
DC15 <- DC15[-seq(nrow(DC15),nrow(DC15)),]

# Delaware15 (48)
Delaware15 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2015 Delaware.xlsx", skip = 1) %>% 
  as.data.frame()
Delaware15 <- Delaware15[-seq(nrow(Delaware15),nrow(Delaware15)),]

# Maryland15 (36)
Maryland15 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2015 Maryland.xlsx", skip = 1) %>% 
  as.data.frame()
Maryland15 <- Maryland15[-seq(nrow(Maryland15),nrow(Maryland15)),]

# NorthCarolina15 (92)
NorthCarolina15 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2015 North Carolina.xlsx", skip = 1) %>% 
  as.data.frame()
NorthCarolina15 <- NorthCarolina15[-seq(nrow(NorthCarolina15),nrow(NorthCarolina15)),]

# Virginia15 (120)
Virginia15 <- read_excel("D:/NA/spreedsheet/xls/OC Marine Debris Data for 2015 Virginia.xlsx", skip = 1) %>% 
  as.data.frame()
Virginia15 <- Virginia15[-seq(nrow(Virginia15),nrow(Virginia15)),]

# comb15 (302)
comb15 <- rbind(DC15,Delaware15,Maryland15,NorthCarolina15,Virginia15)

# Replace the NAs and combine the data
comb15$`County/Zone`[is.na(comb15$`County/Zone`)] <- "DC"

na.replace(comb15, 0)

combdata15 <- comb15 %>%
  dplyr::mutate(Latitude = as.numeric(Latitude),
                Longitude = as.numeric(Longitude),
                Adults = as.numeric(Adults),
                Children = as.numeric(Children),
                Adults__1 = as.numeric(Adults__1),
                Children__1 = as.numeric(Children__1),
                Adults__2 = as.numeric(Adults__2),
                Children__2 = as.numeric(Children__2),
                XofBags = as.numeric(`# of Bags`),
                XofBags1 = as.numeric(`# of Bags__1`),
                XofBags2 = as.numeric(`# of Bags__2`)
  ) %>% 
  na.replace(0) %>% 
  dplyr::mutate(Adults = Adults + Adults__1 + Adults__2,
                Children = Children + Children__1 + Children__2,
                People = People + People__1 + People__2,
                Pounds = Pounds + Pounds__1 + Pounds__2,
                Miles = Miles + Miles__1 + Miles__2,
                XofBags = XofBags + XofBags1 + XofBags2) %>% 
  select(-Adults__1,-Children__1,-People__1,-Pounds__1,-Miles__1,`# of Bags__1`,
         -Adults__2,-Children__2,-People__2,-Pounds__2,-Miles__2,`# of Bags__2`,
         -XofBags1,-XofBags2,
         -`# of Bags`,-`# of Bags__1`,-`# of Bags__2`)

#---------------------------------Reading in the 15-18 data----------------------------------------------

# DC1518 (251)
DC1518 <- read.csv("D:/NA/spreedsheet/xls/OC Marine Debris Data for DC.csv") %>% 
  as.data.frame()
DC1518 <- DC1518[-seq(nrow(DC1518),nrow(DC1518)),]

# Delaware1518 (239) 
Delaware1518 <- read.csv("D:/NA/spreedsheet/xls/OC Marine Debris Data for Delaware.csv") %>% 
  as.data.frame()
Delaware1518 <- Delaware1518[-seq(nrow(Delaware1518),nrow(Delaware1518)),]

# Maryland1518 (223)
Maryland1518 <- read.csv("D:/NA/spreedsheet/xls/OC Marine Debris Data for Maryland.csv") %>% 
  as.data.frame()
Maryland1518 <- Maryland1518[-seq(nrow(Maryland1518),nrow(Maryland1518)),]

# NorthCarolina (1323)
NorthCarolina1518 <- read.csv("D:/NA/spreedsheet/xls/OC Marine Debris Data for North Carolina.csv") %>% 
  as.data.frame()
NorthCarolina1518 <- NorthCarolina1518[-seq(nrow(NorthCarolina1518),nrow(NorthCarolina1518)),]

# Virginia1518 (651)
Virginia1518 <- read.csv("D:/NA/spreedsheet/xls/OC Marine Debris Data for Virginia.csv") %>% 
  as.data.frame()
Virginia1518 <- Virginia1518[-seq(nrow(Virginia1518),nrow(Virginia1518)),]

#Don't know y but the loop is not working yet
# data1518 <- list(DC1518,Delaware1518,Maryland1518,NorthCarolina1518,Virginia1518)
# for (i in data1518){
#   colnames(data1518[i]) <- c( 
#                     "CleanupID",                   "Zone",                                     
#                     "State",                       "Country",                                  
#                     "GPS",                         "CleanupType",
#                     "CleanupDate",                 "GroupName",                             
#                     "Adults",                      "Children",
#                     "People",                      "Pounds",
#                     "Miles",                       "Xofbags",
#                     "CigaretteButts",              "FoodWrapperscandychipsetc",
#                     "TakeOutAwayContainersPlastic","TakeOutAwayContainersFoam",          
#                     "BottleCapsPlastic",           "BottleCapsMetal",                      
#                     "LidsPlastic",                 "StrawsStirrers",                   
#                     "ForksKnivesSpoons",           "BeverageBottlesPlastic",
#                     "BeverageBottlesGlass",        "BeverageCans",
#                     "GroceryBagsPlastic",          "OtherPlasticBags",
#                     "PaperBags",                   "CupsPlatesPaper",
#                     "CupsPlatesPlastic.",          "CupsPlatesFoam",
#                     "FishingBuoysPotsTraps",       "FishingNetPieces",
#                     "FishingLine1yardmeter1piece", "Rope1yardmeter1piece",
#                     "FishingGearCleanSwell",       "X6PackHolders",                    
#                     "OtherPlasticFoamPackaging",   "OtherPlasticBottlesoilbleachetc",
#                     "StrappingBands",              "TobaccoPackagingWrap",                  
#                     "OtherPackagingCleanSwell",    "Appliancesrefrigeratorswashersetc",
#                     "Balloons",                    "CigarTips",                
#                     "CigaretteLighters",           "ConstructionMaterials",
#                     "Fireworks",                   "Tires",                  
#                     "Toys",                        "OtherTrashCleanSwell",
#                     "Condoms",                     "Diapers",               
#                     "Syringes",                    "TamponsTamponApplicators",
#                     "PersonalHygieneCleanSwell",   "FoamPieces",
#                     "GlassPieces",                 "PlasticPieces",
#                     "TotalItemsCollected")}


# Rename and combine the 1518 datasets
colnames(DC1518) <- c("CleanupID",                  "Zone",                                     
                           "State",                       "Country",                                  
                           "GPS",                         "CleanupType",
                           "CleanupDate",                 "GroupName",                             
                           "Adults",                      "Children",
                           "People",                      "Pounds",
                           "Miles",                       "Xofbags",
                           "CigaretteButts",              "FoodWrapperscandychipsetc",
                           "TakeOutAwayContainersPlastic","TakeOutAwayContainersFoam",          
                           "BottleCapsPlastic",           "BottleCapsMetal",                      
                           "LidsPlastic",                 "StrawsStirrers",                   
                           "ForksKnivesSpoons",           "BeverageBottlesPlastic",
                           "BeverageBottlesGlass",        "BeverageCans",
                           "GroceryBagsPlastic",          "OtherPlasticBags",
                           "PaperBags",                   "CupsPlatesPaper",
                           "CupsPlatesPlastic.",          "CupsPlatesFoam",
                           "FishingBuoysPotsTraps",       "FishingNetPieces",
                           "FishingLine1yardmeter1piece", "Rope1yardmeter1piece",
                           "FishingGearCleanSwell",       "X6PackHolders",                    
                           "OtherPlasticFoamPackaging",   "OtherPlasticBottlesoilbleachetc",
                           "StrappingBands",              "TobaccoPackagingWrap",                  
                           "OtherPackagingCleanSwell",    "Appliancesrefrigeratorswashersetc",
                           "Balloons",                    "CigarTips",                
                           "CigaretteLighters",           "ConstructionMaterials",
                           "Fireworks",                   "Tires",                  
                           "Toys",                        "OtherTrashCleanSwell",
                           "Condoms",                     "Diapers",               
                           "Syringes",                    "TamponsTamponApplicators",
                           "PersonalHygieneCleanSwell",   "FoamPieces",
                           "GlassPieces",                 "PlasticPieces",
                           "TotalItemsCollected")

colnames(Delaware1518) <- c("CleanupID",                  "Zone",                                     
                      "State",                       "Country",                                  
                      "GPS",                         "CleanupType",
                      "CleanupDate",                 "GroupName",                             
                      "Adults",                      "Children",
                      "People",                      "Pounds",
                      "Miles",                       "Xofbags",
                      "CigaretteButts",              "FoodWrapperscandychipsetc",
                      "TakeOutAwayContainersPlastic","TakeOutAwayContainersFoam",          
                      "BottleCapsPlastic",           "BottleCapsMetal",                      
                      "LidsPlastic",                 "StrawsStirrers",                   
                      "ForksKnivesSpoons",           "BeverageBottlesPlastic",
                      "BeverageBottlesGlass",        "BeverageCans",
                      "GroceryBagsPlastic",          "OtherPlasticBags",
                      "PaperBags",                   "CupsPlatesPaper",
                      "CupsPlatesPlastic.",          "CupsPlatesFoam",
                      "FishingBuoysPotsTraps",       "FishingNetPieces",
                      "FishingLine1yardmeter1piece", "Rope1yardmeter1piece",
                      "FishingGearCleanSwell",       "X6PackHolders",                    
                      "OtherPlasticFoamPackaging",   "OtherPlasticBottlesoilbleachetc",
                      "StrappingBands",              "TobaccoPackagingWrap",                  
                      "OtherPackagingCleanSwell",    "Appliancesrefrigeratorswashersetc",
                      "Balloons",                    "CigarTips",                
                      "CigaretteLighters",           "ConstructionMaterials",
                      "Fireworks",                   "Tires",                  
                      "Toys",                        "OtherTrashCleanSwell",
                      "Condoms",                     "Diapers",               
                      "Syringes",                    "TamponsTamponApplicators",
                      "PersonalHygieneCleanSwell",   "FoamPieces",
                      "GlassPieces",                 "PlasticPieces",
                      "TotalItemsCollected")

colnames(Maryland1518) <- c("CleanupID",                  "Zone",
                            "State",                       "Country",
                            "GPS",                         "CleanupType",
                            "CleanupDate",                 "GroupName",
                            "Adults",                      "Children",
                            "People",                      "Pounds",
                            "Miles",                       "Xofbags",
                            "CigaretteButts",              "FoodWrapperscandychipsetc",
                            "TakeOutAwayContainersPlastic","TakeOutAwayContainersFoam",          
                            "BottleCapsPlastic",           "BottleCapsMetal",                      
                            "LidsPlastic",                 "StrawsStirrers",                   
                            "ForksKnivesSpoons",           "BeverageBottlesPlastic",
                            "BeverageBottlesGlass",        "BeverageCans",
                            "GroceryBagsPlastic",          "OtherPlasticBags",
                            "PaperBags",                   "CupsPlatesPaper",
                            "CupsPlatesPlastic.",          "CupsPlatesFoam",
                            "FishingBuoysPotsTraps",       "FishingNetPieces",
                            "FishingLine1yardmeter1piece", "Rope1yardmeter1piece",
                            "FishingGearCleanSwell",       "X6PackHolders",                    
                            "OtherPlasticFoamPackaging",   "OtherPlasticBottlesoilbleachetc",
                            "StrappingBands",              "TobaccoPackagingWrap", 
                            "OtherPackagingCleanSwell",    "Appliancesrefrigeratorswashersetc",
                            "Balloons",                    "CigarTips",                
                            "CigaretteLighters",           "ConstructionMaterials",
                            "Fireworks",                   "Tires",                  
                            "Toys",                        "OtherTrashCleanSwell",
                            "Condoms",                     "Diapers",               
                            "Syringes",                    "TamponsTamponApplicators",
                            "PersonalHygieneCleanSwell",   "FoamPieces",
                            "GlassPieces",                 "PlasticPieces",
                            "TotalItemsCollected")

colnames(NorthCarolina1518) <- c("CleanupID",                  "Zone",
                            "State",                       "Country",
                            "GPS",                         "CleanupType",
                            "CleanupDate",                 "GroupName",
                            "Adults",                      "Children",
                            "People",                      "Pounds",
                            "Miles",                       "Xofbags",
                            "CigaretteButts",              "FoodWrapperscandychipsetc",
                            "TakeOutAwayContainersPlastic","TakeOutAwayContainersFoam",          
                            "BottleCapsPlastic",           "BottleCapsMetal",                      
                            "LidsPlastic",                 "StrawsStirrers",                   
                            "ForksKnivesSpoons",           "BeverageBottlesPlastic",
                            "BeverageBottlesGlass",        "BeverageCans",
                            "GroceryBagsPlastic",          "OtherPlasticBags",
                            "PaperBags",                   "CupsPlatesPaper",
                            "CupsPlatesPlastic.",          "CupsPlatesFoam",
                            "FishingBuoysPotsTraps",       "FishingNetPieces",
                            "FishingLine1yardmeter1piece", "Rope1yardmeter1piece",
                            "FishingGearCleanSwell",       "X6PackHolders",                    
                            "OtherPlasticFoamPackaging",   "OtherPlasticBottlesoilbleachetc",
                            "StrappingBands",              "TobaccoPackagingWrap", 
                            "OtherPackagingCleanSwell",    "Appliancesrefrigeratorswashersetc",
                            "Balloons",                    "CigarTips",                
                            "CigaretteLighters",           "ConstructionMaterials",
                            "Fireworks",                   "Tires",                  
                            "Toys",                        "OtherTrashCleanSwell",
                            "Condoms",                     "Diapers",               
                            "Syringes",                    "TamponsTamponApplicators",
                            "PersonalHygieneCleanSwell",   "FoamPieces",
                            "GlassPieces",                 "PlasticPieces",
                            "TotalItemsCollected")


colnames(Virginia1518) <- c("CleanupID",                  "Zone",                                     
                            "State",                       "Country",                                  
                            "GPS",                         "CleanupType",
                            "CleanupDate",                 "GroupName",                             
                            "Adults",                      "Children",
                            "People",                      "Pounds",
                            "Miles",                       "Xofbags",
                            "CigaretteButts",              "FoodWrapperscandychipsetc",
                            "TakeOutAwayContainersPlastic","TakeOutAwayContainersFoam",          
                            "BottleCapsPlastic",           "BottleCapsMetal",                      
                            "LidsPlastic",                 "StrawsStirrers",                   
                            "ForksKnivesSpoons",           "BeverageBottlesPlastic",
                            "BeverageBottlesGlass",        "BeverageCans",
                            "GroceryBagsPlastic",          "OtherPlasticBags",
                            "PaperBags",                   "CupsPlatesPaper",
                            "CupsPlatesPlastic.",          "CupsPlatesFoam",
                            "FishingBuoysPotsTraps",       "FishingNetPieces",
                            "FishingLine1yardmeter1piece", "Rope1yardmeter1piece",
                            "FishingGearCleanSwell",       "X6PackHolders",                    
                            "OtherPlasticFoamPackaging",   "OtherPlasticBottlesoilbleachetc",
                            "StrappingBands",              "TobaccoPackagingWrap",                  
                            "OtherPackagingCleanSwell",    "Appliancesrefrigeratorswashersetc",
                            "Balloons",                    "CigarTips",                
                            "CigaretteLighters",           "ConstructionMaterials",
                            "Fireworks",                   "Tires",                  
                            "Toys",                        "OtherTrashCleanSwell",
                            "Condoms",                     "Diapers",               
                            "Syringes",                    "TamponsTamponApplicators",
                            "PersonalHygieneCleanSwell",   "FoamPieces",
                            "GlassPieces",                 "PlasticPieces",
                            "TotalItemsCollected")

# Comb1518 
comb1518 <- rbind(DC1518,Delaware1518,Maryland1518,NorthCarolina1518,Virginia1518)

#---------------------------------Combine everything into one----------------------------------------------

# combine 08 to 15
comb0812 <- rbind(combdata08,combdata09,combdata10,
                  combdata11,combdata12)

names0812 <- names(comb0812)

comb1315 <- rbind(combdata13,combdata14,combdata15)

names1315 <- names(comb1315)

names1518 <- names(comb1518)

write.csv(names0812,"names0812.csv")
write.csv(names1315,"names1315.csv")
write.csv(names1518,"names1518.csv")

#****problem here: data structurt difference

# clean the comb0812 for join
comb0812forjoin <- comb0812 %>% 
  mutate(TotalItemsCollected = `Paper Bags` + `Bags(Plastic)`+
           `Balloons` + `Beverage Bottles (Plastic)` +
           `Beverage Bottles (Glass)` + `Beverage Cans` +
           `Caps, Lids` + `Clothing, Shoes` + 
           `Cups, Plates, Forks, Knives, Spoons` + `Food Wrappers/Containers` +
           `Pull Tabs` + `6-Pack Holders` + 
           `Shotgun Shells/Wadding` + `Straws, Stirrers` + 
           `Toys` + `Bait Containers/Packaging` + 
           `Bleach/Cleaner Bottles` + `Buoys/Floats` +
           `Crab/Lobster/Fish Traps` + `Crates` +
           `Fishing Line` + `Fishing Lures/Light Sticks` + 
           `Fishing Net & Pieces` + `Light Bulbs/Tubes` + 
           `Oil/Lube Bottles` + `Pallets` + 
           `Plastic Sheeting/Tarps` + `Rope` + 
           `Strapping Bands` + `Cigarette Butts` +
           `Cigarette Lighters` + `Cigar Tips` +
           `Tobacco Packaging/Wrap` + `Appliances (refigerators, washers, etc.)` + 
           `Batteries` + `Construction Materials` +
           `Cars/Car Parts` + `55-Gallon Drums` +
           `Tires` + `Condoms` +
           `Diapers` + `Syringes` +
           `Tampons/Tampon Applicators`
         ) %>%
  dplyr::select( Country,State,`County/Zone`,
                 # `Site Name`,`Site Type`,
                 `Cleanup Date`,`Cleanup Type`,`Latitude`,
                 `Longitude`, Adults, Children,
                 People, Pounds, Miles,
                 XofBags,TotalItemsCollected
                 ) %>% 
  rename(Zone = `County/Zone`,
         CleanupDate = `Cleanup Date`, 
         CleanupType = `Cleanup Type`,
         Xofbags = XofBags)

# clean the comb1315 for join
comb1315forjoin <- comb1315 %>%
  mutate(TotalItemsCollected = `Cigarette Butts`+`Food Wrappers (candy, chips, etc.)`+
           `Take Out/Away Containers (Plastic)` + `Take Out/Away Containers (Foam)` + 
           `Bottle Caps (Plastic)` + `Take Out/Away Containers (Foam)` +
           `Bottle Caps (Plastic)` + `Bottle Caps (Metal)` +
           `Lids (Plastic)` + `Straws, Stirrers` + 
           `Forks, Knives, Spoons` + `Beverage Bottles (Plastic)` + 
           `Beverage Bottles (Glass)` + `Beverage Cans` + 
           `Grocery Bags (Plastic)` + `Other Plastic Bags` + 
           `Paper Bags` + `Cups & Plates (Paper)` +
           `Cups & Plates (Plastic)` + `Cups & Plates (Foam)` +
           `Fishing Buoys, Pots & Traps` + `Fishing Net & Pieces` +
           `Fishing Line (1 yard/meter = 1 piece)` + `Rope (1 yard/meter = 1 piece)`+
           `6-Pack Holders` + `Other Plastic/Foam Packaging` +
           `Other Plastic Bottles (oil, bleach, etc.)` + `Strapping Bands` +
           `Tobacco Packaging/Wrap` + `Appliances (refigerators, washers, etc.)` +
           `Balloons` + `Cigar Tips` +
           `Cigarette Lighters` + `Construction Materials` +
           `Fireworks` + `Tires` +
           `Condoms` + `Diapers` +
           `Syringes` + `Tampons/Tampon Applicators` + 
           `Foam Pieces` + `Glass Pieces` + 
           `Plastic Pieces`) %>% 
  dplyr::select(Country,State,`County/Zone`,
                                 # `Site Name`,`Site Type`,
                                 `Cleanup Date`,`Cleanup Type`,`Latitude`,
                                 `Longitude`, Adults, Children,
                                 People, Pounds, Miles,
                                 XofBags,TotalItemsCollected) %>% 
  rename(Zone = `County/Zone`,
         CleanupDate = `Cleanup Date`, 
         CleanupType = `Cleanup Type`,
         Xofbags = XofBags)

# clean the comb1518 for join
comb1518forjoin <- dplyr::select(comb1518,
                                 Country,State,Zone,
                                 # `Site Name`,`Site Type`,
                                 CleanupDate, CleanupType,GPS,
                                 Adults, Children,
                                 People, Pounds, Miles,
                                 Xofbags,TotalItemsCollected) %>% 
  separate(GPS, c("Latitude","Longitude"), sep = ",", remove = TRUE)

# Everything combined together
# It might make more sense to predict the bag/mile or totalitem/mile
# Is it better to remove the NAs?
finalcombdata <- rbind(comb0812forjoin, comb1315forjoin, comb1518forjoin) %>% 
  mutate(bagpermile = Xofbags/Miles) %>% 
  mutate(bagpermile = ifelse(bagpermile == Inf, Xofbags, bagpermile)) %>% 
  mutate(bagpermile = ifelse(is.na(bagpermile), Xofbags, bagpermile)) %>%
  mutate(itemspermile = TotalItemsCollected/Miles) %>% 
  mutate(itemspermile = ifelse(itemspermile == Inf, TotalItemsCollected, itemspermile)) %>% 
  mutate(itemspermile = ifelse(is.na(itemspermile), TotalItemsCollected, itemspermile)) %>% 
  mutate(poundspermile = Pounds/Miles) %>% 
  mutate(poundspermile = ifelse(poundspermile == Inf, Pounds, poundspermile)) %>% 
  mutate(poundspermile = ifelse(is.na(poundspermile), Pounds, poundspermile))

# Deciding the cell size
# Median 0.8973 Mean 3.7856
summary(finalcombdata$Miles)

summary(finalcombdata$Xofbags)

summary(finalcombdata$TotalItemsCollected)

# Write final data for display into one csv
write.csv(finalcombdata,"finalcombdata.csv")


#---------------------------------Display the data----------------------------------------------

finalpoints <- st_read("D:/NA/WP/06_17_allpoints_dis2water.shp") %>% 
  st_transform(3857)

states <- st_read("D:/NA/WP/basemap/selectedstates.shp") %>% 
  st_transform(3857)

waterbody <- st_read("D:/NA/WP/basemap/slipwater_dissolve1.shp") %>% 
  st_transform(3857)

# !!!! don't plot this data
# waterclip <- st_read("D:/NA/WP/waterclip_proj.shp")

# Define plot theme
plotTheme <- function(base_size = 24) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 18,colour = "black"),
    plot.subtitle = element_text(size = 12,face="italic", colour = "#477371", hjust = 0),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.y = element_line("#477371", size = 0.1),
    panel.border = element_rect(colour = "#477371", fill=NA, size=1),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size=10),
    axis.title = element_text(size=8),
    axis.text = element_text(size=8),
    axis.title.y = element_text(size=12),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    axis.ticks.y = element_line(color="grey70"),
    axis.text.x =  element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank())
}

# basemap
baseMap <- ggplot() + 
  geom_sf(data=states, aes(), fill = "gray70", colour="darkgray", size = 0.5, alpha = 0.2) +
  # geom_sf(data=streets, aes(), fill=NA, colour='white', size = 0.3,alpha=0.8) + 
  geom_sf(data=waterbody, aes(), fill="white", colour = NA) +
  # geom_sf(data=roads, aes(), fill=NA, colour='gray90', alpha = 0.8) +
  # geom_sf(data=parks, aes(), fill="white", colour = NA, alpha = 0.9) +
  # geom_sf(data=minneapolis, aes(), fill=NA, colour="darkgray", size = 0.3) +
  # geom_sf(data=boundary, aes(), fill = NA, colour="darkgray", size = 0.5, alpha = 0.2) +
  plotTheme()

#---------------------------------Feature Engineering----------------------------------------------
library(AppliedPredictiveModeling)
library(randomForest)
#library(popbio) #
library(caret)
library(pscl)
library(plotROC) 
library(ggmap)
library(pROC)
library(grid)
library(gridExtra)
library(ggplot2)
library(tidycensus)

#Install api key
#census_api_key("7ec628d12f68cb9fce1d74c77ababf2078dbc766", overwrite = FALSE, install = TRUE)

readRenviron("~/.Renviron")

us <- unique(fips_codes$state)[1:51]

# for variables
# 2016
v16 <- load_variables(2016, "acs5", cache = TRUE)

v16 <- load_variables(2016, "acs5", cache = TRUE)

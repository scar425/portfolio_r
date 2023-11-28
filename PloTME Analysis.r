---
title: "PLoT ME Analysis"
author: "Scar Winter Kelsey"
date: "10/20/2020"
output: html_document
---

```{r setup, include=FALSE}
chooseCRANmirror(graphics=FALSE, ind=1)
knitr::opts_chunk$set(echo = TRUE)


# #
# install.packages("tidyverse")
# install.packages("here")
# install.packages("sf")
# install.packages("mapview")
# install.packages("sp")
# install.packages("flextable")
# install.packages("magrittr")


library(tidyverse)
library(dplyr)
library(here)
library(ggplot2)
library(sf)
library(mapview)
library(sp)
library(flextable)
library(magrittr)




```

```{r echo = TRUE}
#confirm wd
here()

# venue <- read.csv("Participant to Venue Data - FINAL.csv")
# apps <- read.csv("Participant to App Data - FINAL.csv")
censusConversion <- read.csv("censusConversionNew-2.csv")
# RADAR_egos <- read.csv("egoData_2020-10-14.csv")
# ego_locs <- read.csv("ego_loc_2020-11-19.csv")
# venue_types <- read.csv("NOT_DISTINCT_venueTypeData_2018-12-04.csv")
```


## Data Cleaning


#### Confirm accurate geolocation

```{r}

#filter censusConversion based on the geolocations you need to map

censusConversion_3arts <- censusConversion %>% 
  filter(censusCode == 'geo_5jrd-6zik-1.17' | censusCode == 'geo_5jrd-6zik-1.290')


geo_3arts <- st_as_sf(censusConversion_3arts, coords = c("long", "lat"), crs = 4326)
geo_3arts <- mapview(geo_3arts)
geo_3arts
censusConversion_3arts

censusConversion_jefferyPub <- censusConversion %>% 
  filter(censusCode == 'geo_5jrd-6zik-1.420' | censusCode == 'geo_5jrd-6zik-1.152' )


geo_jefferyPub <- st_as_sf(censusConversion_jefferyPub, coords = c("long", "lat"), crs = 4326)
geo_jefferyPub <- mapview(geo_jefferyPub)
geo_jefferyPub
censusConversion_jefferyPub
```

### RADAR ego and ego loc dataset

#### Join RADAR ego and ego loc
``` {r echo = TRUE}
#below is the correct clean code to join ego_locs and RADAR egos

ego_locs$radarVisit <- paste0(ego_locs$radarid,ego_locs$visit_number)
RADAR_egos$radarVisit <- paste0(RADAR_egos$radarid,RADAR_egos$wave)

comb_egodata <- merge(RADAR_egos,ego_locs,by="radarVisit",all.x=TRUE)

which(is.na(comb_egodata$tractLat))
which(is.na(comb_egodata$tractLong))

```


#### Find and Remove NAs
```{r echo = FALSE}

#look at NAs
comb_NAs <- comb_egodata %>%
  filter(is.na(tractLong))

#confirm NA ego_locs are outside Chicago
unique(comb_NAs$ego_loc)

#compare number of NAs to number of non NAs
comb_notNAs <- comb_egodata %>%
  filter(!is.na(tractLong))

view(comb_notNAs)

#confirm only 5 NAs in combined dataset
table(comb_egodata$ego_loc,useNA='always')


#drop NAs from combined dataset
comb_egodata_clean <- comb_egodata %>%
  filter(!is.na(tractLong))

#confirm number of non-NAs are equal to number of not NAs in comb_notNAs

```

#### Add Race, Gender, and Sexuality Labels to RADAR Egos


```{r echo = FALSE}


comb_egodata_clean$race_ethnicity <- factor(comb_egodata_clean$race_ethnicity, levels = c(1,2,3,4,5,6,7,8), labels = c("American Indian or Alaska Native", "Asian", "Black or African American", "Hispanic/Latino", "Missing", "White", "Multi-racial (does not include Hispanic/Latino)", "Other"))

#confirm labels names correspond with levels
unique(comb_egodata_clean$race_ethnicity)

comb_egodata_clean$sexIdentity <- factor(comb_egodata_clean$sexIdentity, levels = c(1,2,3,4,5,6,7), labels = c("Gay", "Lesbian", "Bisexual", "Queer", "Unsure/Questioning", "Straight/Heterosexual", "Not listed (please specify)"))

#confirm labels names correspond with levels
unique(comb_egodata_clean$sexIdentity)

comb_egodata_clean$gender <- factor(comb_egodata_clean$gender, levels = c(1,2,3,4,5), labels = c("Male", "Female", "Transgender: Male-to-Female", "Transgender: Female-to-Male", "Not listed (please specify)"))


#confirm labels names correspond with levels
unique(comb_egodata_clean$gender)


```



#### Include latest address only in ego_locs / remove duplicate

```{r echo = FALSE}

#include latest address only in combined file
comb_egodata_clean <- comb_egodata_clean %>%
  group_by(radarid.y) %>%
  arrange(desc(visit_number)) %>%
  slice(1) %>%
  ungroup()



#check for NAs in essential fields
which(is.na(comb_egodata_clean$radarid.x))
which(is.na(comb_egodata_clean$ego_loc))

#remove NAs

```




#### Spin off full ID for later joins

```{r echo = TRUE}


#check length of partial RADAR ID to ensure we are adding enough 0's
unique(nchar(comb_egodata_clean$radarid.x))

#check length of visit number to ensure we are adding enough 0's
unique(nchar(comb_egodata_clean$visit_number))

#combine radarID and visit from ego location file into a new variable
##then combine strings from both cols into new variable with 4 0's separating the ID and the visit, just like the normal RADAR ID
comb_egodata_final <- comb_egodata_clean %>%
  unite(combo, radarid.x, visit_number, remove = FALSE) %>%
  mutate(fullId = sub("_", "0000", combo))

#verify
colnames(comb_egodata_final)
unique(nchar(comb_egodata_final$fullId))

```



### Clean combined dataset

```{r echo = TRUE}

#check for NAs
which(is.na(comb_egodata_final$fullId))
which(is.na(comb_egodata_final$race_ethnicity))
which(is.na(comb_egodata_final$gender))
which(is.na(comb_egodata_final$sexIdentity))
which(is.na(comb_egodata_final$hiv))
which(is.na(comb_egodata_final$age))
which(is.na(comb_egodata_final$wave))


```

### Venue DF Cleaning

```{r echo = FALSE}

#check for NAs in venue df
which(is.na(venue$radarid))

#confirmed NA; drop rows where NA is found in radarID column
venue_cleaned <-
  venue %>% drop_na(radarid)

#filter for only TRUE blanks / not NAs to see how many there are
venue_blanks <- venue_cleaned %>% 
  filter(venueNameNew=="" & (!is.na(venueNameNew)))

#696 obs; convert true blanks to NAs
venue_cleaned$venueNameNew[venue_cleaned$venueNameNew==""] <- NA

#check for count of more NAs in venue df
length(which(is.na(venue_cleaned$venueNameNew)))
#896 total, which makes sense (696 true NAs plus 200 already NAs)


#look at NAs in venueNameNew
NA_venuenameNew <- venue_cleaned %>% 
  filter(is.na(venueNameNew))
#696 --> good

view(NA_venuenameNew)

#these venues are real places, they just do not have a corresponding venue name New applied

#export NA_venuenameNew to csv
#write.csv(NA_venuenameNew,"R:/ISGMH/EDIT/Projects/PLoT ME/Data/Scarlett/PLoT ME//NA_venuenameNew_2.csv", row.names = FALSE)

#reimport
#NA_venuenameNew <- read.csv("NA_venuenameNew_2.csv")

#merge back with venue_cleaned and re-run analyses


#subset from NA_venuenameNew bc you want everything from venue_cleaned since you're merging into that
NA_venuenameNew <- NA_venuenameNew %>% 
  dplyr::select(venueNameNew)

# venue_cleaned_test <- left_join(venue_cleaned, NA_venuenameNew, by = "venueNameNew")
# venue_cleaned_test <- merge(venue_cleaned, NA_venuenameNew, by = "venueNameNew")

#there are other venueNameNews with "" instead of NA in venue_cleaned df; will need to also assign these too

#remove blanks df, don't need
rm(venue_blanks)
```





#### First, join census conversion and venue in order to bring in venue lat/long

```{r echo = TRUE}

#rename RADAR ID in venue to "fullId" to match same in new_ego_locs
#UPDATE: col name already updated when fullID was spun off above?
venue_cleaned <- venue_cleaned %>%
  rename(fullId = radarid)

#verify change made
colnames(venue_cleaned)
glimpse(venue_cleaned)

#fullID is data type int. needs to be char for later joins
venue_cleaned$fullId <- as.character(venue_cleaned$fullId)
glimpse(venue_cleaned)

#still do not have lat and long of venues, just of ego homes; need to translate venue geolocations to lat long

#convert geocodes to census tracts

censusjoin_venue <- inner_join(censusConversion, venue_cleaned, by = c("censusCode" = "venueLocationNew"))

#rename lat long columns here to venue specific lat long so you can tell the difference b/w this and ego home later
censusjoin_venue <- censusjoin_venue %>%
  rename(lat_venue = lat) %>%
  rename(long_venue = long)

colnames(censusjoin_venue)
```





### Join venue-level and ego-level dataframes

```{r echo = TRUE}
#
venue_egos <- inner_join(comb_egodata_final, censusjoin_venue, by = "fullId")


#check for NAs in merged venue df for required variables
which(is.na(venue_egos$fullId))
which(is.na(venue_egos$lat_venue))
which(is.na(venue_egos$long_venue))
which(is.na(venue_egos$tractLat))
which(is.na(venue_egos$tractLong))

```


### Look at difference in RADAR IDs between venue_egos and comb_egodata_final

#### spin off new df and anti-join them based on RADAR ID

```{r}

venueego_diff <- venue_egos[c(16)]
venueego_diff <- venueego_diff %>% 
 # rename(fullId_venueegos = fullId) %>% 
  distinct(fullId, .keep_all = TRUE)

comb_diff <- comb_egodata_final[c(16)]
comb_diff <- comb_diff %>% 
 # rename(fullId_comb_diff = fullId) %>% 
  distinct(fullId, .keep_all = TRUE)

ego_novenues <- anti_join(comb_diff, venueego_diff, by = "fullId")

nrow(ego_novenues)

#confirm number of rows in ego_novenues is equal to diff from two dfs:
614 - 567
614 - 47

#but number of MSM is 557, not 567 --> check to see if removing filter evens it out
```

### Venue Types

#### Clean Venue Types Dataframe

##### Exploratory + NA Removal

```{r echo = TRUE}


glimpse(venue_types)

#find NAs in venue name new
which(is.na(venue_types$venueNameNew))

#investigate NAs

NA_venue_types <- venue_types %>% 
  filter(is.na(venueNameNew))

view(NA_venue_types)

#all venue names are NA, remove from df

venue_types_cleaned <- venue_types %>% 
  filter(!is.na(venueNameNew))

#confirm NAs are gone
which(is.na(venue_types_cleaned$venueNameNew))

#remove NA venue types df
rm(NA_venue_types)

```



##### Create Full RADARID to join on venue df later

```{r echo = TRUE}

###create full RADARID to join on venue df later

venue_types_cleaned <- venue_types_cleaned %>%
   unite(combo, radarid, visit, remove = FALSE) %>%
   mutate(fullId = sub("_", "0000", combo))

```


##### Remove duplicate venues

```{r echo = TRUE}

venue_types_dedup <- venue_types_cleaned %>%
  distinct(venueNameNew, .keep_all = TRUE)

```

##### Look at venue type names in non-distinct df to make sure names are clean

```{r}

unique_venue_names <- unique(venue_types_cleaned$venueNameNew)
unique_venue_names <- as.data.frame(unique_venue_names)
unique_venue_names$unique_venue_names[unique_venue_names$unique_venue_names == ""] <- NA
unique_venue_names <- unique_venue_names %>% 
  filter(!is.na(unique_venue_names))
#rm(unique_venue_names)


```

##### Assign ID to venue names based on venue name in non-distinct df

```{r}

#pull only venue name and venue type
require(dplyr)

#assign ID to venue names based on venue name in non-distinct df
#also, spin off new venue type col based on old venue type col to do future edits
venue_types_group <- venue_types_cleaned %>%
  group_by(venueNameNew) %>%
  dplyr::select(fullId, radarid, visit, venueNameNew, venueType) %>%
  mutate(groupid = cur_group_id()) %>%
  mutate(new_venueType = venueType)
#%>% filter(!is.na(venueNameNew)) comment out since we troubleshoot based on vector below

#NAs were not removed. tried again in a code chunk below. same thing happens. tells me those "NAs" are not being read as NA's but instead as something else. let's look below

glimpse(venue_types_group[c(27, 40),])

#blank venueNameNew's come in as blanks instead of NAs. convert blanks to NAs
venue_types_group$venueNameNew[venue_types_group$venueNameNew==""] <- NA

#test out to make sure it worked
glimpse(venue_types_group[c(27, 40),])

#now we can filter NAs
venue_types_group <- venue_types_group %>%
  filter(!is.na(venueNameNew))


```

##### populate NA and duplicate venue types

```{r}

# Select venues name that have a venue type
listOfNonmissingVenues <- venue_types_group$venueNameNew[!is.na(venue_types_group$venueType)]

# Select venues non-missing and "fake" missing for venue type
venueNonNa <- venue_types_group[venue_types_group$venueNameNew %in% listOfNonmissingVenues,]

# Create key for "fake" missing, exclude rows with missing values
venueNonNaKey <- unique(venueNonNa[!is.na(venueNonNa$venueType),c("venueNameNew", "venueType", "groupid")])

# Recode venueType name:
colnames(venueNonNaKey)[colnames(venueNonNaKey) %in% "venueType"] <- "venueTypeNew"

##find all rows with duplicate venueNameNew in venueNonNaKey df
venueNonNaKey_dups <- venueNonNaKey %>%
  group_by(groupid) %>%
  filter(n() > 1)

# Select venues that are not duplicates
veueNonNaKey_nonDups <- venueNonNaKey[!(venueNonNaKey$groupid %in% venueNonNaKey_dups$groupid),]
# Create subcategory code as
veueNonNaKey_nonDups$Subcategory <- NA

#export to csv
#write.csv(venueNonNaKey_dups,"R:/ISGMH/EDIT/Projects/PLoT ME/Data/Scarlett/PLoT ME//venueNonNaKey_dups.csv", row.names = FALSE)

#bring file back in
venueNonNaKey_dups <- read.csv("venueNonNaKey_dups - Copy.csv")

#deduplicate venueNonNaKey_dups
venueNonNaKey_deduped <- venueNonNaKey_dups %>% 
  distinct(groupid, .keep_all = TRUE)

#Remove z_venueTypeNew variable
venueNonNaKey_deduped <- venueNonNaKey_deduped[,-which(colnames(venueNonNaKey_deduped) %in% "z_venueTypeNew")]

#Row biond deduped and non dupe keys
venueNonNaKey_combined <- rbind(veueNonNaKey_nonDups,venueNonNaKey_deduped)

# Merge back key into "fake" missing data frame
# Should create new data frame with one new column  "venueTypeNew" with no missing values
venueNonNaFinal <- merge(venueNonNa[,-which(colnames(venueNonNa) %in% "new_venueType")],venueNonNaKey_combined[,c("venueTypeNew","venueNameNew","Subcategory")],by="venueNameNew",all.x=TRUE)

#Select venues with "true" missing type
venueTrueNa <- venue_types_group[!(venue_types_group$venueNameNew %in% listOfNonmissingVenues),]

#bring in updated venue file
venueTrueNaKey <- read.csv("venue_typeNAs.csv")

# Subset columns with venueNameNew and venueTypeCoded (i.e., whatever you called column with code)
venueTrueNaKey <- venueTrueNaKey[,c("venueNameNew", "new_venueType", "Subcategory")]

# Rename new venue type name to be same as other data frame
colnames(venueTrueNaKey)[colnames(venueTrueNaKey) %in% "new_venueType"] <- "venueTypeNew"

# Merge in key for missing data
# Should create new data frame with one new column "venueTypeNew" with no missing values
#venueTrueNaFinal <- merge(venueTrueNa,venueTrueNaKey,by="new_venueType")
venueTrueNaFinal <- merge(venueTrueNa[,-which(colnames(venueTrueNa) %in% c("new_venueType"))],venueTrueNaKey,by="venueNameNew")

venueDataCoded <- rbind(venueNonNaFinal,venueTrueNaFinal)

nrow(venueDataCoded)==nrow(venue_types_group)

which(is.na(venueDataCoded$venueNameNew))


#remove dfs used to create venueDataCoded
rm(venueNonNa, venueNonNaFinal, venueNonNaKey, venueNonNaKey_combined, venueNonNaKey_deduped, venueNonNaKey_dups, venueTrueNa, venueTrueNaFinal, venueTrueNaKey, veueNonNaKey_nonDups)

```

#### Bring groupid key of distinct venues to code the 900 without venueNameNew
```{r}


distinct_venueDataCoded <- venueDataCoded %>% 
  distinct(groupid, .keep_all = TRUE)

#merge with venue_cleaned to bring in venueLocationNew

distinct_venueDataCoded_loc <- merge(distinct_venueDataCoded, venue_cleaned, by = "venueNameNew")

distinct_venueDataCoded_test <- distinct_venueDataCoded_loc %>% 
  distinct(venueNameNew, .keep_all = TRUE)

#write.csv(distinct_venueDataCoded_test,"R:/ISGMH/EDIT/Projects/PLoT ME/Data/Scarlett/PLoT ME//distinct_venueDataCodedloc.csv", row.names = FALSE)


```

##### Merge Chinatown and Chinatown Square

```{r}
#use separate df to make sure this works
venue_Chinatown <- venueDataCoded %>% 
  filter(grepl("Chinatown$", venueNameNew) | grepl("Chinatown Square", venueNameNew))


venue_Chinatown$groupid[venue_Chinatown$groupid==246] <- 248

#make change to venueDataCoded and then remove venue_Chinatown

#first confirm nrows for venueDataCoded in case anything changes

nrow(venueDataCoded)
#5876

#now confirm that groupids 246 and 248 are Chinatown and Chinatown Square in venueDataCoded df

venueDataCoded %>% 
  filter(groupid == 246 | groupid == 248)
#yes

venueDataCoded$groupid[venueDataCoded$groupid==246] <- 248

nrow(venueDataCoded)
#5876

#remove venue_Chinatown
rm(venue_Chinatown)

```


### Clean and Join App Data

```{r echo = TRUE}

#check for NAs in key variables
which(is.na(apps$radarid))

#look at NAs for these rows
NA_appRADARID <- apps[c(2548:2554),]

view(NA_appRADARID)

#drop these NAs from app df
apps <- apps %>%
  drop_na(radarid)

#check that NAs are gone
which(is.na(apps$radarid))

#remove NA_appRADARID df
rm(NA_appRADARID)

#check NAs in more fields
length(which(is.na(apps$appNameNew)))

#look at NAs in appNameNew col
NA_appnameNew <- apps %>% 
  filter(is.na(appNameNew))
         
view(NA_appnameNew)


#some of these apps may be actual apps. need to look at list of NAs and run against the final unique apps list in the working directory
#all apps are valid except for reelfish; drop reelfish row
apps <- apps[-c(1999),]

#confirm reelfish row is gone
unique(apps$appName)

#check out appNameNews
unique(apps$appNameNew)


#we see a blank appNameNew, pull it up. pull by apps listed "not an app" in the excel sheet.
grep("Not an app", apps$notes, fixed = TRUE)

apps[c(529, 2138),]

#Sugar Daddy is an app/website, so it can stay. remove the hotel though

# remove hotel by index
apps <- apps[-c(2138),]
unique(apps$appName)


distinct_appnamenew <- apps %>% distinct(appNameNew, .keep_all = TRUE)
distinct_appname <- apps %>% distinct(appName, .keep_all = TRUE)

```








### Normalize nulls in lgbt column:

There are three inputs for lgbt column: 1, -99, and NA. 1 denotes space has primary LGBT patrons. -99 and NA denote don't want to answer or data unavailable--

We will normalize to 1 and 0.--

```{r echo=TRUE}

#check options for lgbt flag
unique(venue_egos$lgbt)


#change -99's to NA's, and then change NA's to 0
venue_egos$lgbt[venue_egos$lgbt==-99] <- NA
venue_egos$lgbt[is.na(venue_egos$lgbt)] <- 0


#verify only 1 and 0 exist for lgbt column
unique(venue_egos$lgbt)

#
# venue$lgbt[venue$lgbt==-99] <- NA
# venue$lgbt[is.na(venue$lgbt)] <- 0
#
#
# #verify only 1 and 0 exist for lgbt column
# unique(venue$lgbt)


```

### Normalize met_sex flag


```{r}

#check options for metSex flag
unique(venue_egos$metSex)

#convert -99 to 0's to treat metSex as a yes/no flag
venue_egos$metSex[venue_egos$metSex==-99] <- 0

#confirm changes made
unique(venue_egos$metSex)


#
# #check options for metSex flag
# unique(venue$metSex)
#
# #convert -99 to 0's to treat metSex as a yes/no flag
# venue$metSex[venue$metSex==-99] <- 0
#
# #confirm changes made
# unique(venue$metSex)


```


### Normalize nulls in alcohol freq column:

Alcohol Freq scale:
•	2: Very Common
•	1: Somewhat Common
•	0: Not at All Common
•	-1: Don’t Want to Answer

Will cluster -99's into -1

```{r}


unique(venue_egos$alcoholFreq)

#make -1 instead of -99 since 0 means not at all common instead of a yes/no flag. -1 is Don't want to answer
venue_egos$alcoholFreq[venue_egos$alcoholFreq==-99] <- -1

#confirm changes made
unique(venue_egos$alcoholFreq)







#
# unique(venue$alcoholFreq)
#
# #make -1 instead of -99 since 0 means not at all common instead of a yes/no flag. -1 is Don't want to answer
# venue$alcoholFreq[venue$alcoholFreq==-99] <- -1
#
# #confirm changes made
# unique(venue$alcoholFreq)
#

```


### Normalize nulls in drug Freq column:

Drug freq scale:


•	2: Very Common
•	1: Somewhat Common
•	0: Not at All Common
•	-1: Don’t Want to Answer


```{r}

unique(venue_egos$drugFreq)

#need to cluster -99's into -1s since -1 is do not want to answer
venue_egos$drugFreq[venue_egos$drugFreq==-99] <- -1


unique(venue_egos$drugFreq)


# unique(venue$drugFreq)
#
# #need to cluster -99's into -1s since -1 is do not want to answer
# venue$drugFreq[venue$drugFreq==-99] <- -1
#
#
# unique(venue$drugFreq)

```


### Normalize sex Freq column

•	2: Very Common
•	1: Somewhat Common
•	0: Not at All Common
•	-1: Don’t Want to Answer


```{r}

unique(venue_egos$sexFreq)

#need to convert -99s into 0
venue_egos$sexFreq[venue_egos$sexFreq==-99] <- -1

unique(venue_egos$sexFreq)



# unique(venue$sexFreq)
#
# #need to convert -99s into 0
# venue$sexFreq[venue$sexFreq==-99] <- -1
#
# unique(venue$sexFreq)

```


### Normalize Venue Freq

•	3: Daily
•	2: Weekly
•	1: Monthly
•	0: Less Than Monthly
•	-1: Don’t Want to Answer



```{r}


unique(venue_egos$venueFreq)

#convert -99's into -1
venue_egos$venueFreq[venue_egos$venueFreq==-99] <- -1

unique(venue_egos$venueFreq)



```

### Look at distance travelled between home and venue

```{r echo = TRUE}

#12/7/20: NULL: have both venue and home lat and long; just need to separate them into their own dataframe
#Need to bring in both lat and long from venue AND home venue
#create separate objects for ego and venue locs
spatial_ego <- venue_egos[c(4,7,15:14)]
spatial_venue <- venue_egos[(21:20)]
spatial_combined <- bind_cols(spatial_ego, spatial_venue)

#ensure nrows between the three dfs above are the same

nrow(spatial_ego)==nrow(spatial_venue)
nrow(spatial_ego)==nrow(spatial_combined)

#check data types for spatial_ego and spatial_venue

glimpse(spatial_ego)
glimpse(spatial_venue)


distances <- geodist(spatial_ego, spatial_venue, paired = TRUE)

#continue to get error 'list' object cannot be coerced to type 'double' even though we converted to numeric above

#11/17/2020: ISSUE SOLVED BY INSTALLING DEV PACKAGE OF GEODIST FROM GITHUB

view(distances)
glimpse(distances)

#convert distances from meters to miles
#need manual function


#convert distances from numeric vector to dataframe

class(distances)
distances <- as.data.frame(distances)


distances <- distances %>%
  mutate(distmiles = distances/1609.344)

#append distances list to spatial_combined
spatial_distances <- bind_cols(spatial_combined, distances)




```

## Geospatial Analysis



### Find top 5 venues by CT 

```{r}
#pull venueNameNew, CT, lat/long for venue
popular_CTs <- venue_egos[c(19:21, 33)]

#spin off freq table 
freq_CTs <- as.data.frame(table(unlist(popular_CTs$censusTract.y)))

freq_CTs <- freq_CTs %>% 
  arrange(desc(Freq)) 

#need censusTract now

```

### Find top 5 venues by lat/long and plot

```{r}

popular_latlong <- popular_CTs %>% 
  group_by(venueNameNew) %>% 
  mutate(latlong_concat = paste(lat_venue, long_venue, sep = "_"))

popular_latlong <- as.data.frame(table(unlist(popular_latlong$latlong_concat)))

popular_latlong_freq <- 
  popular_latlong %>% 
  arrange(desc(Freq))


lat <- c("41.9426914", "41.9425769", "41.875817", "41.8850465", "41.8810267")
long <- c("-87.6517704", "-87.6470786", "-87.6172556", "-87.6209322", "-87.6327464")
freq_geo <- c("383", "199", "95", "93", "91")

popular_latlong_geo <- data.frame(lat, long, freq_geo)

popularvenues_geo <- st_as_sf(popular_latlong_geo, coords = c("long", "lat"), crs = 4326)
popularvenues_geo_map <- mapview(popularvenues_geo)
popularvenues_geo_map


```


### Show top 5 venues by venueNameNew

```{r}

popular_venueNames <- venue_egos[c(4, 33)]

freq_venues <- as.data.frame(table(unlist(popular_venueNames$venueNameNew)))

freq_venues <- freq_venues %>% 
  arrange(desc(Freq)) 

```

### Map out home locations

```{r}

#make sure data types are correct
glimpse(spatial_distances)


geo_home <- st_as_sf(spatial_distances, coords = c("tractLong", "tractLat"), crs = 4326)

geo_home_map <- mapview(geo_home)
geo_home_map


#would like to see racial breakout

library(raster)
#install.packages("rgdal")
library(rgdal)
library(ggplot2)
library(broom)
library(RColorBrewer)
#install.packages("rgeos")
library(rgeos)
library(dplyr)




#https://www.earthdatascience.org/courses/earth-analytics/spatial-data-r/make-maps-with-ggplot-in-R/

race_palette <- c("Black or African American" = "green",
                  "White" = "grey40",
                  "Hispanic/Latino" = "purple",
                  "Asian" = "blue",
                  "Multi-racial (does not include Hispanic/Latino)" = "orange",
                  "American Indian or Alaska Native" = "black",
                  "Other" = "red")
race_palette


# plot with custom colors

ggplot() +
  geom_point(data = spatial_distances, aes(x = tractLong, y = tractLat,
      color=factor(race_ethnicity))) +
      scale_colour_manual(values = race_palette) +
  labs(title = "Homes by Race ")


```

```{r}


summarise(spatial_distances, avg = median(distmiles))
#median = 4.2 miles

summarise(spatial_distances, avg = mean(distmiles))
#mean: 5.18 miles



aggregate(x = spatial_distances$distmiles,
          by = list(spatial_distances$race_ethnicity),
          FUN = mean)

```


### Map out geocoded physical venues



#### Show All Venues

```{r echo = TRUE}


#confirm both geocode columns are the same datatype:
glimpse(censusConversion)



#check for NAs
which(is.na(censusConversion))


venue_egos_Black_filtered <- 
  venue_egos %>% filter(race_ethnicity == "Black" & lgbt == 1)

censusmap_all <- venue_egos



#test for filter
#unique(censusjoin_lgbt$lgbt)


#need to see if those IDs associated w emporium in venue data sheet have corresponding ego locs


#some but not all of them exist in the ego-level data

#NEXT STEP: filter by WP only in excel sheet and see if all those IDs in emporium_IDs are only from wicker
#
# geo_all <- st_as_sf(censusmap_all, coords = c("long_venue", "lat_venue"), crs = 4326)
# geo_all_map <- mapview(geo_all)
# geo_all_map

geo_all <- st_as_sf(censusmap_all, coords = c("long_venue", "lat_venue"), crs = 4326)
geo_all <- st_as_sf(venue_egos, coords = c("long_venue", "lat_venue"), crs = 4326)
geo_all_map <- mapview(geo_all)
geo_all_map


```


### Show lgbt venues exclusively

```{r echo = TRUE}


#filter by lgbt
censusmap_lgbt <- censusjoin_venue %>%
  filter(lgbt == 1)


#test for filter
unique(censusmap_lgbt$lgbt)

#spin off map
geo_lgbt <- st_as_sf(censusmap_lgbt, coords = c("long_venue", "lat_venue"), crs = 4326)
geo_lgbt_map <- mapview(geo_lgbt)
geo_lgbt_map




```


### Show High Sex Freq Only

```{r echo = TRUE}


#filter by lgbt
censusmap_sexfreqhigh <- censusjoin_venue %>%
  filter(sexFreq == 2)


#test for filter
unique(censusmap_sexfreqhigh$sexFreq)

#spin off map
geo_sexfreqhigh <- st_as_sf(censusmap_sexfreqhigh, coords = c("long_venue", "lat_venue"), crs = 4326)
geo_sexfreq_map <- mapview(geo_sexfreqhigh)
geo_sexfreq_map



```

### Show Met Sex Only

```{r echo = TRUE}


#filter by lgbt
censusmap_metSex <- censusjoin_venue %>%
  filter(metSex == 1)

#test for filter
unique(censusmap_metSex$metSex)

#spin off map
geo_metSex <- st_as_sf(censusmap_metSex, coords = c("long_venue", "lat_venue"), crs = 4326)
geo_metSex_map <- mapview(geo_metSex)
geo_metSex_map



```

### Show Race

#### Black

```{r echo = TRUE}


#filter by lgbt
censusmap_metSexBlack <- venue_egos %>%
  filter(metSex == 1) %>%
  filter(race_ethnicity == 'Black or African American')

#test for filter
unique(censusmap_metSexBlack$metSex)

#spin off map
geo_metSexBlack <- st_as_sf(censusmap_metSexBlack, coords = c("long_venue", "lat_venue"), crs = 4326)
geo_metSex_mapBlack <- mapview(geo_metSexBlack)
geo_metSex_mapBlack


```
#### White
```{r echo = TRUE}


#filter by lgbt
censusmap_metSexWhite <- venue_egos %>%
  filter(metSex == 1) %>%
  filter(race_ethnicity == 'White')

#test for filter
unique(censusmap_metSexWhite$metSex)

#spin off map
geo_metSexWhite <- st_as_sf(censusmap_metSexWhite, coords = c("long_venue", "lat_venue"), crs = 4326)
geo_metSex_mapWhite <- mapview(geo_metSexWhite)
geo_metSex_mapWhite


```


#### Latino

```{r echo = TRUE}


#filter by lgbt
censusmap_metSexLatino <- venue_egos %>%
  filter(metSex == 1) %>%
  filter(race_ethnicity == 'Hispanic/Latino')

#test for filter
unique(censusmap_metSexLatino$metSex)

#spin off map
geo_metSexLatino <- st_as_sf(censusmap_metSexLatino, coords = c("long_venue", "lat_venue"), crs = 4326)
geo_metSex_mapLatino <- mapview(geo_metSexLatino)
geo_metSex_mapLatino


```


## Exploratory Statistical Analysis on Key Variables

### Count of Race/Ethnicity in Ego Data


```{r echo = TRUE}


RADAR_egos %>%
  group_by(race_ethnicity) %>%
  tally()


```

Unique set of egos by race


### Count of Race/Ethnicity on Merged Data
```{r echo = TRUE}


venue_egos %>%
  group_by(race_ethnicity) %>%
  tally()

```

Number of egos by race and ethnicity multiplied because the numbers account for multiple venues one person attends



### Measures of LGBT venue Frequencies

```{r echo = TRUE}

#create separate df for distinct venues and include LGBT, alcohol, sex, and drug freqs

distinct_venues <- venue_cleaned %>% distinct(venueNameNew, .keep_all = TRUE)

#look at df
glimpse(distinct_venues)


#1,326 distinct venues


#run tallies based on distinct df

unique(distinct_venues$lgbt)
unique(distinct_venues$metSex)

#count of lgbt venue visits
venue_egos %>%
  group_by(lgbt) %>%
  tally()

#count of distinct lgbt venues

venue_egos %>%
  group_by(lgbt) %>%
  tally()



```

Above tallies show distinct counts of LGBT spaces with metSex filter on/off for venues.

BUT this only shows lgbt flags for one person per venue. Now we look at average distribution of LGBT ratings across venues using NOT distinct venue df

#### Look at LGBT tally distributions
```{r echo = TRUE}


summarise(venue_egos, avg = mean(lgbt))
#0.47 lgbt rating across DISTINCT venues

summarise(venue, avg = mean(lgbt))
#0.41 across non-distinct venues

summarise(venue_egos, var(lgbt))
# 0.249 variance


summarise(venue_egos, sd(lgbt))
#sd is 0.449


```

### Count of general high-risk places

```{r}
#create high risk flag
venue_egos <- venue_egos %>% 
  mutate(high_risk = as.integer(alcoholFreq == 2 & drugFreq == 2 & sexFreq == 2))

venue_egos %>% 
  group_by(high_risk) %>% 
  tally()

summarise(venue_egos, avg = mean(high_risk == 1))



```



### Alcohol Freq Breakouts

```{r}
#first get general tally
venue_egos %>%
  group_by(alcoholFreq) %>%
  tally()

summarise(venue_egos, avg = mean(alcoholFreq == 2))


```

Higher average alcohol frequency




```{r}

#general tally
venue_egos %>%
  group_by(drugFreq) %>%
  tally()

#average very common hard drug use at venues
summarise(venue_egos, avg = mean(drugFreq == 2))
#0.1404629	


```



### Count of sex frequency


```{r echo = TRUE}

venue_egos %>%
  group_by(sexFreq) %>%
  tally()


```

### N Venue Frequency

```{r}

#first look at how many in each category
venue_egos %>%
  group_by(venueFreq) %>%
  tally()

#average daily
summarise(venue_egos, avg = mean(venueFreq == 3))

#average weekly
summarise(venue_egos, avg = mean(venueFreq == 2))

#average monthly
summarise(venue_egos, avg = mean(venueFreq == 1))

#average less-than-monthly
summarise(venue_egos, avg = mean(venueFreq == 0))

#average DWTA
summarise(venue_egos, avg = mean(venueFreq == -1))

```


Majority of venues are rated as Not At All Common to meet sexual partners. But SOmewhat and Very Common have almost the same count of spaces

### Count of MetSex


```{r}

#general tally
venue_egos %>%
  group_by(metSex) %>%
  tally()

summarise(venue_egos, avg = mean(sexFreq == 2))

#0.2669593

summarise(venue_egos, var(sexFreq))
#0.7370265

summarise(venue_egos, sd(sexFreq))
#0.8585025



```

Higher average of sex frequency


### N where most people are LGBT

```{r}

distinct_venues %>%
  group_by(lgbt) %>%
  tally()



```


### N types of venues

```{r}


# venueDataCoded_sliced <- venueDataCoded[c(1:2, 7:8)]
# venuetype_egos <- semi_join(venue_egos, venueDataCoded_sliced, by = c("fullId", "venueNameNew"))
# 
# 
# 
# venuetype_egos %>% 
#   group_by(venueTypeNew) %>% 
#   tally()





```



### Show top 5 venues by race lat/long and graph

```{r}





```










## Racial Breakouts


### Look at racial differences in average travel in miles between home and venue

```{r}

spatial_distances_race <- spatial_distances %>% 
  filter(race_ethnicity == "Black or African American" | race_ethnicity == "White" | race_ethnicity == "Hispanic/Latino")

aggregate(x = spatial_distances_race$distmiles,
          by = list(spatial_distances_race$race_ethnicity),
          FUN = mean)


```

### Look at top 5 neighborhoods venues are located by censusTract by Race

```{r}


popular_CTs_race <- venue_egos[c(4, 19:21, 33)]

#spin off freq table 
freq_CTs_race <- as.data.frame(table(unlist(popular_CTs$censusTract.y)))


freq_CTs <- freq_CTs %>% 
  arrange(desc(Freq)) %>% 
  rename(CensusTract = Var1)


#by Black pop
popular_CTs_race %>% 
  group_by(censusTract.y) %>% 
  filter(race_ethnicity == "Black or African American") %>% 
  tally() %>% 
  ungroup() %>% 
  arrange(desc(n)) 

#by Latino pop
popular_CTs_race %>% 
  group_by(censusTract.y) %>% 
  filter(race_ethnicity == "Hispanic/Latino") %>% 
  tally() %>% 
  ungroup() %>% 
  arrange(desc(n)) 

#by white pop
popular_CTs_race %>% 
  group_by(censusTract.y) %>% 
  filter(race_ethnicity == "White") %>% 
  tally() %>% 
  ungroup() %>% 
  arrange(desc(n)) 


```
### Show top 5 venues by lat/long and graph

```{r}
#Black
popular_latlong_Black <- popular_CTs_race %>% 
  filter(race_ethnicity == 'Black or African American') %>% 
  mutate(latlong_concat = paste(lat_venue, long_venue, sep = "_"))

popular_latlong_Black <- as.data.frame(table(unlist(popular_latlong_Black$latlong_concat)))
view(popular_latlong_Black)

# popular_latlong_Blackfreq_test <- 
#   popular_latlong_Blackfreq %>% 
#   arrange(desc(Freq))
# 
# view(popular_latlong_Blackfreq_test)

latBlack <- c("41.9426914", "41.9425769", "41.8983354", "41.9492206", "41.8810267")
longBlack <- c("-87.6517704", "-87.6470786", "-87.6207531", "-87.6519703", "-87.6327464")
freq_geoBlack <- c("98", "54", "32", "31", "29")

popular_latlong_geoBlack <- data.frame(latBlack, longBlack, freq_geoBlack)

popularvenues_geoBlack <- st_as_sf(popular_latlong_geoBlack, coords = c("longBlack", "latBlack"), crs = 4326)
popularvenues_geo_mapBlack <- mapview(popularvenues_geoBlack)
popularvenues_geo_mapBlack


#Latino

popular_latlong_Latino <- popular_CTs_race %>% 
  filter(race_ethnicity == 'Hispanic/Latino') %>% 
  mutate(latlong_concat = paste(lat_venue, long_venue, sep = "_"))

popular_latlong_Latino <- as.data.frame(table(unlist(popular_latlong_Latino$latlong_concat)))
view(popular_latlong_Latino)

# popular_latlong_Blackfreq_test <- 
#   popular_latlong_Blackfreq %>% 
#   arrange(desc(Freq))
# 
# view(popular_latlong_Blackfreq_test)

latLatino <- c("41.9426914", "41.9425769", "41.875817", "41.8850465", "41.8983354")
longLatino <- c("-87.6517704", "-87.6470786", "-87.6172556", "-87.6209322", "-87.6207531")
freq_geoLatino <- c("114", "45", "38", "37", "30")

popular_latlong_geoLatino <- data.frame(latLatino, longLatino, freq_geoLatino)

popularvenues_geoLatino <- st_as_sf(popular_latlong_geoLatino, coords = c("longLatino", "latLatino"), crs = 4326)
popularvenues_geo_mapLatino <- mapview(popularvenues_geoLatino)
popularvenues_geo_mapLatino



#White


popular_latlong_White <- popular_CTs_race %>% 
  filter(race_ethnicity == 'White') %>% 
  mutate(latlong_concat = paste(lat_venue, long_venue, sep = "_"))

popular_latlong_White <- as.data.frame(table(unlist(popular_latlong_White$latlong_concat)))
view(popular_latlong_White)

# popular_latlong_Blackfreq_test <- 
#   popular_latlong_Blackfreq %>% 
#   arrange(desc(Freq))
# 
# view(popular_latlong_Blackfreq_test)

latWhite <- c("41.9426914", "41.9425769", "41.875817", "41.8810267", "41.8850465")
longWhite <- c("-87.6517704", "-87.6470786", "-87.6172556", "-87.6327464", "-87.6209322")
freq_geoWhite <- c("124", "77", "32", "20", "19")

popular_latlong_geoWhite <- data.frame(latWhite, longWhite, freq_geoWhite)

popularvenues_geoWhite <- st_as_sf(popular_latlong_geoWhite, coords = c("longWhite", "latWhite"), crs = 4326)
popularvenues_geo_mapWhite <- mapview(popularvenues_geoWhite)
popularvenues_geo_mapWhite

```

### Spin off venue_egos df with only the races you want to look at

```{r}

venue_egos_race <- 
  venue_egos %>% 
  filter(race_ethnicity == "Black or African American" | race_ethnicity == "White" | race_ethnicity == "Hispanic/Latino")

unique(venue_egos_race$race_ethnicity)


venue_egos_race %>% 
  group_by(race_ethnicity) %>% 
  tally()

#write.csv(venue_egos_race,"R:/ISGMH/EDIT/Projects/PLoT ME/Data/Scarlett/PLoT ME//venue_egos_race.csv", row.names = FALSE)

```

#### metSex Prevalence

```{r}

venue_egos_race %>% 
  group_by(race_ethnicity) %>% 
  filter(metSex == 1) %>% 
  tally()




```


#### Look at Racial Venue Attendance Patterns by Risk Variables

##### Heavy Drinking Alcohol Frequency by Race

```{r}


venue_egos_race %>%
  group_by(race_ethnicity) %>%
  filter(alcoholFreq == 2) %>%
  tally()


```



##### Drug Frequency

```{r}


venue_egos_race %>%
  group_by(race_ethnicity) %>%
  filter(drugFreq == 2) %>%
  tally()


```

Black folks have higher attendance at venues where drug usage is Somewhat or Very Common


##### Sex Frequency


```{r}

venue_egos_race %>%
  group_by(race_ethnicity) %>%
  filter(sexFreq == 2) %>%
  tally()



```


Black folks have highest attendance at spaces attended to meet sexual partners, followed closely by Hispanic/Latino people


##### High-risk Venue Frequency

```{r}


venue_egos_race %>%
  group_by(race_ethnicity) %>%
  filter(high_risk == 1) %>% 
  tally()




```

##### LGBT Venue Frequency

```{r}

venue_egos_race %>%
  group_by(race_ethnicity) %>%
  filter(lgbt == 1) %>% 
  tally()


```



##### General Venue Frequency

```{r}
#Daily
venue_egos_race %>%
  group_by(race_ethnicity) %>%
  filter(venueFreq == 3) %>% 
  tally()

#Weekly
venue_egos_race %>%
  group_by(race_ethnicity) %>%
  filter(venueFreq == 2) %>% 
  tally()

#Monthly
venue_egos_race %>%
  group_by(race_ethnicity) %>%
  filter(venueFreq == 1) %>% 
  tally()


#Less than Monthly
venue_egos_race %>%
  group_by(race_ethnicity) %>%
  filter(venueFreq == 0) %>% 
  tally()
```

##### Most popular Venues by race

```{r}

popular_venues_race <- venue_egos[c(4, 33)]

#by Black pop
popular_venues_race %>% 
  group_by(venueNameNew) %>% 
  filter(race_ethnicity == "Black or African American") %>% 
  tally() %>% 
  ungroup() %>% 
  arrange(desc(n)) 

#by Latino pop
popular_venues_race %>% 
  group_by(venueNameNew) %>% 
  filter(race_ethnicity == "Hispanic/Latino") %>% 
  tally() %>% 
  ungroup() %>% 
  arrange(desc(n)) 


#by white pop
popular_venues_race %>% 
  group_by(venueNameNew) %>% 
  filter(race_ethnicity == "White") %>% 
  tally() %>% 
  ungroup() %>% 
  arrange(desc(n)) 

```































```{r echo = TRUE}


sex_venues <- censusjoin_venue %>%
  filter(sexFreq == 2 & lgbt == 1)



#test for filter
unique(sex_venues$sexFreq)


unique(sex_venues$lgbt)


```




<!-- ### Venue Categories -->

```{r echo=TRUE}


metPartnerVenues <- metPartnerVenues %>%
  rename(
  venue_types = dyad_edge.locMet
)


nrow(metPartnerVenues)



metPartnerVenues <- metPartnerVenues %>%
  group_by(venue_types) %>%
  summarise(n = n())


metPartnerVenues %>%
  top_n(5, n) %>%
  ggplot(aes(x = venue_types, y = n, fill = venue_types)) +
  geom_bar(stat='identity')





```

<!-- Most frequented venues are apps. Second is Somewhere Else. Let's look into this one later on. -->




### App Frequency

The below graph shows the top 5 apps used for meeting partners


```{r echo=TRUE}


glimpse(apps)

summary(apps)

unique(apps$appNameNew)



sum_apps <- apps %>%
  group_by(appNameNew) %>%
  summarise(n = n())


sum_apps %>%
  top_n(5, n) %>%
  ggplot(aes(x = appNameNew, y = n, fill = appNameNew)) +
  geom_bar(stat='identity')



```

Grindr and Tinder are our top two. What is the racial break out per app? Is Scruff a sub-community app? What about Jack'd?


```{r echo = TRUE}



# plot_metSex <- venue %>%
#   filter(lgbt == 1 & metSex == 1) %>%
#   group_by(race_ethnicity)
#
#



```

### Next Steps: Look at venues and apps by freq (daily, weekly, monthly)

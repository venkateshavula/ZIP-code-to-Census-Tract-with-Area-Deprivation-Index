# ZIP-code-to-Census-Tract-with-Area-Deprivation-Index

##### Loading required libraries

``` r
library(readxl)
library(readr)
library(dplyr)
library(maps)
library(ggplot2)
library(ggpubr)
```

##### Read data from different public websites

``` r
## U.S. FIPS codes for counties and county equivalent entities
national_county <- read_csv("national_county.txt",col_names = FALSE)
names(national_county) <- c("state_code","fips_state","fips_county","county","fips_class_code")
national_county$state <- state.name[match(national_county$state_code,state.abb)]
national_county<- national_county %>%
  mutate(state=ifelse(fips_state=="60","American Samoa",
                      ifelse(fips_state=="66","Guam", 
                             ifelse(fips_state=="69","Northern Mariana Islands",
                                    ifelse(fips_state=="72","Puerto Rico",
                                           ifelse(fips_state=="74","U.S. Minor Outlying Islands",
                                                  ifelse(fips_state=="78","Virgin Islands",
                                                         ifelse(fips_state=="11","District of Columbia",state)
                                                         )
                                                  )
                                           )
                                    )
                             )
                      )
         )

## Area Deprivation Index(ADI) data at the block group level
ADI_US_fips <- read.csv("us_bg_v1.5.txt")
ADI_US_fips$fips_state <- substr(ADI_US_fips$GISJOIN,2,3)
ADI_US_fips$fips_county <- substr(ADI_US_fips$GISJOIN,5,7)
ADI_US_fips$fips_tract <- substr(ADI_US_fips$GISJOIN,9,14)
ADI_US_fips$fips_block_group <- substr(ADI_US_fips$GISJOIN,14,15)

## HUD-USPS ZIP Crosswalk Files
ZIP_TRACT <- read_excel("ZIP_TRACT_062018.xlsx")
ZIP_TRACT$fips_state <- substr(ZIP_TRACT$tract,1,2)
ZIP_TRACT$fips_county <- substr(ZIP_TRACT$tract,3,5)
ZIP_TRACT$fips_tract <- substr(ZIP_TRACT$tract,6,11)
```

##### Data Wrangling

``` r
## Link FIPS codes of block groups with ZIP
ADI <- ADI_US_fips[,c(3:9)] %>%
  left_join(ZIP_TRACT[,c(1,7:9)],by=c("fips_state","fips_county","fips_tract")) %>%
  left_join(national_county[,c(1:4,6)],by=c("fips_state","fips_county")) 

## Aggregating ranks per ZIP codes
agg_ranks<- ADI %>%
               group_by(zip) %>%
               summarise(ADI_national_median=ceiling(median(ADI_NATRANK)),
                         ADI_national_avg = round(mean(ADI_NATRANK),3),
                         ADI_state_median=ceiling(median(ADI_STATERNK)),
                         ADI_state_avg = round(mean(ADI_STATERNK),3))

## Area Deprivation Index(ADI) data at the block group level including ZIP codes
ADI <- ADI %>% 
       left_join(agg_ranks,by=c("zip")) %>%
       rename(ADI_national_rank=ADI_NATRANK,ADI_state_rank=ADI_STATERNK)%>%
       select(FIPS,fips_state,fips_county,fips_county,fips_tract,fips_block_group,
              state_code,state,county,zip,ADI_national_rank,ADI_national_avg,
              ADI_national_median,ADI_state_rank,ADI_state_avg,ADI_state_median)
```

##### Save data to a local file

``` r
## Save data to a local file
write.table(ADI,"ADI.txt",row.names=F,quote=FALSE,sep="\u0001")
```

###### <i><em> References </i></em>:-

<small> U.S. Census Bureau:<i> <https://www.census.gov/geo/reference/codes/cou.html> </i></br> U.S. Department of Housing and Urban Development:<i><https://www.huduser.gov/portal/datasets/usps_crosswalk.html> </i></br> University of Wisconsin School of Medicine and Public Health:<i> <https://www.neighborhoodatlas.medicine.wisc.edu/> </i> </small>

## code to prepare `texas_prostate` dataset goes here
# library(haven)
# texas_prostate <- read_sas(".private_data/TCR/texas_prostate_cancer.sas7bdat")
# usethis::use_data(texas_prostate, overwrite = TRUE)




library(readxl)
rgcsdi <- tibble(read.csv("data-raw/rgcsdi-2015-2019-county.csv"))
rgcsdi <- rgcsdi|>
    rename(county_fips=COUNTY_FIPS)

rural_urban <- read_excel("data-raw/2020_UA_COUNTY.xlsx",
                          sheet =1)
rural_urban <- rural_urban|>
    mutate(
        county_fips = as.numeric(paste0(STATE, COUNTY)), 
        rural_percent = ALAND_PCT_RUR,
        is_rural = rural_percent> quantile(rural_percent,0.5)
    )|>
    rename(
        state_name = STATE_NAME,
        county_name = COUNTY_NAME
    )|>
    select(county_fips, state_name, county_name, rural_percent,is_rural)

SDOH <- rgcsdi|>
    left_join(rural_urban, by = "county_fips")


keep <- c("county_fips", "County_population", 
"sdi", "pct_Poverty_LT100", "pct_Single_Parent_Fam", "pct_Education_LT12years",
"pct_NonEmployed", "pctHH_No_Vehicle", "pctHH_Renter_Occupied",
"pctHH_Crowding", "state_name", "county_name", "rural_percent",
"is_rural")

SDOH$is_rural <- factor(SDOH$is_rural, levels = c(FALSE, TRUE), labels = c("Urban", "Rural"))

SDOH|> 
select(all_of(keep))->
SDOH



usethis::use_data(SDOH, overwrite = TRUE)


# VA counties - downloaded via the awesome tigris package
library(tigris)
counties <- counties(state="TX",class = "sf")
counties$county_fips <- as.numeric(paste0(counties$STATEFP, counties$COUNTYFP))
usethis::use_data(counties, overwrite = TRUE)


nursingHome <- read_excel("data-raw/uri_arcgis_df.xls")
usethis::use_data(nursingHome, overwrite = TRUE)


## states data
states <- tigris::states()
#usethis::use_data(states, overwrite = TRUE)


## simulate individual data
texas_counties <- SDOH|>
    filter(state_name == "Texas")
ADRD <- list()
for(i in seq_len(nrow(texas_counties))){
    county <- texas_counties[i,]
    population <- county$County_population
    rural_percent <- county$rural_percent
    ## incidence rate = 0.0001 + 0.0001*sdi + 0.001*female + 0.001*pctHH_Crowding + 0.0001*age
    sex <- sample(c(0, 1), size = population, replace = TRUE, prob = c(0.5, 0.5))
    age <- runif(population, 40, 100)
    rate <- 0.0001 + 0.001*sex + 0.1*county$pctHH_Crowding + 0.0001*age + runif(1, 0, 0.04) + 0.02*county$is_rural*county$sdi
    ## limit the range of rate to 0 and 1
    rate <- pmin(pmax(rate, runif(1,0.00001, 0.0001)), 1)
    outcome <- rbinom(population, 1, rate)
    nsub <- sum(outcome)
    sex = sex[outcome==1]
    age = age[outcome==1]
    ADRD[[i]] <- tibble(
        id = seq_len(nsub),
        county_fips = rep(county$county_fips, nsub),
        county_name = rep(county$county_name, nsub),
        state_name = rep(county$state_name, nsub),
        age = age,
        sex = sex,
        ADRD_diag = 1
    )
}
individuals <- do.call(rbind, ADRD)
usethis::use_data(individuals, overwrite = TRUE)



################################################
## Final data that is going to be used by shiny
################################################
defaultDatasets <- new.env()

createDataset <- function(data, name, level, geoColumns){
  uniques <- unname(!sapply(geoColumns,function(x)anyDuplicated(data[[x]])))
  defaultDatasets[[name]] <- list(
    level = level,
    geoColumns = geoColumnsProp(geoColumns, uniques),
    data = data
  )
}

createMapDataset <- function(name, level, geoColumns, data){
  uniques <- unname(!sapply(geoColumns,function(x)anyDuplicated(data[[x]])))
  defaultDatasets[[name]] <- list(
    data = data,
    level = level,
    geoColumns = geoColumnsProp(geoColumns, uniques)
  )
}

createDataset(SDOH, "SDOH", "county", c("county_fips"))
createDataset(individuals, "ADRD", "subject", c("county_fips"))

usethis::use_data(defaultDatasets, overwrite = TRUE)

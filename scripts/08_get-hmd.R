#######################################
# START DATA SET W/ HMD COUNTRY CODES #
#######################################

HMD.code <-
  data.frame(
  rbind(
    c("Australia", "AUS"),  
    c("Austria", "AUT"), 
    c("Belarus", "BLR"), 
    c("Belgium",  "BEL"),
    c("Bulgaria", "BGR"),
    c("Canada", "CAN"), 
    c("Chile", "CHL"),
    c("Croatia", "HRV"),
    c("Czechia", "CZE"),
    c("Denmark", "DNK"),
    c("Estonia", "EST"),
    c("Finland", "FIN"),
    c("France",  "FRATNP"), 
    c("Germany", "DEUTNP"), 
    c("Greece", "GRC"),
    c("Hong Kong", "HKG"),
    c("Hungary", "HUN"),
    c("Iceland", "ISL"),
    c("Ireland", "IRL"),
    c("Israel", "ISR"),
    c("Italy", "ITA"),
    c("Japan", "JPN"), 
    c("Latvia", "LVA"),
    c("Lithuania", "LTU"),
    c("Luxembourg", "LUX"),
    c("Netherlands", "NLD"),
    c("New Zealand", "NZL_NP"),
    c("Norway", "NOR"),
    c("Poland", "POL"),
    c("Portugal", "PRT"),
    c("Republic of Korea", "KOR"),
    c("Russia", "RUS"),
    c("Slovakia", "SVK"),
    c("Slovenia", "SVN"),
    c("Spain", "ESP"),
    c("Sweden", "SWE"),
    c("Switzerland", "CHE"),
    c("Taiwan", "TWN"),
    c("United Kingdom", "GBR_NP"),
    c("USA", "USA"),
    c("Ukraine", "UKR")
    )
  )

## assign column names
names(HMD.code) <- c("Country", "Code")

#####################################
# END DATA SET W/ HMD COUNTRY CODES #
#####################################

# ----

############################################
# START SELECT COUNTRIES W/ AVAILABLE DATA #
############################################

## load HMD exposures
Nx.1 <-
  lapply(HMD.code$Code, 
         function(x)
           HMDHFDplus::readHMD(unz(description = here::here("data", "hmd_statistics_20231006.zip"), 
                                   filename = paste0("exposures/Exposures_1x1/", x, ".Exposures_1x1.txt"))) %>%
           select(-c(Total, OpenInterval)) %>% 
           mutate(Country = HMD.code$Country[which(HMD.code$Code == x)],
                  Code = x) %>% 
           pivot_longer(cols = c("Female", "Male"),
                        names_to = "Sex",
                        values_to = "Exposures"))

## load HMD death counts
Dx.1 <-
  lapply(HMD.code$Code, 
         function(x)
           HMDHFDplus::readHMD(unz(description = here::here("data", "hmd_statistics_20231006.zip"), 
                                   filename = paste0("deaths/Deaths_1x1/", x, ".Deaths_1x1.txt"))) %>%
           select(-c(Total, OpenInterval)) %>% 
           mutate(Country = HMD.code$Country[which(HMD.code$Code == x)],
                  Code = x) %>% 
           pivot_longer(cols = c("Female", "Male"),
                        names_to = "Sex",
                        values_to = "Deaths"))

## combine HMD data sets
HMD.full <-
  data.table::rbindlist(Nx.1) %>% 
  left_join(data.table::rbindlist(Dx.1)) %>% ## merge deaths and exposures
  mutate(Age = ifelse(Age >= 100, 100, Age)) %>% ## aggregate deaths and exposures above age 100
  summarize(Deaths = sum(Deaths),
            Exposures = sum(Exposures),
            .by = c(Year, Country, Code, Sex, Age)) %>% 
  mutate(mx = Deaths / Exposures) ## calculate death rates

## restrict to countries with available data in observation period
## exclusion criterion: at least one age group (above 25) with 0 death counts and/or exposures in 2010 and/or 2019
HMD.available <-
  HMD.full %>% 
  mutate(Start = min(Year),
         End = max(Year), .by = c(Country)) %>% 
  mutate(Missing = ((mx == 0 | is.na(mx) | is.infinite(mx)) & Year %in% c(2010, 2019) & Age >= 25)) %>% 
  mutate(Missing = sum(Missing), .by = c(Country)) %>% 
  filter(Start <= 2010 & End >= 2019 & Missing == 0)

HMD.select <-
  HMD.full %>% 
  filter(Country %in% unique(HMD.available$Country))

##########################################
# END SELECT COUNTRIES W/ AVAILABLE DATA #
##########################################

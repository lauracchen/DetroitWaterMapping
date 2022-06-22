##title: 
##author: "Winn Costantini"
##date: "4/24/2021"
##output: "csv files and geojson files"

#this script is using data provided by DWSD with one row for each account enrolled in a payment plan as of 4/22. The original data has all but the first number of each address redacted along with the date of shutoff and zipcode. In another script, I replaced the #s with zeros so that I could geocode the addresses and get lat/lon.

require("tidyverse")
require("tidycensus")
require("tools")
require("dplyr")
require("stringr") 
require("data.table")
require("plyr")
require('sf')
require("tidyr")
library(readxl)
library(dplyr)
options(tigris_use_cache = TRUE) #this caches the ACS geometry

##read in data
geocoded <- read.csv("/Desktop/Water Affordability UROP/Detroit PP/.csv", encoding="UTF-8")

##clean data
#exclude tax class "apartment building"
geo_res <- geocoded[geocoded["Sales Class"]=="RESIDENTIAL",]

#connect with census API
census_api_key("d463ce221e5a0730da51aa1e4200ec68d7ee6c81", install = TRUE, overwrite = TRUE)

variables <- load_variables(2019, 'acs5', cache = TRUE)
#View(variables)

#choose variables that exist at this level
my_var <- c(total_pop = "B01003_001", 
            
            lowestq_inc = "B19080_001",
            pov_150 = "B06012_004",
            
            total_houses = "B11001_001",
            family_house = "B11001_002",
            nonfamily_house = "B11001_007",
            
            total_houses_withUnitNum = "B25024_001",
            oneUnitDet = "B25024_002",
            oneUnitAtt = "B25024_003",
            mobileHome = "B25024_010",
            vanRV = "B25024_011",
            
            total_pop_with_race_data  = "B02001_001",
            total_white = "B02001_002", 
            total_black = "B02001_003", 
            total_native = "B02001_004", 
            total_asian = "B02001_005", 
            total_hawaii = "B02001_006", 
            total_other = "B02001_007",
            total_twoormore = "B02001_008",
            
            total_pop_w_hispdata = "B03003_001",
            notHispLat = "B03003_002",
            HispLat = "B03003_003",
            
            all_tenure = "B25032_001",
            renters = "B25032_013",
            rentersOneDet = "B25032_014",
            rentersOneAtt = "B25032_015",
            rentersMobile = "B25032_022",
            rentersVanRV = "B25032_023",
            owners= "B25032_002", 
            ownersOneDet = "B25032_003",
            ownersOneAtt = "B25032_004",
            ownersMobile = "B25032_011",
            ownersVanRV = "B25032_012",
            
            total_pop_vetData = "B21001_001",
            veterans = "B21001_002",
            
            over25 = "B06009_001",
            lessthanHS = "B06009_002",
            HSorEq = "B06009_003",
            someCollege = "B06009_004",
            BAdegree = "B06009_005",
            gradDegree = "B06009_006",
            
            age_estimated_total = "B01001_001", 
            male_65to66 = "B01001_020", 
            male_67to69 = "B01001_021", 
            male_70to74 = "B01001_022", 
            male_75to79 = "B01001_023", 
            male_80to84 = "B01001_024", 
            male_85andover = "B01001_025", 
            female_65to66 = "B01001_044", 
            female_67to69 = "B01001_045", 
            female_70to74 = "B01001_046", 
            female_75to79 = "B01001_047", 
            female_80to84 = "B01001_048", 
            female_85andover = "B01001_049",
            
            owner_occ_houses = "B25081_001",
            house_with_mortgage = "B25081_002",
            second_mortgage = "B25081_004",
            homeequityloan = "B25081_005",
            secmort_and_homeeq = "B25081_006",
            house_no_mortgage = "B25081_008",
            
            total_pop_disability = "B18101_001",
            MU5_disability = "B18101_004",
            M5to17_disability = "B18101_007",
            M18to34_disability = "B18101_010",
            M35to64_disability = "B18101_013",
            M65to74_disability = "B18101_016",
            M75up_disability = "B18101_019",
            FU5_disability = "B18101_023",
            F5to17_disability = "B18101_026",
            F18to34_disability = "B18101_029",
            F35to64_disability = "B18101_032",
            F65to74_disability = "B18101_035",
            F75up_disability = "B18101_038",
            
            total_pop_HI = "B27001_001",
            MU6_HI = "B27001_004",
            M6to17_HI = "B27001_007",
            M18to24 = "B27001_010",
            M25to34_HI = "B27001_013",
            M35to44_HI = "B27001_016",
            M45_54 = "B27001_019",
            M55to64_HI = "B27001_022",
            M65to74_HI = "B27001_025",
            M75up_HI = "B27001_028",
            FU6_HI = "B27001_032",
            F6to17_HI = "B27001_035",
            F18to24 = "B27001_038",
            F25to34_HI = "B27001_041",
            F35to44_HI = "B27001_044",
            F45_54 = "B27001_047",
            F55to64_HI = "B27001_050",
            F65to74_HI = "B27001_053",
            F75up_HI = "B27001_056",
            
            total_pop_pov = "B17001_001",
            incomeBelPov = "B17001_002",
            incomeBelPov_male = "B17001_003",
            incomeBelPov_female = "B17001_017", 
            incomeAbovePov = "B17001_031",
            incomeAbovePov_male = "B17001_032",
            incomeAbovePov_female = "B17001_046",
            
            total_houseIncome = "B19001_001",
            houseIncome_U10k = "B19001_002",
            houseIncome_10to15k = "B19001_003",
            houseIncome_15to20k ="B19001_004",
            houseIncome_20to25k ="B19001_005",
            houseIncome_25to30k ="B19001_006",
            houseIncome_30to35k ="B19001_007",
            houseIncome_35to40k ="B19001_008",
            houseIncome_40to45k ="B19001_009",
            houseIncome_45to50k ="B19001_010",
            houseIncome_50to69k ="B19001_011",
            houseIncome_60to75k ="B19001_012",
            houseIncome_75to100k ="B19001_013",
            houseIncome_100to125k ="B19001_014",
            houseIncome_125to150k ="B19001_015",
            houseIncome_150to200k ="B19001_016",
            houseIncome_200kIUp ="B19001_017",
            
            total_families = "B11004_001",
            marrFam_relatedChild = "B11004_003",
            otherFamMale_relatedChild = "B11004_010",
            otherFamFemale_relatedChild = "B11004_016",
            marrFam_ownChild = "B11003_003",
            otherFamMale_ownChild = "B11003_010",
            otherFamFemale_ownChild = "B11003_016"
            
)

#get variables at tract level in king county WA in 2019 with geometry
bg_data_19 <- get_acs(
  geography = "block group",
  county = "King",
  year = 2019,
  state = "WA",
  variables = my_var,
  survey='acs5',
  geometry=TRUE
)

#separate table so that pivot will work
bg_data_19_var <- bg_data_19 %>% dplyr::select(c("GEOID", "variable","estimate", "geometry"))
bg_data_19_geom <- bg_data_19 %>% dplyr::select(c("GEOID", "geometry"))

#pivot and group
bg_data_19_var <- bg_data_19_var %>%
  group_by(GEOID, variable) %>%
  st_set_geometry(NULL) %>%
  pivot_wider(id_cols = GEOID,
              names_from = variable,
              values_from = estimate, 
              values_fill = list(estimate = 0))

#merge pivoted table with geometry
bg_data_19_merge <- merge(bg_data_19_var, bg_data_19_geom)

#created derived variables (normalized)
bg_data_19_merge <- bg_data_19_merge %>%
  mutate(percWhite = ((total_white / total_pop) * 100)) %>%
  mutate(percBlack = ((total_black / total_pop) * 100)) %>%
  mutate(percNative = ((total_native / total_pop) * 100)) %>%
  mutate(percAsian = ((total_asian / total_pop) * 100)) %>%
  mutate(percHIPI = ((total_hawaii / total_pop) * 100)) %>%
  mutate(percOther = ((total_other / total_pop) * 100)) %>%
  mutate(percTwoormore = ((total_twoormore / total_pop) * 100)) %>%
  mutate(percNonWhite = (100 - percWhite)) %>%
  mutate(percRenter = ((renters / all_tenure) * 100)) %>%
  mutate(percRenterSinFam = (((rentersOneAtt + rentersOneDet + rentersMobile) / all_tenure) * 100)) %>%
  mutate(percOwner = ((owners / all_tenure) * 100)) %>%
  mutate(percOwnersSinFam = (((ownersOneAtt + ownersOneDet + ownersMobile) / all_tenure) * 100)) %>%
  mutate(SinFamHouses = (oneUnitDet + oneUnitAtt + mobileHome + vanRV)) %>%
  mutate(percentageAllOver65 = (((male_65to66 + male_67to69 + male_70to74 + male_75to79 + 
                                    male_80to84 + male_85andover + female_65to66 + female_67to69 + 
                                    female_70to74 + female_75to79 + female_80to84 + female_85andover) / age_estimated_total) * 100)) %>%
  mutate(percLessthanHS = ((lessthanHS/over25)*100)) %>%
  mutate(percHSorEq = ((HSorEq/over25)*100)) %>%
  mutate(percSomColl = ((someCollege/over25)*100)) %>%
  mutate(percBAdegree = ((BAdegree/over25)*100)) %>%
  mutate(percGrad = ((gradDegree/over25)*100)) %>%
  mutate(percHispLat = ((HispLat/total_pop_w_hispdata)*100)) %>%
  mutate(percNotHispLat = ((notHispLat/total_pop_w_hispdata)*100)) %>%
  mutate(percMortgage = ((house_with_mortgage/owner_occ_houses)*100)) %>%
  mutate(percNoMortgage = ((house_no_mortgage/owner_occ_houses)*100)) %>%
  mutate(percSecMortgage = ((second_mortgage/owner_occ_houses)*100)) %>%
  mutate(percHomeEq = ((homeequityloan/owner_occ_houses)*100)) %>%
  mutate(percSecMorAndtHomeEq = ((secmort_and_homeeq/owner_occ_houses)*100)) %>%
  mutate(percDisabled = (((MU5_disability + M5to17_disability + M18to34_disability + 
                             M35to64_disability + M65to74_disability + M75up_disability + 
                             FU5_disability + F5to17_disability + 
                             F18to34_disability + F35to64_disability + F65to74_disability + 
                             F75up_disability)/total_pop_disability)*100)) %>% 
  mutate(percHealthInsur = (((MU6_HI + M6to17_HI + M18to24 + M25to34_HI + M35to44_HI + 
                                M45_54 + M55to64_HI + M65to74_HI + M75up_HI +   
                                FU6_HI + F6to17_HI + F18to24 + F25to34_HI + F35to44_HI + 
                                F45_54 + F55to64_HI + F65to74_HI + F75up_HI
  )/total_pop_HI)*100)) %>%
  mutate(percBelPov = ((incomeBelPov/total_pop_pov)*100)) %>%
  mutate(percMaleBelPov = ((incomeBelPov_male/(incomeBelPov_male+incomeAbovePov_male))*100)) %>%
  mutate(percFemaleBelPov = ((incomeBelPov_female/(incomeBelPov_female + incomeAbovePov_female))*100)) %>%
  mutate(houseIncomeU45k = (((houseIncome_U10k + houseIncome_10to15k + houseIncome_15to20k + 
                                houseIncome_20to25k + houseIncome_25to30k + houseIncome_30to35k +
                                houseIncome_35to40k + houseIncome_40to45k)/total_houseIncome)*100)) %>%
  mutate(houseIncomeU50k = (((houseIncome_U10k + houseIncome_10to15k + houseIncome_15to20k + 
                                houseIncome_20to25k + houseIncome_25to30k + houseIncome_30to35k +
                                houseIncome_35to40k + houseIncome_40to45k + 
                                houseIncome_45to50k)/total_houseIncome)*100)) %>%
  mutate(famWithChildren = (((marrFam_relatedChild + otherFamMale_relatedChild + 
                                otherFamFemale_relatedChild + marrFam_ownChild + otherFamMale_ownChild 
                              + otherFamFemale_ownChild)/total_families)*100)) %>%
  mutate(percabPov150 = ((pov_150 / total_pop) * 100)) %>%
  mutate(percbelPov150 = (100 - percabPov150)) %>%
  mutate(percVeteran = (veterans / total_pop_vetData)*100)

#transform to 4326
bg_data_19_merge <- st_set_geometry(bg_data_19_merge, 'geometry') %>% st_transform(4326)  

#get rid of duplicates
bg_data_19_merge <- unique(bg_data_19_merge)

#write out bg geojson
##st_write(bg_data_19_merge, dsn="/Users/flavioskrzypek/Dropbox (MIT)/ESI water affordability/2020-2022/CityResearch/Seattle-Winn/singlefam_data/data-packet1/ACS19bg.geojson", delete_dsn=TRUE)


##Tract Level ACS Data (repeating same steps as above but for tract level)
#get variables at tract level in king county WA in 2019 with geometry
tract_data_19 <- get_acs(
  geography = "tract",
  county = "King",
  year = 2019,
  state = "WA",
  variables = my_var,
  survey='acs5',
  geometry=TRUE
)

#separate table before pivot
tract_data_19_var <- tract_data_19 %>% dplyr::select(c("GEOID", "variable","estimate", "geometry"))

tract_data_19_geom <- tract_data_19 %>% dplyr::select(c("GEOID", "geometry"))

#pivot
tract_data_19_var <- tract_data_19_var %>%
  group_by(GEOID, variable) %>%
  st_set_geometry(NULL) %>%
  pivot_wider(id_cols = GEOID,
              names_from = variable,
              values_from = estimate, 
              values_fill = list(estimate = 0))

#put back together
tract_data_19_merge <- merge(tract_data_19_var, tract_data_19_geom) 

#create derived and normalized variables
tract_data_19_merge <- tract_data_19_merge %>%
  mutate(percWhite = ((total_white / total_pop) * 100)) %>%
  mutate(percBlack = ((total_black / total_pop) * 100)) %>%
  mutate(percNative = ((total_native / total_pop) * 100)) %>%
  mutate(percAsian = ((total_asian / total_pop) * 100)) %>%
  mutate(percHIPI = ((total_hawaii / total_pop) * 100)) %>%
  mutate(percOther = ((total_other / total_pop) * 100)) %>%
  mutate(percTwoormore = ((total_twoormore / total_pop) * 100)) %>%
  mutate(percNonWhite = (100 - percWhite)) %>%
  mutate(percRenter = ((renters / all_tenure) * 100)) %>%
  mutate(percRenterSinFam = (((rentersOneAtt + rentersOneDet + rentersMobile) / all_tenure) * 100)) %>%
  mutate(percOwner = ((owners / all_tenure) * 100)) %>%
  mutate(percOwnersSinFam = (((ownersOneAtt + ownersOneDet + ownersMobile) / all_tenure) * 100)) %>%
  mutate(SinFamHouses = (oneUnitDet + oneUnitAtt + mobileHome + vanRV)) %>%
  mutate(percentageAllOver65 = (((male_65to66 + male_67to69 + male_70to74 + male_75to79 + 
                                    male_80to84 + male_85andover + female_65to66 + female_67to69 + 
                                    female_70to74 + female_75to79 + female_80to84 + female_85andover) / age_estimated_total) * 100)) %>%
  mutate(percLessthanHS = ((lessthanHS/over25)*100)) %>%
  mutate(percHSorEq = ((HSorEq/over25)*100)) %>%
  mutate(percSomColl = ((someCollege/over25)*100)) %>%
  mutate(percBAdegree = ((BAdegree/over25)*100)) %>%
  mutate(percGrad = ((gradDegree/over25)*100)) %>%
  mutate(percHispLat = ((HispLat/total_pop_w_hispdata)*100)) %>%
  mutate(percNotHispLat = ((notHispLat/total_pop_w_hispdata)*100)) %>%
  mutate(percMortgage = ((house_with_mortgage/owner_occ_houses)*100)) %>%
  mutate(percNoMortgage = ((house_no_mortgage/owner_occ_houses)*100)) %>%
  mutate(percSecMortgage = ((second_mortgage/owner_occ_houses)*100)) %>%
  mutate(percHomeEq = ((homeequityloan/owner_occ_houses)*100)) %>%
  mutate(percSecMorAndtHomeEq = ((secmort_and_homeeq/owner_occ_houses)*100)) %>%
  mutate(percDisabled = (((MU5_disability + M5to17_disability + M18to34_disability + 
                             M35to64_disability + M65to74_disability + M75up_disability + 
                             FU5_disability + F5to17_disability + 
                             F18to34_disability + F35to64_disability + F65to74_disability + 
                             F75up_disability)/total_pop_disability)*100)) %>% 
  mutate(percHealthInsur = (((MU6_HI + M6to17_HI + M18to24 + M25to34_HI + M35to44_HI + 
                                M45_54 + M55to64_HI + M65to74_HI + M75up_HI +   
                                FU6_HI + F6to17_HI + F18to24 + F25to34_HI + F35to44_HI + 
                                F45_54 + F55to64_HI + F65to74_HI + F75up_HI
  )/total_pop_HI)*100)) %>%
  mutate(percBelPov = ((incomeBelPov/total_pop_pov)*100)) %>%
  mutate(percMaleBelPov = ((incomeBelPov_male/(incomeBelPov_male+incomeAbovePov_male))*100)) %>%
  mutate(percFemaleBelPov = ((incomeBelPov_female/(incomeBelPov_female + incomeAbovePov_female))*100)) %>%
  mutate(houseIncomeU45k = (((houseIncome_U10k + houseIncome_10to15k + houseIncome_15to20k + 
                                houseIncome_20to25k + houseIncome_25to30k + houseIncome_30to35k +
                                houseIncome_35to40k + houseIncome_40to45k)/total_houseIncome)*100)) %>%
  mutate(houseIncomeU50k = (((houseIncome_U10k + houseIncome_10to15k + houseIncome_15to20k + 
                                houseIncome_20to25k + houseIncome_25to30k + houseIncome_30to35k +
                                houseIncome_35to40k + houseIncome_40to45k + 
                                houseIncome_45to50k)/total_houseIncome)*100)) %>%
  mutate(famWithChildren = (((marrFam_relatedChild + otherFamMale_relatedChild + 
                                otherFamFemale_relatedChild + marrFam_ownChild + otherFamMale_ownChild 
                              + otherFamFemale_ownChild)/total_families)*100)) %>%
  mutate(percabPov150 = ((pov_150 / total_pop) * 100)) %>%
  mutate(percbelPov150 = (100 - percabPov150)) %>%
  mutate(percVeteran = (veterans / total_pop_vetData)*100) 

#set to correct CRS and get rid of duplicates
tract_data_19_merge <- st_set_geometry(tract_data_19_merge, 'geometry') %>% st_transform(4326)  

tract_data_19_merge <- unique(tract_data_19_merge)

##st_write(tract_data_19_merge, dsn="//Users/flavioskrzypek/Dropbox (MIT)/ESI water affordability/2020-2022/CityResearch/Seattle-Winn/singlefam_data/data-packet1/ACS19tract.geojson", delete_dsn=TRUE)

##Combining PP and ACS data
#Finding number of PPs in tracts and blockgroup
pp_sf<-st_as_sf(geo_res, coords=c("lon", "lat"), crs=4326)

#Total number of shutoffs across all years
tract_pp_merge$pp<-lengths(st_intersects(tract_data_19_merge, pp_sf) )
bg_pp_merge$pp<-lengths(st_intersects(bg_data_19_merge, pp_sf) )
zip_pp_merge$pp <-lengths(st_intersects(zip_data_19_merge, pp_sf))

#zip variables (should be same as bg)

#normalize shutoffs by number of single family accounts
# assign number of accounts to zip
# create normalized column

st_write()
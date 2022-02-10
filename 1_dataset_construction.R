#----- Loading packages, options and initializations -----
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("haven", "tidyverse", "magrittr", "foreign", "tidyverse","ggplot2",
       "survey", "tableone", "mice", "openxlsx")

#---- path to Box ----
#Crystal's path
#path_to_box <- "/Users/CrystalShaw/Box"
# #Taylor's path
path_to_box <- "C:/Users/tmobley/Box"

#---- Analytic dataset construction for MI and analysis ----
time_to_event <- 
  read_sas(paste0(path_to_box, "/Asian_Americans_dementia_data/", 
                  "analysis_data_tables/aa_adrd_time_to_event.sas7bdat"))
census2000 <- 
  read_sas(paste0(path_to_box, "/Asian_Americans_dementia_data/", 
          "raw_data_tables/Census_data/c10049_cen2000_w_adi20211008.sas7bdat"))

colnames(time_to_event) <- tolower(colnames(time_to_event))
colnames(census2000) <- tolower(colnames(census2000))

#----select census vars ---- 
#[ADI, ethnic enclave, geo indicator vars]

census2000_vars <- census2000 %>% 
  select(c("subjid", "state", "statecode","countycode_masked",
           "tractcode_masked","blkgroupcode_masked", 
           "pop_foreignborn_pct","pop_asianalone_pct","kind_2000adi_natrank",
           "kind_2000adi_staternk","blkgroupdensity_mi2","blkgroupdensity_m2"))

#Merge
adi_tte_merge <- merge(x = time_to_event, y = census2000_vars, 
                       by = "subjid", all.x = TRUE)

# #Sanity check
# reclass <- time_to_event %>% mutate_at("subjid", as.character)
# test_merge <- left_join(reclass, census2000_vars, by = "subjid")
# sum(adi_tte_merge$subjid == test_merge$subjid)

#---- Data cleaning geo vars ----
#- Binary indicators

  table(adi_tte_merge$kind_2000adi_staternk, exclude=NULL)
  table(adi_tte_merge$kind_2000adi_natrank, exclude=NULL)
  
adi_tte_merge$kind_2000adi_staternk <- 
  as.numeric(ifelse(adi_tte_merge$kind_2000adi_staternk %in% 
            c(1,2,3,4,5,6,7,8,9,10),adi_tte_merge$kind_2000adi_staternk,NA))

adi_tte_merge$kind_2000adi_natrank <- 
  as.numeric(ifelse(is.na(adi_tte_merge$kind_2000adi_natrank) |
                      adi_tte_merge$kind_2000adi_natrank=='GQ' |
                      adi_tte_merge$kind_2000adi_natrank=='GQ-PH' |
                      adi_tte_merge$kind_2000adi_natrank=='PH' |
                      adi_tte_merge$kind_2000adi_natrank=='', NA,
                    adi_tte_merge$kind_2000adi_natrank))

table(adi_tte_merge$kind_2000adi_staternk, exclude=NULL)
table(adi_tte_merge$kind_2000adi_natrank, exclude=NULL)

#highest 20% vs others
adi_tte_merge$kind2000adi_state20v80 <- 
  ifelse(adi_tte_merge$kind_2000adi_staternk %in% c(9, 10), 1, 
      ifelse(adi_tte_merge$kind_2000adi_staternk %in% c(1,2,3,4,5,6,7,8),0,NA)) 
adi_tte_merge$kind2000adi_nat20v80 <- 
  ifelse(adi_tte_merge$kind_2000adi_natrank >= 80, 1, 
      ifelse(adi_tte_merge$kind_2000adi_natrank > 0 &
               adi_tte_merge$kind_2000adi_natrank < 80,0,NA)) 

#highest 10% vs others
adi_tte_merge$kind2000adi_state10v90 <- 
  ifelse(adi_tte_merge$kind_2000adi_staternk==10, 1, 
    ifelse(adi_tte_merge$kind_2000adi_staternk %in% c(1,2,3,4,5,6,7,8,9),0,NA)) 
adi_tte_merge$kind2000adi_nat10v90 <- 
  ifelse(adi_tte_merge$kind_2000adi_natrank >= 90, 1, 
         ifelse(adi_tte_merge$kind_2000adi_natrank > 0 &
                  adi_tte_merge$kind_2000adi_natrank < 90,0,NA)) 

#highest 30% vs others
adi_tte_merge$kind2000adi_state30v70 <- 
  ifelse(adi_tte_merge$kind_2000adi_staternk %in% c(8,9,10), 1, 
    ifelse(adi_tte_merge$kind_2000adi_staternk %in% c(1,2,3,4,5,6,7),0,NA)) 
adi_tte_merge$kind2000adi_nat30v70 <- 
  ifelse(adi_tte_merge$kind_2000adi_natrank >= 70, 1, 
         ifelse(adi_tte_merge$kind_2000adi_natrank > 0 &
                  adi_tte_merge$kind_2000adi_natrank < 70,0,NA)) 

#Median cutoff indicators
adi_tte_merge$kind2000adi_state50v50 <- 
  ifelse(adi_tte_merge$kind_2000adi_staternk %in% c(6,7,8,9,10), 1, 
    ifelse(adi_tte_merge$kind_2000adi_staternk %in% c(1,2,3,4,5),0,NA)) 
adi_tte_merge$kind2000adi_nat50v50 <- 
    ifelse(adi_tte_merge$kind_2000adi_natrank >= 50, 1, 
         ifelse(adi_tte_merge$kind_2000adi_natrank > 0 &
                  adi_tte_merge$kind_2000adi_natrank < 50,0,NA)) 

#- quintile vars
adi_tte_merge$quintile_kind2000adi_state <- 
  ifelse(adi_tte_merge$kind_2000adi_staternk %in% c(9,10), 5,
    ifelse(adi_tte_merge$kind_2000adi_staternk %in% c(7,8), 4,
      ifelse(adi_tte_merge$kind_2000adi_staternk %in% c(5,6), 3,
         ifelse(adi_tte_merge$kind_2000adi_staternk %in% c(3,4), 2,
            ifelse(adi_tte_merge$kind_2000adi_staternk %in% c(1,2), 1, NA)))))

adi_tte_merge$quintile_kind2000adi_nat <- 
  ifelse(adi_tte_merge$kind_2000adi_natrank >= 80, 5,
   ifelse(adi_tte_merge$kind_2000adi_natrank >= 60 &
            adi_tte_merge$kind_2000adi_natrank < 80, 4,
    ifelse(adi_tte_merge$kind_2000adi_natrank >= 40 &
              adi_tte_merge$kind_2000adi_natrank < 60, 3,
      ifelse(adi_tte_merge$kind_2000adi_natrank >= 20 &
                adi_tte_merge$kind_2000adi_natrank < 40, 2,
        ifelse(adi_tte_merge$kind_2000adi_natrank >= 0 &
                adi_tte_merge$kind_2000adi_natrank < 20, 1, NA)))))

#- quartile nat var
adi_tte_merge$quartile_kind2000adi_nat <- 
  ifelse(adi_tte_merge$kind_2000adi_natrank >= 75, 4,
         ifelse(adi_tte_merge$kind_2000adi_natrank >= 50 &
            adi_tte_merge$kind_2000adi_natrank < 75, 3,
                ifelse(adi_tte_merge$kind_2000adi_natrank >= 25 &
                    adi_tte_merge$kind_2000adi_natrank < 50, 2,
                        ifelse(adi_tte_merge$kind_2000adi_natrank >= 0 &
                          adi_tte_merge$kind_2000adi_natrank < 25, 1, NA))))

  #checks -- 5 = highest ADI
  # table(adi_tte_merge$quintile_kind2000adi_state,
  #       adi_tte_merge$asian, exclude=NULL)
  # table(adi_tte_merge$quintile_kind2000adi_state,
  #       adi_tte_merge$kind_2000adi_staternk, exclude=NULL)
  # table(adi_tte_merge$kind2000adi_state20v80,
  #       adi_tte_merge$kind_2000adi_staternk, exclude=NULL)
  # table(adi_tte_merge$quintile_kind2000adi_nat, adi_tte_merge$asian,
  #       exclude=NULL)
  # table(adi_tte_merge$quintile_kind2000adi_state, adi_tte_merge$asian,
  #       exclude=NULL)
  # table(adi_tte_merge$quartile_kind2000adi_nat, adi_tte_merge$asian,
  #       exclude=NULL)

#- deciles and quintiles relative to the sample
adi_tte_merge = mutate(adi_tte_merge, 
                         decile_kind2000adi_sample = 
                           ntile(adi_tte_merge$kind_2000adi_natrank, 10)) 
  table(adi_tte_merge$decile_kind2000adi_sample,
        adi_tte_merge$kind_2000adi_natrank, exclude=NULL)

adi_tte_merge = mutate(adi_tte_merge, 
                         quintile_kind2000adi_sample = 
                           ntile(adi_tte_merge$decile_kind2000adi_sample, 5)) 

  table(adi_tte_merge$quintile_kind2000adi_sample, exclude=NULL)
  table(adi_tte_merge$decile_kind2000adi_sample,
        adi_tte_merge$quintile_kind2000adi_sample,exclude=NULL)
  
#- Add county, tract, and block grpup ID vars
table(adi_tte_merge$countycode_masked, exclude = NULL)
table(adi_tte_merge$statecode, exclude = NULL)

adi_tte_merge$statecode_char <- 
  as.character(ifelse(!is.na(adi_tte_merge$statecode),
                      paste0("0", (adi_tte_merge$statecode)), NA))

table(adi_tte_merge$statecode_char)

adi_tte_merge$countyid_masked <- 
  as.factor(ifelse(!is.na(adi_tte_merge$countycode_masked),
                    paste0(adi_tte_merge$statecode_char,
                           adi_tte_merge$countycode_masked,
                           sep = "", collapse = NULL), NA))

table(adi_tte_merge$countyid_masked, exclude = NULL)

#R1 TMM create block group ID var to report # block groups in sample
adi_tte_merge$blkgroupid_masked <- 
  ifelse(!is.na(adi_tte_merge$blkgroupcode_masked),
         paste0(adi_tte_merge$statecode_char,
                adi_tte_merge$countycode_masked,
                adi_tte_merge$tractcode_masked,
                adi_tte_merge$blkgroupcode_masked,
                sep = "", collapse = NULL), NA)

#---- Subset dataset to no missing adi block group measures ----

#1. Remove PIs from sample --- from 184,929
table(adi_tte_merge$ethnicity_rev, exclude = NULL) #437 PIs

adi_tte_merge %<>% filter(ethnicity_rev %in% c(1, 2, 3, 4, 5, 6, 7, 9, 10, NA))

table(adi_tte_merge$ethnicity_rev, adi_tte_merge$asian, exclude=NULL)

#2. Subset to main dem v1 sample --- from 184,492
table(adi_tte_merge$main_dem_v1_sample, exclude = NULL) #4524 -- now incl 0 fu time

adi_tte_maindem1 <- adi_tte_merge %>% filter(main_dem_v1_sample == 1)

#3. No missing adi --- from 179,968
table(adi_tte_maindem1$kind_2000adi_staternk, exclude=NULL) #8614

adi_tte_maindem1 %<>% filter(!is.na(kind_2000adi_staternk)) 

table(adi_tte_maindem1$kind_2000adi_staternk, exclude = NULL)

  #double check countyid
  table(adi_tte_maindem1$countyid_masked, exclude = NULL)
 
#-- analytic sample should be n=171,354  

#---- Asian immigrant enclave vars ----
  adi_tte_maindem1 %>% summarize(min_pct = min(pop_foreignborn_pct), 
                                 max_pct = max(pop_foreignborn_pct), 
                                 avg_pct = mean(pop_foreignborn_pct))

  adi_tte_maindem1 %>% summarize(min_pct = min(pop_asianalone_pct), 
                                  max_pct = max(pop_asianalone_pct), 
                                  avg_pct = mean(pop_asianalone_pct))
  
  adi_tte_maindem1 = mutate(adi_tte_maindem1, 
                            pop_foreignborn_sample = 
                            ntile(adi_tte_maindem1$pop_foreignborn_pct, 100),
                            pop_asianalone_sample = 
                              ntile(adi_tte_maindem1$pop_asianalone_pct, 100))
  
  adi_tte_maindem1 %<>% 
  mutate("pop_foreignborn_clean" = 
           (((adi_tte_maindem1$pop_foreignborn_pct*100) - 
               mean((adi_tte_maindem1$pop_foreignborn_pct*100)))/10),
        "pop_foreignborn_clean_s" = ((pop_foreignborn_sample - 50)/10),
        "pop_asianalone_clean" = 
          (((adi_tte_maindem1$pop_asianalone_pct*100) - 
              mean((adi_tte_maindem1$pop_asianalone_pct*100)))/10),
        "pop_asianalone_clean_s" = ((pop_asianalone_sample - 50)/10))

  # summary(adi_tte_maindem1$pop_foreignborn_sample)
  # summary(adi_tte_maindem1$pop_foreignborn_clean_s)
  # summary(adi_tte_maindem1$pop_foreignborn_pct)
  # summary(adi_tte_maindem1$pop_foreignborn_clean)
  # 
  # summary(adi_tte_maindem1$pop_asianalone_sample)
  # summary(adi_tte_maindem1$pop_asianalone_clean_s)
  # summary(adi_tte_maindem1$pop_asianalone_pct)
  # summary(adi_tte_maindem1$pop_asianalone_clean)

#---- Clean variables for analysis ----

#Center/rescale block group density
summary(adi_tte_maindem1$blkgroupdensity_mi2, exclude = NULL)
adi_tte_maindem1$bgdensity_clean <- 
  (adi_tte_maindem1$blkgroupdensity_mi2 - 
     round(mean(adi_tte_maindem1$blkgroupdensity_mi2, na.rm = TRUE)))/1000

#Flag for end of follow up = dementia (to use in surv analysis)
adi_tte_maindem1$dem_end_flag <- 
  ifelse(adi_tte_maindem1$main_dem_v1_end_type == 'DEMENTIA', 1, 0)

#confirmed dem end flag variables are equivalent 
table(adi_tte_maindem1$main_dem_v1_end_dem_flag, 
      adi_tte_maindem1$dem_end_flag, exclude = NULL)

#Collapse employment indicators for MI
table(adi_tte_maindem1$employment_part_time_employed, 
      adi_tte_maindem1$employment_full_time_employed, exclude = NULL)

adi_tte_maindem1$full_part_employment <- 
  ifelse(adi_tte_maindem1$employment_full_time_employed == 1 | 
           adi_tte_maindem1$employment_part_time_employed == 1, 1, 0)

table(adi_tte_maindem1$full_part_employment, exclude = NULL)

#create ethnicity var with "Asian, unspecified" category and collapse white
adi_tte_maindem1$ethnicity_new <-
  ifelse(!is.na(adi_tte_maindem1$ethnicity_rev), 
         adi_tte_maindem1$ethnicity_rev,
         ifelse(is.na(adi_tte_maindem1$ethnicity_rev) & 
                  adi_tte_maindem1$asian == 0, 9,
                ifelse(is.na(adi_tte_maindem1$ethnicity_rev) &
                         adi_tte_maindem1$asian == 1, 11, NA)))

#check
table(adi_tte_maindem1$ethnicity_new, exclude = NULL)

#create education4 variable for Table 1
table(adi_tte_maindem1$education_rev, exclude=NULL)

adi_tte_maindem1$education4 <- 
  as.factor(ifelse(adi_tte_maindem1$education_rev %in% c(1, 2), 1,
                ifelse(adi_tte_maindem1$education_rev == 3, 2, 
                    ifelse(adi_tte_maindem1$education_rev == 4, 3, 
                         ifelse(adi_tte_maindem1$education_rev %in% c(5,6),4,
                                NA)))))
table(adi_tte_maindem1$education_rev, adi_tte_maindem1$education4, 
      useNA = "ifany")

#numbers in adi state deciles
adi_tte_maindem1 %>% filter(asian == 1) %>%
  group_by(kind_2000adi_staternk) %>% tally()
adi_tte_maindem1 %>% filter(asian == 0) %>%
  group_by(kind_2000adi_staternk) %>% tally()

adi_tte_maindem1 %<>% 
  mutate("ethnicity" = case_when(ethnicity_new == 1 ~ "South Asian", 
                                 ethnicity_new == 2 ~ "Chinese", 
                                 ethnicity_new == 3 ~ "Japanese", 
                                 ethnicity_new == 4 ~ "Korean", 
                                 ethnicity_new == 5 ~ "Filipino", 
                                 ethnicity_new == 6 ~ "Vietnamese", 
                                 ethnicity_new == 7 ~ "Other SE Asian", 
                                 ethnicity_new == 8 ~ "Any Pacific Islander", 
                                 ethnicity_new == 9 ~ "White", 
                                 ethnicity_new == 10 ~ "Multiple Asian Eth", 
                                 ethnicity_new == 11 ~ "Asian, unspecified"))

#- check number of dem outcomes by group 
# 
# ns <-adi_tte_maindem1 %>%
#   group_by(ethnicity, quintile_kind2000adi_state, dem_end_flag) %>% tally()
# view(ns)
# 
# ns3 <-adi_tte_maindem1 %>%
#   group_by(ethnicity, quartile_kind2000adi_nat, dem_end_flag) %>% tally()
# view(ns3)
# 
# ns4 <-adi_tte_maindem1 %>%
#   group_by(ethnicity, quintile_kind2000adi_nat, dem_end_flag) %>% tally()
# view(ns4)

#---- Create data subset >=15 AA per county ----

adi_asian_filter <- adi_tte_maindem1 %>% filter(asian == 1)

asian_county_n <- adi_asian_filter %>%  
  group_by(countyid_masked) %>% tally(sort = TRUE) %>% print(n = 60) 

# >= 15 people/county subsamples 
asian_county15aa <- asian_county_n %>% filter(n >= 15)

#merge
adi_county15aa_tte <- adi_tte_maindem1 %>%
  filter(countyid_masked %in% c(asian_county15aa$countyid_masked)) %>% 
  droplevels()

# n <- nrow(adi_tte_maindem1 %>% filter(asian == 1)) #18177
# n <- nrow(adi_tte_maindem1 %>% filter(asian == 0)) #153177
# n <- nrow(adi_county15aa_tte %>% filter(asian == 1)) #18103
# n <- nrow(adi_county15aa_tte %>% filter(asian == 0)) #149385

#numbers in counties
# adi_county15aa_tte %>% filter(asian == 1) %>% 
#   group_by(countyid_masked) %>% tally(sort = TRUE) %>% print(n = 35)
# adi_county15aa_tte %>% filter(asian == 0) %>% 
#   group_by(countyid_masked) %>% tally(sort = TRUE) %>% print(n = 35)
# 
# #numbers in adi state deciles
# adi_county15aa_tte %>% filter(asian == 1) %>%
#   group_by(kind_2000adi_staternk) %>% tally()
# adi_county15aa_tte %>% filter(asian == 0) %>%
#   group_by(kind_2000adi_staternk) %>% tally()

#---- group counties according to Paola's email ----

table(adi_county15aa_tte$countyid_masked)

adi_county15aa_tte$countyid_collapse <- 
  ifelse(adi_county15aa_tte$countyid_masked %in% 
           c('06042','06299','06499','06818','06865'),1,
         ifelse(adi_county15aa_tte$countyid_masked %in% 
                  c('06064','06067','06296','06520','06573','06694','06878','06974'),2,
                ifelse(adi_county15aa_tte$countyid_masked=='06004',3,
                       ifelse(adi_county15aa_tte$countyid_masked=='06673',4,
                              ifelse(adi_county15aa_tte$countyid_masked=='06708',5,
                                     ifelse(adi_county15aa_tte$countyid_masked=='06720',
                                            6,NA))))))

table(adi_county15aa_tte$countyid_collapse,exclude=NULL)
table(adi_county15aa_tte$countyid_masked,
      adi_county15aa_tte$countyid_collapse,exclude=NULL)

#---- R1 TMM Number of block groups in sample ----

length(unique(adi_county15aa_tte$blkgroupid_masked))

#---- save dataframes ----

save(adi_county15aa_tte, 
     file = paste0(path_to_box, "/Asian_Americans_dementia_data/adi_adrd/", 
                   "analysis_data/adi_county15aa_tte.R"))

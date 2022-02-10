#---
#---- Loading packages, options and initializations ----
#---
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("haven", "tidyverse", "magrittr", "foreign", "tidyverse","ggplot2",
       "survey", "tableone", "mice", "openxlsx")

#---
# Multiple Imputations #
#---

#---- path to Box ----
#Crystal's MRG desktop path
# path_to_box <- "C:/Users/cshaw/Box"
#Crystal's path
# path_to_box <- "/Users/CrystalShaw/Box"
# #Taylor's path
path_to_box <- "C:/Users/tmobley/Box"

#---- load and format dataset ----
load(paste0(path_to_box, "/Asian_Americans_dementia_data/adi_adrd/",
            "analysis_data/adi_county15aa_tte.R"))

adi_mi <- adi_county15aa_tte
colnames(adi_mi) <- tolower(colnames(adi_mi))
vars <- colnames(adi_mi)

#---- var check ----
#check sociodemographic var missingness 
summary(adi_mi$survey_age)
#R1 TMM adding area-level variables for MI
summary(adi_mi$pop_asianalone_clean)
summary(adi_mi$bgdensity_clean)

table(adi_mi$education_rev, exclude = NULL)
table(adi_mi$female, exclude = NULL)
table(adi_mi$asian, exclude = NULL)
table(adi_mi$income, exclude = NULL)
table(adi_mi$sizeofhh, exclude = NULL)
table(adi_mi$full_part_employment, exclude = NULL)
table(adi_mi$maritalstatus, exclude = NULL)
table(adi_mi$usaborn_rev, exclude = NULL)
table(adi_mi$usabornfather_rev, exclude = NULL)
table(adi_mi$usabornmother_rev, exclude = NULL)
table(adi_mi$asian, adi_mi$ethnicity_rev, exclude = NULL)
table(adi_mi$generalhealth, exclude = NULL)
#R1 TMM adding ADI variable for MI
table(adi_mi$quintile_kind2000adi_state, exclude=NULL)

#**Code adapted from EHL's KW multiple imputations script
#---- var list ----
#List of variables to impute/include in MICE
#R1 TMM adding area-level vars
impute.var.list<- c("survey_age", "female", "asian", "income_pp",
                    "ethnicity_new", "education_rev","income", "sizeofhh", 
                    "usaborn_rev", "usabornfather_rev", 
                    "usabornmother_rev", "maritalstatus", 
                    "full_part_employment", "generalhealth",
                    "quintile_kind2000adi_state", "pop_asianalone_clean",
                    "bgdensity_clean")

#---- check missing ----
#Assess missingness in vars we want to impute / use for analysis
missingsummary <- data.frame(varname = impute.var.list, pctmiss = NA)
row.names(missingsummary) <- impute.var.list
for (i in impute.var.list){
  missingsummary[i, "pctmiss"] <- 100*sum(is.na(adi_mi[, i]))/nrow(adi_mi)
  
  print(i)
  print(table(adi_mi[, i], exclude = NULL))
}

missingordered <- missingsummary[order(missingsummary$pctmiss), ]
missingordered

ordered.var.list <- c(paste(missingordered$varname))

#--- data prep ----
## Run single imputation
#prep data by dropping vars we don't need, ordering by missingness
impute.data <- adi_mi[, ordered.var.list]

#Set variable classes by type
str(impute.data)

#survey_age and income_pp continuous

#binary vars
impute.data$female <- as.factor(impute.data$female)
impute.data$asian <- as.factor(impute.data$asian)
impute.data$usaborn_rev <- as.factor(impute.data$usaborn_rev)
impute.data$usabornmother_rev <- as.factor(impute.data$usabornmother_rev)
impute.data$usabornfather_rev <- as.factor(impute.data$usabornfather_rev)
impute.data$full_part_employment<-as.factor(impute.data$full_part_employment)

#categorical vars
impute.data$ethnicity_new <- factor(impute.data$ethnicity_new, ordered = F)
impute.data$maritalstatus <- factor(impute.data$maritalstatus, ordered = F)
#R1 TMM adding area-level vars
impute.data$quintile_kind2000adi_state <- 
  factor(impute.data$quintile_kind2000adi_state, ordered = F)

#ordinal vars
impute.data$education_rev <- factor(impute.data$education_rev, ordered = T)
impute.data$income <- factor(impute.data$income, ordered = T)
impute.data$sizeofhh <- factor(impute.data$sizeofhh, ordered = T)
impute.data$generalhealth <- factor(impute.data$generalhealth, ordered = T)

#recheck classes
str(impute.data)

#---- initiate imputations ----
ini <- mice(impute.data, maxit = 0, 
            defaultMethod = c("pmm", "logreg", "polyreg", "polr"), seed = 12345)

ini$method
meth <- ini$method
meth

ini$predictorMatrix
pred <- ini$predictorMatrix

#change predictor matrix so income_pp doesn't predict income or sizeofhh
pred[c("income", "sizeofhh"), "income_pp"] <- 0
pred

#---- run imputations ----
#R1 TMM change m = 5 to m = 20
imp_fcs <- mice(impute.data, m = 20, maxit = 10, pred = pred, meth = meth, 
                defaultMethod = c("pmm", "logreg", "polyreg", "polr"), 
                seed = 12345)

#---- examine diagnostics ----
imp_fcs$loggedEvents
plot(imp_fcs)
densityplot(imp_fcs, ~income)
densityplot(imp_fcs, ~sizeofhh)
densityplot(imp_fcs, ~income_pp)
densityplot(imp_fcs, ~education_rev)
densityplot(imp_fcs, ~usaborn_rev)

#---
# ---- save stacked imputed dataset ----
#---
imp.temp <- list()
for (i in 1:20){
  imp.temp[[i]] <- complete(imp_fcs, action = i)
  
  imp.temp[[i]] <- cbind(adi_mi$subjid, imp.temp[[i]][, c(impute.var.list)])
  imp.temp[[i]][, "imp"] <- i
}

county15aa_fcs_stacked <- do.call(rbind, imp.temp)
colnames(county15aa_fcs_stacked)
names(county15aa_fcs_stacked)[names(county15aa_fcs_stacked) == 
                                "adi_mi$subjid"] <- "subjid"

#Save stacked imputed dataset
save(county15aa_fcs_stacked, 
     file = paste0(path_to_box, "/Asian_Americans_dementia_data/adi_adrd/", 
     "analysis_data/county15aa_fcs_stacked.R"))

#---- Add other vars to dataset for analysis ----
load(paste0(path_to_box, "/Asian_Americans_dementia_data/adi_adrd/", 
"analysis_data/county15aa_fcs_stacked.R"))

#R1 TMM adding area-level vars
other.vars <- adi_mi[, !names(adi_mi) %in% 
                       c("survey_age", "female", "asian", "income_pp",
                         "ethnicity_new", "education_rev","income", "sizeofhh", 
                         "usaborn_rev", "usabornfather_rev", 
                         "usabornmother_rev", "maritalstatus", 
                         "full_part_employment", "generalhealth",
                         "quintile_kind2000adi_state", "pop_asianalone_clean",
                         "bgdensity_clean")]

 #Merge
 adi_county15aa_analysis <- merge(x = county15aa_fcs_stacked, y = other.vars,
 by = "subjid")

#---- clean covariates ----
#center age
 adi_county15aa_analysis$age60 <- (adi_county15aa_analysis$survey_age - 60)

#scale/centerincome_pp
summary(adi_county15aa_analysis$income_pp)
#hist(adi_county15aa_analysis$income_pp)

adi_county15aa_analysis$incomepp_clean <- 
  (adi_county15aa_analysis$income_pp - 
     round(median(adi_county15aa_analysis$income_pp)))/10000

#education
adi_county15aa_analysis$education_rev <- 
  factor(adi_county15aa_analysis$education_rev, ordered = FALSE)

adi_county15aa_analysis$education4 <- 
  as.factor(ifelse(adi_county15aa_analysis$education_rev %in% c(1, 2), 1,
                   ifelse(adi_county15aa_analysis$education_rev == 3, 2, 
                          ifelse(adi_county15aa_analysis$education_rev == 4, 3, 
                                 4))))

# #Sanity check
# table(adi_county15aa_analysis$education_rev, 
# adi_county15aa_analysis$education4, useNA = "ifany")

#R1 TMM Recenter ethnic enclave variable for analyses 
adi_county15aa_analysis %>% summarize(min_pct = min(pop_asianalone_pct), 
                               max_pct = max(pop_asianalone_pct), 
                               avg_pct = mean(pop_asianalone_pct))

adi_county15aa_analysis %>% filter(imp==1) %>% 
                            summarize(min_pct = min(pop_asianalone_pct), 
                                      max_pct = max(pop_asianalone_pct), 
                                      avg_pct = mean(pop_asianalone_pct))

adi_county15aa_analysis %<>% 
  mutate("pop_asianalone_clean" = 
           (((adi_county15aa_analysis$pop_asianalone_pct*100) - 
               mean((adi_county15aa_analysis$pop_asianalone_pct*100)))/10))

#R1 TMM Center/rescale block group density
summary(adi_county15aa_analysis$blkgroupdensity_mi2, exclude = NULL)

adi_county15aa_analysis %>% filter(imp==1) %>% 
  summarize(min_pct = min(blkgroupdensity_mi2), 
            max_pct = max(blkgroupdensity_mi2), 
            avg_pct = mean(blkgroupdensity_mi2))

adi_county15aa_analysis$bgdensity_clean <- 
  (adi_county15aa_analysis$blkgroupdensity_mi2 - 
     round(mean(adi_county15aa_analysis$blkgroupdensity_mi2, 
                na.rm = TRUE)))/1000

#---- Save analytic dataset (15aa per county)----
save(adi_county15aa_analysis, 
     file = paste0(path_to_box, "/Asian_Americans_dementia_data/adi_adrd/", 
     "analysis_data/adi_county15aa_analysis.R"))


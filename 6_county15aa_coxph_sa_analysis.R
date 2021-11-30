#------------------------------------------------------------------
# Loading packages, options and initializations                   #
#------------------------------------------------------------------
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("haven", "tidyverse", "magrittr", "foreign", "tidyverse","ggplot2",
       "survey", "tableone", "mice", "openxlsx", "survival", "miceadds", "mgcv",
       "openxlsx", "ggpubr", "mitools", "lmtest")

options(scipen = 999, digits=8)

# #Taylor's path
path_to_box <- "C:/Users/tmobley/Box"

#---- source scripts ----
# source(paste0(path_to_box, "/Asian_Americans_dementia/Manuscripts/ADI_ADRD/", 
#               "Code/analysis/HR_function.R"))

#---- load analysis dataset ----
load(paste0(path_to_box, 
            "/Asian_Americans_dementia_data/adi_adrd/analysis_data/", 
            "adi_county15aa_analysis.R"))

adi_county15aa_analysis$education4 = 
  relevel(adi_county15aa_analysis$education4, ref = "4")
adi_county15aa_analysis$quintile_kind2000adi_state <- 
  relevel(factor(adi_county15aa_analysis$quintile_kind2000adi_state, 
                 ordered=FALSE), ref = '1')
adi_county15aa_analysis$kind2000adi_state20v80 <- 
  relevel(factor(adi_county15aa_analysis$kind2000adi_state20v80, 
                 ordered=FALSE), ref = '0')

#- recode survey age from JF's nativity cox ph code
adi_county15aa_analysis$survey_age_recode <- 
  adi_county15aa_analysis$survey_age-0.0001

#Sanity check
levels(adi_county15aa_analysis$education4)
levels(adi_county15aa_analysis$kind2000adi_state20v80)
levels(adi_county15aa_analysis$quintile_kind2000adi_state)

#---
#---- Main CoxPH models 1-4 ----
#---
#model matrix
model_matrix <- 
  tibble("model_name" = rep(c("m1", "m2", "m3", "m3.5", "m4",
                              "age_m1", "age_m2", "age_m3", "age_m3.5",
                              "age_m4"), 1), 
         "model" = 
           c(paste0("Surv(main_dem_v1_fu_time, dem_end_flag) ~ age60 +",
                    "factor(female) + kind2000adi_state20v80 + ",
                    "bgdensity_clean"),
             paste0("Surv(main_dem_v1_fu_time, dem_end_flag) ~ age60 +",
                    "factor(female) + kind2000adi_state20v80 +",
                    "bgdensity_clean + factor(education4)"),
             paste0("Surv(main_dem_v1_fu_time, dem_end_flag) ~ age60 + ",
                    "factor(female) + kind2000adi_state20v80 +", 
                    "bgdensity_clean +factor(education4) + incomepp_clean"),
             paste0("Surv(main_dem_v1_fu_time, dem_end_flag) ~ age60 + ",
                    "factor(female) + kind2000adi_state20v80 +",
                    "bgdensity_clean +factor(education4) + incomepp_clean +",
                    "pop_asianalone_clean"),
             paste0("Surv(main_dem_v1_fu_time, dem_end_flag) ~ age60 +",
                    "factor(female) + kind2000adi_state20v80 +",
                    "bgdensity_clean +factor(education4) + incomepp_clean + ",
                    "pop_asianalone_clean + ",
                    "pop_asianalone_clean*kind2000adi_state20v80"),
           c(paste0("Surv(survey_age_recode, main_dem_v1_end_age,",
                    "dem_end_flag) ~ factor(female) +",
                    "quintile_kind2000adi_state + bgdensity_clean"),
             paste0("Surv(survey_age_recode, main_dem_v1_end_age, ",
                    "dem_end_flag) ~ ",
                    "factor(female) + quintile_kind2000adi_state +",
                    "bgdensity_clean + factor(education4)"),
             paste0("Surv(survey_age_recode, main_dem_v1_end_age,",
                    "dem_end_flag) ~ ",
                    "factor(female) + quintile_kind2000adi_state +", 
                    "bgdensity_clean +factor(education4) + incomepp_clean"),
             paste0("Surv(survey_age_recode, main_dem_v1_end_age, ",
                    "dem_end_flag) ~ ",
                    "factor(female) + quintile_kind2000adi_state +",
                    "bgdensity_clean +factor(education4) + incomepp_clean +",
                    "pop_asianalone_clean"),
             paste0("Surv(survey_age_recode, main_dem_v1_end_age,",
                    "dem_end_flag) ~ ",
                    "factor(female) + quintile_kind2000adi_state +",
                    "bgdensity_clean +factor(education4) + incomepp_clean + ",
                    "pop_asianalone_clean + ",
                    "pop_asianalone_clean*quintile_kind2000adi_state"))))

#for(grepl("m3", model)){
for(model in unique(model_matrix$model_name)){
  formula <- unlist(model_matrix[which(model_matrix$model_name == model), 
                                 "model"])
  
  for(eth in c("asian", "white")){
    model_list <- list()
    
    if(eth == "asian"){
      dataset <- adi_county15aa_analysis %>% filter(asian == 1)
    } else{
      dataset <- adi_county15aa_analysis %>% filter(asian == 0)
    }
    
    for(i in 1:max(dataset$imp)){
      subset <- dataset %>% filter(imp == i)
      
      if(grepl("age", model)){
        model_list[[i]] <- 
          coxph(as.formula(formula), data = subset, id = subjid, robust=TRUE)
      }else{
        model_list[[i]] <- 
          coxph(as.formula(formula), data = subset, robust=TRUE)
      }
    }
    
    coef.names <- names(model_list[[i]]$coefficients)
    stuff <- summary(MIcombine(model_list))
    if(!exists("all_models")){
      assign("all_models", stuff %>%
               mutate("raceth" = eth, "model" = model, "names" = coef.names))
    } else{
      all_models <- 
        rbind(all_models, stuff %>% 
                mutate("raceth" = eth, "model" = model, "names" = coef.names))
      }
    
    #---- **asian subset analyses ----
    if(eth == "asian"){
      for(subeth in c("Chinese", "Filipino", "Japanese", "South Asian",
                      "Korean", "Vietnamese")){
        subeth_models_list = list()
        dataset <- adi_county15aa_analysis %>% filter(asian == 1)
        subset <- dataset %>% filter(ethnicity == subeth)
        
        for(i in 1:max(subset$imp)){
          imp_subset <- subset %>% filter(imp == i)
          
          if(grepl("age", model)){
            subeth_models_list[[i]] <- 
              coxph(as.formula(formula), data = subset, id = subjid, 
                    robust=TRUE)
          }else{
            subeth_models_list[[i]] <- 
              coxph(as.formula(formula), data = subset, robust=TRUE)
          }
        }
        
        coef.names <- names(subeth_models_list[[i]]$coefficients)
        stuff <- summary(MIcombine(subeth_models_list))
    
      if(!exists("all_models")){
        assign("all_models", stuff %>%
               mutate("raceth" = subeth, "model" = model, 
                      "names" = coef.names))
      } else{
        all_models <- 
          rbind(all_models, stuff %>% 
                mutate("raceth" = subeth, "model" = model, 
                       "names" = coef.names))
        }
      }
    }
  }
}

all_models %<>% 
  mutate(HR = exp(results), LL = exp(`(lower`), UL = exp(`upper)`))

specific_results <- all_models %>% filter(str_detect(names, "adi") |
                                            str_detect(names, "asianalone"))


#--- save results ----
write.xlsx(all_models, 
           file = paste0(path_to_box, "/Asian_Americans_dementia/", 
                         "Manuscripts/ADI_ADRD/Output/", 
                         "quintilekind2000adi_coxph_sa_all.xlsx"),
           overwrite = TRUE)

write.xlsx(specific_results,
           file = paste0(path_to_box, "/Asian_Americans_dementia/",
                         "Manuscripts/ADI_ADRD/Output/",
                         "quintilekind2000adi_coxph_sa_all_",
                         "filtered.xlsx"), overwrite = TRUE)

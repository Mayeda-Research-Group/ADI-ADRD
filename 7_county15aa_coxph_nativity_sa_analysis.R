#------------------------------------------------------------------
# Loading packages, options and initializations                   #
#------------------------------------------------------------------
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("haven", "tidyverse", "magrittr", "foreign", "tidyverse","ggplot2",
       "survey", "tableone", "mice", "openxlsx", "survival", "miceadds", "mgcv",
       "openxlsx", "ggpubr", "mitools", "lmtest", "scales","ggpubr", "ggdist")

options(scipen = 999, digits=8)

#---- path to box ----
#Crystal's path
#path_to_box <- "/Users/CrystalShaw/Box"
# #Taylor's path
path_to_box <- "C:/Users/ehlarson/Box"
# Joey's path
#path_to_box <-"C:/Users/j_fong/Box"

#---- ** source scripts ----
source(paste0(path_to_box, "/Asian_Americans_dementia/Manuscripts/ADI_ADRD/", 
              "Code/analysis/HR_function.R"))

load(paste0(path_to_box, 
            "/Asian_Americans_dementia_data/adi_adrd/analysis_data/", 
            "adi_county15aa_analysis.R"))

adi_county15aa_analysis %<>% 
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

#---- **relevel factors ----
adi_county15aa_analysis$education4 = 
  relevel(adi_county15aa_analysis$education4, ref = "4")

adi_county15aa_analysis$quintile_kind2000adi_state <-
  relevel(factor(adi_county15aa_analysis$quintile_kind2000adi_state,
                 ordered=FALSE), ref = '1')

#---
#---- ** Main CoxPH model 3 ----
#---
#model matrix
model_matrix <- 
  tibble("model_name" = rep(c("m3"), 1), 
         "model" = 
           c(paste0("Surv(main_dem_v1_fu_time, dem_end_flag) ~ age60 + ",
                    "factor(female) + quintile_kind2000adi_state +", 
                    "bgdensity_clean +factor(education4) + incomepp_clean +
                    factor(usaborn_rev)")))
#EHL pasting from main coxph script to compare. 
             # paste0("Surv(main_dem_v1_fu_time, dem_end_flag) ~ age60 + ",
             #       "factor(female) + quintile_kind2000adi_state +", 
             #       "bgdensity_clean +factor(education4) + incomepp_clean"),
model<-"m3"
for(model in c("m3")){
  #for(model in unique(model_matrix$model_name)){
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
      
      model_list[[i]] <- 
        coxph(as.formula(formula), data = subset, robust=TRUE)
    }
    
    coef.names <- names(model_list[[i]]$coefficients)
    stuff <- summary(MIcombine(model_list))
    
    #---- **LRT main models ----
    if(grepl("m4", model)){
      reduced_model_list = list()
      if(grepl("sens", model)){
        new_formula  <-
          paste0("Surv(main_dem_v1_fu_time, dem_end_flag) ~ age60 + ",
                 "factor(female) + quintile_kind2000adi_state +",
                 "bgdensity_clean + factor(education4) + incomepp_clean +",
                 "pop_asianalone_clean + factor(countyid_masked)")
      } else{
        new_formula  <-
          paste0("Surv(main_dem_v1_fu_time, dem_end_flag) ~ age60 + ",
                 "factor(female) + quintile_kind2000adi_state +",
                 "bgdensity_clean +factor(education4) + incomepp_clean +",
                 "pop_asianalone_clean")
      }
      
      for(i in 1:max(dataset$imp)){
        subset <- dataset %>% filter(imp == i)
        
        reduced_model_list[[i]] <-
          coxph(as.formula(new_formula), data = subset, robust=TRUE)
      }
      
      reduced_model <- as.mira(reduced_model_list)
      bigger_model <- as.mira(model_list)
      LRT <- D1(bigger_model, reduced_model)
      
      LRT_results[which(LRT_results$raceth == eth &
                          LRT_results$model == model), "LRT_pval"] <-
        LRT$result[, "P(>F)"]
      
      #---- HR comparison ----
      MI_model <- MIcombine(model_list)
      LRT_results[which(LRT_results$raceth == eth &
                          LRT_results$model == model),
                  c("HR_25", "LL_25", "UL_25")] <-
        t(custom_HR_calc(as.numeric(quantile(adi_county15aa_analysis$pop_asianalone_clean, probs = .25)), version, MI_model))
      LRT_results[which(LRT_results$raceth == eth &
                          LRT_results$model == model),
                  c("HR_75", "LL_75", "UL_75")] <-
        t(custom_HR_calc(as.numeric(quantile(adi_county15aa_analysis$pop_asianalone_clean, probs = .75)), version, MI_model))
    }
    
    if(!exists("all_models")){
      assign("all_models", stuff %>%
               mutate("raceth" = eth, "model" = model, "names" = coef.names))
    } else{
      all_models <- 
        rbind(all_models, stuff %>% 
                mutate("raceth" = eth, "model" = model, "names" = coef.names))
    }
  }
}

all_models %<>% 
  mutate(HR = exp(results), LL = exp(`(lower`), UL = exp(`upper)`))

specific_results <- all_models %>% filter(str_detect(names, "adi") | 
                                            str_detect(names, "asianalone"))

#--- save results ----
# write.xlsx(all_models, 
#            file = paste0(path_to_box, "/Asian_Americans_dementia/", 
#                          "Manuscripts/ADI_ADRD/Output/", 
#                          "quintilekind2000adi_coxph_nativity_m3.xlsx"),
#            overwrite = TRUE)
# 
# write.xlsx(specific_results, 
#            file = paste0(path_to_box, "/Asian_Americans_dementia/", 
#                          "Manuscripts/ADI_ADRD/Output/", 
#                          "quintilekind2000adi_coxph_nativity_m3_", 
#                          "filtered.xlsx"), overwrite = TRUE)

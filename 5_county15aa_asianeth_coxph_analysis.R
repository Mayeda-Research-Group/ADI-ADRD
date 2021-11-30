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
source(paste0(path_to_box, "/Asian_Americans_dementia/Manuscripts/ADI_ADRD/", 
              "Code/analysis/HR_function.R"))

#---- load analysis dataset ----
load(paste0(path_to_box, 
            "/Asian_Americans_dementia_data/adi_adrd/analysis_data/", 
            "adi_county15aa_analysis.R"))

adi_county15aa_analysis$education4 = 
  relevel(adi_county15aa_analysis$education4, ref = "4") 
adi_county15aa_analysis$quintile_kind2000adi_state <- 
  relevel(factor(adi_county15aa_analysis$quintile_kind2000adi_state, 
                 ordered=FALSE), ref = '1')

#Sanity check
levels(adi_county15aa_analysis$education4)
levels(adi_county15aa_analysis$quintile_kind2000adi_state)

#---- Save LRT results ----
LRT_results <- tibble("raceth" = rep(c("Chinese", "Filipino", "Japanese",
                                       "South Asian", "Korean", "Vietnamese"),
                                     each = 2),
                      "model" = rep(c("m4", "sens_m4"), 6),
                      "LRT_pval" = 0,
                      "HR_25" = 0, "LL_25" = 0, "UL_25" = 0,
                      "HR_75" = 0, "LL_75" = 0, "UL_75" = 0)

#---
#---- Main CoxPH models 1-4 ----
#---
#model matrix
model_matrix <- 
  tibble("model_name" = rep(c("m1", "m2", "m3", "m3.5", "m4",
                              "sens_m1", "sens_m2", "sens_m3", 
                              "sens_m3.5", "sens_m4"), 1), 
         "model" = 
          c(paste0("Surv(main_dem_v1_fu_time, dem_end_flag) ~ age60 +",
                  "factor(female) + quintile_kind2000adi_state + bgdensity_clean"),
           paste0("Surv(main_dem_v1_fu_time, dem_end_flag) ~ age60 +",
                  "factor(female) + quintile_kind2000adi_state +",
                  "bgdensity_clean + factor(education4)"),
           paste0("Surv(main_dem_v1_fu_time, dem_end_flag) ~ age60 + ",
                  "factor(female) + quintile_kind2000adi_state +", 
                  "bgdensity_clean +factor(education4) + incomepp_clean"),
           paste0("Surv(main_dem_v1_fu_time, dem_end_flag) ~ age60 + ",
                  "factor(female) + quintile_kind2000adi_state +",
                  "bgdensity_clean +factor(education4) + incomepp_clean +",
                  "pop_asianalone_clean"),
           paste0("Surv(main_dem_v1_fu_time, dem_end_flag) ~ age60 +",
                  "factor(female) + quintile_kind2000adi_state +",
                  "bgdensity_clean +factor(education4) + incomepp_clean + ",
                  "pop_asianalone_clean + pop_asianalone_clean*quintile_kind2000adi_state"),
           c(paste0("Surv(main_dem_v1_fu_time, dem_end_flag) ~ age60 +",
                    "factor(female) + quintile_kind2000adi_state + ",
                    "bgdensity_clean + factor(countyid_collapse)"),
             paste0("Surv(main_dem_v1_fu_time, dem_end_flag) ~ age60 +",
                    "factor(female) + quintile_kind2000adi_state +",
                    "bgdensity_clean + factor(education4) +",
                    "factor(countyid_collapse)"),
             paste0("Surv(main_dem_v1_fu_time, dem_end_flag) ~ age60 + ",
                    "factor(female) + quintile_kind2000adi_state +", 
                    "bgdensity_clean +factor(education4) + ",
                    "incomepp_clean + factor(countyid_collapse)"),
             paste0("Surv(main_dem_v1_fu_time, dem_end_flag) ~ age60 + ",
                    "factor(female) + quintile_kind2000adi_state +",
                    "bgdensity_clean +factor(education4) + incomepp_clean +",
                    "factor(countyid_collapse) + pop_asianalone_clean"),
             paste0("Surv(main_dem_v1_fu_time, dem_end_flag) ~ age60 +",
                    "factor(female) + quintile_kind2000adi_state +",
                    "bgdensity_clean +factor(education4) + incomepp_clean + ",
                    "factor(countyid_collapse) + pop_asianalone_clean + ",
                    "pop_asianalone_clean*quintile_kind2000adi_state"))))

# for(model in c("m4")){
for(model in unique(model_matrix$model_name)){

    #---- **asian subset analyses ----
    dataset <- adi_county15aa_analysis %>% filter(asian == 1)
    
    formula <- unlist(model_matrix[which(model_matrix$model_name == model),
                                   "model"])
    
    for(subeth in c("Chinese", "Filipino", "Japanese", "South Asian",
                    "Korean", "Vietnamese")){
          
        subeth_models_list = list()
        subset <- dataset %>% filter(ethnicity == subeth)
      
      for(i in 1:max(subset$imp)){
        imp_subset <- subset %>% filter(imp == i)
        
        subeth_models_list[[i]] <- 
          coxph(as.formula(formula), data = imp_subset, robust=TRUE)
      
      }
        
      coef.names <- names(subeth_models_list[[i]]$coefficients)
      stuff <- summary(MIcombine(subeth_models_list))
      
      #---- ****LRT submodels ----
      if(grepl("m4", model)){
        reduced_model_list = list()
        if(grepl("sens", model)){
          new_formula  <-
            paste0("Surv(main_dem_v1_fu_time, dem_end_flag) ~ age60 + ",
                   "factor(female) + quintile_kind2000adi_state +",
                   "factor(countyid_collapse) + bgdensity_clean +",
                   "factor(education4) + incomepp_clean +",
                   "pop_asianalone_clean")
        } else{
          new_formula  <-
            paste0("Surv(main_dem_v1_fu_time, dem_end_flag) ~ age60 + ",
                   "factor(female) + quintile_kind2000adi_state +",
                   "bgdensity_clean + factor(education4) + incomepp_clean +",
                   "pop_asianalone_clean")
        }
        
        for(i in 1:max(dataset$imp)){
          subset <- dataset %>% filter(imp == i)
          
          reduced_model_list[[i]] <-
            coxph(as.formula(new_formula), data = imp_subset, robust=TRUE)
        }
        
        reduced_model <- as.mira(reduced_model_list)
        bigger_model <- as.mira(subeth_models_list)
        LRT <- D1(bigger_model, reduced_model)
        
        LRT_results[which(LRT_results$raceth == subeth &
                            LRT_results$model == model), "LRT_pval"] <-
          LRT$result[, "P(>F)"]
        
        #---- HR comparison ----
        MI_model <- MIcombine(subeth_models_list)
        LRT_results[which(LRT_results$raceth == subeth &
                            LRT_results$model == model),
                    c("HR_25", "LL_25", "UL_25")] <-
          t(custom_HR_calc(as.numeric(quantile(adi_county15aa_analysis$pop_asianalone_clean, probs = .25)), 
                           version, MI_model))
        LRT_results[which(LRT_results$raceth == subeth &
                            LRT_results$model == model),
                    c("HR_75", "LL_75", "UL_75")] <-
          t(custom_HR_calc(as.numeric(quantile(adi_county15aa_analysis$pop_asianalone_clean, probs = .75)), 
                           version, MI_model))
      }
      
    if(!exists("all_models")){
      assign("all_models", stuff %>%
          mutate("raceth" = subeth, "model" = model, "names" = coef.names))
    } else{
      all_models <- 
          rbind(all_models, stuff %>% 
          mutate("raceth" = subeth, "model" = model, "names" = coef.names))
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
                     "quintilekind2000adi_asianeth_coxph_all.xlsx"),
           overwrite = TRUE)

write.xlsx(specific_results, 
           file = paste0(path_to_box, "/Asian_Americans_dementia/", 
                    "Manuscripts/ADI_ADRD/Output/", 
                     "quintilekind2000adi_asianeth_coxph_all_", 
                     "filtered.xlsx"), overwrite = TRUE)

write.xlsx(LRT_results,
           file = paste0(path_to_box, "/Asian_Americans_dementia/",
                         "Manuscripts/ADI_ADRD/Output/",
                         "quintilekind2000adi_asianeth_coxph_all_",
                         "intx_LRT.xlsx"), overwrite=TRUE)

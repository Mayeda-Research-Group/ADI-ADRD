#---
# Loading packages, options and initializations
#---
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("tidyverse","haven","openxlsx","rlang","dplyr","tidyr","openxlsx",
       "xlsx","rlang")

options(scipen = 999, digits=6)


#---- path to box ----
#Comment out irrelevant path and comment in your path 
#Crystal's path
# path_to_box <- "/Users/CrystalShaw/Box"
# #Joey's path
path_to_box <- "C:/Users/j_fong/Box"

#---- Importing Datsets ----
#Importing dataset using "haven" package
load(paste0(path_to_box, "/Asian_Americans_dementia_data/adi_adrd/", 
            "analysis_data/adi_county15aa_tte.R"))
load(paste0(path_to_box, "/Asian_Americans_dementia_data/adi_adrd/", 
            "analysis_data/adi_county15aa_analysis.R"))

#outputting R dataset to use the SAS mortality rate macro on it
write.csv(adi_county15aa_tte, paste0(
  path_to_box, "/Asian_Americans_dementia_data/adi_adrd/", 
  "analysis_data/adi_county15aa_tte.csv"))

#Making copies of dataset w/ clearer names
AA_ADRD_noimpute <- adi_county15aa_tte
AA_ADRD_impute <- adi_county15aa_analysis
table(adi_county15aa_tte$quintile_kind2000adi_state, exclude=NULL)
table(adi_county15aa_analysis$quintile_kind2000adi_state, exclude = NULL)/5

#---- Cleaning Datsets ----
#Pooling together the Asian ethnicities not used in subgroup analyses
AA_ADRD_noimpute$ethnicity_rec <- 
  recode_factor(AA_ADRD_noimpute$ethnicity_new,
                '1' = '1', '2' = '2', '3' = '3', '4' = '4', '5' = '5', '6' = '6', 
                '7' = '12', '8' = '12', '9' = '9', '10' = '12', '11' = '12')

AA_ADRD_impute$ethnicity_rec <- 
  recode_factor(AA_ADRD_impute$ethnicity_new,
                '1' = '1', '2' = '2', '3' = '3', '4' = '4','5' = '5','6' = '6', 
                '7' = '12', '8' = '12', '9' = '9', '10' = '12', '11' = '12')

#checks
table(AA_ADRD_noimpute$ethnicity_rec)
table(AA_ADRD_impute$education_rev)

#making a list of variables to keep
colnames(AA_ADRD_noimpute)
colnames(AA_ADRD_impute)

keepvars<-c("subjid", "survey_age", "female", "asian", "ethnicity_new", 
            "income_pp", "education_rev", "usaborn_rev", "income", "sizeofhh",
            "usabornfather_rev", "usabornmother_rev", "maritalstatus", 
            "generalhealth", "quintile_kind2000adi_county", 
            "quintile_kind2000adi_state", "main_dem_v1_end_type", 
            "main_dem_v1_fu_time", "ethnicity_rec", "education4", 
            "pop_asianalone_pct")

#Drop step - subsetting dataset to those selected values
#Note: imputed dataset includes 1 additional variable, 'imp' that has the 
#imputation number.
AA_ADRD_noimpute <- AA_ADRD_noimpute %>% select(all_of(keepvars))
AA_ADRD_impute <- AA_ADRD_impute %>% select(all_of(keepvars, "imp"))

#making a list of categorical variables to run the loop over
catvars <- c("female", "education_rev", "usaborn_rev", "income", "sizeofhh", 
             "usabornfather_rev", "usabornmother_rev", "maritalstatus", 
             "quintile_kind2000adi_county", "quintile_kind2000adi_state",
             "generalhealth", "ethnicity_new", "main_dem_v1_end_type",
             "education4", "ethnicity_rec")

#----Asian vs White Table 1, nonimputed dataset - Categorical Variables----
#Get n's by Asian vs white ethnicity 
#Note: need to add an additional row if variable you are stratifying on has
#missingness
T1results_cat<-matrix(nrow=1, ncol=3) 
T1results_cat[1,]<- c("Race/ethnicity total",
                      table(AA_ADRD_noimpute$asian, exclude = NULL))

for (i in 1:length(catvars)){
  tab.to.add<-table(eval(rlang::parse_expr(paste0("AA_ADRD_noimpute$",catvars[i]))),
                    AA_ADRD_noimpute$asian, exclude=NULL)
  labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
  T1results_cat<-rbind(T1results_cat, c(paste(catvars[i]), rep(NA,2))) 
  T1results_cat<-rbind(T1results_cat,cbind(labs, tab.to.add))
}#Notes: parse_expr() spits out the string as an expression. Must use eval() fxn
#in order to use the expression (if eval() removed, will cause an error)

colnames(T1results_cat)<-c("Variablename", "White", "Asian") 
rownames(T1results_cat)<-NULL
T1results_cat<-as.data.frame(T1results_cat)

#Get %'s by race/ethnicity
T1results_prop<-matrix(nrow=1, ncol=3) 
T1results_prop[1,]<- c("Race/Ethnicity total",table(
  AA_ADRD_noimpute$asian)/nrow(AA_ADRD_noimpute))

#Getting the denominators for the percentage calculations
Racethmargins<-as.numeric(table(AA_ADRD_noimpute$asian))

for (i in 1:length(catvars)){
  tab.to.add<-t(t(table(eval(rlang::parse_expr(paste0("AA_ADRD_noimpute$",catvars[i])))
                        ,AA_ADRD_noimpute$asian, exclude=NULL))/Racethmargins)
  labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
  T1results_prop<-rbind(T1results_prop, c(paste(catvars[i]),rep(NA,2))) 
  T1results_prop<-rbind(T1results_prop,cbind(labs, tab.to.add))
}

colnames(T1results_prop)<-c("Variablename", "White_prop", "Asian_prop") 
rownames(T1results_prop)<-NULL
T1results_prop<-as.data.frame(T1results_prop)

#merge n and % results
T1results_cat<-left_join(T1results_cat, T1results_prop, by="Variablename")
T1results_cat<-T1results_cat[,c("Variablename", "Asian", "Asian_prop", 
                                "White", "White_prop")]



#----Asian vs White Table 1, nonimputed datset - continuous----
contvars<-c("survey_age", "income_pp","main_dem_v1_fu_time", 
            "pop_asianalone_pct")
T1results_cont<-matrix(nrow=0, ncol=3) 
colnames(T1results_cont)<-c("Variablename", "White", "Asian")

for (i in 1:length(contvars)){
  
  tab.to.add<-tapply(eval(parse_expr(paste0("AA_ADRD_noimpute$",contvars[i]))), 
                     AA_ADRD_noimpute$asian, mean, na.rm=T)
  T1results_cont<-rbind(T1results_cont, c(paste0(contvars[i],"_mean"),tab.to.add)) 
  
  tab.to.add2<-tapply(eval(parse_expr(paste0("AA_ADRD_noimpute$",contvars[i]))), 
                      AA_ADRD_noimpute$asian, sd, na.rm=T)
  T1results_cont<-rbind(T1results_cont, c(paste0(contvars[i],"_SD"),tab.to.add2)) 
  
  if (sum(is.na(eval(parse_expr(paste0("AA_ADRD_noimpute$",contvars[i]))))) >0){
    tab.to.add3<-table(is.na(eval(parse_expr(paste0("AA_ADRD_noimpute$",contvars[i]))))
                       ,AA_ADRD_noimpute$asian, exclude=NULL)["TRUE",]
    T1results_cont<-rbind(T1results_cont, c(paste0(contvars[i],"_missing"),tab.to.add3))
    
  }
  
  tab.to.add4<-tapply(eval(parse_expr(paste0("AA_ADRD_noimpute$",contvars[i]))), 
                      AA_ADRD_noimpute$asian, median, na.rm=T)
  T1results_cont<-rbind(T1results_cont, c(paste0(contvars[i],"_median"),tab.to.add4)) 
  
  tab.to.add5<-tapply(eval(parse_expr(paste0("AA_ADRD_noimpute$",contvars[i]))), 
                      AA_ADRD_noimpute$asian, quantile, probs=0.25, na.rm=T)
  T1results_cont<-rbind(T1results_cont, c(paste0(contvars[i],"_q1"),tab.to.add5)) 

  tab.to.add6<-tapply(eval(parse_expr(paste0("AA_ADRD_noimpute$",contvars[i]))), 
                      AA_ADRD_noimpute$asian, quantile, probs=0.75, na.rm=T)
  T1results_cont<-rbind(T1results_cont, c(paste0(contvars[i],"_q3"),tab.to.add6)) 
  
}
T1results_cont<-data.frame(T1results_cont)


#----Asian vs White Table 1, imputed datset - categorical ----
#Getting values for the imputed dataset; there should be no missingness!
T1results_cat_impute<-matrix(nrow=1, ncol=3) 
T1results_cat_impute[1,]<- c("Race/ethnicity total",
                             table(AA_ADRD_impute$asian, exclude = NULL)/5)

for (i in 1:length(catvars)){
  tab.to.add<-table(eval(parse_expr(paste0("AA_ADRD_impute$",catvars[i]))),
                    AA_ADRD_impute$asian, exclude=NULL)/5
  labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
  T1results_cat_impute<-rbind(T1results_cat_impute, c(paste(catvars[i]), rep(NA,2))) 
  T1results_cat_impute<-rbind(T1results_cat_impute,cbind(labs, tab.to.add))
}
#Notes: parse_expr() spits out the string as an expression. Must use eval() fxn
#in order to use the expression (if eval() removed, will cause an error)

colnames(T1results_cat_impute)<-c("Variablename", "White", "Asian") 
rownames(T1results_cat_impute)<-NULL
T1results_cat_impute<-as.data.frame(T1results_cat_impute)

#Get %'s by race/ethnicity
T1results_prop_impute<-matrix(nrow=1, ncol=3) 
T1results_prop_impute[1,]<- c("Race/Ethnicity total",table(
  AA_ADRD_impute$asian)/nrow(AA_ADRD_impute))

#Getting the denominators for the percentage calculations
Racethmargins<-as.numeric(table(AA_ADRD_impute$asian))

for (i in 1:length(catvars)){
  tab.to.add<-t(t(table(eval(parse_expr(paste0("AA_ADRD_impute$",catvars[i])))
                        ,AA_ADRD_impute$asian, exclude=NULL))/Racethmargins)
  labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
  T1results_prop_impute<-rbind(T1results_prop_impute, c(paste(catvars[i]),rep(NA,2))) 
  T1results_prop_impute<-rbind(T1results_prop_impute,cbind(labs, tab.to.add))
}

colnames(T1results_prop_impute)<-c("Variablename", "White_prop", "Asian_prop") 
rownames(T1results_prop_impute)<-NULL
T1results_prop_impute<-as.data.frame(T1results_prop_impute)

#merge n and % results
T1results_cat_impute<-left_join(T1results_cat_impute, 
                                T1results_prop_impute, by="Variablename")
T1results_cat_impute<-T1results_cat_impute[,c(
  "Variablename", "Asian", "Asian_prop", "White", "White_prop")]


#----Asian subgroups Table 1, non-imputed dataset - categorical ----
#Overwriting Catvars to remove ethnicity subgroups, since we are adding
#columns for each of them anyway
catvars <- c("female", "education_rev", "usaborn_rev", "income", "sizeofhh", 
             "usabornfather_rev", "usabornmother_rev", "maritalstatus", 
             "quintile_kind2000adi_county", "quintile_kind2000adi_state",
             "generalhealth", "main_dem_v1_end_type",
             "education4")

#making a list of categorical variables to run the loop over

#Get n's for all race/ethnicities vs white ethnicity 
#Note: need to add an additional row if variable you are stratifying on has
#missingness
table(AA_ADRD_noimpute$ethnicity_rec, exclude = NULL)

T1results_cat_substr<-matrix(nrow=1, ncol=9) 
T1results_cat_substr[1,]<- c("Race/ethnicity total",
                             table(AA_ADRD_noimpute$ethnicity_rec, exclude = NULL))

for (i in 1:length(catvars)){
  tab.to.add<-table(eval(parse_expr(paste0("AA_ADRD_noimpute$",catvars[i]))),
                    AA_ADRD_noimpute$ethnicity_rec, exclude=NULL)
  labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
  T1results_cat_substr<-rbind(T1results_cat_substr, c(paste(catvars[i]), rep(NA,8))) 
  T1results_cat_substr<-rbind(T1results_cat_substr, cbind(labs, tab.to.add))
}#Notes: parse_expr() spits out the string as an expression. Must use eval() fxn
#in order to use the expression (if eval() removed, will cause an error)

colnames(T1results_cat_substr)<-c("Variablename", "SouthAsian", "Chinese", "Japanese",
                                  "Korean","Filipino","Vietnamese","OtherAsian",
                                  "White") 
rownames(T1results_cat_substr)<-NULL
T1results_cat_substr<-as.data.frame(T1results_cat_substr)

#Get %'s by race/ethnicity
T1results_prop_substr<-matrix(nrow=1, ncol=9) 
T1results_prop_substr[1,]<- c("Race/Ethnicity total",table(
  AA_ADRD_noimpute$ethnicity_rec)/nrow(AA_ADRD_noimpute))

#Getting the denominators for the percentage calculations
Racethmargins<-as.numeric(table(AA_ADRD_noimpute$ethnicity_rec))

for (i in 1:length(catvars)){
  tab.to.add<-t(t(table(eval(parse_expr(paste0("AA_ADRD_noimpute$",catvars[i])))
                        ,AA_ADRD_noimpute$ethnicity_rec, exclude=NULL))/Racethmargins)
  labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
  T1results_prop_substr<-rbind(T1results_prop_substr, c(paste(catvars[i]),rep(NA,8))) 
  T1results_prop_substr<-rbind(T1results_prop_substr, cbind(labs, tab.to.add))
}

colnames(T1results_prop_substr)<-c("Variablename", "SouthAsian_prop", "Chinese_prop", 
                                   "Japanese_prop","Korean_prop","Filipino_prop",
                                   "Vietnamese_prop", "OtherAsian_prop", "White_prop") 
rownames(T1results_prop_substr)<-NULL
T1results_prop_substr<-as.data.frame(T1results_prop_substr)

#merge n and % results
T1results_cat_substr<-left_join(T1results_cat_substr, T1results_prop_substr, by="Variablename")
T1results_cat_substr<-T1results_cat_substr[
  ,c("Variablename", "SouthAsian","SouthAsian_prop", "Chinese", "Chinese_prop",
     "Japanese","Japanese_prop", "Korean", "Korean_prop","Filipino", 
     "Filipino_prop", "Vietnamese", "Vietnamese_prop", "OtherAsian", 
     "OtherAsian_prop", "White","White_prop")]

#----Asian subgroups Table 1, nonimputed dataset - continuous----
#Now adding continuous variables
contvars<-c("survey_age", "income_pp","main_dem_v1_fu_time", 
            "pop_asianalone_pct")
T1results_cont_substr<-matrix(nrow=0, ncol=9) 
colnames(T1results_cont_substr)<-c("Variablename", "SouthAsian", "Chinese", "Japanese",
                                   "Korean","Filipino","Vietnamese","OtherAsian", "White") 

for (i in 1:length(contvars)){
  
  tab.to.add<-tapply(eval(parse_expr(paste0("AA_ADRD_noimpute$",contvars[i]))), 
                     AA_ADRD_noimpute$ethnicity_rec, mean, na.rm=T)
  T1results_cont_substr<-rbind(T1results_cont_substr, c(paste0(contvars[i],"_mean"),tab.to.add)) 
  
  tab.to.add2<-tapply(eval(parse_expr(paste0("AA_ADRD_noimpute$",contvars[i]))), 
                      AA_ADRD_noimpute$ethnicity_rec, sd, na.rm=T)
  T1results_cont_substr<-rbind(T1results_cont_substr, c(paste0(contvars[i],"_SD"),tab.to.add2)) 
  
  if (sum(is.na(eval(parse_expr(paste0("AA_ADRD_noimpute$",contvars[i]))))) >0){
    tab.to.add3<-table(is.na(eval(parse_expr(paste0("AA_ADRD_noimpute$",contvars[i]))))
                       ,AA_ADRD_noimpute$ethnicity_rec, exclude=NULL)["TRUE",]
    T1results_cont_substr<-rbind(T1results_cont_substr, c(paste0(contvars[i],"_missing"),tab.to.add3)) 
  }
  tab.to.add4<-tapply(eval(parse_expr(paste0("AA_ADRD_noimpute$",contvars[i]))), 
                      AA_ADRD_noimpute$ethnicity_rec, median, na.rm=T)
  T1results_cont_substr<-rbind(T1results_cont_substr, c(paste0(contvars[i],"_median"),tab.to.add4)) 
  
  tab.to.add5<-tapply(eval(parse_expr(paste0("AA_ADRD_noimpute$",contvars[i]))), 
                      AA_ADRD_noimpute$ethnicity_rec, quantile, probs=0.25, na.rm=T)
  T1results_cont_substr<-rbind(T1results_cont_substr, c(paste0(contvars[i],"_q1"),tab.to.add5)) 
  
  tab.to.add6<-tapply(eval(parse_expr(paste0("AA_ADRD_noimpute$",contvars[i]))), 
                      AA_ADRD_noimpute$ethnicity_rec, quantile, probs=0.75, na.rm=T)
  T1results_cont_substr<-rbind(T1results_cont_substr, c(paste0(contvars[i],"_q3"),tab.to.add6)) 
}
T1results_cont_substr<-data.frame(T1results_cont_substr)

#----Asian subgroups Table 1, imputed dataset - categorical----
#Getting values for the imputed dataset; there should be no missingness!
T1results_cat_impute_substr<-matrix(nrow=1, ncol=9) 
T1results_cat_impute_substr[1,]<- c("Race/ethnicity total",
                                    table(AA_ADRD_impute$ethnicity_rec, exclude = NULL)/5)

for (i in 1:length(catvars)){
  tab.to.add<-table(eval(parse_expr(paste0("AA_ADRD_impute$",catvars[i]))),
                    AA_ADRD_impute$ethnicity_rec, exclude=NULL)/5
  labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
  T1results_cat_impute_substr<-rbind(T1results_cat_impute_substr, c(paste(catvars[i]), rep(NA,8))) 
  T1results_cat_impute_substr<-rbind(T1results_cat_impute_substr,cbind(labs, tab.to.add))
}
#Notes: parse_expr() spits out the string as an expression. Must use eval() fxn
#in order to use the expression (if eval() removed, will cause an error).
#Number of repeated values for rowbind should equal number of strata you have

colnames(T1results_cat_impute_substr)<-c("Variablename", "SouthAsian", "Chinese", "Japanese",
                                         "Korean","Filipino","Vietnamese", "OtherAsian","White") 
rownames(T1results_cat_impute_substr)<-NULL
T1results_cat_impute_substr<-as.data.frame(T1results_cat_impute_substr)

#Get %'s by race/ethnicity
T1results_prop_impute_substr<-matrix(nrow=1, ncol=9) 
T1results_prop_impute_substr[1,]<- c("Race/Ethnicity total",table(
  AA_ADRD_impute$ethnicity_rec)/nrow(AA_ADRD_impute))

#Getting the denominators for the percentage calculations
Racethmargins<-as.numeric(table(AA_ADRD_impute$ethnicity_rec))

for (i in 1:length(catvars)){
  tab.to.add<-t(t(table(eval(parse_expr(paste0("AA_ADRD_impute$",catvars[i])))
                        ,AA_ADRD_impute$ethnicity_rec, exclude=NULL))/Racethmargins)
  labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
  T1results_prop_impute_substr<-rbind(T1results_prop_impute_substr, c(paste(catvars[i]),rep(NA,8))) 
  T1results_prop_impute_substr<-rbind(T1results_prop_impute_substr,cbind(labs, tab.to.add))
}

colnames(T1results_prop_impute_substr)<-c("Variablename", "SouthAsian_prop", "Chinese_prop", 
                                          "Japanese_prop","Korean_prop","Filipino_prop",
                                          "Vietnamese_prop","OtherAsian_prop",
                                          "White_prop")  
rownames(T1results_prop_impute_substr)<-NULL
T1results_prop_impute_substr<-as.data.frame(T1results_prop_impute_substr)

#merge n and % results
T1results_cat_impute_substr<-left_join(T1results_cat_impute_substr, 
                                       T1results_prop_impute_substr, by="Variablename")
T1results_cat_impute_substr<-T1results_cat_impute_substr[
  ,c("Variablename", "SouthAsian","SouthAsian_prop", "Chinese", "Chinese_prop",
     "Japanese","Japanese_prop", "Korean", "Korean_prop","Filipino", 
     "Filipino_prop", "Vietnamese", "Vietnamese_prop", "OtherAsian", 
     "OtherAsian_prop", "White","White_prop")]


#----Asian subgroups, imputed dataset - continuous w/ Rubin's----
#Rubin's rules for continuous values#
#Pivoting long datasets to wide datasets for each of the variables


cont_results <- 
  tibble("var" = 
           rep(c("main_dem_v1_fu_time", "survey_age", "income_pp", "pop_asianalone_pct"), 
               each = 5), 
         "stat" = rep(c("mean", "SD", "median", "q1", "q3"), 4), 
         "Asian" = 0, "1" = 0, "2" = 0, "3" = 0, "4" = 0, "5" = 0, "6" = 0, 
         "12" = 0, "9" = 0)

for(var in c("main_dem_v1_fu_time", "survey_age", "income_pp", "pop_asianalone_pct")){
  subset <- AA_ADRD_impute %>% 
    dplyr::select("subjid", "imp", "asian", "ethnicity_rec", all_of(var))
  
  AA_subset <- subset %>% filter(asian == 1) %>% 
    dplyr::select(c("subjid", "imp", all_of(var))) %>% 
    pivot_wider(id_cols = subjid, names_from = imp, values_from = var)
  
  AA_data <- as.matrix(AA_subset[, 2:6])
  AA_imp_mean = rowMeans(AA_data)
  cont_results[which(cont_results$var == var & cont_results$stat == "mean"), 
               "Asian"] <- mean(AA_imp_mean)
  cont_results[which(cont_results$var == var & cont_results$stat == "SD"), 
               "Asian"] <- sd(AA_imp_mean)
  cont_results[which(cont_results$var == var & cont_results$stat == "median"), 
               "Asian"] <- median(AA_imp_mean)
  cont_results[which(cont_results$var == var & cont_results$stat == "q1"), 
               "Asian"] <- quantile(AA_imp_mean, probs = 0.25)
  cont_results[which(cont_results$var == var & cont_results$stat == "q3"), 
               "Asian"] <- quantile(AA_imp_mean, probs = 0.75)
  
  
  
  for(group in names(table(subset$ethnicity_rec))){
    subgroup <- subset %>% filter(ethnicity_rec == group) %>% 
      dplyr::select(c("subjid", "imp", all_of(var))) %>% 
      pivot_wider(id_cols = subjid, names_from = imp, values_from = var)
    
    subgroup_data <- as.matrix(subgroup[, 2:6])
    
    subgroup_imp_mean = rowMeans(subgroup_data)
    cont_results[which(cont_results$var == var & cont_results$stat == "mean"), 
                 group] <- mean(subgroup_imp_mean)
    cont_results[which(cont_results$var == var & cont_results$stat == "SD"), 
                 group] <- sd(subgroup_imp_mean)
    cont_results[which(cont_results$var == var & cont_results$stat == "median"), 
                 group] <- median(subgroup_imp_mean)
    cont_results[which(cont_results$var == var & cont_results$stat == "q1"), 
                 group] <- quantile(subgroup_imp_mean, probs = 0.25)
    cont_results[which(cont_results$var == var & cont_results$stat == "q3"), 
                 group] <- quantile(subgroup_imp_mean, probs = 0.75)
  }
}
colnames(cont_results)<-c("var","stat","Asian","SouthAsian","Chinese", 
                          "Japanese","Korean","Filipino","Vietnamese",
                          "OtherAsian","White")

cont_results$concat<-paste(cont_results$var, sep = "_", cont_results$stat)
#----Export results to a raw Excel----
t1_res<-list(catvars=T1results_cat, 
             contvars=T1results_cont, 
             catvars_impute=T1results_cat_impute,
             catvars_substr = T1results_cat_substr,
             contvars_substr = T1results_cont_substr,
             catvars_impute_substr = T1results_cat_impute_substr,
             contvars_impute_all = cont_results)

write.xlsx(t1_res, file = "C:/Users/j_fong/Box/Asian_Americans_dementia/Manuscripts/ADI_ADRD/Output/Table1/Table1_raw_21102021.xlsx")

asianzz<-adi_county15aa_tte%>%filter(asian==1)
table(asianzz$decadi_v5_1_new)

#Outcome Descriptive statistics
adi_end_age_strat<-adi_county20aa_tte%>%group_by(asian)%>%
  summarise(
    enframe(quantile(main_dem_v1_end_age, c(0.0, 0.1, 0.25, 0.5, 0.75, 0.9, 1.0)),
            "quantile","FU End Age"),
    mean=(mean(main_dem_v1_end_age,na.rm = TRUE)),
    stddev = sd(main_dem_v1_end_age,na.rm = TRUE))

adi_end_age_unstrat<-adi_county20aa_tte%>%summarise(
  enframe(quantile(main_dem_v1_end_age, c(0.0, 0.1, 0.25, 0.5, 0.75, 0.9, 1.0)),
          "quantile","FU End Age"),
  mean=(mean(main_dem_v1_end_age,na.rm = TRUE)),
  stddev = sd(main_dem_v1_end_age,na.rm = TRUE))

adi_fu_time_strat<-adi_county20aa_tte%>%group_by(asian)%>%
  summarise(
    enframe(quantile(main_dem_v1_fu_time, c(0.0, 0.1, 0.25, 0.5, 0.75, 0.9, 1.0)),
            "quantile","FU Time"),
    mean=(mean(main_dem_v1_fu_time,na.rm = TRUE)),
    stddev = sd(main_dem_v1_fu_time,na.rm = TRUE))

adi_fu_time_unstrat<-adi_county20aa_tte%>%summarise(
  enframe(quantile(main_dem_v1_fu_time, c(0.0, 0.1, 0.25, 0.5, 0.75, 0.9, 1.0)),
          "quantile","FU Time"),
  mean=(mean(main_dem_v1_fu_time,na.rm = TRUE)),
  stddev = sd(main_dem_v1_fu_time,na.rm = TRUE))

adi_dem_unstrat<-adi_county20aa_tte%>%
  group_by(main_dem_v1_end_type)%>%
  summarise(n=n())%>%
  mutate(freq=100*n/sum(n))

adi_dem_strat<-adi_county20aa_tte%>%
  group_by(asian, main_dem_v1_end_type)%>%
  summarise(n=n())%>%
  mutate(freq=100*n/sum(n))
#Exporting to xlsx
dem_fu<-list(unstrat_end_age = adi_end_age_unstrat,
             strat_end_age = adi_end_age_strat,
             unstrat_fu_time = adi_fu_time_unstrat,
             strat_fu_time = adi_fu_time_strat,
             unstrat_end_fu = adi_dem_unstrat,
             strat_end_fu = adi_dem_strat)

write.xlsx(dem_fu, file = "C:/Users/j_fong/Box/Asian_Americans_dementia/Manuscripts/ADI_ADRD/Output/adi_results_descriptive_stats.xlsx")






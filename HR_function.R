#---- Coxph custom HR function ----
custom_HR_calc <- function(centered_pop_percentile, version, model){
  pt_est <- coef(model)[paste0("quintile_kind2000adi_state5")] + 
    coef(model)[paste0("quintile_kind2000adi_state5:pop_asianalone_clean")]*
    centered_pop_percentile
  
  sd <- 
    sqrt(vcov(model)[paste0("quintile_kind2000adi_state5"),paste0("quintile_kind2000adi_state5")] + 
           (centered_pop_percentile)^2*
           vcov(model)[paste0("quintile_kind2000adi_state5:pop_asianalone_clean"), 
                       paste0("quintile_kind2000adi_state5:pop_asianalone_clean")] + 
           2*centered_pop_percentile*
           vcov(model)[paste0("quintile_kind2000adi_state5"), 
                       paste0("quintile_kind2000adi_state5:pop_asianalone_clean")])
  
  ll <- pt_est - 1.96*sd
  ul <- pt_est + 1.96*sd
  
  return(exp(c(pt_est, ll, ul)))
}
 

# #---- GAM custom HR function ----
# custom_HR_calc <- function(centered_pop_percentile, version, model){
#   pt_est <- coef(model)[paste0("quintile_kind2000adi_", version,"5")] + 
#     coef(model)[paste0("pop_asianalone_clean")]*centered_pop_percentile + 
#     coef(model)[paste0("quintile_kind2000adi_",version,"5:pop_asianalone_clean")]*
#     centered_pop_percentile
#   
#   sd <- 
#     sqrt(vcov(model)[paste0("quintile_kind2000adi_",version,"5"),paste0("quintile_kind2000adi_", version,"5")] + 
#            (centered_pop_percentile)^2*
#            vcov(model)[paste0("pop_asianalone_clean"), 
#                        paste0("pop_asianalone_clean")] + 
#            (centered_pop_percentile)^2*
#            vcov(model)[paste0("quintile_kind2000adi_", version,"5:pop_asianalone_clean"), 
#                        paste0("quintile_kind2000adi_", version,"5:pop_asianalone_clean")] + 
#            2*centered_pop_percentile*
#            vcov(model)[paste0("quintile_kind2000adi_", version,"5"), paste0("pop_asianalone_clean")] + 
#            2*centered_pop_percentile*
#            vcov(model)[paste0("quintile_kind2000adi_", version,"5"), 
#                        paste0("quintile_kind2000adi_", version,"5:pop_asianalone_clean")] + 
#            2*(centered_pop_percentile)^2*
#            vcov(model)[paste0("pop_asianalone_clean"), 
#                        paste0("quintile_kind2000adi_", version,"5:pop_asianalone_clean")])
#   
#   ll <- pt_est - 1.96*sd
#   ul <- pt_est + 1.96*sd
#   
#   return(exp(c(pt_est, ll, ul)))
# }

# #---- custom HR function ----
# custom_HR_calc <- function(centered_pop_percentile, version, model){
#   pt_est <- coef(model)[paste0("quintile_kind2000adi_", version,"5")] + 
#     coef(model)[paste0("asianalone_", version, "_clean")]*centered_pop_percentile + 
#     coef(model)[paste0("quintile_kind2000adi_",version,"5:asianalone_",version,"_clean")]*
#     centered_pop_percentile
#   
#   sd <- 
#     sqrt(vcov(model)[paste0("quintile_kind2000adi_",version,"5"),paste0("quintile_kind2000adi_", version,"5")] + 
#            (centered_pop_percentile)^2*
#            vcov(model)[paste0("asianalone_", version, "_clean"), 
#                        paste0("asianalone_", version, "_clean")] + 
#            (centered_pop_percentile)^2*
#            vcov(model)[paste0("quintile_kind2000adi_", version,"5:asianalone_", version, "_clean"), 
#                        paste0("quintile_kind2000adi_", version,"5:asianalone_", version, "_clean")] + 
#            2*centered_pop_percentile*
#            vcov(model)[paste0("quintile_kind2000adi_", version,"5"), paste0("asianalone_", version, "_clean")] + 
#            2*centered_pop_percentile*
#            vcov(model)[paste0("quintile_kind2000adi_", version,"5"), 
#                        paste0("quintile_kind2000adi_", version,"5:asianalone_", version, "_clean")] + 
#            2*(centered_pop_percentile)^2*
#            vcov(model)[paste0("asianalone_", version, "_clean"), 
#                        paste0("quintile_kind2000adi_", version,"5:asianalone_", version, "_clean")])
#   
#   ll <- pt_est - 1.96*sd
#   ul <- pt_est + 1.96*sd
#   
#   return(exp(c(pt_est, ll, ul)))
# }


# #---- custom HR function ----
# custom_HR_calc <- function(centered_pop_percentile, model){
#   pt_est <- coef(model)["quintile_adi_v5_1_new5"] + 
#     coef(model)["pop_asiaborn_percentile_clean"]*centered_pop_percentile + 
#     coef(model)["quintile_adi_v5_1_new5:pop_asiaborn_percentile_clean"]*
#     centered_pop_percentile
#   
#   sd <- 
#     sqrt(vcov(model)["quintile_adi_v5_1_new5", "quintile_adi_v5_1_new5"] + 
#            (centered_pop_percentile)^2*
#            vcov(model)["pop_asiaborn_percentile_clean", 
#                        "pop_asiaborn_percentile_clean"] + 
#            (centered_pop_percentile)^2*
#            vcov(model)["quintile_adi_v5_1_new5:pop_asiaborn_percentile_clean", 
#                        "quintile_adi_v5_1_new5:pop_asiaborn_percentile_clean"] + 
#            2*centered_pop_percentile*
#            vcov(model)["quintile_adi_v5_1_new5", "pop_asiaborn_percentile_clean"] + 
#            2*centered_pop_percentile*
#            vcov(model)["quintile_adi_v5_1_new5", 
#                        "quintile_adi_v5_1_new5:pop_asiaborn_percentile_clean"] + 
#            2*(centered_pop_percentile)^2*
#            vcov(model)["pop_asiaborn_percentile_clean", 
#                        "quintile_adi_v5_1_new5:pop_asiaborn_percentile_clean"])
#   
#   ll <- pt_est - 1.96*sd
#   ul <- pt_est + 1.96*sd
#   
#   return(exp(c(pt_est, ll, ul)))
# }
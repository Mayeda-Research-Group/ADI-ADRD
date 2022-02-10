#---
# Loading packages, options and initializations
#---
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("haven", "tidyverse", "magrittr", "foreign", "tidyverse","ggplot2",
       "survey", "tableone", "mice", "openxlsx", "survival", "scales",
       "ggpubr", "ggdist")

options(scipen = 999, digits=6)

#---- path to box ----
#Crystal's path
#path_to_box <- "/Users/CrystalShaw/Box"
# #Taylor's path
#path_to_box <- "C:/Users/tmobley/Box"
# Joey's path
path_to_box <-"C:/Users/j_fong/Box"

#---- load results ----
load(paste0(path_to_box,
            "/Asian_Americans_dementia_data/adi_adrd/analysis_data/",
            "adi_county15aa_analysis.R"))

#--- Spearman correlations ----
# check correl between ADI and participant edu, income per person
#---

#- running on first imputation
imp1 <- adi_county15aa_analysis %>% filter(imp == 1)
colnames(imp1)

imp1$decile_kind2000adi_county <-
  as.factor(imp1$decile_kind2000adi_county)
imp1$quintile_kind2000adi_state <-
  factor(imp1$quintile_kind2000adi_state, ordered = TRUE)
imp1$education_rev <-
  factor(imp1$education_rev, ordered=TRUE)
imp1$education4 <-
  factor(imp1$education4, ordered=TRUE)
class(imp1$incomepp_clean)
imp1$quintile_kind2000adi_state_new<-as.numeric(imp1$quintile_kind2000adi_state)
p1 <- ggscatter(imp1, y = "quintile_kind2000adi_state_new", x = "incomepp_clean", 
            add = "reg.line", conf.int = FALSE, facet.by = "asian",
            cor.coef = TRUE, cor.method = "spearman",
            title = 'Income per person vs ADI correlation plot (with spearman r)',
            xlab = "Income per person (centered at $42,000, per $10,000)", 
            ylab = "2000 ADI Quintiles, relative to state") 

p1 + theme_bw()
ggsave(filename = paste0("C:/Users/j_fong/Box/Asian_Americans_dementia/", 
                         "Manuscripts/ADI_ADRD/Manuscript/figures/",
                         "spearman_correl_inc_adi.png"), device = "png", 
       height = 5, width = 7, units = "in")


imp1$education_rev<-as.numeric(imp1$education_rev)
p2 <- ggscatter(imp1, x ='education_rev', y = "quintile_kind2000adi_state_new", 
                add = "reg.line", conf.int = FALSE, facet.by = "asian",
                cor.coef = TRUE, cor.method = "spearman",
                xlab = "education, 6-level", 
                ylab = "2000 ADI deciles, relative to state")

p2 + theme_bw()
ggsave(filename = paste0("C:/Users/j_fong/Box/Asian_Americans_dementia/", 
                         "Manuscripts/ADI_ADRD/Manuscript/figures/",
                         "spearman_correl_edu6_adi.png"), device = "png", 
       height = 5, width = 7, units = "in")


imp1$education4<-as.numeric(imp1$education4)
p3 <- ggscatter(imp1, x ='education4', y = "quintile_kind2000adi_state_new", 
                add = "reg.line", conf.int = TRUE, facet.by = "asian",
                cor.coef = TRUE, cor.method = "spearman",
                xlab = "education, 4-level",
                ylab = "2000 ADI deciles, relative to state")

p3 + theme_bw()
ggsave(filename = paste0("C:/Users/j_fong/Box/Asian_Americans_dementia/", 
                         "Manuscripts/ADI_ADRD/Manuscript/figures/",
                         "spearman_correl_edu4_adi.png"), device = "png", 
       height = 5, width = 7, units = "in")


imp1$education_rev <- 
  as.factor(imp1$education_rev)

imp1$education4 <- 
  as.factor(imp1$education4)

imp1$education_rev <- 
  as.factor(imp1$education_rev)

#---- **Creating a new dataset for boxplots and relabeling ----
adi_county15aa_analysis_boxplot <- imp1

adi_county15aa_analysis_boxplot$ethnicity_rec <- 
  recode_factor(adi_county15aa_analysis_boxplot$ethnicity_new,
                '1' = '1', '2' = '2', '3' = '3', '4' = '4', '5' = '5', '6' = '6', 
                '7' = '13', '8' = '13', '9' = '9', '10' = '13', '11' = '13')

#Checking recode of ethnicity
#table(adi_county15aa_analysis_boxplot$ethnicity_rec, adi_county15aa_analysis_boxplot$ethnicity_new)

#---- **subsetting to only AA's ----
adi_asian_only <- adi_county15aa_analysis_boxplot %>% filter(asian == 1)
table(adi_asian_only$ethnicity_new)

#---- **Duplicating rows for AA's; added as a separate category in ggplot ----
aa_rowbind <- adi_asian_only
aa_rowbind$ethnicity_rec <- as.factor(12)
table(aa_rowbind$ethnicity_rec)
adi_asianwhite <- adi_county15aa_analysis_boxplot
adi_county15aa_analysis_boxplot <- 
  rbind(adi_county15aa_analysis_boxplot, aa_rowbind)
table(adi_county15aa_analysis_boxplot$ethnicity_rec)

table(adi_county15aa_analysis_boxplot$ethnicity_rec)

adi_county15aa_analysis_boxplot$asian_f <- 
  factor(adi_county15aa_analysis_boxplot$asian, levels = c(0, 1),
         labels = c("Non-Latino White", "Asian Americans"))

adi_asianwhite$asian_f <- 
  factor(adi_asianwhite$asian,
         levels = c(0, 1),
         labels = c("Non-Latino White", "Asian Americans"))

adi_county15aa_analysis_boxplot %<>% 
  mutate("asian_c" = case_when(asian == 1 ~ "Asian American", 
                               TRUE ~ "Non-Latino White"))


#---- **Releveling data levels of ethnicity ----
adi_county15aa_analysis_boxplot$ethnicity_new_f <- 
  factor(adi_county15aa_analysis_boxplot$ethnicity_rec,
         levels = c(9, 12, 2, 5, 3, 1, 4, 6, 13),
         labels = c("Non-Latino Whites",
                    "Asian Americans",
                    "Chinese",
                    "Filipino",
                    "Japanese",
                    "South Asian",
                    "Korean",
                    "Vietnamese",
                    "Other Asian Ethnicity"))

#---- **Adding new variable to use for boxplot race/ethnicity axes ----
#NOTE: this code probably not necessary, but keeping since other bits of code 
#rely on the naming
adi_county15aa_analysis_boxplot %<>% 
  mutate("ethnicity_new_c" = 
           case_when(ethnicity_rec == 9 ~ "Non-Latino White", 
                     ethnicity_rec == 12 ~ "Asian American", 
                     ethnicity_rec == 2 ~ "Chinese", 
                     ethnicity_rec == 5 ~ "Filipino", 
                     ethnicity_rec == 3 ~ "Japanese", 
                     ethnicity_rec == 1 ~ "South Asian", 
                     ethnicity_rec == 4 ~ "Korean", 
                     ethnicity_rec == 6 ~ "Vietnamese", 
                     ethnicity_rec == 13 ~ "Other Asian Ethnicity")) 

#---- **Creating a new variable to facet other analyses on ----
adi_county15aa_analysis_boxplot %<>% 
  mutate("data" = 
           case_when(ethnicity_new_c %in% 
                       c("Non-Latino White", "Asian American") ~ 
                       "Total Sample", 
                     TRUE ~ "Asian Subgroups"))
adi_county15aa_analysis_boxplot$data <- 
  factor(adi_county15aa_analysis_boxplot$data,
         levels = c("Total Sample", "Asian Subgroups"))

#---- **ordering the data (for facet appearance only)----
#create ordering variable
adi_county15aa_analysis_boxplot %<>% 
  mutate("order" = 
           case_when(ethnicity_new_c == "Non-Latino White" ~ 1,
                     ethnicity_new_c == "Asian American" ~ 2,
                     ethnicity_new_c == "Chinese" ~ 3,
                     ethnicity_new_c == "Filipino" ~ 4,
                     ethnicity_new_c == "Japanese" ~ 5, 
                     ethnicity_new_c == "South Asian" ~ 6, 
                     ethnicity_new_c == "Korean" ~ 7, 
                     ethnicity_new_c == "Vietnamese" ~ 8, 
                     ethnicity_new_c == "Other Asian Ethnicity" ~ 9))

adi_county15aa_analysis_boxplot %<>% arrange(data, order)

#---- **Collapsing other SE, unspecified, and multiple Asian into 1 cat ----
#Note: Order should be white, Asians, then all the other Asian subgrps
adi_asian_only$ethnicity_new_f <- 
  factor(adi_asian_only$ethnicity_rec,
         levels = c(12, 2, 5, 3, 1, 4, 6, 13),
         labels = c("Asian Americans",
                    "Chinese",
                    "Filipino",
                    "Japanese",
                    "South Asian",
                    "Korean",
                    "Vietnamese",
                    "Other Asian Ethnicity"))

#---- Figure 1a: ADI Quintile Stacked Barcharts----
#---- **NLW + all AA subgroups ----
ggplot() + 
  geom_bar(data = adi_county15aa_analysis_boxplot,
           aes(x = factor(ethnicity_new_f), 
               fill = factor(quintile_kind2000adi_state,
                             levels = c(5,4,3,2,1))),
           position = "fill", alpha = 0.6) + 
  xlab("Race/Ethnicity") + ylab("Total %") + 
  scale_y_continuous(labels = scales::percent) +
  labs(caption = "*Higher neighborhood disadvantage quintile indicates more disadvantage") +
  theme_bw()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_text(angle=30, hjust = 1),
        plot.caption = element_text(hjust = 0)) +
    scale_fill_brewer(name = "Neighborhood \ndisadvantage quintile*",
                      labels = c("Quintile 5", "Quintile 4", "Quintile 3",
                                 "Quintile 2", "Quintile 1"),
                      direction = -1,
                      palette = 1,
                      limits = as.factor(c(5,4,3,2,1,0)),
                      breaks = c(5,4,3,2,1)) +
  #Adding in frequency % labels
  geom_text(data = adi_county15aa_analysis_boxplot %>% group_by(data, ethnicity_new_f)%>%
              summarise(vals = prop.table(table(quintile_kind2000adi_state))%>% round(4),
                        pos = cumsum(prop.table(table(quintile_kind2000adi_state)))-0.5*vals), 
            aes(x = ethnicity_new_f,  y =pos, label = paste0(format(round(100.00*vals, digits = 1),nsmall = 1),'%')),size=5)+ 
    geom_text(data=adi_county15aa_analysis_boxplot %>% 
                group_by(data, ethnicity_new_f) %>% summarise(n=n()), 
              aes(x=ethnicity_new_f, y=0, label= paste0("n = ", formatC(n, big.mark = ","))), 
              nudge_y=-.025, size = 5)+
  facet_grid(. ~ data, space = "free_x", scales = "free_x", drop = TRUE) +
    theme(text = element_text(size = 20),
          axis.text.x = element_text(size = 18),
          axis.text.y = element_text(size = 18),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16))

  ggsave(filename = paste0(path_to_box, "/Asian_Americans_dementia/", 
                         "Manuscripts/ADI_ADRD/Manuscript/figures/",
                         "adi_stacked_barchart_asiansubgrp_nlw_color_r1.jpg"), device = "jpg", 
       height = 8, width = 16, units = "in")


#---- Figure 1b: Asian Ethnic Density boxplots ----
#---- **NLW + all AA subgroups ----
cbPalette <- c("#F8766D","#525252", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
               "#0072B2", "#D55E00", "#CC79A7")

table(adi_county15aa_analysis_boxplot$order)
ggplot(data = adi_county15aa_analysis_boxplot, 
       aes(x = factor(order), y = pop_asianalone_pct,
           fill = factor(order))) + 
  scale_fill_manual(values = cbPalette) +
  geom_boxplot(width = 0.25, alpha = 0.7) +
  labs(caption = "Histograms and boxplots (median and IQR) shown") + 
  xlab('Race/Ethnicity') + ylab('Asian ethnic density (%)') + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        text = element_text(size = 16),
        plot.caption = element_text(hjust = 0)) + 
  facet_grid(. ~ data, space = "free_x", scales = "free_x", drop = TRUE) + 
  geom_text(data = adi_county15aa_analysis_boxplot %>%
              group_by(data, order) %>%
              summarise(top = min(pop_asianalone_pct), n = n()),
            aes(size = 5, x = factor(order), y = top, label = paste0("n = ", formatC(n, big.mark = ","))),
            nudge_y = -.025) + 
  scale_x_discrete(breaks = factor(adi_county15aa_analysis_boxplot$order),
                   labels = adi_county15aa_analysis_boxplot$ethnicity_new_c) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10),
                     labels = scales::percent) + 
  theme(text = element_text(size = 16),
        legend.position = "none") +
  ggdist::stat_halfeye(slab_type = 'pdf',
                       adjust=1,
                       justification = -0.5,
                       alpha = 0.7,
                       width = .5) + 
  theme(text = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18))

ggsave(filename = paste0(path_to_box, "/Asian_Americans_dementia/", 
                         "Manuscripts/ADI_ADRD/Manuscript/figures/",
                         "asianalone_pct_asiansubgrp_nlw_raincloud_color_r1.jpg"), device = "jpg", 
       height = 8, width = 15, units = "in")

#---- Figure 2 & 3 ----
#---- **Reading in model output ----
output_estims1 <- 
  read.xlsx(paste0(path_to_box, "/Asian_Americans_dementia/Manuscripts/", 
                   "ADI_ADRD/Output/", 
                   "quintilekind2000adi_coxph_all_filtered.xlsx"))

output_estims2 <- 
  read.xlsx(paste0(path_to_box, "/Asian_Americans_dementia/Manuscripts/", 
                   "ADI_ADRD/Output/", 
                   "quintilekind2000adi_asianeth_coxph_all_filtered.xlsx"))

output_estims <- bind_rows(output_estims1, output_estims2)

#---- **making a color palette for ggplot----
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", 
               "#525252", "#F8766D")

racethlist <- c('Chinese','Filipino','Japanese','South Asian', 'Korean', 
                'Vietnamese','asian', 'white')

#---- **Creating/duplicating facets for ggplot ----
#Notes: Needed to create duplicate facets in order to plot total sample
#summaries (i.e. all Asian vs NLW) on a facet separate from Asian subethnicity
#output.
output_estims_m3 <- output_estims %>% filter(
  model == 'm3' &  (str_detect(names, "state") | str_detect(names, "county")))

for (i in 1:length(output_estims_m3$raceth)){
  for (j in 1:length(racethlist)){
    if(output_estims_m3$raceth[i] == racethlist[j]){
      output_estims_m3$facetcnt[i] = j
    }
  }
}

rowstobind <- output_estims_m3 %>%
  filter(raceth == 'asian' & model == "m3" &
           str_detect(names, "quintile_kind2000adi"))

rowstobind$facetcnt <- 1
newrowstobind <- rowstobind

for (i in 2:7){
  for (j in 1:4){
    rowstobind$facetcnt[j] = i
  }
  newrowstobind<-rbind(newrowstobind, rowstobind)
}

asianwhiteest <- output_estims_m3 %>% 
  filter(raceth %in% c("asian", "white") & 
           str_detect(names, "quintile_kind2000adi"))

asiansubgrp <- rbind(output_estims_m3, newrowstobind) %>% 
  filter(facetcnt %in% seq(1, 6) & str_detect(names, "quintile_kind2000adi"))

asianwhiteest$names_f <- factor(asianwhiteest$names,
                                levels = c("quintile_kind2000adi_state2",
                                           "quintile_kind2000adi_state3",
                                           "quintile_kind2000adi_state4",
                                           "quintile_kind2000adi_state5"),
                                labels = c("2", "3", "4", "5"))

asianwhiteest$raceth_f <- factor(
  asianwhiteest$raceth, levels = c("white", "asian"),
  labels = c("Non-Latino White", "Asian Americans"))

asiansubgrp$facet_cnt_f <- 
  factor(asiansubgrp$facetcnt,
         levels = c(1, 2, 3, 4, 5, 6, 7),
         labels = c("Chinese",
                    "Filipino",
                    "Japanese",
                    "South~Asian",
                    "Korean",
                    "Vietnamese",
                    "Asian~Americans"))

asiansubgrp$names_f <- 
  factor(asiansubgrp$names,
         levels = c("quintile_kind2000adi_state2",
                    "quintile_kind2000adi_state3",
                    "quintile_kind2000adi_state4",
                    "quintile_kind2000adi_state5"),
         labels = c("2", "3", "4", "5"))

asiansubgrp$raceth_f<-
  factor(asiansubgrp$raceth,
         levels = c("Chinese",
                    "Filipino",
                    "Japanese",
                    "South Asian",
                    "Korean",
                    "Vietnamese",
                    "asian"),
         labels = c("Chinese",
                    "Filipino",
                    "Japanese",
                    "South Asian",
                    "Korean",
                    "Vietnamese",
                    "Asian Americans"))


#- subset to state and county ADI quintiles 
asianwhiteest$version <- ifelse(str_detect(asianwhiteest$names, "state"),1, 2)

subset <- asianwhiteest
title_index <- ifelse(subset$version==1,"state", "county")

#---- **Figure 2: Cox model HRs (Asian vs NLW) ----
p<- ggplot(data = subset,
           aes(x = names_f, y = HR, ymin = LL, ymax = UL, 
               col = raceth_f, fill = raceth_f)) + 
  geom_pointrange(aes(col = raceth_f), 
                  position=position_dodge(width = 0.5)) + 
  geom_errorbar(aes(ymin = LL, ymax = UL), 
                position = position_dodge(width = 0.5), width = 0.2, cex = 1) +
  geom_hline(aes(fill = names_f), yintercept = 1, linetype = 2) +
  #scale_y_continuous(limits = c(0.8, 1.2), breaks = scales::pretty_breaks(n = 5)) +
  xlab('Neighborhood Disadvantage Quintile') + 
  ylab('Hazard Ratio') +
  theme_bw() +
  coord_cartesian(ylim = c(0.8, 1.3), expand = TRUE) +
  scale_y_log10(breaks = c(0.8, 0.9, 1.0, 1.1, 1.2, 1.3),
                limits = c(.775, 1.325))+
  theme(legend.title = element_blank(),
        legend.key = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0),
        text = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12)) + 
  scale_color_manual(values = c("#F8766D", "#525252"))
#ggtitle(paste0("AA and NLW ADI quintile M3, relative to ", title_index))

p
ggsave(p, filename = paste0(path_to_box, 
                            "/Asian_Americans_dementia/", 
                            "Manuscripts/ADI_ADRD/Manuscript/figures/",
                            "coxph_hr_asian_white_state_r1.jpg"), device = "jpg", 
       height = 5, width = 7, units = "in")


#---- **Figure 3: Cox model HRs (Asian subeths) log scale w arrows----
#---- **Dropping Quintiles 4 and 5 for Vietnamese ethnicity ----

#- 11/29/2021: JF using CS's more efficient code -
#-- commenting out old code for reference --
# asiansubgrpviet <- asiansubgrp %>% filter(str_detect(raceth, 'Vietnamese')) %>%
#   filter(!str_detect(names, 'quintile_kind2000adi_state5')) %>% 
#   filter(!str_detect(names, 'quintile_kind2000adi_state4'))
 
asiansubgrp_state <- asiansubgrp %>% filter(str_detect(asiansubgrp$names, "state"))
# asiansubgrp_state <- rbind(asiansubgrp_state, asiansubgrpviet)


coordlimit<-4.0
asiansubgrp_state <- asiansubgrp_state %>% mutate(
  LL_1 = ifelse(LL < 0.25, .25, NA),
  UL_1 = ifelse(UL > coordlimit, coordlimit, NA)) %>%
  filter(!(raceth_f == "Vietnamese" & names %in% 
             c("quintile_kind2000adi_state4", "quintile_kind2000adi_state5")))

ggplot(data = asiansubgrp_state,
       aes(x = names_f, y = HR, ymin = LL, ymax = UL, 
           col = raceth_f, fill = raceth_f)) +
  geom_pointrange(aes(col = raceth_f), position = position_dodge(width = 0.5), 
                  cex = 1, fatten = 2) + 
  geom_errorbar(aes(ymin = LL, ymax = UL), 
                position = position_dodge(width = 0.5),
                width = 0.4, cex = 1) +
  geom_hline(aes(fill = names_f), yintercept = 1, linetype = 2) +
  geom_segment(
    aes(x = as.numeric(names_f)-.125, xend = as.numeric(names_f)-.125,
        y = LL, yend = UL_1),
    arrow = arrow(length = unit(.5, "cm")),
    show.legend = FALSE,
    size = 1) +
  geom_segment(
    aes(x = as.numeric(names_f)-.125, xend = as.numeric(names_f)-.125,
        y = UL, yend = LL_1),
    arrow = arrow(length = unit(.5, "cm")),
    show.legend = FALSE,
    size = 1) +
  #scale_y_continuous(trans = log_trans()) +
  scale_y_log10(breaks = c(0.25,.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0),
                limits = c(.25, 4.0))+
  facet_wrap(~ facet_cnt_f, labeller = label_parsed) + theme_bw() +
  xlab('Neighborhood Disadvantage Quintile') + ylab('Hazard Ratio') + 
  theme(legend.title = element_blank(),
        legend.key = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0),
        text = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 9),
        legend.text = element_text(size = 12)) + 
  scale_color_manual(values = cbPalette)

ggsave(filename = paste0(path_to_box, "/Asian_Americans_dementia/", 
                         "Manuscripts/ADI_ADRD/Manuscript/figures/",
                         "hr_asian_subgrp_newm3_log_scale_r1.jpeg"), 
       device = "jpeg", 
       height = 8, width = 10, units = "in")

#----R1 JF - Figure S1: Adding in model output for Asian-White nativity HR's----
#----**Loading in Model Output----
all_models <- 
  read.xlsx(paste0(path_to_box, "/Asian_Americans_dementia/Manuscripts/", 
                   "ADI_ADRD/Output/", 
                   "quintilekind2000adi_coxph_nativity_m3.xlsx"))

#----**Preparing labels----
asianwhiteest <- all_models %>% filter(model == 'm3' &
                                         (str_detect(names, "adi")))
asianwhiteest$names_f <- factor(asianwhiteest$names,
                                levels = c("quintile_kind2000adi_state2",
                                           "quintile_kind2000adi_state3",
                                           "quintile_kind2000adi_state4",
                                           "quintile_kind2000adi_state5"),
                                labels = c("2", "3", "4", "5"))
asianwhiteest$raceth_f <- factor(
  asianwhiteest$raceth, levels = c("white", "asian"),
  labels = c("Non-Latino White", "Asian Americans"))
#---- **Nativity Model 3 Asian vs NLW plot ----
p<- ggplot(data = asianwhiteest,
           aes(x = names_f, y = HR, ymin = LL, ymax = UL,
               col = raceth_f, fill = raceth_f)) +
  geom_pointrange(aes(col = raceth_f),
                  position=position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin =LL, ymax = UL),
                position = position_dodge(width = 0.5), width = 0.2, cex = 1) +
  geom_hline(aes(fill = names_f), yintercept = 1, linetype = 2) +
  #scale_y_continuous(limits = c(0.8, 1.2), breaks = scales::pretty_breaks(n = 5)) +
  xlab('Neighborhood Disadvantage Quintile') +
  ylab('Hazard Ratio') +
  theme_bw() +
  coord_cartesian(ylim = c(0.8, 1.3), expand = TRUE) +
  scale_y_log10(breaks = c(0.8, 0.9, 1.0, 1.1, 1.2, 1.3),
                limits = c(.775, 1.325))+
  theme(legend.title = element_blank(),
        legend.key = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0),
        text = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_color_manual(values = c("#F8766D", "#525252"))
#ggtitle(paste0("AA and NLW ADI quintile M3, relative to ", title_index))
p
ggsave(p, filename = paste0(path_to_box,
                            "/Asian_Americans_dementia/",
                            "Manuscripts/ADI_ADRD/Manuscript/",
                            "figures/coxph_hr_nativity_asian_white_state_r1.jpg"),
       height = 5, width = 7, units = "in")

#---- Figure S2: Top quintile vs lowest quintile figures & Age as Timescale Figures ----
#NOTE: Code cleaning for scripts for supplemental figures (Figs 1S and 2S) 
#contain code that overwrites previous names. Must run new cleaning script
#before creating figures, or else will use old references to previous estimates.
#---- **Loading in all output estimates ----
output_estims3 <- 
  read.xlsx(paste0(path_to_box, "/Asian_Americans_dementia/Manuscripts/", 
                   "ADI_ADRD/Output/", 
                   "quintilekind2000adi_coxph_sa_all.xlsx"))


#Note: color palette same as before, but including in case only want to run 
#this section

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", 
               "#525252")

racethlist <- c('Chinese','Filipino','Japanese','South Asian', 'Korean', 
                'Vietnamese','asian', 'white')

output_estims_m3_2080 <- output_estims3 %>% 
  filter(model == 'm3' & (str_detect(names, "state"))) %>%
  filter(!raceth == "Vietnamese")

output_estims_m3_agetimescale <- output_estims3 %>% 
  filter(model == 'age_m3' & (str_detect(names, "state")))

for (i in 1:length(output_estims_m3_2080$raceth)){
  for (j in 1:length(racethlist)){
    if(output_estims_m3_2080$raceth[i] == racethlist[j]){
      output_estims_m3_2080$facetcnt[i] = j
    }
  }
}

asianwhiteest_2080 <- output_estims_m3_2080 %>% 
  filter(raceth %in% c("asian", "white") & 
           str_detect(names, "2000adi"))

asianwhiteest_2080$raceth_f <- factor(
  asianwhiteest_2080$raceth, levels = c("white", "asian"),
  labels = c("Non-Latino White", "Asian Americans"))

#---- **AA-NLW 20% v 80% HR Plot ----
ggplot(data = asianwhiteest_2080,
       aes(x = raceth_f, y = HR, ymin = LL, ymax = UL, 
           col = raceth_f, fill = raceth_f)) + 
  geom_pointrange(aes(col = raceth_f), 
                  position=position_dodge(width = 0.5)) + 
  geom_errorbar(aes(ymin = LL, ymax = UL), 
                position = position_dodge(width = 0.5), width = 0.2, cex = 1) +
  geom_hline(aes(fill = raceth_f), yintercept = 1, linetype = 2) +
  #scale_y_continuous(limits = c(0.8, 1.2), breaks = scales::pretty_breaks(n = 5)) +
  xlab('Neighborhood Disadvantage Quintile') + 
  ylab('Hazard Ratio') +
  theme_bw() +
  scale_y_log10(breaks = c(0.8,.9, 1.0, 1.1, 1.2),
                limits = c(0.775, 1.225))+
  theme(legend.title = element_blank(),
        legend.key = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0),
        text = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12)) + 
  scale_color_manual(values = c("#F8766D", "#525252"))
  

ggsave(filename = paste0(path_to_box, 
                            "/Asian_Americans_dementia/", 
                            "Manuscripts/ADI_ADRD/Manuscript/figures/",
                            "2080_hr_asian_white_state_r1.jpg"), device = "jpg", 
       height = 5, width = 7, units = "in")

#---- **Preparing Asian subgroup labels ----
output_estims_m3_2080<-mutate(output_estims_m3_2080, bigfacet = ifelse(raceth == 'asian' |
                                                                         raceth == 'white',
                                                                       'Total Sample', 
                                                                       'Asian Subgroups'))

output_estims_m3_2080$bigfacet <- factor(output_estims_m3_2080$bigfacet,
                                         levels = c("Total Sample", 
                                                    "Asian Subgroups"))

output_estims_m3_2080$raceth_f<-
  factor(output_estims_m3_2080$raceth,
         levels = c("white",
                    "asian",
                    "Chinese",
                    "Filipino",
                    "Japanese",
                    "South Asian",
                    "Korean",
                    "Vietnamese"),
         labels = c("Non-Latino white",
                    "Asian Americans",
                    "Chinese",
                    "Filipino",
                    "Japanese",
                    "South Asian",
                    "Korean",
                    "Vietnamese"))

cbPalette_new <- c("#F8766D", "#525252","#E69F00", "#56B4E9", "#009E73", 
                   "#F0E442", "#0072B2", "#D55E00")

coordlimit<-4.0
output_estims_m3_2080 <- output_estims_m3_2080 %>% mutate(
  LL_1 = ifelse(LL < 0.25, .25, NA),
  UL_1 = ifelse(UL > coordlimit, coordlimit, NA)) %>%
  filter(!(raceth_f == "Vietnamese" & names %in% 
             c("quintile_kind2000adi_state4", "quintile_kind2000adi_state5")))

#---- **All NLW & Asian 20% v 80% HR Plot ----
ggplot(data = output_estims_m3_2080,
       aes(x = raceth_f, y = HR, ymin = LL, ymax = UL, 
           col = raceth_f, fill = raceth_f)) + 
  geom_pointrange(aes(col = raceth_f), 
                  position=position_dodge(width = 0.5)) + 
  geom_errorbar(aes(ymin = LL, ymax = UL), 
                position = position_dodge(width = 0.5), width = 0.2, cex = 1) +
  geom_hline(aes(fill = raceth_f), yintercept = 1, linetype = 2) +
  
  #scale_y_continuous(limits = c(0.8, 1.2), breaks = scales::pretty_breaks(n = 5)) +
  xlab('Race/Ethnicity') + 
  ylab('Hazard Ratio') +
  theme_bw() +
  
  # geom_segment(
  #   aes(x = as.numeric(raceth_f)-.125, xend = as.numeric(raceth_f)-.125,
  #       y = LL, yend = UL_1),
  #   arrow = arrow(length = unit(.5, "cm")),
  #   show.legend = FALSE,
  #   size = 1) +
  # geom_segment(
  #   aes(x = as.numeric(raceth_f)-.125, xend = as.numeric(raceth_f)-.125,
  #       y = UL, yend = LL_1),
  #   arrow = arrow(length = unit(.5, "cm")),
  #   show.legend = FALSE,
  #   size = 1) +
  #R1 JF: Increasing y-axis scale to 4.0
  scale_y_log10(breaks = c(0.25,.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0),
                limits = c(0.25, 4.0))+
  theme(legend.title = element_blank(), panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(angle = 30, hjust = 1)) + 
  scale_color_manual(values = cbPalette_new) + 
  facet_grid(. ~ bigfacet, space = "free_x", scales = "free_x", drop = TRUE) +
  theme(legend.title = element_blank(),
        legend.key = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0),
        text = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12)) 
  
ggsave(filename = paste0(path_to_box, 
                         "/Asian_Americans_dementia/", 
                         "Manuscripts/ADI_ADRD/Manuscript/figures/",
                         "2080_hr_all_raceeth_state_r1.jpg"), device = "jpg", 
       height = 8, width = 10, units = "in")

#---- Figure S3: Age as Timescale HR's ----
#---- **preparing age as timescale data ----
for (i in 1:length(output_estims3$raceth)){
  for (j in 1:length(racethlist)){
    if(output_estims3$raceth[i] == racethlist[j]){
      output_estims3$facetcnt[i] = j
    }
  }
}

output_estims_m3_agetimescale <- output_estims3 %>% 
  filter(model == 'age_m3' & (str_detect(names, "state")))

rowstobind <- output_estims_m3_agetimescale %>%
  filter(raceth == 'asian' & model == "age_m3" &
           str_detect(names, "quintile_kind2000adi_state"))

rowstobind$facetcnt <- 1
newrowstobind <- rowstobind

for (i in 2:7){
  for (j in 1:4){
    rowstobind$facetcnt[j] = i
  }
  newrowstobind<-rbind(newrowstobind, rowstobind)
}

asianwhiteest <- output_estims_m3_agetimescale %>% 
  filter(raceth %in% c("asian", "white") & 
           str_detect(names, "quintile_kind2000adi"))

asiansubgrp <- rbind(output_estims_m3_agetimescale, newrowstobind) %>% 
  filter(facetcnt %in% seq(1, 6) & str_detect(names, "quintile_kind2000adi"))


asiansubgrp <- rbind(output_estims_m3_agetimescale, newrowstobind) %>% 
  filter(facetcnt %in% seq(1, 6) & str_detect(names, "quintile_kind2000adi"))

asiansubgrp$facet_cnt_f <- 
  factor(asiansubgrp$facetcnt,
         levels = c(1, 2, 3, 4, 5, 6, 7),
         labels = c("Chinese",
                    "Filipino",
                    "Japanese",
                    "South~Asian",
                    expression(Korean^1),
                    expression(Vietnamese^2),
                    "Asian~Americans"))

asiansubgrp$names_f <- 
  factor(asiansubgrp$names,
         levels = c("quintile_kind2000adi_state2",
                    "quintile_kind2000adi_state3",
                    "quintile_kind2000adi_state4",
                    "quintile_kind2000adi_state5",
                    "quintile_kind2000adi_county2",
                    "quintile_kind2000adi_county3",
                    "quintile_kind2000adi_county4",
                    "quintile_kind2000adi_county5"),
         labels = c("2", "3", "4", "5",
                    "2", "3", "4", "5"))

asiansubgrp$raceth_f<-
  factor(asiansubgrp$raceth,
         levels = c("Chinese",
                    "Filipino",
                    "Japanese",
                    "South Asian",
                    "Korean",
                    "Vietnamese",
                    "asian"),
         labels = c("Chinese",
                    "Filipino",
                    "Japanese",
                    "South Asian",
                    "Korean",
                    "Vietnamese",
                    "Asian Americans"))


cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00",
               "#525252","#F8766D")

racethlist <- c('Chinese','Filipino','Japanese','South Asian', 'Korean', 
                'Vietnamese','asian', 'white')


asianwhiteest$names_f <- factor(asianwhiteest$names,
                                levels = c("quintile_kind2000adi_state2",
                                           "quintile_kind2000adi_state3",
                                           "quintile_kind2000adi_state4",
                                           "quintile_kind2000adi_state5",
                                           "quintile_kind2000adi_county2",
                                           "quintile_kind2000adi_county3",
                                           "quintile_kind2000adi_county4",
                                           "quintile_kind2000adi_county5"),
                                labels = c("2", "3", "4", "5",
                                           "2", "3", "4", "5"))

asianwhiteest$raceth_f <- factor(
  asianwhiteest$raceth, levels = c("white", "asian"),
  labels = c("Non-Latino White", "Asian Americans"))

asiansubgrp$facet_cnt_f <- 
  factor(asiansubgrp$facetcnt,
         levels = c(1, 2, 3, 4, 5, 6, 7),
         labels = c("Chinese",
                    "Filipino",
                    "Japanese",
                    "South~Asian",
                    "Korean",
                    "Vietnamese",
                    "Asian~Americans"))

asiansubgrp$names_f <- 
  factor(asiansubgrp$names,
         levels = c("quintile_kind2000adi_state2",
                    "quintile_kind2000adi_state3",
                    "quintile_kind2000adi_state4",
                    "quintile_kind2000adi_state5",
                    "quintile_kind2000adi_county2",
                    "quintile_kind2000adi_county3",
                    "quintile_kind2000adi_county4",
                    "quintile_kind2000adi_county5"),
         labels = c("2", "3", "4", "5",
                    "2", "3", "4", "5"))

asiansubgrp$raceth_f<-
  factor(asiansubgrp$raceth,
         levels = c("Chinese",
                    "Filipino",
                    "Japanese",
                    "South Asian",
                    "Korean",
                    "Vietnamese",
                    "asian"),
         labels = c("Chinese",
                    "Filipino",
                    "Japanese",
                    "South Asian",
                    "Korean",
                    "Vietnamese",
                    "Asian Americans"))


#- subset to state and county ADI quintiles 
asianwhiteest$version <- ifelse(str_detect(asianwhiteest$names, "state"),1, 2)

subset <- asianwhiteest
title_index <- ifelse(subset$version==1,"state", "county")

#---- **AA-NLW age as timescale HR plot ----
ggplot(data = subset,
       aes(x = names_f, y = HR, ymin = LL, ymax = UL, 
           col = raceth_f, fill = raceth_f)) + 
  geom_pointrange(aes(col = raceth_f), 
                  position=position_dodge(width = 0.5)) + 
  geom_errorbar(aes(ymin = LL, ymax = UL), 
                position = position_dodge(width = 0.5), width = 0.2, cex = 1) +
  geom_hline(aes(fill = names_f), yintercept = 1, linetype = 2) +
  #scale_y_continuous(limits = c(0.8, 1.2), breaks = scales::pretty_breaks(n = 5)) +
  xlab('Neighborhood Disadvantage Quintile') + 
  ylab('Hazard Ratio') +
  theme_bw() +
  scale_y_log10(breaks = c(0.8, 0.9, 1.0, 1.1, 1.2, 1.3),
                limits = c(.775, 1.325)) +
  theme(legend.title = element_blank(),
        legend.key = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0),
        text = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12)) + 
  scale_color_manual(values = c("#F8766D", "#525252"))
#ggtitle(paste0("AA and NLW ADI quintile M3, relative to ", title_index))

ggsave(filename = paste0(path_to_box, 
                         "/Asian_Americans_dementia/", 
                         "Manuscripts/ADI_ADRD/Manuscript/figures/",
                         "ageastimescale_hr_aa_nlw_state_r1.jpg"), device = "jpg", 
       height = 5, width = 7, units = "in")

#---- **Asian Asian subethnicities age as timescale ----
#---- **Dropping Quintiles 4 and 5 for Vietnamese ethnicity ----
#- 11/29/2021: JF using CS's more efficient code for dropping instead
#--Commenting out old code --
# asiansubgrpviet <- asiansubgrp %>% filter(str_detect(raceth, 'Vietnamese')) %>%
#   filter(!str_detect(names, 'quintile_kind2000adi_state5')) %>% 
#   filter(!str_detect(names, 'quintile_kind2000adi_state4'))

asiansubgrp_state <- asiansubgrp %>% filter(str_detect(asiansubgrp$names, "state"))
# asiansubgrp_state <- rbind(asiansubgrp_state, asiansubgrpviet)

coordlimit<-4.0
asiansubgrp_state <- asiansubgrp_state %>% mutate(
  LL_1 = ifelse(LL < 0.25, .25, NA),
  UL_1 = ifelse(UL > coordlimit, coordlimit, NA)) %>% 
  filter(!(raceth_f == "Vietnamese" & names %in% 
             c("quintile_kind2000adi_state4", "quintile_kind2000adi_state5")))

ggplot(data = asiansubgrp_state,
       aes(x = names_f, y = HR, ymin = LL, ymax = UL, 
           col = raceth_f, fill = raceth_f)) +
  geom_pointrange(aes(col = raceth_f), position = position_dodge(width = 0.5), 
                  cex = 1, fatten = 2) + 
  geom_errorbar(aes(ymin = LL, ymax = UL), 
                position = position_dodge(width = 0.5),
                width = 0.4, cex = 1) +
  geom_hline(aes(fill = names_f), yintercept = 1, linetype = 2) +
  geom_segment(
    aes(x = as.numeric(names_f)-.125, xend = as.numeric(names_f)-.125,
        y = LL, yend = UL_1),
    arrow = arrow(length = unit(.5, "cm")),
    show.legend = FALSE,
    size = 1) +
  geom_segment(
    aes(x = as.numeric(names_f)-.125, xend = as.numeric(names_f)-.125,
        y = UL, yend = LL_1),
    arrow = arrow(length = unit(.5, "cm")),
    show.legend = FALSE,
    size = 1) +
  scale_y_log10(breaks = c(0.25,.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0),
                limits = c(0.25, 4.0))+
  facet_wrap(~ facet_cnt_f, labeller = label_parsed) + theme_bw() +
  xlab('Neighborhood Disadvantage Quintile') + ylab('Hazard Ratio') + 
  
  theme(legend.title = element_blank(),
        legend.key = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0),
        text = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 9),
        legend.text = element_text(size = 12)) + 
  scale_color_manual(values = cbPalette)

ggsave(filename = paste0(path_to_box, "/Asian_Americans_dementia/", 
                         "Manuscripts/ADI_ADRD/Manuscript/figures/",
                         "ageastimescale_hr_asian_subgrp_log_scale_r1.jpeg"), 
       device = "jpeg", 
       height = 8, width = 10, units = "in")



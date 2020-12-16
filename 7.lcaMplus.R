library(MplusAutomation)
library(gridExtra)

ISC_lvRlca <- ISC_lvR %>% 
  dplyr::select(all_of(Id), all_of(sampleID), all_of(Scales), all_of(Scalesb), all_of(Man_cate), all_of(Man_cont)) 

load("LCA_MplusModels.RData")

#------------Results with 5 classes by cycle
#----C1

MAllScalesbycyclec1cl5Mplus <- MAllScalesBycycleMplus$Mplus.lca_c1cl5.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(MAllScalesbycyclec1cl5Mplus, nclass = 5)
MAllScalesbycyclec1cl5Mplus$Class <- factor(MAllScalesbycyclec1cl5Mplus$Class, 
                                            levels = c("2", "3", "4", "5", "1"), 
                                            labels = 
                                         c("Strongly agree \nwith all", "Agree all",
                                           "Strongly agree \nwith Gender", 
                                           "Agree but no \nwith political",  
                                           "Disagree all"))
graphclass(MAllScalesbycyclec1cl5Mplus, nclass = 5, title = "Mplus Results - LCA 1999 with 5 classes")

#----C2
MAllScalesbycyclec2cl5Mplus <- MAllScalesBycycleMplus$Mplus.lca_c2cl5.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(MAllScalesbycyclec2cl5Mplus, nclass = 5)
MAllScalesbycyclec2cl5Mplus$Class <- factor(MAllScalesbycyclec2cl5Mplus$Class, 
                                            levels = c("4", "3", "2", "5", "1"), labels = 
                                         c("Strongly agree \nwith all", "Agree all",
                                           "Strongly agree \nwith Gender", 
                                           "Agree but no \nwith political",  
                                           "Disagree all"))

graphclass(MAllScalesbycyclec2cl5Mplus, nclass = 5, title = "Mplus Results - LCA 2009 with 5 classes")

#----C3
MAllScalesbycyclec3cl5Mplus <- MAllScalesBycycleMplus$Mplus.lca_c3cl5.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(MAllScalesbycyclec3cl5Mplus, nclass = 5)
MAllScalesbycyclec3cl5Mplus$Class <- factor(MAllScalesbycyclec3cl5Mplus$Class, 
                                            levels = c("4", "5", "1", "3", "2"), labels = 
                                         c("Strongly agree \nwith all", "Agree all",
                                           "Strongly agree \nwith Gender", 
                                           "Agree but no \nwith political",  
                                           "Disagree all"))

graphclass(MAllScalesbycyclec3cl5Mplus, nclass = 5, title = "Mplus Results - LCA 2016 with 5 classes")


#Mplus
#--Model fit
Graph_modelfit <- function(Modellist){
  resultsbyallo <- SummaryTable(eval(parse(text=paste0(Modellist))), 
                                keepCols = c("Title", "Observations", "NLatentClasses", "NDependentVars", "Parameters", "LL",
                                             "AIC", "BIC", "aBIC", "Entropy", "AICC"), sortBy = "BIC") #type = "html"
  resultsbyallo$cycle <- str_remove(resultsbyallo$Title, " with [1-9] classes;")
  resultsbyallo$cycle <- str_remove(resultsbyallo$cycle, "LCA ")
  resultsbyall<-tidyr::gather(resultsbyallo, "Fit", "Value", 6:11)
  fit.plot <- ggplot(resultsbyall) + 
    geom_point(aes(x = factor(NLatentClasses), y = Value), size=3) +
    geom_line(aes(factor(NLatentClasses), Value, group = 1)) +
    theme_bw()+
    labs(x = "NClasses", y="", title = "Model fit for all scales") + 
    facet_grid(Fit ~ cycle , scales = "free") 
  print(fit.plot)
}
Graph_modelfit("MAllScalesBycycleMplus")


# compareModels(MAllScalesBycycleMplus$Mplus.lca_c1cl4.out,
#               MAllScalesBycycleMplus$Mplus.lca_c1cl5.out, diffTest = TRUE)
# compareModels(MAllScalesBycycleMplus$Mplus.lca_c2cl4.out,
#               MAllScalesBycycleMplus$Mplus.lca_c2cl5.out, diffTest = TRUE)
# compareModels(MAllScalesBycycleMplus$Mplus.lca_c3cl4.out,
#               MAllScalesBycycleMplus$Mplus.lca_c3cl5.out, diffTest = TRUE)

# MeansBycycle <- rbind(cbind(Cycle = "C1", MAllScalesBycycleMplus$Mplus.lca_c1cl5.out$parameters$unstandardized %>% 
#                               filter(paramHeader == "Means")),
#                       cbind(Cycle = "C2", MAllScalesBycycleMplus$Mplus.lca_c2cl5.out$parameters$unstandardized %>% 
#                               filter(paramHeader == "Means")),
#                       cbind(Cycle = "C3", MAllScalesBycycleMplus$Mplus.lca_c3cl5.out$parameters$unstandardized %>% 
#                               filter(paramHeader == "Means"))) %>% dplyr::select(-paramHeader,-LatentClass) %>% 
#   knitr::kable(caption = "Means")
# MeansBycycle
# c1 c("2", "3", "4", "5", "1")
# c("Strongly agree \nwith all", "Agree all",
#   "Strongly agree \nwith Gender", 
#   "Agree but no \nwith political",  
#   "Disagree all")
# c2 c("4", "3", "2", "5", "1")
# c3 c("4", "5", "1", "3", "2")
modelEstimated_Bycycle <- left_join(left_join(MAllScalesBycycleMplus$Mplus.lca_c1cl5.out$class_counts$modelEstimated %>%
                          rename_with(~ c("count C1", "prop C1")[which(c("count", "proportion") == .x)], .cols = c("count", "proportion")) %>% 
                            mutate(class =  
                                     case_when(
                                       class == 2 ~ "Strongly agree with all", 
                                       class == 3 ~ "Agree all",
                                       class == 4 ~ "Strongly agree with Gender",
                                       class == 5 ~ "Agree but no with political",
                                       class == 1 ~ "Disagree all")),
                          MAllScalesBycycleMplus$Mplus.lca_c2cl5.out$class_counts$modelEstimated %>% 
                            rename_with(~ c("count C2", "prop C2")[which(c("count", "proportion") == .x)], .cols = c("count", "proportion")) %>% 
                            mutate(class =  
                                     case_when(
                                       class == 4 ~ "Strongly agree with all", 
                                       class == 3 ~ "Agree all",
                                       class == 2 ~ "Strongly agree with Gender",
                                       class == 5 ~ "Agree but no with political",
                                       class == 1 ~ "Disagree all")), by = "class"),
                          MAllScalesBycycleMplus$Mplus.lca_c3cl5.out$class_counts$modelEstimated %>% 
                            rename_with(~ c("count C3", "prop C3")[which(c("count", "proportion") == .x)], .cols = c("count", "proportion")) %>% 
                            mutate(class =  
                                     case_when(
                                       class == 4 ~ "Strongly agree with all", 
                                       class == 5 ~ "Agree all",
                                       class == 1 ~ "Strongly agree with Gender",
                                       class == 3 ~ "Agree but no with political",
                                       class == 2 ~ "Disagree all")), by = "class") %>% 
  knitr::kable(caption = "Class counts")
modelEstimated_Bycycle

SummaryTable(MAllScalesAllcycleMplus)
SummaryTable(MByScalesAllcycleMplus)

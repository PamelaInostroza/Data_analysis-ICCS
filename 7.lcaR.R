library(poLCA)
library(gridExtra)

ISC_lvRlca <- ISC_lvR %>% 
  dplyr::select(all_of(Id), all_of(sampleID), all_of(Scales), all_of(Scalesb), all_of(Man_cate), all_of(Man_cont)) 

load("LCA_RModelsv2.RData")

MAllScalesbycycle <- MAllScalesbycyclev2
Mbyscalebycycle <- Mbyscalebycyclev2

#Summary of Model fit and class size all models
r <- summaryLCAR2(Modellist=MAllScalesbycycle, level1 = 1:3, level2 = 1:7, level3 = NA)
r$Fits %>% knitr::kable(caption = paste("Model fit 1999 with all scales"), row.names = FALSE) %>% print()

#------------Results with 4 classes by cycle-----
ncl = 4 #Number of classes selected
cat('\n')
cat('\n')
cat("## LCA for all scales with 4 classes")
cat('\n')
cat('\n')
#----C1
r$Fits[[1]] %>% knitr::kable(caption = paste("Model fit 1999 with all scales"), row.names = FALSE) %>% print()

MAllScalesbycyclec1cl4 <- reshape2::melt(MAllScalesbycycle$c1[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(MAllScalesbycyclec1cl4, nclass = ncl)
MAllScalesbycyclec1cl4$Class <- factor(MAllScalesbycyclec1cl4$Class, 
                                       levels = c("class 2: ", "class 3: ", "class 1: ", "class 4: "), 
                                       labels = 
                                         c("Strongly agree \nwith all", "Agree all",
                                           "Strongly agree \nwith Gender", 
                                           "No tendency"))
graphclass(MAllScalesbycyclec1cl4, nclass = ncl, title = paste0("poLCA Results - LCA 1999 with ",ncl," classes"), 
           orden=c(7,8,10,11,13,14,4,15,12,1,2,3,5,6,9))
r$Size[[1]] %>% filter(NClass == ncl) %>% knitr::kable(caption = paste("Size (s.e) of each latent class 1999 with all scales")) %>% print()
#plot(MAllScalesbycycle$c1[[ncl]])

#----C2
r$Fits[[2]] %>% knitr::kable(caption = paste("Model fit 2009 with all scales"), row.names = FALSE)  %>% print()
MAllScalesbycyclec2cl4 <- reshape2::melt(MAllScalesbycycle$c2[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(MAllScalesbycyclec2cl4, nclass = ncl)
MAllScalesbycyclec2cl4$Class <- factor(MAllScalesbycyclec2cl4$Class, 
                                       levels = c("class 2: ", "class 3: ", "class 4: ", "class 1: "), 
                                       labels = 
                                         c("Strongly agree \nwith all", "Agree all",
                                           "Strongly agree \nwith Gender", 
                                           "No tendency"))

graphclass(MAllScalesbycyclec2cl4, nclass = ncl, title = paste0("poLCA Results - LCA 2009 with ",ncl," classes"), 
           orden=c(7,8,10,11,13,14,4,15,12,1,2,3,5,6,9))
r$Size[[2]] %>% filter(NClass == ncl) %>% knitr::kable(caption = paste("Size (s.e) of each latent class 2009 with all scales"))  %>% print()
#plot(MAllScalesbycycle$c2[[ncl]])

#----C3
r$Fits[[3]] %>% knitr::kable(caption = paste("Model fit 2016 with all scales"), row.names = FALSE) %>% print()
MAllScalesbycyclec3cl4 <- reshape2::melt(MAllScalesbycycle$c3[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(MAllScalesbycyclec3cl4, nclass = ncl)
MAllScalesbycyclec3cl4$Class <- factor(MAllScalesbycyclec3cl4$Class, 
                                       levels = c("class 4: ", "class 1: ", "class 2: ", "class 3: "), labels = 
                                         c("Strongly agree \nwith all", "Agree all",
                                           "Strongly agree \nwith Gender", 
                                           "No tendency"))

graphclass(MAllScalesbycyclec3cl4, nclass = ncl, title = paste0("poLCA Results - LCA 2016 with ",ncl," classes"), 
           orden=c(7,8,10,11,13,14,4,15,12,1,2,3,5,6,9))
r$Size[[3]] %>% filter(NClass == ncl) %>% knitr::kable(caption = paste("Size (s.e) of each latent class 2016 with all scales"))  %>% print()
plot(MAllScalesbycycle$c3[[ncl]])

lc4 <- rbind(cbind(Cycle = "C1", MAllScalesbycyclec1cl4),
             cbind(Cycle = "C2", MAllScalesbycyclec2cl4),
             cbind(Cycle = "C3", MAllScalesbycyclec3cl4))
lc4$Cycle <- factor(lc4$Cycle, labels = c("1999", "2009", "2016"))

pc4 <- lc4 %>% 
  ggplot() + 
  geom_point(aes(x = param, y = value, group = category, color = category, shape = Cycle), size = 2) +
  geom_line(aes(param, value, group = interaction(category,Cycle), color = category)) + 
  theme_bw() +
  labs(y="Response probabilities", color = "Response category")  +
  facet_grid(Class ~ ., switch = "y") +
  theme(legend.position = "top", axis.title.x = element_blank(), axis.text.y = element_text(size = 8), 
        legend.title = element_text(size = 8), axis.text.x = element_text(angle = 90, size = 8)) +
  scale_color_discrete(labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"  )) +
  scale_y_continuous(breaks = c(0.25,0.5,0.75))
print(pc4)


#------------Results with 5 classes by cycle----
ncl = 5
cat('\n')
cat('\n')
cat("## LCA for all scales with 5 classes")
cat('\n')
cat('\n')
#----C1
r$Fits[[1]] %>% knitr::kable(caption = paste("Model fit 1999 with all scales"), row.names = FALSE) %>% print()

MAllScalesbycyclec1cl5 <- reshape2::melt(MAllScalesbycycle$c1[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(MAllScalesbycyclec1cl5, nclass = ncl)
MAllScalesbycyclec1cl5$Class <- factor(MAllScalesbycyclec1cl5$Class, 
                                       levels = c("class 5: ", "class 4: ", "class 2: ", "class 1: ", "class 3: "), 
                                       labels = 
                                         c("Strongly agree \nwith all", "Agree all",
                                           "Strongly agree \nwith Gender", 
                                           "Agree but no \nwith political",  
                                           "No tendency"))
graphclass(MAllScalesbycyclec1cl5, nclass = ncl, title = paste0("poLCA Results - LCA 1999 with ",ncl," classes"), 
           orden=c(7,8,10,11,13,14,4,15,12,1,2,3,5,6,9))
r$Size[[1]] %>% filter(NClass == ncl) %>% knitr::kable(caption = paste("Size (s.e) of each latent class 1999 with all scales"))  %>% print()
#plot(MAllScalesbycycle$c1[[ncl]])

#----C2
r$Fits[[2]] %>% knitr::kable(caption = paste("Model fit 2009 with all scales"), row.names = FALSE) %>% print()
MAllScalesbycyclec2cl5 <- reshape2::melt(MAllScalesbycycle$c2[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(MAllScalesbycyclec2cl5, nclass = ncl)
MAllScalesbycyclec2cl5$Class <- factor(MAllScalesbycyclec2cl5$Class, 
                                       levels = c("class 4: ", "class 2: ", "class 1: ", "class 5: ", "class 3: "), 
                                       labels = 
                                         c("Strongly agree \nwith all", "Agree all",
                                           "Strongly agree \nwith Gender", 
                                           "Agree but no \nwith political",  
                                           "No tendency"))

graphclass(MAllScalesbycyclec2cl5, nclass = ncl, title = paste0("poLCA Results - LCA 2009 with ",ncl," classes"), 
           orden=c(7,8,10,11,13,14,4,15,12,1,2,3,5,6,9))
r$Size[[2]] %>% filter(NClass == ncl) %>% knitr::kable(caption = paste("Size (s.e) of each latent class 2009 with all scales"))  %>% print()
#plot(MAllScalesbycycle$c2[[ncl]])

#----C3
r$Fits[[3]] %>% knitr::kable(caption = paste("Model fit 2016 with all scales"), row.names = FALSE) %>% print()
MAllScalesbycyclec3cl5 <- reshape2::melt(MAllScalesbycycle$c3[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(MAllScalesbycyclec3cl5, nclass = ncl)
MAllScalesbycyclec3cl5$Class <- factor(MAllScalesbycyclec3cl5$Class, 
                                       levels = c("class 2: ", "class 4: ", "class 5: ", "class 3: ", "class 1: "), labels = 
                                         c("Strongly agree \nwith all", "Agree all",
                                           "Strongly agree \nwith Gender", 
                                           "Agree but no \nwith political",  
                                           "No tendency"))

graphclass(MAllScalesbycyclec3cl5, nclass = ncl, title = paste0("poLCA Results - LCA 2016 with ",ncl," classes"), 
           orden=c(7,8,10,11,13,14,4,15,12,1,2,3,5,6,9))
r$Size[[3]] %>% filter(NClass == ncl) %>% knitr::kable(caption = paste("Size (s.e) of each latent class 2016 with all scales"))  %>% print()
#plot(MAllScalesbycycle$c3[[ncl]])

lc5 <- rbind(cbind(Cycle = "C1", MAllScalesbycyclec1cl5),
             cbind(Cycle = "C2", MAllScalesbycyclec2cl5),
             cbind(Cycle = "C3", MAllScalesbycyclec3cl5))
lc5$Cycle <- factor(lc5$Cycle, labels = c("1999", "2009", "2016"))

pc5 <- lc5 %>% 
  ggplot() + 
  geom_point(aes(x = param, y = value, group = category, color = category, shape = Cycle), size = 2) +
  geom_line(aes(param, value, group = interaction(category,Cycle), color = category)) + 
  theme_bw() +
  labs(y="Response probabilities", color = "Response category")  +
  facet_grid(Class ~ ., switch = "y") +
  theme(legend.position = "top", axis.title.x = element_blank(), axis.text.y = element_text(size = 8), 
        legend.title = element_text(size = 8), axis.text.x = element_text(angle = 90, size = 8)) +
  scale_color_discrete(labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"  )) +
  scale_y_continuous(breaks = c(0.25,0.5,0.75))
print(pc5)




# ###!!!! order based on the classes selected
# #inspect the estimated class memberships:
# PredClass_Bycycle <- data.frame(t(rbind(rbind(sort(round(prop.table(table(MAllScalesbycycle$c1[[ncl]]$predclass)),4)*100)[c(3,4,1,5,2)],
#                                               sort(round(prop.table(table(MAllScalesbycycle$c2[[ncl]]$predclass)),4)*100)[c(1,5,2,3,4)],
#                                               sort(round(prop.table(table(MAllScalesbycycle$c3[[ncl]]$predclass)),4)*100)[c(1,3,4,2,5)]))))
# PredClass_Bycycle$Class <- c("Strongly agree with all", "Agree all",
#                              "Strongly agree with Gender", 
#                              "Agree but no with political",  
#                              "Disagree all")
# colnames(PredClass_Bycycle) <- c("C1", "C2", "C3", "Class")
# PredClass_Bycycle %>% 
#   knitr::kable(caption = "Predicted class membership") %>% print()
# 

#------------------------------
#------------------------------
#---------Binomial response----
#------------------------------
# 
# summaryLCAR2(Modellist=MBAllScalesbycycle, level1 = 1:3, level2 = 4:7, level3 = NA)
# ncl = 4 #Number of classes selected
# 
# #----C1
# MBAllScalesbycyclec1cl4 <- reshape2::melt(MBAllScalesbycycle$c1[[ncl]]$probs, level=2) %>% 
#   rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
#   mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
# 
# #graphclass(MBAllScalesbycyclec1cl4, nclass = 4)
# MBAllScalesbycyclec1cl4$Class <- factor(MBAllScalesbycyclec1cl4$Class, 
#                                        levels = c("class 1: ", "class 2: ", "class 3: ", "class 4: "), 
#                                        labels = 
#                                          c("Disagree all",
#                                            "Agree \nwith Gender", 
#                                            "Agree but no \nwith political",  
#                                            "Agree all"))
# graphclass(MBAllScalesbycyclec1cl4, nclass = 5, title = "poLCA Binomial Results - LCA 1999 with 4 classes")
# 
# #----C2
# MBAllScalesbycyclec2cl4 <- reshape2::melt(MBAllScalesbycycle$c2[[ncl]]$probs, level=2) %>% 
#   rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
#   mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
# 
# #graphclass(MBAllScalesbycyclec2cl4, nclass = 4)
# MBAllScalesbycyclec2cl4$Class <- factor(MBAllScalesbycyclec2cl4$Class, 
#                                         levels = c("class 3: ", "class 4: ", "class 2: ", "class 1: "), 
#                                         labels = 
#                                           c("Disagree all",
#                                             "Agree \nwith Gender", 
#                                             "Agree but no \nwith political",  
#                                             "Agree all"))
# graphclass(MBAllScalesbycyclec2cl4, nclass = 4, title = "poLCA Binomial Results - LCA 2009 with 4 classes")
# 
# #----C3
# MBAllScalesbycyclec3cl4 <- reshape2::melt(MBAllScalesbycycle$c3[[ncl]]$probs, level=2) %>% 
#   rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
#   mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
# 
# #graphclass(MBAllScalesbycyclec3cl4, nclass = 4)
# MBAllScalesbycyclec3cl4$Class <- factor(MBAllScalesbycyclec3cl4$Class, 
#                                         levels = c("class 4: ", "class 3: ", "class 2: ", "class 1: "), 
#                                         labels = 
#                                           c("Disagree all",
#                                             "Agree \nwith Gender", 
#                                             "Agree but no \nwith political",  
#                                             "Agree all"))
# graphclass(MBAllScalesbycyclec3cl4, nclass = 4, title = "poLCA Binomial Results - LCA 2016 with 4 classes")
# 
# 
# plot(MBAllScalesbycyclec3cl4$c1[[5]])
# plot(MBAllScalesbycyclec3cl4$c2[[5]])
# plot(MBAllScalesbycyclec3cl4$c3[[5]])
# 
# ###!!!! order based on the classes selected
# #inspect the estimated class memberships:
# PredClass_Bycycle <- data.frame(t(rbind(rbind(sort(round(prop.table(table(MBAllScalesbycycle$c1[[ncl]]$predclass)),4)*100)[c(1,2,3,4)],
#                                               sort(round(prop.table(table(MAllScalesbycycle$c2[[ncl]]$predclass)),4)*100)[c(3,4,2,1)],
#                                               sort(round(prop.table(table(MAllScalesbycycle$c3[[ncl]]$predclass)),4)*100)[c(4,3,2,1)]))))
# PredClass_Bycycle$Class <-  c("Disagree all",
#                               "Agree with Gender", 
#                               "Agree but no with political",  
#                               "Agree all")
# colnames(PredClass_Bycycle) <- c("C1", "C2", "C3", "Class")
# PredClass_Bycycle %>% 
#   knitr::kable(caption = "Predicted class membership")
# 
# Blc4 <- rbind(cbind(Cycle = "C1", MBAllScalesbycyclec1cl4),
#              cbind(Cycle = "C2", MBAllScalesbycyclec2cl4),
#              cbind(Cycle = "C3", MBAllScalesbycyclec3cl4))
# Blc4$Cycle <- c("1999", "2009", "2016")
# 
# Bpc4 <- Blc4 %>% 
#   ggplot() + 
#   geom_point(aes(x = param, y = value, group = category, color = category, shape = Cycle), size = 2) +
#   geom_line(aes(param, value, group = interaction(category,Cycle), color = category)) + 
#   theme_bw() +
#   labs(y="Response probabilities", color = "Response category")  +
#   facet_grid(Class ~ ., switch = "y") +
#   theme(legend.position = "top", axis.title.x = element_blank(), axis.text.y = element_text(size = 8), 
#         legend.title = element_text(size = 8), axis.text.x = element_text(angle = 90, size = 8)) +
#   scale_color_discrete(labels = c("Agree", "Disagree")) +
#   scale_y_continuous(breaks = c(0.25,0.5,0.75))
# print(Bpc4)

#---- By each scale-----
r2 <- summaryLCAR2(Modellist=Mbyscalebycycle, level1 = 1:3, level2 = 1:3, level3 = 1:3)
r2$Fits %>% knitr::kable(caption = paste("Model fit 1999 with all scales"), row.names = FALSE) %>% print()

#------------Results with 2 classes by cycle----
ncl = 2 #Number of classes selected
cat('\n')
cat('\n')
cat("## LCA Attitudes towards immigrants equality scales with 2 classes")
cat('\n')
cat('\n')
#----C1 IMMI
Mbyscalebycyclec1IMMIcl2 <- reshape2::melt(Mbyscalebycycle$C1$IMMI[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(Mbyscalebycyclec1IMMIcl2, nclass = ncl)
Mbyscalebycyclec1IMMIcl2$Class <- factor(Mbyscalebycyclec1IMMIcl2$Class, 
                                       levels = c("class 1: ", "class 2: "), 
                                       labels = c("Strongly agree", "Agree"))
graphclass(Mbyscalebycyclec1IMMIcl2, nclass = ncl, title = "poLCA Results - LCA IMMIGRATION scale 1999 with 2 classes")
r2$Size$C1[[1]] %>% filter(NClass == ncl) %>% knitr::kable(caption = paste("Size (s.e) of each latent class 1999 with IMMIGRANT scales"))  %>% print()

#----C2 IMMI
Mbyscalebycyclec2IMMIcl2 <- reshape2::melt(Mbyscalebycycle$C1$IMMI[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(Mbyscalebycyclec2IMMIcl2, nclass = ncl)
Mbyscalebycyclec2IMMIcl2$Class <- factor(Mbyscalebycyclec2IMMIcl2$Class, 
                                       levels = c("class 1: ", "class 2: "), 
                                       labels = c("Strongly agree", "Agree"))
graphclass(Mbyscalebycyclec2IMMIcl2, nclass = ncl, title = "poLCA Results - LCA IMMIGRATION scale 2009 with 2 classes")
r2$Size$C2[[1]] %>% filter(NClass == ncl) %>% knitr::kable(caption = paste("Size (s.e) of each latent class 2009 with IMMIGRANT scales"))  %>% print()

#----C3 IMMI
Mbyscalebycyclec3IMMIcl2 <- reshape2::melt(Mbyscalebycycle$C3$IMMI[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(Mbyscalebycyclec3IMMIcl2, nclass = ncl)
Mbyscalebycyclec3IMMIcl2$Class <- factor(Mbyscalebycyclec3IMMIcl2$Class, 
                                       levels = c("class 2: ", "class 1: "), 
                                       labels = c("Strongly agree", "Agree"))
graphclass(Mbyscalebycyclec3IMMIcl2, nclass = ncl, title = "poLCA Results - LCA IMMIGRATION scale 2016 with 2 classes")
r2$Size$C3[[1]] %>% filter(NClass == ncl) %>% knitr::kable(caption = paste("Size (s.e) of each latent class 2016 with IMMIGRANT scales"))  %>% print()

IMMIlc2 <- rbind(cbind(Cycle = "C1", Mbyscalebycyclec1IMMIcl2),
                 cbind(Cycle = "C2", Mbyscalebycyclec2IMMIcl2),
                 cbind(Cycle = "C3", Mbyscalebycyclec3IMMIcl2))
IMMIpc2 <- IMMIlc2 %>% 
  ggplot() + 
  geom_point(aes(x = param, y = value, group = category, color = category, shape = Cycle), size = 2) +
  geom_line(aes(param, value, group = interaction(category,Cycle), color = category)) + 
  theme_bw() +
  labs(y="Response probabilities", color = "Response category")  +
  scale_shape_manual(values = c(4,5,6), labels = c("1999", "2009", "2016")) +
  facet_grid(Class ~ ., switch = "y") +
  theme(legend.position = "top", axis.title.x = element_blank(), axis.text.y = element_text(size = 8), 
        legend.title = element_text(size = 8), axis.text.x = element_text(angle = 90, size = 8)) +
  scale_color_discrete(labels = c("Strongly agree", "Agree", "Disagree", "Strongly disagree")) +
  scale_y_continuous(breaks = c(0.25,0.5,0.75))
print(IMMIpc2)

#----C1 GNDR
cat('\n')
cat('\n')
cat("## LCA Attitudes towards gender equality scales with 2 classes")
cat('\n')
cat('\n')
Mbyscalebycyclec1GNDRcl2 <- reshape2::melt(Mbyscalebycycle$C1$GNDR[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(Mbyscalebycyclec1GNDRcl2, nclass = ncl)
Mbyscalebycyclec1GNDRcl2$Class <- factor(Mbyscalebycyclec1GNDRcl2$Class, 
                                         levels = c("class 2: ", "class 1: "), 
                                         labels = c("Strongly agree", "Agree"))
graphclass(Mbyscalebycyclec1GNDRcl2, nclass = ncl, title = "poLCA Results - LCA GENDER scale 1999 with 2 classes")
r2$Size$C1[[2]] %>% filter(NClass == ncl) %>% knitr::kable(caption = paste("Size (s.e) of each latent class 1999 with GENDER scales"))  %>% print()

#----C2 GNDR
Mbyscalebycyclec2GNDRcl2 <- reshape2::melt(Mbyscalebycycle$C1$GNDR[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(Mbyscalebycyclec2GNDRcl2, nclass = ncl)
Mbyscalebycyclec2GNDRcl2$Class <- factor(Mbyscalebycyclec2GNDRcl2$Class, 
                                         levels = c("class 1: ", "class 2: "), 
                                         labels = c("Strongly agree", "Agree"))

graphclass(Mbyscalebycyclec2GNDRcl2, nclass = ncl, title = "poLCA Results - LCA GENDER scale 2009 with 2 classes")
r2$Size$C2[[2]] %>% filter(NClass == ncl) %>% knitr::kable(caption = paste("Size (s.e) of each latent class 2009 with GENDER scales"))  %>% print()

#----C3 GNDR
Mbyscalebycyclec3GNDRcl2 <- reshape2::melt(Mbyscalebycycle$C3$GNDR[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(Mbyscalebycyclec3GNDRcl2, nclass = ncl)
Mbyscalebycyclec3GNDRcl2$Class <- factor(Mbyscalebycyclec3GNDRcl2$Class, 
                                         levels = c("class 2: ", "class 1: "), labels = 
                                           c("Strongly agree", "Agree"))
graphclass(Mbyscalebycyclec3GNDRcl2, nclass = ncl, title = "poLCA Results - LCA GENDER scale 2016 with 2 classes")
r2$Size$C3[[2]] %>% filter(NClass == ncl) %>% knitr::kable(caption = paste("Size (s.e) of each latent class 2016 with GENDER scales"))  %>% print()


GNDRlc2 <- rbind(cbind(Cycle = "C1", Mbyscalebycyclec1GNDRcl2),
                 cbind(Cycle = "C2", Mbyscalebycyclec2GNDRcl2),
                 cbind(Cycle = "C3", Mbyscalebycyclec3GNDRcl2))
GNDRpc2 <- GNDRlc2 %>% 
  ggplot() + 
  geom_point(aes(x = param, y = value, group = category, color = category, shape = Cycle), size = 2) +
  geom_line(aes(param, value, group = interaction(category,Cycle), color = category)) + 
  theme_bw() +
  labs(y="Response probabilities", color = "Response category")  +
  scale_shape_manual(values = c(4,5,6), labels = c("1999", "2009", "2016")) +
  facet_grid(Class ~ ., switch = "y") +
  theme(legend.position = "top", axis.title.x = element_blank(), axis.text.y = element_text(size = 8), 
        legend.title = element_text(size = 8), axis.text.x = element_text(angle = 90, size = 8)) +
  scale_color_discrete(labels = c("Strongly agree", "Agree", "Disagree", "Strongly disagree")) +
  scale_y_continuous(breaks = c(0.25,0.5,0.75))
print(GNDRpc2)

#----C1 ETHN
cat('\n')
cat('\n')
cat("## LCA Attitudes towards ethnic/race equality scales with 2 classes")
cat('\n')
cat('\n')
Mbyscalebycyclec1ETHNcl2 <- reshape2::melt(Mbyscalebycycle$C1$ETHN[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(Mbyscalebycyclec1ETHNcl2, nclass = ncl)
Mbyscalebycyclec1ETHNcl2$Class <- factor(Mbyscalebycyclec1ETHNcl2$Class, 
                                         levels = c("class 2: ", "class 1: "), 
                                         labels = c("Strongly agree", "Agree"))
graphclass(Mbyscalebycyclec1ETHNcl2, nclass = ncl, title = "poLCA Results - LCA ETHNIC scale 1999 with 2 classes")
r2$Size$C1[[3]] %>% filter(NClass == ncl) %>% knitr::kable(caption = paste("Size (s.e) of each latent class 1999 with ETHNIC scales"))  %>% print()

#----C2 ETHN
Mbyscalebycyclec2ETHNcl2 <- reshape2::melt(Mbyscalebycycle$C2$ETHN[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(Mbyscalebycyclec2ETHNcl2, nclass = ncl)
Mbyscalebycyclec2ETHNcl2$Class <- factor(Mbyscalebycyclec2ETHNcl2$Class, 
                                         levels = c("class 1: ", "class 2: "), 
                                         labels = c("Strongly agree", "Agree"))

graphclass(Mbyscalebycyclec2ETHNcl2, nclass = ncl, title = "poLCA Results - LCA ETHNIC scale 2009 with 2 classes")
r2$Size$C2[[3]] %>% filter(NClass == ncl) %>% knitr::kable(caption = paste("Size (s.e) of each latent class 2009 with ETHNIC scales")) %>% print() 

#----C3 ETHN
Mbyscalebycyclec3ETHNcl2 <- reshape2::melt(Mbyscalebycycle$C3$ETHN[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(Mbyscalebycyclec3ETHNcl2, nclass = ncl)
Mbyscalebycyclec3ETHNcl2$Class <- factor(Mbyscalebycyclec3ETHNcl2$Class, 
                                         levels = c("class 1: ", "class 2: "), 
                                         labels = c("Strongly agree", "Agree"))

graphclass(Mbyscalebycyclec3ETHNcl2, nclass = ncl, title = "poLCA Results - LCA ETHNIC scale 2016 with 2 classes")
r2$Size$C3[[3]] %>% filter(NClass == ncl) %>% knitr::kable(caption = paste("Size (s.e) of each latent class 2016 with ETHNIC scales"))  %>% print()



ETHNlc2 <- rbind(cbind(Cycle = "C1", Mbyscalebycyclec1ETHNcl2),
                 cbind(Cycle = "C2", Mbyscalebycyclec2ETHNcl2),
                 cbind(Cycle = "C3", Mbyscalebycyclec3ETHNcl2))
ETHNpc2 <- ETHNlc2 %>% 
  ggplot() + 
  geom_point(aes(x = param, y = value, group = category, color = category, shape = Cycle), size = 2) +
  geom_line(aes(param, value, group = interaction(category,Cycle), color = category)) + 
  theme_bw() +
  labs(y="Response probabilities", color = "Response category")  +
  scale_shape_manual(values = c(4,5,6), labels = c("1999", "2009", "2016")) +
  facet_grid(Class ~ ., switch = "y") +
  theme(legend.position = "top", axis.title.x = element_blank(), axis.text.y = element_text(size = 8), 
        legend.title = element_text(size = 8), axis.text.x = element_text(angle = 90, size = 8)) +
  scale_color_discrete(labels = c("Strongly agree", "Agree", "Disagree", "Strongly disagree")) +
  scale_y_continuous(breaks = c(0.25,0.5,0.75))
print(ETHNpc2)


###!!!! order based on the classes selected
#inspect the estimated class memberships:
# PredClass_Bycycle <- data.frame(t(rbind(rbind(sort(round(prop.table(table(Mbyscalebycycle$C1$IMMI[[ncl]]$predclass)),4)*100)[c(1,2)],
#                                               sort(round(prop.table(table(Mbyscalebycycle$C2$IMMI[[ncl]]$predclass)),4)*100)[c(1,2)],
#                                               sort(round(prop.table(table(Mbyscalebycycle$C3$IMMI[[ncl]]$predclass)),4)*100)[c(1,2)]))))
# PredClass_Bycycle$Class <-    c("Agree", 
#                                 "Strongly agree")
# colnames(PredClass_Bycycle) <- c("C1", "C2", "C3", "Class")
# PredClass_Bycycle %>% 
#   knitr::kable(caption = "Predicted class membership IMMI")
# 
# 
# PredClass_Bycycle <- data.frame(t(rbind(rbind(sort(round(prop.table(table(Mbyscalebycycle$C1$GNDR[[ncl]]$predclass)),4)*100)[c(1,2)],
#                                               sort(round(prop.table(table(Mbyscalebycycle$C2$GNDR[[ncl]]$predclass)),4)*100)[c(1,2)],
#                                               sort(round(prop.table(table(Mbyscalebycycle$C3$GNDR[[ncl]]$predclass)),4)*100)[c(1,2)]))))
# PredClass_Bycycle$Class <-    c("Strongly agree", "Agree")
# colnames(PredClass_Bycycle) <- c("C1", "C2", "C3", "Class")
# PredClass_Bycycle %>% 
#   knitr::kable(caption = "Predicted class membership GNDR")
# 
# PredClass_Bycycle <- data.frame(t(rbind(rbind(sort(round(prop.table(table(Mbyscalebycycle$C1$ETHN[[ncl]]$predclass)),4)*100)[c(1,2)],
#                                               sort(round(prop.table(table(Mbyscalebycycle$C2$ETHN[[ncl]]$predclass)),4)*100)[c(1,2)],
#                                               sort(round(prop.table(table(Mbyscalebycycle$C3$ETHN[[ncl]]$predclass)),4)*100)[c(1,2)]))))
# PredClass_Bycycle$Class <-    c("Agree", 
#                                 "Strongly agree")
# colnames(PredClass_Bycycle) <- c("C1", "C2", "C3", "Class")
# PredClass_Bycycle %>% 
#   knitr::kable(caption = "Predicted class membership ETHN")



#summaryLCAR2(Modellist=MAllScalesCycles, level1 = 4:10, level2 = NA, level3 = NA)
#summaryLCAR2(MAllcyclebyscale, level1 = 4:6, level2 = 1:5, level3 = NA)


#examine frequencies of profile membership by country and cohort.

#summaryLCAR(MbyscaleCov1, ncy="By", nsca = "By")


# "Mplus/lca_*_C*.inp"

# lcacfa$C..Users.pamel.OneDrive...KU.Leuven.Master.in.Statistics.Master.Thesis.Data_analysis.Mplus.lca_2_c1.out$parameters$probability.scale



# 
# 
# LCA_best_model <- MbyscaleCov1[[3]][[1]][[5]]
# pidmat <- cbind(1,c(1:7))
# exb <- exp(pidmat %*% LCA_best_model$coeff)
# matplot(c(1:7),(cbind(1,exb)/(1+rowSums(exb))),ylim=c(0,1),type="l",
#         main="Country ID as a predictor of attitude class",
#         xlab="Country ID: ",
#         ylab="Probability of latent class membership",lwd=2,col=1)
# text(5.9,0.35,"Other")
# text(5.4,0.7,"Bush affinity")
# text(1.8,0.6,"Gore affinity")

# if(lc$bic < min_bic){
#   min_bic <- lc$bic
#   LCA_best_model<-lc
# }
#cbind(m[[2]]$Chisq)
#m[[2]]$predcell
#poLCA.table(formula=T_GNDREQ1~1,condition=list(T_GNDREQ3=3,T_GNDREQ4=3),lc=LCA_best_model)

#ISC_lvR$Clasif <- LCA_best_model$predclass

# #with covariates
# f2<- as.formula(paste0("cbind(",paste(Scales[1:6], collapse = ","),") ~ T_AGE")) 
# pidmat <- cbind(1,c(1:7))
# exb <- exp(pidmat %*% LCA_best_model$coeff)
# matplot(c(1:7),(cbind(1,exb)/(1+rowSums(exb))),ylim=c(0,1),type="l",
#         main="Party ID as a predictor of candidate affinity class",
#         xlab="Party ID: strong Democratic (1) to strong Republican (7)",
#         ylab="Probability of latent class membership",lwd=2,col=1)
# text(5.9,0.35,"Other")
# text(5.4,0.7,"Bush affinity")
# text(1.8,0.6,"Gore affinity")


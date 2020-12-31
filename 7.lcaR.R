library(poLCA)
library(gridExtra)

ISC_lvRlca <- ISC_lvR %>% 
  dplyr::select(all_of(Id), all_of(sampleID), all_of(Scales), all_of(Scalesb), all_of(Man_cate), all_of(Man_cont)) 

# European countries
load("LCA_RModelsEU2.RData")

#----------------------
#Non-European countries
load("LCA_RModelsNEU2.RData")
classes2 <- c("Engaged", "Conventional")
classes3 <- c("Engaged", "Engaged but not with political or customs equality", "Engaged but not with gender political difference")
classes4 <- c("Engaged", "Conventional", "Engaged with Gender equality", "Against immigrant and ethnics political equality")
classes4NEU <- c("Engaged", "Conventional", "Engaged with Gender equality", "Engaged but not with political equality")
classes5 <- c("Engaged", "Conventional", "Engaged with Gender equality", "Engaged but not with political equality", "Against immigrant and ethnics political equality")
classes5NEU3 <- c("Engaged", "Conventional", "Engaged with Gender equality", "Engaged but not with political equality", "Engaged but not with gender political difference")
classes5cnty <- c("Engaged", "Conventional", "Engaged with Gender equality", "Engaged but not with political equality", "Against immigrant and ethnics political equality")


cat('\n')
cat('\n')
cat("# 1. By construct and cycle   \n")
cat('\n')
cat('\n')
cat('## 1.1 European countries {.tabset .tabset-pills} \n')
cat('\n')
cat('\n')  
ncl = 2 #Number of classes selected

#---- By each scale-----
r2 <- summaryLCAR2(Modellist=MbyscalebycycleEU, level1 = 1:3, level2 = 1:3, level3 = 1:3)
cat('\n')
cat('\n')
cat("### 1.1.1 EU Attitudes towards ethnics/racial equality items, 2 latent classes  \n")
cat('\n')
cat('\n')
r2$Fits$C1$ETHN %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit EU 1999 with ETHNIC scales", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% 
  row_spec(2, bold = TRUE, italic = TRUE) %>% print()
r2$Fits$C2$ETHN %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit EU 2009 with ETHNIC scales", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% 
  row_spec(2, bold = TRUE, italic = TRUE) %>% print()
r2$Fits$C3$ETHN %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit EU 2016 with ETHNIC scales", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% 
  row_spec(2, bold = TRUE, italic = TRUE) %>% print()

# ---C1 ETHN----
MbyscalebycycleEUc1ETHNcl2 <- reshape2::melt(MbyscalebycycleEU$C1$ETHN[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(MbyscalebycycleEUc1ETHNcl2, nclass = ncl)
MbyscalebycycleEUc1ETHNcl2$Class <- factor(MbyscalebycycleEUc1ETHNcl2$Class, 
                                         levels = c("class 2: ", "class 1: "), 
                                         labels = classes2)
#----C2 ETHN
MbyscalebycycleEUc2ETHNcl2 <- reshape2::melt(MbyscalebycycleEU$C2$ETHN[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(MbyscalebycycleEUc2ETHNcl2, nclass = ncl)
MbyscalebycycleEUc2ETHNcl2$Class <- factor(MbyscalebycycleEUc2ETHNcl2$Class, 
                                         levels = c("class 2: ", "class 1: "), 
                                         labels = classes2)
#----C3 ETHN
MbyscalebycycleEUc3ETHNcl2 <- reshape2::melt(MbyscalebycycleEU$C3$ETHN[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(MbyscalebycycleEUc3ETHNcl2, nclass = ncl)
MbyscalebycycleEUc3ETHNcl2$Class <- factor(MbyscalebycycleEUc3ETHNcl2$Class, 
                                         levels = c("class 2: ", "class 1: "), 
                                         labels = classes2)

MbyscalebycycleEUETHNcl2<- rbind(cbind(Cycle="1999", MbyscalebycycleEUc1ETHNcl2),
      cbind(Cycle="2009", MbyscalebycycleEUc2ETHNcl2),
      cbind(Cycle="2016", MbyscalebycycleEUc3ETHNcl2))
graphclasstogether(MbyscalebycycleEUETHNcl2, nclass = ncl, title = "LCA ETHNIC scale EU with 2 classes")

r2$Size$C1[[1]] %>% filter(NClass == ncl) %>% mutate(Year = "1999") %>% 
  rename_with(~ classes2[which(c("P.2", "P.1") == .x)], .cols = c("P.2", "P.1")) %>% 
  dplyr::select("Year", all_of(classes2)) %>% 
  bind_rows(r2$Size$C2[[1]] %>% filter(NClass == ncl) %>% mutate(Year = "2009") %>% 
              rename_with(~ classes2[which(c("P.2", "P.1") == .x)], .cols = c("P.2", "P.1")) %>% 
              dplyr::select("Year", all_of(classes2))) %>% 
  bind_rows(r2$Size$C3[[1]] %>% filter(NClass == ncl) %>% mutate(Year = "2016") %>% 
              rename_with(~ classes2[which(c("P.2", "P.1") == .x)], .cols = c("P.2", "P.1")) %>% 
              dplyr::select("Year", all_of(classes2))) %>% 
  kbl(caption = "Size of each latent class EU with ETHNIC scales", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11, full_width = FALSE) %>% 
  column_spec (c(2), border_left = T) %>%
  column_spec(2:3, width = "10em") %>% print()

ETHNlc2 <- rbind(cbind(Cycle = "C1", MbyscalebycycleEUc1ETHNcl2),
                 cbind(Cycle = "C2", MbyscalebycycleEUc2ETHNcl2),
                 cbind(Cycle = "C3", MbyscalebycycleEUc3ETHNcl2))
ETHNpc2 <- ETHNlc2 %>% 
  ggplot() + 
  geom_point(aes(x = param, y = value, group = category, color = category, shape = Cycle), size = 2) +
  geom_line(aes(param, value, group = interaction(category,Cycle), color = category)) + 
  theme_bw() +
  ggtitle("Highest probabilities for each latent class \nEU Ethnic scale") +
  labs(y="Response probabilities", color = "Response category")  +
  scale_shape_manual(values = c(4,5,6), labels = c("1999", "2009", "2016")) +
  facet_grid(Class ~ ., switch = "y") +
  theme(legend.position = "top", legend.box="vertical",  
        strip.text.y = element_text(size = 8), 
        axis.title.x = element_blank(), axis.text.y = element_text(size = 8), 
        legend.title = element_text(size = 8), axis.text.x = element_text(angle = 90, size = 8)) +
  scale_color_discrete(labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree")) +
  scale_y_continuous(breaks = c(0.25,0.5,0.75))
print(ETHNpc2)

#-----

cat('\n')
cat('\n')
cat("### 1.1.2 EU Attitudes towards gender equality items, 2 latent classes  \n")
cat('\n')
cat('\n')
r2$Fits$C1$GNDR %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit EU 1999 with GENDER scales", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% 
  row_spec(2, bold = TRUE, italic = TRUE) %>% print()
r2$Fits$C2$GNDR %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit EU 2009 with GENDER scales", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% 
  row_spec(2, bold = TRUE, italic = TRUE) %>% print()
r2$Fits$C3$GNDR %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit EU 2016 with GENDER scales", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% 
  row_spec(2, bold = TRUE, italic = TRUE) %>% print()

#----C1 GNDR----
MbyscalebycycleEUc1GNDRcl2 <- reshape2::melt(MbyscalebycycleEU$C1$GNDR[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(MbyscalebycycleEUc1GNDRcl2, nclass = ncl)
MbyscalebycycleEUc1GNDRcl2$Class <- factor(MbyscalebycycleEUc1GNDRcl2$Class, 
                                         levels = c("class 1: ", "class 2: "), 
                                         labels = classes2)

#----C2 GNDR
MbyscalebycycleEUc2GNDRcl2 <- reshape2::melt(MbyscalebycycleEU$C2$GNDR[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(MbyscalebycycleEUc2GNDRcl2, nclass = ncl)
MbyscalebycycleEUc2GNDRcl2$Class <- factor(MbyscalebycycleEUc2GNDRcl2$Class, 
                                         levels = c("class 2: ", "class 1: "), 
                                         labels = classes2)


#----C3 GNDR
MbyscalebycycleEUc3GNDRcl2 <- reshape2::melt(MbyscalebycycleEU$C3$GNDR[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(MbyscalebycycleEUc3GNDRcl2, nclass = ncl)
MbyscalebycycleEUc3GNDRcl2$Class <- factor(MbyscalebycycleEUc3GNDRcl2$Class, 
                                         levels = c("class 2: ", "class 1: "), labels = 
                                           classes2)

MbyscalebycycleEUGNDRcl2<- rbind(cbind(Cycle="1999", MbyscalebycycleEUc1GNDRcl2),
                                 cbind(Cycle="2009", MbyscalebycycleEUc2GNDRcl2),
                                 cbind(Cycle="2016", MbyscalebycycleEUc3GNDRcl2))
graphclasstogether(MbyscalebycycleEUGNDRcl2, nclass = ncl, title = "LCA GENDER scale EU with 2 classes")


r2$Size$C1[[1]] %>% filter(NClass == ncl) %>% mutate(Year = "1999") %>% 
  rename_with(~ classes2[which(c("P.1", "P.2") == .x)], .cols = c("P.1", "P.2")) %>% 
  dplyr::select("Year", all_of(classes2)) %>% 
  bind_rows(r2$Size$C2[[1]] %>% filter(NClass == ncl) %>% mutate(Year = "2009") %>% 
              rename_with(~ classes2[which(c("P.2", "P.1") == .x)], .cols = c("P.2", "P.1")) %>% 
              dplyr::select("Year", all_of(classes2))) %>% 
  bind_rows(r2$Size$C3[[1]] %>% filter(NClass == ncl) %>% mutate(Year = "2016") %>% 
              rename_with(~ classes2[which(c("P.2", "P.1") == .x)], .cols = c("P.2", "P.1")) %>% 
              dplyr::select("Year", all_of(classes2))) %>% 
  kbl(caption = "Size of each latent class EU with GENDER scales", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11, full_width = FALSE) %>% 
  column_spec (c(2), border_left = T) %>%
  column_spec(2:3, width = "10em") %>% print()

GNDRlc2 <- rbind(cbind(Cycle = "C1", MbyscalebycycleEUc1GNDRcl2),
                 cbind(Cycle = "C2", MbyscalebycycleEUc2GNDRcl2),
                 cbind(Cycle = "C3", MbyscalebycycleEUc3GNDRcl2))
GNDRpc2 <- GNDRlc2 %>% 
  ggplot() + 
  geom_point(aes(x = param, y = value, group = category, color = category, shape = Cycle), size = 2) +
  geom_line(aes(param, value, group = interaction(category,Cycle), color = category)) + 
  theme_bw() +
  ggtitle("Highest probabilities for each latent class \nEU Gender scale") +
  labs(y="Response probabilities", color = "Response category")  +
  scale_shape_manual(values = c(4,5,6), labels = c("1999", "2009", "2016")) +
  facet_grid(Class ~ ., switch = "y") +
  theme(legend.position = "top", legend.box="vertical", 
        strip.text.y = element_text(size = 8), 
        axis.title.x = element_blank(), axis.text.y = element_text(size = 8), 
        legend.title = element_text(size = 8), axis.text.x = element_text(angle = 90, size = 8)) +
  scale_color_discrete(labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree")) +
  scale_y_continuous(breaks = c(0.25,0.5,0.75))
print(GNDRpc2)

#-----
cat('\n')
cat('\n')
cat("### 1.1.3 EU Attitudes towards immigrants equality items - 2 latent classes  \n")
cat('\n')
cat('\n')
r2$Fits$C1$IMMI %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit EU 1999 with IMMIGRATION scales", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% 
  row_spec(2, bold = TRUE, italic = TRUE) %>% print()
r2$Fits$C2$IMMI %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit EU 2009 with IMMIGRATION scales", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% 
  row_spec(2, bold = TRUE, italic = TRUE) %>% print()
r2$Fits$C3$IMMI %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit EU 2016 with IMMIGRATION scales", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% 
  row_spec(2, bold = TRUE, italic = TRUE) %>% print()

#----C1 IMMI----
MbyscalebycycleEUc1IMMIcl2 <- reshape2::melt(MbyscalebycycleEU$C1$IMMI[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(MbyscalebycycleEUc1IMMIcl2, nclass = ncl)
MbyscalebycycleEUc1IMMIcl2$Class <- factor(MbyscalebycycleEUc1IMMIcl2$Class, 
                                         levels = c("class 2: ", "class 1: "), 
                                         labels = classes2)

#----C2 IMMI
MbyscalebycycleEUc2IMMIcl2 <- reshape2::melt(MbyscalebycycleEU$C2$IMMI[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(MbyscalebycycleEUc2IMMIcl2, nclass = ncl)
MbyscalebycycleEUc2IMMIcl2$Class <- factor(MbyscalebycycleEUc2IMMIcl2$Class, 
                                         levels = c("class 1: ", "class 2: "), 
                                         labels = classes2)

#----C3 IMMI
MbyscalebycycleEUc3IMMIcl2 <- reshape2::melt(MbyscalebycycleEU$C3$IMMI[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(MbyscalebycycleEUc3IMMIcl2, nclass = ncl)
MbyscalebycycleEUc3IMMIcl2$Class <- factor(MbyscalebycycleEUc3IMMIcl2$Class, 
                                         levels = c("class 1: ", "class 2: "), 
                                         labels = classes2)

MbyscalebycycleEUIMMIcl2<- rbind(cbind(Cycle="1999", MbyscalebycycleEUc1IMMIcl2),
                                 cbind(Cycle="2009", MbyscalebycycleEUc2IMMIcl2),
                                 cbind(Cycle="2016", MbyscalebycycleEUc3IMMIcl2))
graphclasstogether(MbyscalebycycleEUIMMIcl2, nclass = ncl, title = "LCA IMMIGRATION scale EU with 2 classes")

r2$Size$C1[[1]] %>% filter(NClass == ncl) %>% mutate(Year = "1999") %>% 
  rename_with(~ classes2[which(c("P.2", "P.1") == .x)], .cols = c("P.2", "P.1")) %>% 
  dplyr::select("Year", all_of(classes2)) %>% 
  bind_rows(r2$Size$C2[[1]] %>% filter(NClass == ncl) %>% mutate(Year = "2009") %>% 
              rename_with(~ classes2[which(c("P.2", "P.1") == .x)], .cols = c("P.2", "P.1")) %>% 
              dplyr::select("Year", all_of(classes2))) %>% 
  bind_rows(r2$Size$C3[[1]] %>% filter(NClass == ncl) %>% mutate(Year = "2016") %>% 
              rename_with(~ classes2[which(c("P.1", "P.2") == .x)], .cols = c("P.1", "P.2")) %>% 
              dplyr::select("Year", all_of(classes2))) %>% 
  kbl(caption = "Size of each latent class EU with IMMIGRANT scales", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11, full_width = FALSE) %>% 
  column_spec (c(2), border_left = T) %>%
  column_spec(2:3, width = "10em") %>% print()

IMMIlc2 <- rbind(cbind(Cycle = "C1", MbyscalebycycleEUc1IMMIcl2),
                 cbind(Cycle = "C2", MbyscalebycycleEUc2IMMIcl2),
                 cbind(Cycle = "C3", MbyscalebycycleEUc3IMMIcl2))
IMMIpc2 <- IMMIlc2 %>% 
  ggplot() + 
  geom_point(aes(x = param, y = value, group = category, color = category, shape = Cycle), size = 2) +
  geom_line(aes(param, value, group = interaction(category,Cycle), color = category)) + 
  theme_bw() +
  ggtitle("Highest probabilities for each latent class \nEU Immigrant scale") +
  labs(y="Response probabilities", color = "Response category")  +
  scale_shape_manual(values = c(4,5,6), labels = c("1999", "2009", "2016")) +
  facet_grid(Class ~ ., switch = "y") +
  theme(legend.position = "top", legend.box="vertical", 
        strip.text.y = element_text(size = 8), 
        axis.title.x = element_blank(), axis.text.y = element_text(size = 8), 
        legend.title = element_text(size = 8), axis.text.x = element_text(angle = 90, size = 8)) +
  scale_color_discrete(labels = c("Engaged", "Agree", "Disagree", "Strongly disagree")) +
  scale_y_continuous(breaks = c(0.25,0.5,0.75))
print(IMMIpc2)

#-----------------

cat('\n')
cat('\n')
cat('## 1.2 Non-European countries {.tabset .tabset-pills} \n')
cat('\n')
cat('\n')

#---- By each scale-----
r2 <- summaryLCAR2(Modellist=MbyscalebycycleNEU, level1 = 1:3, level2 = 1:3, level3 = 1:3, data = "NEU")
ncl = 2 #Number of classes selected
#-----------------
cat('\n')
cat('\n')
cat("### 1.2.1 NEU Attitudes towards ethnics/racial equality items, 2 latent classes  \n")
cat('\n')
cat('\n')

r2$Fits$C1$ETHN %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit NEU 1999 with ETHNIC scales", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% 
  row_spec(2, bold = TRUE, italic = TRUE) %>% print()
r2$Fits$C2$ETHN %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit NEU 2009 with ETHNIC scales", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% 
  row_spec(2, bold = TRUE, italic = TRUE) %>% print()
r2$Fits$C3$ETHN %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit NEU 2016 with ETHNIC scales", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% 
  row_spec(2, bold = TRUE, italic = TRUE) %>% print()

ncl = 2
# ---C1 ETHN----
MbyscalebycycleNEUc1ETHNcl2 <- reshape2::melt(MbyscalebycycleNEU$C1$ETHN[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(MbyscalebycycleNEUc1ETHNcl2, nclass = ncl)
MbyscalebycycleNEUc1ETHNcl2$Class <- factor(MbyscalebycycleNEUc1ETHNcl2$Class, 
                                           levels = c("class 1: ", "class 2: "), 
                                           labels = classes2)
#----C2 ETHN
MbyscalebycycleNEUc2ETHNcl2 <- reshape2::melt(MbyscalebycycleNEU$C2$ETHN[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(MbyscalebycycleNEUc2ETHNcl2, nclass = ncl)
MbyscalebycycleNEUc2ETHNcl2$Class <- factor(MbyscalebycycleNEUc2ETHNcl2$Class, 
                                           levels = c("class 2: ", "class 1: "), 
                                           labels = classes2)

#----C3 ETHN
MbyscalebycycleNEUc3ETHNcl2 <- reshape2::melt(MbyscalebycycleNEU$C3$ETHN[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(MbyscalebycycleNEUc3ETHNcl2, nclass = ncl)
MbyscalebycycleNEUc3ETHNcl2$Class <- factor(MbyscalebycycleNEUc3ETHNcl2$Class, 
                                           levels = c("class 2: ", "class 1: "), 
                                           labels = classes2)

MbyscalebycycleEUETHNcl2<- rbind(cbind(Cycle="1999", MbyscalebycycleEUc1ETHNcl2),
                                 cbind(Cycle="2009", MbyscalebycycleEUc2ETHNcl2),
                                 cbind(Cycle="2016", MbyscalebycycleEUc3ETHNcl2))
graphclasstogether(MbyscalebycycleEUETHNcl2, nclass = ncl, title = "LCA ETHNIC scale NEU with 2 classes")

r2$Size$C1[[1]] %>% filter(NClass == ncl) %>% 
  mutate(Year = "1999") %>% 
  rename_with(~ classes2[which(c("P.1", "P.2") == .x)], .cols = c("P.1", "P.2")) %>% 
  dplyr::select("Year", all_of(classes2)) %>% 
  bind_rows(r2$Size$C2[[1]] %>% filter(NClass == ncl) %>% mutate(Year = "2009") %>% 
              rename_with(~ classes2[which(c("P.2", "P.1") == .x)], .cols = c("P.2", "P.1")) %>%
              dplyr::select("Year", all_of(classes2))) %>% 
  bind_rows(r2$Size$C3[[1]] %>% filter(NClass == ncl) %>% mutate(Year = "2016") %>% 
              rename_with(~ classes2[which(c("P.2", "P.1") == .x)], .cols = c("P.2", "P.1")) %>% 
              dplyr::select("Year", all_of(classes2))) %>% 
  kbl(caption = "Size of each latent class NEU with ETHNIC scales", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11, full_width = FALSE) %>% 
  column_spec (c(2), border_left = T) %>%
  column_spec(2:3, width = "10em") %>% print()

ETHNlc2 <- rbind(cbind(Cycle = "C1", MbyscalebycycleNEUc1ETHNcl2),
                 cbind(Cycle = "C2", MbyscalebycycleNEUc2ETHNcl2),
                 cbind(Cycle = "C3", MbyscalebycycleNEUc3ETHNcl2))
ETHNpc2 <- ETHNlc2 %>% 
  ggplot() + 
  geom_point(aes(x = param, y = value, group = category, color = category, shape = Cycle), size = 2) +
  geom_line(aes(param, value, group = interaction(category,Cycle), color = category)) + 
  theme_bw() +
  ggtitle("Highest probabilities for each latent class \nNEU Ethnic scale") +
  labs(y="Response probabilities", color = "Response category")  +
  scale_shape_manual(values = c(4,5,6), labels = c("1999", "2009", "2016")) +
  facet_grid(Class ~ ., switch = "y") +
  theme(legend.position = "top", legend.box="vertical", 
        strip.text.y = element_text(size = 8), 
        axis.title.x = element_blank(), axis.text.y = element_text(size = 8), 
        legend.title = element_text(size = 8), axis.text.x = element_text(angle = 90, size = 8)) +
  scale_color_discrete(labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree")) +
  scale_y_continuous(breaks = c(0.25,0.5,0.75))
print(ETHNpc2)

#-----------------

cat('\n')
cat('\n')
cat("### 1.2.2 NEU Attitudes towards gender equality items, 2 latent classes  \n")
cat('\n')
cat('\n')
r2$Fits$C1$GNDR %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit NEU 1999 with GENDER scales", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% 
  row_spec(2, bold = TRUE, italic = TRUE) %>% print()
r2$Fits$C2$GNDR %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit NEU 2009 with GENDER scales", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% 
  row_spec(2, bold = TRUE, italic = TRUE) %>% print()
r2$Fits$C3$GNDR %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit NEU 2016 with GENDER scales", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% 
  row_spec(2, bold = TRUE, italic = TRUE) %>% print()

#----C1 GNDR----
MbyscalebycycleNEUc1GNDRcl2 <- reshape2::melt(MbyscalebycycleNEU$C1$GNDR[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(MbyscalebycycleNEUc1GNDRcl2, nclass = ncl)
MbyscalebycycleNEUc1GNDRcl2$Class <- factor(MbyscalebycycleNEUc1GNDRcl2$Class, 
                                           levels = c("class 2: ", "class 1: "), 
                                           labels = classes2)
#----C2 GNDR
MbyscalebycycleNEUc2GNDRcl2 <- reshape2::melt(MbyscalebycycleNEU$C2$GNDR[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(MbyscalebycycleNEUc2GNDRcl2, nclass = ncl)
MbyscalebycycleNEUc2GNDRcl2$Class <- factor(MbyscalebycycleNEUc2GNDRcl2$Class, 
                                           levels = c("class 1: ", "class 2: "), 
                                           labels = classes2)

#----C3 GNDR
MbyscalebycycleNEUc3GNDRcl2 <- reshape2::melt(MbyscalebycycleNEU$C3$GNDR[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(MbyscalebycycleNEUc3GNDRcl2, nclass = ncl)
MbyscalebycycleNEUc3GNDRcl2$Class <- factor(MbyscalebycycleNEUc3GNDRcl2$Class, 
                                           levels = c("class 2: ", "class 1: "), labels = 
                                             classes2)
MbyscalebycycleEUGNDRcl2<- rbind(cbind(Cycle="1999", MbyscalebycycleEUc1GNDRcl2),
                                 cbind(Cycle="2009", MbyscalebycycleEUc2GNDRcl2),
                                 cbind(Cycle="2016", MbyscalebycycleEUc3GNDRcl2))
graphclasstogether(MbyscalebycycleEUGNDRcl2, nclass = ncl, title = "LCA GENDER scale NEU with 2 classes")


r2$Size$C1[[1]] %>% filter(NClass == ncl) %>% mutate(Year = "1999") %>% 
  rename_with(~ classes2[which(c("P.2", "P.1") == .x)], .cols = c("P.2", "P.1")) %>% 
  dplyr::select("Year", all_of(classes2)) %>% 
  bind_rows(r2$Size$C2[[1]] %>% filter(NClass == ncl) %>% mutate(Year = "2009") %>% 
              rename_with(~ classes2[which(c("P.1", "P.2") == .x)], .cols = c("P.1", "P.2")) %>% 
              dplyr::select("Year", all_of(classes2))) %>% 
  bind_rows(r2$Size$C3[[1]] %>% filter(NClass == ncl) %>% mutate(Year = "2016") %>% 
              rename_with(~ classes2[which(c("P.2", "P.1") == .x)], .cols = c("P.2", "P.1")) %>% 
              dplyr::select("Year", all_of(classes2))) %>% 
  kbl(caption = "Size of each latent class NEU with GENDER scales", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11, full_width = FALSE) %>% 
  column_spec (c(2), border_left = T) %>%
  column_spec(2:3, width = "10em") %>% print()

GNDRlc2 <- rbind(cbind(Cycle = "C1", MbyscalebycycleNEUc1GNDRcl2),
                 cbind(Cycle = "C2", MbyscalebycycleNEUc2GNDRcl2),
                 cbind(Cycle = "C3", MbyscalebycycleNEUc3GNDRcl2))
GNDRpc2 <- GNDRlc2 %>% 
  ggplot() + 
  geom_point(aes(x = param, y = value, group = category, color = category, shape = Cycle), size = 2) +
  geom_line(aes(param, value, group = interaction(category,Cycle), color = category)) + 
  theme_bw() +
  ggtitle("Highest probabilities for each latent class \nNEU Gender scale") +
  labs(y="Response probabilities", color = "Response category")  +
  scale_shape_manual(values = c(4,5,6), labels = c("1999", "2009", "2016")) +
  facet_grid(Class ~ ., switch = "y") +
  theme(legend.position = "top", legend.box="vertical", 
        strip.text.y = element_text(size = 8), 
        axis.title.x = element_blank(), axis.text.y = element_text(size = 8), 
        legend.title = element_text(size = 8), axis.text.x = element_text(angle = 90, size = 8)) +
  scale_color_discrete(labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree")) +
  scale_y_continuous(breaks = c(0.25,0.5,0.75))
print(GNDRpc2)
#-----------------

cat('\n')
cat('\n')
cat("### 1.2.3 NEU Attitudes towards immigrants equality items - 2 latent classes  \n")
cat('\n')
cat('\n')
r2$Fits$C1$IMMI %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit NEU 1999 with IMMIGRATION scales", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% 
  row_spec(2, bold = TRUE, italic = TRUE) %>% print()
r2$Fits$C2$IMMI %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit NEU 2009 with IMMIGRATION scales", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% 
  row_spec(2, bold = TRUE, italic = TRUE) %>% print()

#----C1 IMMI----
MbyscalebycycleNEUc1IMMIcl2 <- reshape2::melt(MbyscalebycycleNEU$C1$IMMI[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(MbyscalebycycleNEUc1IMMIcl2, nclass = ncl)
MbyscalebycycleNEUc1IMMIcl2$Class <- factor(MbyscalebycycleNEUc1IMMIcl2$Class, 
                                           levels = c("class 1: ", "class 2: "), 
                                           labels = classes2)

#----C2 IMMI
MbyscalebycycleNEUc2IMMIcl2 <- reshape2::melt(MbyscalebycycleNEU$C2$IMMI[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(MbyscalebycycleNEUc2IMMIcl2, nclass = ncl)
MbyscalebycycleNEUc2IMMIcl2$Class <- factor(MbyscalebycycleNEUc2IMMIcl2$Class, 
                                           levels = c("class 1: ", "class 2: "), 
                                           labels = classes2)

MbyscalebycycleNEUIMMIcl2<- rbind(cbind(Cycle="1999", MbyscalebycycleNEUc1IMMIcl2),
                                 cbind(Cycle="2009", MbyscalebycycleNEUc2IMMIcl2))
graphclasstogether(MbyscalebycycleNEUIMMIcl2, nclass = ncl, title = "LCA IMMIGRATION scale NEU with 2 classes")



r2$Size$C1[[1]] %>% filter(NClass == ncl) %>% mutate(Year = "1999") %>% 
  rename_with(~ classes2[which(c("P.1", "P.2") == .x)], .cols = c("P.1", "P.2")) %>% 
  dplyr::select("Year", all_of(classes2)) %>% 
  bind_rows(r2$Size$C2[[1]] %>% filter(NClass == ncl) %>% mutate(Year = "2009") %>% 
              rename_with(~ classes2[which(c("P.1", "P.2") == .x)], .cols = c("P.1", "P.2")) %>% 
              dplyr::select("Year", all_of(classes2))) %>% 
  kbl(caption = "Size of each latent class NEU with IMMIGRANT scales", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11, full_width = FALSE) %>% 
  column_spec (c(2), border_left = T) %>%
  column_spec(2:3, width = "10em") %>% print()

IMMIlc2 <- rbind(cbind(Cycle = "C1", MbyscalebycycleNEUc1IMMIcl2),
                 cbind(Cycle = "C2", MbyscalebycycleNEUc2IMMIcl2))
IMMIpc2 <- IMMIlc2 %>% 
  ggplot() + 
  geom_point(aes(x = param, y = value, group = category, color = category, shape = Cycle), size = 2) +
  geom_line(aes(param, value, group = interaction(category,Cycle), color = category)) + 
  theme_bw() +
  ggtitle("Highest probabilities for each latent class \nNEU Immigrant scale") +
  labs(y="Response probabilities", color = "Response category")  +
  scale_shape_manual(values = c(4,5,6), labels = c("1999", "2009", "2016")) +
  facet_grid(Class ~ ., switch = "y") +
  theme(legend.position = "top", legend.box="vertical", 
        strip.text.y = element_text(size = 8), 
        axis.title.x = element_blank(), axis.text.y = element_text(size = 8), 
        legend.title = element_text(size = 8), axis.text.x = element_text(angle = 90, size = 8)) +
  scale_color_discrete(labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree")) +
  scale_y_continuous(breaks = c(0.25,0.5,0.75))
print(IMMIpc2)

#-----------------

cat('\n')
cat('\n')
cat("# 2. All items with 4 response categories by cycle   \n")
cat('\n')
cat('\n')
cat('## 2.1 European countries {.tabset .tabset-pills} \n')
cat('\n')
cat('\n')
#-----
#Summary of Model fit and class size all models
r <- summaryLCAR2(Modellist=MAllScalesbycycleEU, level1 = 1:3, level2 = 1:7, level3 = NA)
r$Fits$c1 %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit EU 1999 with all scales", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(3,6,8), border_left = T) %>% 
  row_spec(4:5, bold = TRUE, italic = TRUE) %>% print()
r$Fits$c2 %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit EU 2009 with all scales", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(3,6,8), border_left = T) %>% 
  row_spec(4:5, bold = TRUE, italic = TRUE) %>% print()
r$Fits$c3 %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit EU 2016 with all scales", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(3,6,8), border_left = T) %>% 
  row_spec(4:5, bold = TRUE, italic = TRUE) %>% print()
#-----
cat('\n')
cat('\n')
cat("### 2.1.1 EU - All scales - 4 latent classes  \n")
cat('\n')
cat('\n')
#------------Results with 4 classes by cycle-----
ncl = 4 #Number of classes selected

#----C1
MAllScalesbycycleEUc1cl4 <- reshape2::melt(MAllScalesbycycleEU$c1[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesbycycleEUc1cl4, nclass = ncl)
MAllScalesbycycleEUc1cl4$Class <- factor(MAllScalesbycycleEUc1cl4$Class, 
                                       levels = c("class 4: ", "class 1: ", "class 3: ", "class 2: "), 
                                       labels = classes4)
graphclass(MAllScalesbycycleEUc1cl4, nclass = ncl, title = paste0("LCA 1999 EU with ",ncl," classes"))

#----C2
MAllScalesbycycleEUc2cl4 <- reshape2::melt(MAllScalesbycycleEU$c2[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesbycycleEUc2cl4, nclass = ncl)
MAllScalesbycycleEUc2cl4$Class <- factor(MAllScalesbycycleEUc2cl4$Class, 
                                       levels = c("class 3: ", "class 4: ", "class 1: ", "class 2: "), 
                                       labels = classes4)

graphclass(MAllScalesbycycleEUc2cl4, nclass = ncl, title = paste0("LCA 2009 EU with ",ncl," classes"))

#----C3
MAllScalesbycycleEUc3cl4 <- reshape2::melt(MAllScalesbycycleEU$c3[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(MAllScalesbycycleEUc3cl4, nclass = ncl)
MAllScalesbycycleEUc3cl4$Class <- factor(MAllScalesbycycleEUc3cl4$Class, 
                                       levels = c("class 4: ", "class 3: ", "class 1: ", "class 2: "), 
                                       labels = classes4)

graphclass(MAllScalesbycycleEUc3cl4, nclass = ncl, title = paste0("LCA 2016 EU with ",ncl," classes"))

r$Size[[1]] %>% filter(NClass == ncl) %>% mutate(Year = "1999") %>% 
  rename_with(~ classes4[which(c("P.4", "P.1", "P.3", "P.2") == .x)], .cols = c("P.4", "P.1", "P.3", "P.2")) %>% 
  dplyr::select("Year", all_of(classes4)) %>% 
  bind_rows(r$Size[[2]] %>% filter(NClass == ncl) %>% mutate(Year = "2009") %>% 
              rename_with(~ classes4[which(c("P.3", "P.4", "P.1", "P.2") == .x)], .cols = c("P.3", "P.4", "P.1", "P.2")) %>% 
              dplyr::select("Year", all_of(classes4))) %>% 
  bind_rows(r$Size[[3]] %>% filter(NClass == ncl) %>% mutate(Year = "2016") %>% 
              rename_with(~ classes4[which(c("P.4", "P.3", "P.1", "P.2") == .x)], .cols = c("P.4", "P.3", "P.1", "P.2")) %>% 
              dplyr::select("Year", all_of(classes4))) %>% 
  kbl(caption = "Size of each 4 latent class with all scales EU", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11, full_width = FALSE) %>% 
  column_spec (c(2), border_left = T) %>%
  column_spec(2:5, width = "10em") %>% print()

lc4 <- rbind(cbind(Cycle = "C1", MAllScalesbycycleEUc1cl4),
             cbind(Cycle = "C2", MAllScalesbycycleEUc2cl4),
             cbind(Cycle = "C3", MAllScalesbycycleEUc3cl4))
lc4$Cycle <- factor(lc4$Cycle, labels = c("1999", "2009", "2016"))
pc4 <- lc4 %>% 
  ggplot() + 
  geom_point(aes(x = param, y = value, group = category, color = category, shape = Cycle), size = 2) +
  geom_line(aes(param, value, group = interaction(category,Cycle), color = category)) + 
  theme_bw() +
  ggtitle("Highest probabilities for 4 latent classes, \nEU countries") +
  labs(y="Response probabilities", color = "Response category")  +
  facet_grid(Class ~ ., switch = "y", labeller = label_wrap_gen(16)) +
  theme(legend.position = "top", legend.box="vertical", 
        strip.text.y = element_text(size = 8), 
        axis.title.x = element_blank(), axis.text.y = element_text(size = 8), 
        legend.title = element_text(size = 8), axis.text.x = element_text(angle = 90, size = 8)) +
  scale_color_discrete(labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"  )) +
  scale_y_continuous(breaks = c(0.25,0.5,0.75))
print(pc4)

#----------------

cat('\n')
cat('\n')
cat("### 2.1.2 EU - All scales - 5 latent classes  \n")
cat('\n')
cat('\n')
#------------Results with 5 classes by cycle----
ncl = 5
#----C1
MAllScalesbycycleEUc1cl5 <- reshape2::melt(MAllScalesbycycleEU$c1[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesbycycleEUc1cl5, nclass = ncl)
MAllScalesbycycleEUc1cl5$Class <- factor(MAllScalesbycycleEUc1cl5$Class, 
                                       levels = c("class 2: ", "class 4: ", "class 5: ", "class 1: ", "class 3: "), 
                                       labels = classes5)
graphclass(MAllScalesbycycleEUc1cl5, nclass = ncl, title = paste0("LCA 1999 EU with ",ncl," classes"))

#----C2
MAllScalesbycycleEUc2cl5 <- reshape2::melt(MAllScalesbycycleEU$c2[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesbycycleEUc2cl5, nclass = ncl)
MAllScalesbycycleEUc2cl5$Class <- factor(MAllScalesbycycleEUc2cl5$Class, 
                                       levels = c("class 1: ", "class 3: ", "class 2: ", "class 5: ", "class 4: "), 
                                       labels = classes5)

graphclass(MAllScalesbycycleEUc2cl5, nclass = ncl, title = paste0("LCA 2009 EU with ",ncl," classes"))

#----C3
MAllScalesbycycleEUc3cl5 <- reshape2::melt(MAllScalesbycycleEU$c3[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesbycycleEUc3cl5, nclass = ncl)
MAllScalesbycycleEUc3cl5$Class <- factor(MAllScalesbycycleEUc3cl5$Class, 
                                       levels = c("class 3: ", "class 1: ", "class 5: ", "class 2: ", "class 4: "), 
                                       labels = classes5)

graphclass(MAllScalesbycycleEUc3cl5, nclass = ncl, title = paste0("LCA 2016 EU with ",ncl," classes"))

r$Size[[1]] %>% filter(NClass == ncl) %>% mutate(Year = "1999") %>% 
  rename_with(~ classes5[which(c("P.2", "P.4", "P.5", "P.1", "P.3") == .x)], .cols = c("P.2", "P.4", "P.5", "P.1", "P.3")) %>% 
  dplyr::select("Year", all_of(classes5)) %>% 
  bind_rows(r$Size[[2]] %>% filter(NClass == ncl) %>% mutate(Year = "2009") %>% 
              rename_with(~ classes5[which(c("P.1", "P.3", "P.2", "P.5", "P.4") == .x)], .cols = c("P.1", "P.3", "P.2", "P.5", "P.4")) %>% 
              dplyr::select("Year", all_of(classes5))) %>% 
  bind_rows(r$Size[[3]] %>% filter(NClass == ncl) %>% mutate(Year = "2016") %>% 
              rename_with(~ classes5[which(c("P.3", "P.1", "P.5", "P.2", "P.4") == .x)], .cols = c("P.3", "P.1", "P.5", "P.2", "P.4")) %>% 
              dplyr::select("Year", all_of(classes5))) %>% 
  kbl(caption = "Size of each 5 latent class with all scales EU", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11, full_width = FALSE) %>% 
  column_spec (c(2), border_left = T) %>%
  column_spec(2:6, width = "10em") %>% print()


lc5 <- rbind(cbind(Cycle = "C1", MAllScalesbycycleEUc1cl5),
             cbind(Cycle = "C2", MAllScalesbycycleEUc2cl5),
             cbind(Cycle = "C3", MAllScalesbycycleEUc3cl5))
lc5$Cycle <- factor(lc5$Cycle, labels = c("1999", "2009", "2016"))

pc5 <- lc5 %>% 
  ggplot() + 
  geom_point(aes(x = param, y = value, group = category, color = category, shape = Cycle), size = 2) +
  geom_line(aes(param, value, group = interaction(category,Cycle), color = category)) + 
  theme_bw() +
  ggtitle("Highest probabilities for 5 latent classes, \nEU countries") +
  labs(y="Response probabilities", color = "Response category")  +
  facet_grid(Class ~ ., switch = "y", labeller = label_wrap_gen(16)) +
  theme(legend.position = "top", legend.box="vertical", 
        strip.text.y = element_text(size = 7), 
        axis.title.x = element_blank(), axis.text.y = element_text(size = 8), 
        legend.title = element_text(size = 8), axis.text.x = element_text(angle = 90, size = 8)) +
  scale_color_discrete(labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"  )) +
  scale_y_continuous(breaks = c(0.25,0.5,0.75))
print(pc5)

#----------------
cat('\n')
cat('\n')
cat('## 2.2 Non-European countries {.tabset .tabset-pills} \n')
cat('\n')
cat('\n')
#Summary of Model fit and class size all models
#----------------
rNEU <- summaryLCAR2(Modellist=MAllScalesbycycleNEU, level1 = 1:3, level2 = 1:7, level3 = NA)
rNEU$Fits$c1 %>% dplyr::select(-Chisq, -npar) %>% 
     kbl(caption = "Model fit NEU 1999 with all scales", row.names = FALSE) %>% 
     kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
     column_spec (c(3,6,8), border_left = T) %>% 
     row_spec(4:5, bold = TRUE, italic = TRUE) %>% print()
rNEU$Fits$c2 %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit NEU 2009 with all scales", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(3,6,8), border_left = T) %>% 
  row_spec(4:5, bold = TRUE, italic = TRUE) %>% print()
rNEU$Fits$c3 %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit NEU 2016 with all scales", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(3,6,8), border_left = T) %>% 
  row_spec(4:5, bold = TRUE, italic = TRUE) %>% print()
cat('\n')
cat('\n')
#----------------
cat("### 2.2.1 NEU - All scales - 4 latent classes  \n")
#------------Results with 4 classes by cycle-----
cat('\n')
cat('\n')
ncl = 4 #Number of classes selected

#----C1
MAllScalesbycycleNEUc1cl4 <- reshape2::melt(MAllScalesbycycleNEU$c1[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesbycycleNEUc1cl4, nclass = ncl)
MAllScalesbycycleNEUc1cl4$Class <- factor(MAllScalesbycycleNEUc1cl4$Class, 
                                         levels = c("class 2: ", "class 4: ", "class 3: ", "class 1: "), 
                                         labels = classes4NEU)
graphclass(MAllScalesbycycleNEUc1cl4, nclass = ncl, title = paste0("LCA 1999 NEU with ",ncl," classes"))

#----C2
MAllScalesbycycleNEUc2cl4 <- reshape2::melt(MAllScalesbycycleNEU$c2[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesbycycleNEUc2cl4, nclass = ncl)
MAllScalesbycycleNEUc2cl4$Class <- factor(MAllScalesbycycleNEUc2cl4$Class, 
                                         levels = c("class 2: ", "class 1: ", "class 4: ", "class 3: "), 
                                         labels = classes4NEU)
graphclass(MAllScalesbycycleNEUc2cl4, nclass = ncl, title = paste0("LCA 2009 NEU with ",ncl," classes"))

#----C3
MAllScalesbycycleNEUc3cl4 <- reshape2::melt(MAllScalesbycycleNEU$c3[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesbycycleNEUc3cl4, nclass = ncl)
MAllScalesbycycleNEUc3cl4$Class <- factor(MAllScalesbycycleNEUc3cl4$Class, 
                                         levels = c("class 3: ", "class 2: ", "class 4: ", "class 1: "), 
                                         labels = classes4NEU)
graphclass(MAllScalesbycycleNEUc3cl4, nclass = ncl, title = paste0("LCA 2016 NEU with ",ncl," classes"))

rNEU$Size[[1]] %>% filter(NClass == ncl) %>% mutate(Year = "1999") %>% 
  rename_with(~ classes4NEU[which(c("P.2", "P.4", "P.3", "P.1") == .x)], .cols = c("P.2", "P.4", "P.3", "P.1")) %>% 
  dplyr::select("Year", all_of(classes4NEU)) %>% 
  bind_rows(rNEU$Size[[2]] %>% filter(NClass == ncl) %>% mutate(Year = "2009") %>% 
              rename_with(~ classes4NEU[which(c("P.2", "P.1", "P.4", "P.3") == .x)], .cols = c("P.2", "P.1", "P.4", "P.3")) %>% 
              dplyr::select("Year", all_of(classes4NEU))) %>% 
  bind_rows(rNEU$Size[[3]] %>% filter(NClass == ncl) %>% mutate(Year = "2016") %>% 
              rename_with(~ classes4NEU[which(c("P.3", "P.2", "P.4", "P.1") == .x)], .cols = c("P.3", "P.2", "P.4", "P.1")) %>% 
              dplyr::select("Year", all_of(classes4NEU))) %>% 
  kbl(caption = "Size of each 4 latent class with all scales NEU", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11, full_width = FALSE) %>% 
  column_spec (c(2), border_left = T) %>%
  column_spec(2:5, width = "10em") %>% print()

lc4 <- rbind(cbind(Cycle = "C1", MAllScalesbycycleNEUc1cl4),
             cbind(Cycle = "C2", MAllScalesbycycleNEUc2cl4),
             cbind(Cycle = "C3", MAllScalesbycycleNEUc3cl4))
lc4$Cycle <- factor(lc4$Cycle, labels = c("1999", "2009", "2016"))
pc4 <- lc4 %>% 
  ggplot() + 
  geom_point(aes(x = param, y = value, group = category, color = category, shape = Cycle), size = 2) +
  geom_line(aes(param, value, group = interaction(category,Cycle), color = category)) + 
  theme_bw() +
  ggtitle("Highest probabilities for 4 latent classes, \nNEU countries") +
  labs(y="Response probabilities", color = "Response category")  +
  facet_grid(Class ~ ., switch = "y", labeller = label_wrap_gen(16)) +
  theme(legend.position = "top", legend.box="vertical", 
        strip.text.y = element_text(size = 8), 
        axis.title.x = element_blank(), axis.text.y = element_text(size = 8), 
        legend.title = element_text(size = 8), axis.text.x = element_text(angle = 90, size = 8)) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  scale_color_discrete(labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"  )) +
  scale_y_continuous(breaks = c(0.25,0.5,0.75))
print(pc4)

#----------------

cat('\n')
cat('\n')
cat("### 2.2.2 NEU - All scales - 5 latent classes  \n")
cat('\n')
cat('\n')
#------------Results with 5 classes by cycle----
ncl = 5
#----C1
MAllScalesbycycleNEUc1cl5 <- reshape2::melt(MAllScalesbycycleNEU$c1[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesbycycleNEUc1cl5, nclass = ncl)
MAllScalesbycycleNEUc1cl5$Class <- factor(MAllScalesbycycleNEUc1cl5$Class, 
                                         levels = c("class 4: ", "class 1: ", "class 5: ", "class 3: ", "class 2: "), 
                                         labels = classes5)
graphclass(MAllScalesbycycleNEUc1cl5, nclass = ncl, title = paste0("LCA 1999 NEU with ",ncl," classes"))

#----C2
MAllScalesbycycleNEUc2cl5 <- reshape2::melt(MAllScalesbycycleNEU$c2[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesbycycleNEUc2cl5, nclass = ncl)
MAllScalesbycycleNEUc2cl5$Class <- factor(MAllScalesbycycleNEUc2cl5$Class, 
                                         levels = c("class 1: ", "class 3: ", "class 2: ", "class 5: ", "class 4: "), 
                                         labels = classes5)
graphclass(MAllScalesbycycleNEUc2cl5, nclass = ncl, title = paste0("LCA 2009 NEU with ",ncl," classes"))

#----C3
MAllScalesbycycleNEUc3cl5 <- reshape2::melt(MAllScalesbycycleNEU$c3[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesbycycleNEUc3cl5, nclass = ncl)
MAllScalesbycycleNEUc3cl5$Class <- factor(MAllScalesbycycleNEUc3cl5$Class, 
                                         levels = c("class 3: ", "class 2: ", "class 4: ", "class 1: ", "class 5: "), 
                                         labels = classes5NEU3)
graphclass(MAllScalesbycycleNEUc3cl5, nclass = ncl, title = paste0("LCA 2016 NEU with ",ncl," classes"))

r$Size[[1]] %>% filter(NClass == ncl) %>% mutate(Year = "1999") %>% 
  rename_with(~ classes5[which(c("P.4", "P.1", "P.5", "P.3", "P.2") == .x)], .cols = c("P.4", "P.1", "P.5", "P.3", "P.2")) %>% 
  dplyr::select("Year", all_of(classes5)) %>% 
  bind_rows(r$Size[[2]] %>% filter(NClass == ncl) %>% mutate(Year = "2009") %>% 
              rename_with(~ classes5[which(c("P.1", "P.3", "P.2", "P.5", "P.4") == .x)], .cols = c("P.1", "P.3", "P.2", "P.5", "P.4")) %>% 
              dplyr::select("Year", all_of(classes5))) %>% 
  bind_rows(r$Size[[3]] %>% filter(NClass == ncl) %>% mutate(Year = "2016") %>% 
  rename_with(~ classes5NEU3[which(c("P.3", "P.2", "P.4", "P.1", "P.5") == .x)], .cols = c("P.3", "P.2", "P.4", "P.1", "P.5")) %>% 
  dplyr::select("Year", all_of(classes5NEU3))) %>%  
  kbl(caption = "Size of each 5 latent class with all scales NEU", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11, full_width = FALSE) %>% 
  column_spec (c(2), border_left = T) %>%
  column_spec(2:6, width = "10em") %>% print()


lc5 <- rbind(cbind(Cycle = "C1", MAllScalesbycycleNEUc1cl5),
             cbind(Cycle = "C2", MAllScalesbycycleNEUc2cl5),
             cbind(Cycle = "C3", MAllScalesbycycleNEUc3cl5))
lc5$Cycle <- factor(lc5$Cycle, labels = c("1999", "2009", "2016"))

pc5 <- lc5 %>% 
  ggplot() + 
  geom_point(aes(x = param, y = value, group = category, color = category, shape = Cycle), size = 2) +
  geom_line(aes(param, value, group = interaction(category,Cycle), color = category)) + 
  theme_bw() +
  ggtitle("Highest probabilities for 5 latent classes, \nNEU countries") +
  labs(y="Response probabilities", color = "Response category")  +
  facet_grid(Class ~ ., switch = "y", labeller = label_wrap_gen(16)) +
  theme(legend.position = "top", legend.box="vertical", 
        strip.text.y = element_text(size = 8), 
        axis.title.x = element_blank(), axis.text.y = element_text(size = 8), 
        legend.title = element_text(size = 8), axis.text.x = element_text(angle = 90, size = 8)) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  scale_color_discrete(labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"  )) +
  scale_y_continuous(breaks = c(0.25,0.5,0.75))
print(pc5)

#----------------
cat('\n')
cat('\n')
cat("# 3. All items with 2 response categories by cycle  \n")
cat('\n')
cat('\n')
cat('## 3.1 European countries \n')
cat('\n')
cat('\n')
#Summary of Model fit and class size all models
rb <- summaryLCAR2(Modellist=MBAllScalesbycycleEU, level1 = 1:3, level2 = 1:7, level3 = NA)
cat('\n')
cat('\n')
cat('### 3.1.1 EU - Binomial response with 3 latent classes {.tabset .tabset-pills}  \n')
cat('\n')
cat('\n')
#------------Results binomial with 3 classes by cycle----

ncl = 3
#----------1999----
cat('\n')
cat('\n')
cat('#### 1999 \n')
cat('\n')
cat('\n')
#----C1
rb$Fits$c1 %>% dplyr::select(-Chisq, -npar) %>% 
     kbl(caption = "Model fit NEU 1999 with binomial response", row.names = FALSE) %>% 
     kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
     column_spec (c(3,6,8), border_left = T) %>% 
     row_spec(3, bold = TRUE, italic = TRUE) %>% print()

MBAllScalesbycycleEUc1cl5 <- reshape2::melt(MBAllScalesbycycleEU$c1[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MBAllScalesbycycleEUc1cl5, nclass = ncl)
MBAllScalesbycycleEUc1cl5$Class <- factor(MBAllScalesbycycleEUc1cl5$Class, 
                                       levels = c("class 3: ", "class 2: ", "class 1: "), 
                                       labels = classes3)
graphclass(MBAllScalesbycycleEUc1cl5, nclass = ncl, title = paste0("LCA 1999 EU with ",ncl," classes"))
#----------
#----------2009----
cat('\n')
cat('\n')
cat('#### 2009 \n')
cat('\n')
cat('\n')

#----C2
rb$Fits$c2 %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit NEU 2009 with binomial response", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(3,6,8), border_left = T) %>% 
  row_spec(3, bold = TRUE, italic = TRUE) %>% print()

MBAllScalesbycycleEUc2cl5 <- reshape2::melt(MBAllScalesbycycleEU$c2[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MBAllScalesbycycleEUc2cl5, nclass = ncl)
MBAllScalesbycycleEUc2cl5$Class <- factor(MBAllScalesbycycleEUc2cl5$Class, 
                                       levels = c("class 1: ", "class 2: ", "class 3: "), 
                                       labels = classes3)
graphclass(MBAllScalesbycycleEUc2cl5, nclass = ncl, title = paste0("LCA 2009 EU with ",ncl," classes"))
#----------
#----------2016----
cat('\n')
cat('\n')
cat('#### 2016 \n')
cat('\n')
cat('\n')

#----C3
rb$Fits$c3 %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit NEU 2016 with binomial response", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(3,6,8), border_left = T) %>% 
  row_spec(3, bold = TRUE, italic = TRUE) %>% print()

MBAllScalesbycycleEUc3cl5 <- reshape2::melt(MBAllScalesbycycleEU$c3[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 

#graphclass(MBAllScalesbycycleEUc3cl5, nclass = ncl)
MBAllScalesbycycleEUc3cl5$Class <- factor(MBAllScalesbycycleEUc3cl5$Class, 
                                       levels = c("class 1: ", "class 2: ", "class 3: "), 
                                       labels = classes3)
graphclass(MBAllScalesbycycleEUc3cl5, nclass = ncl, title = paste0("LCA 2016 EU with ",ncl," classes"))

#--------
#--------
cat('\n')
cat('\n')
cat('### {-} \n')
cat('\n')
cat('\n')

rb$Size[[1]] %>% filter(NClass == ncl) %>% mutate(Year = "1999") %>% 
  rename_with(~ classes3[which(c("P.3", "P.2", "P.1") == .x)], .cols = c("P.3", "P.2", "P.1")) %>% 
  dplyr::select("Year", all_of(classes3)) %>% 
  bind_rows(rb$Size[[2]] %>% filter(NClass == ncl) %>% mutate(Year = "2009") %>% 
              rename_with(~ classes3[which(c("P.1", "P.2", "P.3") == .x)], .cols = c("P.1", "P.2", "P.3")) %>% 
              dplyr::select("Year", all_of(classes3))) %>% 
  bind_rows(rb$Size[[3]] %>% filter(NClass == ncl) %>% mutate(Year = "2016") %>% 
              rename_with(~ classes3[which(c("P.1", "P.2", "P.3") == .x)], .cols = c("P.1", "P.2", "P.3")) %>% 
              dplyr::select("Year", all_of(classes3))) %>% 
  kbl(caption = "Size of each 3 latent class with all dicotomic scales EU", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11, full_width = FALSE) %>% 
  column_spec (c(2), border_left = T) %>%
  column_spec(2:4, width = "10em") %>% print()

lc5 <- rbind(cbind(Cycle = "C1", MBAllScalesbycycleEUc1cl5),
             cbind(Cycle = "C2", MBAllScalesbycycleEUc2cl5),
             cbind(Cycle = "C3", MBAllScalesbycycleEUc3cl5))
lc5$Cycle <- factor(lc5$Cycle, labels = c("1999", "2009", "2016"))
pc5 <- lc5 %>% na.omit() %>% 
  ggplot() + 
  geom_point(aes(x = param, y = value, group = category, color = category, shape = Cycle), size = 2) +
  geom_line(aes(param, value, group = interaction(category,Cycle), color = category)) + 
  theme_bw() +
  ggtitle("Highest probabilities for 3 latent classes \nwith binomial response, EU countries") +
  labs(y="Response probabilities", color = "Response category")  +
  facet_grid(Class ~ ., switch = "y", labeller = label_wrap_gen(22)) +
  theme(legend.position = "top", legend.box="vertical", 
        strip.text.y = element_text(size = 8), 
        axis.title.x = element_blank(), axis.text.y = element_text(size = 8), 
        legend.title = element_text(size = 8), axis.text.x = element_text(angle = 90, size = 8)) +
  scale_color_discrete(labels = c("Disagree", "Agree")) +
  scale_y_continuous(breaks = c(0.25,0.5,0.75))
print(pc5)
#----------------

cat('\n')
cat('\n')
cat('## 3.2 Non-European countries  \n')
cat('\n')
cat('\n')

#Summary of Model fit and class size all models
rbNEU <- summaryLCAR2(Modellist=MBAllScalesbycycleNEU, level1 = 1:3, level2 = 1:7, level3 = NA)

cat('\n')
cat('\n')
cat('### 3.2.1 NEU - Binomial response with 3 latent classes {.tabset .tabset-pills}   \n')
cat('\n')
cat('\n')
#------------Results binomial with 3 classes by cycle----
ncl = 3
#----------1999----
cat('\n')
cat('\n')
cat('#### 1999 \n')
cat('\n')
cat('\n')

#----C1
rbNEU$Fits$c1 %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit NEU 1999 with binomial response", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(3,6,8), border_left = T) %>% 
  row_spec(3, bold = TRUE, italic = TRUE) %>% print()

MBAllScalesbycycleNEUc1cl5 <- reshape2::melt(MBAllScalesbycycleNEU$c1[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MBAllScalesbycycleNEUc1cl5, nclass = ncl)
MBAllScalesbycycleNEUc1cl5$Class <- factor(MBAllScalesbycycleNEUc1cl5$Class, 
                                          levels = c("class 1: ", "class 3: ", "class 2: "), 
                                          labels = classes3)
graphclass(MBAllScalesbycycleNEUc1cl5, nclass = ncl, title = paste0("LCA 1999 NEU with ",ncl," classes"))

#--------
#----------2009----
cat('\n')
cat('\n')
cat('#### 2009 \n')
cat('\n')
cat('\n')

#----C2
rbNEU$Fits$c2 %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit NEU 2009 with binomial response", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(3,6,8), border_left = T) %>% 
  row_spec(3, bold = TRUE, italic = TRUE) %>% print()

MBAllScalesbycycleNEUc2cl5 <- reshape2::melt(MBAllScalesbycycleNEU$c2[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MBAllScalesbycycleNEUc2cl5, nclass = ncl)
MBAllScalesbycycleNEUc2cl5$Class <- factor(MBAllScalesbycycleNEUc2cl5$Class, 
                                          levels = c("class 1: ", "class 3: ", "class 2: "), 
                                          labels = classes3)
graphclass(MBAllScalesbycycleNEUc2cl5, nclass = ncl, title = paste0("LCA 2009 NEU with ",ncl," classes"))

#--------
#----------2016----
cat('\n')
cat('\n')
cat('#### 2016 \n')
cat('\n')
cat('\n')

#----C3
rbNEU$Fits$c3 %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit NEU 2016 with binomial response", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(3,6,8), border_left = T) %>% 
  row_spec(3, bold = TRUE, italic = TRUE) %>% print()

MBAllScalesbycycleNEUc3cl5 <- reshape2::melt(MBAllScalesbycycleNEU$c3[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MBAllScalesbycycleNEUc3cl5, nclass = ncl)
MBAllScalesbycycleNEUc3cl5$Class <- factor(MBAllScalesbycycleNEUc3cl5$Class, 
                                          levels = c("class 3: ", "class 1: ", "class 2: "), 
                                          labels = classes3)
graphclass(MBAllScalesbycycleNEUc3cl5, nclass = ncl, title = paste0("LCA 2016 NEU with ",ncl," classes"))

#--------
cat('\n')
cat('\n')
cat('### {-} \n')
cat('\n')
cat('\n')

rbNEU$Size[[1]] %>% filter(NClass == ncl) %>% mutate(Year = "1999") %>% 
  rename_with(~ classes3[which(c("P.1", "P.3", "P.2") == .x)], .cols = c("P.1", "P.3", "P.2")) %>% 
  dplyr::select("Year", all_of(classes3)) %>% 
  bind_rows(rbNEU$Size[[2]] %>% filter(NClass == ncl) %>% mutate(Year = "2009") %>% 
              rename_with(~ classes3[which(c("P.1", "P.3", "P.2") == .x)], .cols = c("P.1", "P.3", "P.2")) %>% 
              dplyr::select("Year", all_of(classes3))) %>% 
  bind_rows(rbNEU$Size[[3]] %>% filter(NClass == ncl) %>% mutate(Year = "2016") %>% 
              rename_with(~ classes3[which(c("P.3", "P.1", "P.2") == .x)], .cols = c("P.3", "P.1", "P.2")) %>% 
              dplyr::select("Year", all_of(classes3))) %>% 
  kbl(caption = "Size of each 3 latent class with all dicotomic scales NEU", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11, full_width = FALSE) %>% 
  column_spec (c(2), border_left = T) %>%
  column_spec(2:4, width = "10em") %>% print()

lc5 <- rbind(cbind(Cycle = "C1", MBAllScalesbycycleNEUc1cl5),
             cbind(Cycle = "C2", MBAllScalesbycycleNEUc2cl5),
             cbind(Cycle = "C3", MBAllScalesbycycleNEUc3cl5))
lc5$Cycle <- factor(lc5$Cycle, labels = c("1999", "2009", "2016"))
pc5 <- lc5 %>% na.omit() %>% 
  ggplot() + 
  geom_point(aes(x = param, y = value, group = category, color = category, shape = Cycle), size = 2) +
  geom_line(aes(param, value, group = interaction(category,Cycle), color = category)) + 
  theme_bw() +
  ggtitle("Highest probabilities for 3 latent classes \nwith binomial response, NEU countries") +
  labs(y="Response probabilities", color = "Response category")  +
  facet_grid(Class ~ ., switch = "y", labeller = label_wrap_gen(22)) +
  theme(legend.position = "top", legend.box="vertical", 
        strip.text.y = element_text(size = 8), 
        axis.title.x = element_blank(), axis.text.y = element_text(size = 8), 
        legend.title = element_text(size = 8), axis.text.x = element_text(angle = 90, size = 8)) +
  scale_color_discrete(labels = c("Disagree", "Agree")) +
  scale_y_continuous(breaks = c(0.25,0.5,0.75))
print(pc5)
#----------------
cat('\n')
cat('\n')
cat("# 4. All items with Covariate COUNTRY by Cycle  \n")
cat('\n')
cat('\n')
cat('## 4.1 European countries  \n')
cat('\n')
cat('\n')
cat('### 4.1.1 EU - Controlling by Country with 5 latent classes {.tabset .tabset-pills}  \n')
cat('\n')
cat('\n')
#Summary of Model fit and class size all models
rCountryv <- summaryLCAR2(Modellist=MAllScalesCountryEU, level1 = 1:3, level2 = 1:7, level3 = NA)
ncl = 5
#----------1999----
cat('\n')
cat('\n')
cat('#### 1999 \n')
cat('\n')
cat('\n')
#----C1
rCountryv$Fits$c1 %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit EU 1999 with covariate country", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(3,6,8), border_left = T) %>% 
  row_spec(5, bold = TRUE, italic = TRUE) %>% print()

MAllScalesCountryEUc1cl5 <- reshape2::melt(MAllScalesCountryEU$c1[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesCountryEUc1cl5, nclass = ncl)
MAllScalesCountryEUc1cl5$Class <- factor(MAllScalesCountryEUc1cl5$Class, 
                                         levels = c("class 5: ", "class 3: ", "class 4: ", "class 2: ", "class 1: "), 
                                         labels = classes5cnty)
graphclass(MAllScalesCountryEUc1cl5, nclass = ncl, title = paste0("LCA 1999 EU with ",ncl," classes"))

PredClassC1 <- cbind(ISC_lvRlca[ISC_lvRlca$cycle == "C1" & !ISC_lvRlca$COUNTRY %in% c(CNTne, CNT2cne), "COUNTRY"],
                     MAllScalesCountryEU$c1[[ncl]]$predclass)
t1 <- round(prop.table(table(PredClassC1),margin = 1)*100,1)
cat('\n')
cat('\n')
t1[,c(5,3,4,2,1)] %>% 
  kbl(caption = "Predicted classification (%) by country EU - 1999", col.names = classes5cnty) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>%
  column_spec(2:6, width = "10em") %>% 
  print()

#----------2009----
cat('\n')
cat('\n')
cat('#### 2009 \n')
cat('\n')
cat('\n')
#----C2
rCountryv$Fits$c2 %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit EU 2009 with covariate country", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(3,6,8), border_left = T) %>% 
  row_spec(5, bold = TRUE, italic = TRUE) %>% print()

MAllScalesCountryEUc2cl5 <- reshape2::melt(MAllScalesCountryEU$c2[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesCountryEUc2cl5, nclass = ncl)
MAllScalesCountryEUc2cl5$Class <- factor(MAllScalesCountryEUc2cl5$Class, 
                                         levels = c("class 1: ", "class 5: ", "class 3: ", "class 4: ", "class 2: "), 
                                         labels = classes5cnty)
graphclass(MAllScalesCountryEUc2cl5, nclass = ncl, title = paste0("LCA 2009 EU with ",ncl," classes"))

PredClassC2 <- cbind(ISC_lvRlca[ISC_lvRlca$cycle == "C2" & !ISC_lvRlca$COUNTRY %in% c(CNTne, CNT2cne), "COUNTRY"],
                     MAllScalesCountryEU$c2[[ncl]]$predclass)
t2 <- round(prop.table(table(PredClassC2),margin = 1)*100,1)
cat('\n')
cat('\n')
t2[,c(1,5,3,4,2)] %>% 
  kbl(caption = "Predicted classification (%) by country EU - 2009", col.names = classes5cnty) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec(2:6, width = "10em") %>% print()


#----------2016----
cat('\n')
cat('\n')
cat('#### 2016 \n')
cat('\n')
cat('\n')

#----C3
rCountryv$Fits$c3 %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit EU 2016 with covariate country", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(3,6,8), border_left = T) %>% 
  row_spec(5, bold = TRUE, italic = TRUE) %>% print()

MAllScalesCountryEUc3cl5 <- reshape2::melt(MAllScalesCountryEU$c3[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesCountryEUc3cl5, nclass = ncl)
MAllScalesCountryEUc3cl5$Class <- factor(MAllScalesCountryEUc3cl5$Class, 
                                         levels = c("class 5: ", "class 4: ", "class 3: ", "class 2: ", "class 1: "), 
                                         labels = classes5cnty)
graphclass(MAllScalesCountryEUc3cl5, nclass = ncl, title = paste0("LCA 2016 EU with ",ncl," classes"))


PredClassC3 <- cbind(ISC_lvRlca[ISC_lvRlca$cycle == "C3" & !ISC_lvRlca$COUNTRY %in% c(CNTne, CNT2cne), "COUNTRY"],
                     MAllScalesCountryEU$c3[[ncl]]$predclass)
t3 <- round(prop.table(table(PredClassC3),margin = 1)*100,1)
cat('\n')
cat('\n')
t3[,c(5,4,3,2,1)] %>% 
  kbl(caption = "Predicted classification (%) by country EU - 2016", col.names = classes5cnty) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec(2:6, width = "10em") %>% print()

#------------
cat('\n')
cat('\n')
cat('#### {-} \n')
cat('\n')
cat('\n')
#-----
rCountryv$Size[[1]] %>% filter(NClass == ncl) %>% mutate(Year = "1999") %>% 
  rename_with(~ classes5cnty[which(c("P.5", "P.3", "P.4", "P.2", "P.1") == .x)], .cols = c("P.5", "P.3", "P.4", "P.2", "P.1")) %>% 
  dplyr::select("Year", all_of(classes5cnty)) %>% 
  bind_rows(rCountryv$Size[[2]] %>% filter(NClass == ncl) %>% mutate(Year = "2009") %>% 
              rename_with(~ classes5cnty[which(c("P.1", "P.5", "P.3", "P.4", "P.2") == .x)], .cols = c("P.1", "P.5", "P.3", "P.4", "P.2")) %>% 
              dplyr::select("Year", all_of(classes5cnty))) %>% 
  bind_rows(rCountryv$Size[[3]] %>% filter(NClass == ncl) %>% mutate(Year = "2016") %>% 
              rename_with(~ classes5cnty[which(c("P.5", "P.4", "P.3", "P.2", "P.1") == .x)], .cols = c("P.5", "P.4", "P.3", "P.2", "P.1")) %>% 
              dplyr::select("Year", all_of(classes5cnty))) %>% 
  kbl(caption = "Size of each 5 latent class with all scales controlling by EU countries", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11, full_width = FALSE) %>% 
  column_spec (c(2), border_left = T) %>%
  column_spec(2:6, width = "10em") %>% print()

lc5 <- rbind(cbind(Cycle = "C1", MAllScalesCountryEUc1cl5),
             cbind(Cycle = "C2", MAllScalesCountryEUc2cl5),
             cbind(Cycle = "C3", MAllScalesCountryEUc3cl5))
lc5$Cycle <- factor(lc5$Cycle, labels = c("1999", "2009", "2016"))
pc5 <- lc5 %>% 
  ggplot() + 
  geom_point(aes(x = param, y = value, group = category, color = category, shape = Cycle), size = 2) +
  geom_line(aes(param, value, group = interaction(category,Cycle), color = category)) + 
  theme_bw() +
  ggtitle("Highest probabilities for 5 latent classes, \ncontrolling by EU countries") +
  labs(y="Response probabilities", color = "Response category")  +
  facet_grid(Class ~ ., switch = "y", labeller = label_wrap_gen(22)) +
  theme(legend.position = "top", legend.box="vertical", 
        strip.text.y = element_text(size = 8), 
        axis.title.x = element_blank(), axis.text.y = element_text(size = 8), 
        legend.title = element_text(size = 8), axis.text.x = element_text(angle = 90, size = 8)) +
  scale_color_discrete(labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"  )) +
  scale_y_continuous(breaks = c(0.25,0.5,0.75))
print(pc5)

#------------

cat('\n')
cat('\n')
cat('## 4.2 Non-European countries  \n')
cat('\n')
cat('\n')
#Summary of Model fit and class size all models
rNECountryv <- summaryLCAR2(Modellist=MAllScalesCountryNEU, level1 = 1:3, level2 = 1:6, level3 = NA)
cat('\n')
cat('\n')
cat('### 4.2.1 NEU - Controlling by Country with 5 latent classes {.tabset .tabset-pills}  \n')
cat('\n')
cat('\n')
#------------Results covariate country with 5 classes by cycle ----

ncl = 5
#----------1999----
cat('\n')
cat('\n')
cat('#### 1999 \n')
cat('\n')
cat('\n')

#----C1
rNECountryv$Fits$c1 %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit NEU 1999 with covariate country", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(3,6,8), border_left = T) %>% 
  row_spec(5, bold = TRUE, italic = TRUE) %>% print()

MAllScalesCountryNEUc1cl5 <- reshape2::melt(MAllScalesCountryNEU$c1[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesCountryNEUc1cl5, nclass = ncl)
MAllScalesCountryNEUc1cl5$Class <- factor(MAllScalesCountryNEUc1cl5$Class, 
                                         levels = c("class 4: ", "class 2: ", "class 5: ", "class 3: ", "class 1: "), 
                                         labels = classes5)
graphclass(MAllScalesCountryNEUc1cl5, nclass = ncl, title = paste0("LCA 1999 NEU with ",ncl," classes"))



PredClassC1 <- cbind(ISC_lvRlca[ISC_lvRlca$cycle == "C1" & ISC_lvRlca$COUNTRY %in% c(CNTne, CNT2cne), "COUNTRY"],
                     MAllScalesCountryNEU$c1[[ncl]]$predclass)
t1 <- round(prop.table(table(PredClassC1),margin = 1)*100,1)
cat('\n')
cat('\n')
t1[,c(4,2,5,3,1)] %>% 
  kbl(caption = "Predicted classification (%) by country NEU - 1999", col.names = classes5) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec(2:6, width = "10em") %>% print()

#----------2009----
cat('\n')
cat('\n')
cat('#### 2009 \n')
cat('\n')
cat('\n')
#----C2
rNECountryv$Fits$c2 %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit NEU 2009 with covariate country", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(3,6,8), border_left = T) %>% 
  row_spec(5, bold = TRUE, italic = TRUE) %>% print()


MAllScalesCountryNEUc2cl5 <- reshape2::melt(MAllScalesCountryNEU$c2[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesCountryNEUc2cl5, nclass = ncl)
MAllScalesCountryNEUc2cl5$Class <- factor(MAllScalesCountryNEUc2cl5$Class, 
                                         levels = c("class 1: ", "class 3: ", "class 2: ", "class 5: ", "class 4: "), 
                                         labels = classes5cnty)
graphclass(MAllScalesCountryNEUc2cl5, nclass = ncl, title = paste0("LCA 2009 NEU with ",ncl," classes"))

PredClassC2 <- cbind(ISC_lvRlca[ISC_lvRlca$cycle == "C2" & ISC_lvRlca$COUNTRY %in% c(CNTne, CNT2cne), "COUNTRY"],
                     MAllScalesCountryNEU$c2[[ncl]]$predclass)
t2 <- round(prop.table(table(PredClassC2),margin = 1)*100,1)
cat('\n')
cat('\n')
t2[,c(1,3,2,5,4)] %>% 
  kbl(caption = "Predicted classification (%) by country NEU - 2009", col.names = classes5cnty) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec(2:6, width = "10em") %>% print()
#----------2016----
cat('\n')
cat('\n')
cat('#### 2016 \n')
cat('\n')
cat('\n')
#----C3
rNECountryv$Fits$c3 %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit NEU 2016 with covariate country", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(3,6,8), border_left = T) %>% 
  row_spec(5, bold = TRUE, italic = TRUE) %>% print()

MAllScalesCountryNEUc3cl5 <- reshape2::melt(MAllScalesCountryNEU$c3[[ncl]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesCountryNEUc3cl5, nclass = ncl)
MAllScalesCountryNEUc3cl5$Class <- factor(MAllScalesCountryNEUc3cl5$Class, 
                                         levels = c("class 4: ", "class 1: ", "class 5: ", "class 2: ", "class 3: "), 
                                         labels = classes5NEU3)
graphclass(MAllScalesCountryNEUc3cl5, nclass = ncl, title = paste0("LCA 2016 NEU with ",ncl," classes"))

PredClassC3 <- cbind(ISC_lvRlca[ISC_lvRlca$cycle == "C3" & ISC_lvRlca$COUNTRY %in% c(CNTne, CNT2cne), "COUNTRY"],
                     MAllScalesCountryNEU$c3[[ncl]]$predclass)
t3 <- round(prop.table(table(PredClassC3),margin = 1)*100,1)
cat('\n')
cat('\n')
t3[,c(4,1,5,2,3)] %>% 
  kbl(caption = "Predicted classification (%) by country NEU - 2016", col.names = classes5NEU3) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec(2:6, width = "10em") %>% print()
#------
cat('\n')
cat('\n')
cat('#### {-} \n')
cat('\n')
cat('\n')
rNECountryv$Size[[1]] %>% filter(NClass == ncl) %>% mutate(Year = "1999") %>% 
  rename_with(~ classes5[which(c("P.4", "P.2", "P.5", "P.3", "P.1") == .x)], .cols = c("P.4", "P.2", "P.5", "P.3", "P.1")) %>% 
  dplyr::select("Year", all_of(classes5)) %>% 
  bind_rows(rNECountryv$Size[[2]] %>% filter(NClass == ncl) %>% mutate(Year = "2009") %>% 
              rename_with(~ classes5cnty[which(c("P.1", "P.3", "P.2", "P.5", "P.4") == .x)], .cols = c("P.1", "P.3", "P.2", "P.5", "P.4")) %>% 
              dplyr::select("Year", all_of(classes5cnty))) %>% 
  bind_rows(rNECountryv$Size[[3]] %>% filter(NClass == ncl) %>% mutate(Year = "2016") %>% 
              rename_with(~ classes5NEU3[which(c("P.4", "P.1", "P.5", "P.3", "P.2") == .x)], .cols = c("P.4", "P.1", "P.5", "P.3", "P.2")) %>% 
              dplyr::select("Year", all_of(classes5NEU3))) %>% 
  kbl(caption = "Size of each 5 latent class with all scales controlling by NEU countries", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11, full_width = FALSE) %>% 
  column_spec (c(2), border_left = T) %>%
  column_spec(2:6, width = "10em") %>% print()

lc5 <- rbind(cbind(Cycle = "C1", MAllScalesCountryNEUc1cl5),
             cbind(Cycle = "C2", MAllScalesCountryNEUc2cl5),
             cbind(Cycle = "C3", MAllScalesCountryNEUc3cl5))
lc5$Cycle <- factor(lc5$Cycle, labels = c("1999", "2009", "2016"))
pc5 <- lc5 %>% 
  ggplot() + 
  geom_point(aes(x = param, y = value, group = category, color = category, shape = Cycle), size = 2) +
  geom_line(aes(param, value, group = interaction(category,Cycle), color = category)) + 
  theme_bw() +
  ggtitle("Highest probabilities for 5 latent classes, \ncontrolling by NEU countries") +
  labs(y="Response probabilities", color = "Response category")  +
  facet_grid(Class ~ ., switch = "y", labeller = label_wrap_gen(22)) +
  theme(legend.position = "top", legend.box="vertical", 
        strip.text.y = element_text(size = 8), 
        axis.title.x = element_blank(), axis.text.y = element_text(size = 8), 
        legend.title = element_text(size = 8), axis.text.x = element_text(angle = 90, size = 8)) +
  scale_color_discrete(labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"  )) +
  scale_y_continuous(breaks = c(0.25,0.5,0.75))
print(pc5)

#------------
cat('\n')
cat('\n')
cat("# 5. All items with 4 response categories and Covariate CYCLE   \n")
cat('\n')
cat('\n')
cat('## 5.1 European countries {.tabset .tabset-pills} \n')
cat('\n')
cat('\n')
#------------
#Summary of Model fit and class size all models
rTime <- summaryLCAR2(Modellist=MAllScalesCyclesNumEU, level1 = 1:7, level2 = NA, level3 = NA)
rTime$Fits[[7]] %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit EU with covariate Time (cycle)", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(2,5,7), border_left = T) %>% 
  row_spec(4:5, bold = TRUE, italic = TRUE) %>% print()

#4 cl
MAllScalesCyclesNumEUcl4 <- reshape2::melt(MAllScalesCyclesNumEU[[4]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesCyclesNumEUcl4, nclass = 4)
MAllScalesCyclesNumEUcl4$Class <- factor(MAllScalesCyclesNumEUcl4$Class, 
                                         levels = c("class 4: ", "class 3: ", "class 1: ", "class 2: "), 
                                         labels = classes4)
#5 cl
MAllScalesCyclesNumEUcl5 <- reshape2::melt(MAllScalesCyclesNumEU[[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesCyclesNumEUcl5, nclass = 5)
MAllScalesCyclesNumEUcl5$Class <- factor(MAllScalesCyclesNumEUcl5$Class, 
                                         levels = c("class 3: ", "class 1: ", "class 5: ", "class 4: ", "class 2: "), 
                                         labels = classes5)

lc5 <- rbind(cbind(Nclass = "Classes 4", MAllScalesCyclesNumEUcl4),
             cbind(Nclass = "Classes 5", MAllScalesCyclesNumEUcl5))
labels_x <- NULL
for (each in levels(lc5$param)){
  labels_x[each] <- paste(attr(ISC_lvRlca[[each]], "label"), "-", each)
}
labels_x <- unlist(labels_x)
pc5 <- lc5 %>% group_by(Nclass, Class, param) %>% filter(value == max(value)) %>% 
  ggplot() + 
  geom_point(aes(x = param, y = value, group = Class, color = Class), size = 2) +
  geom_line(aes(param, value, group = Class, color = Class)) + 
  theme_bw() +
  ggtitle("Highest probabilities for 4 and 5 latent classes, \ncontrolling by cycles, EU countries") +
  labs(y="Response probabilities", color = "Classes")  +
  facet_grid(Nclass ~ ., switch = "y") +
  theme(legend.position = "top", axis.title.x = element_blank(), 
        strip.text.y = element_text(size = 8), 
        axis.text.y = element_text(size = 8), 
        legend.title = element_text(size = 8), axis.text.x = element_text(angle = 90, vjust = 0, hjust = 1, size = 8)) +
  guides(color=guide_legend(nrow=3,byrow=TRUE)) +
  scale_y_continuous(breaks = c(0.25,0.5,0.75)) +
  scale_x_discrete(labels = str_wrap(labels_x,35))
print(pc5)

# logit coefficients 
#   Fit for 4 latent classes: 
#   2 / 1 
#               Coefficient  Std. error  t value  Pr(>|t|)
# (Intercept)     0.22036     0.03203    6.879         0
# cycle          -0.51019     0.01462  -34.906         0
#   3 / 1 
#               Coefficient  Std. error  t value  Pr(>|t|)
# (Intercept)     1.01745     0.02655   38.324         0
# cycle          -0.52366     0.01183  -44.283         0
#   4 / 1 
#               Coefficient  Std. error  t value  Pr(>|t|)
# (Intercept)     0.06087     0.02778    2.191     0.028
# cycle           0.02293     0.01159    1.979     0.048

#------------
cat('\n')
cat('\n')
cat('### 5.1.1 EU - Cycle as covariate, 4 latent classes  \n')
cat('\n')
cat('\n')
#------------5.1.1 Results covariate cycle with 4 classes----
graphclass(MAllScalesCyclesNumEUcl4, nclass = 4, title = paste0("LCA all cycles EU with 4 classes"))

#-----------
cat('\n')
cat('\n')
cat('### 5.1.2 EU - Cycle as covariate, 5 latent classes  \n')
cat('\n')
cat('\n')
#------------5.1.2 Results covariate cycle with 5 classes----

graphclass(MAllScalesCyclesNumEUcl5, nclass = 5, title = paste0("LCA all cycles EU with 5 classes"))

cat('\n')
cat('\n')
cat('## {-}  \n')
cat('\n')
cat('\n')

rTime$Size[[7]] %>% filter(NClass == 4) %>% 
  rename_with(~ classes4[which(c("P.4", "P.3", "P.1", "P.2") == .x)], .cols = c("P.4", "P.3", "P.1", "P.2")) %>% 
  dplyr::select("NClass", all_of(classes4)) %>% 
  kbl(caption = "Size of each 4 latent class with all scales controlling by cycle, NEU countries", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11, full_width = FALSE) %>% 
  column_spec (c(2), border_left = T) %>%
  column_spec(2:5, width = "10em") %>% print()

rTime$Size[[7]] %>% filter(NClass == 5) %>% 
  rename_with(~ classes5[which(c("P.3", "P.1", "P.5", "P.4", "P.2") == .x)], .cols = c("P.3", "P.1", "P.5", "P.4", "P.2")) %>% 
  dplyr::select("NClass", all_of(classes5)) %>% 
  kbl(caption = "Size of each 5 latent class with all scales controlling by cycle, EU countries", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11, full_width = FALSE) %>% 
  column_spec (c(2), border_left = T) %>%
  column_spec(2:6, width = "10em") %>% print()

# plot prediction of change 
pidmat <- cbind(1,c(1:3))
exb4 <- exp(pidmat %*% MAllScalesCyclesNumEU[[4]]$coeff)
odd4<- reshape2::melt((cbind(1,exb4)/(1+rowSums(exb4))))
odd4$Var2<-factor(ifelse(odd4$Var2==4,1,
                   ifelse(odd4$Var2==3,2,
                          ifelse(odd4$Var2==1,3,
                                 ifelse(odd4$Var2==2,5,4)))), levels = 1:5, labels = classes5)
exb5 <- exp(pidmat %*% MAllScalesCyclesNumEU[[5]]$coeff)
odd5<- reshape2::melt((cbind(1,exb5)/(1+rowSums(exb5))))
odd5$Var2<-factor(ifelse(odd5$Var2==3,1,
                   ifelse(odd5$Var2==1,2,
                          ifelse(odd5$Var2==5,3,
                                 ifelse(odd5$Var2==4,4,
                                        ifelse(odd5$Var2==2,5,NA))))), labels = classes5)
odd<-rbind(cbind(NClass = "Classes 4", odd4),
           cbind(NClass = "Classes 5", odd5))
g1 <- odd %>% 
  mutate(Var2 = factor(Var2)) %>% 
  ggplot(aes(x=factor(Var1), y=value)) +  
  geom_line(aes(group = Var2, linetype = Var2, color = Var2), size = 1) +
  facet_wrap(NClass ~ .) +
  scale_x_discrete(labels = c("1999", "2009", "2016")) +
  scale_y_continuous(limits = c(0,0.75)) +
  theme_bw() + 
  labs(title = "Time as a predictor of attitude with 4 and 5 classes EU", x = "Year", 
       y = "Probability of latent class membership") +
  theme(legend.position = "bottom", legend.title = element_blank(),
        title = element_text(size = 9), legend.key.size = unit(0.3,"cm")) + 
  guides(linetype = guide_legend(nrow=3,byrow=TRUE))
print(g1)

#------------
cat('\n')
cat('\n')
cat('## 5.2 Non-European countries {.tabset .tabset-pills} \n')
cat('\n')
cat('\n')
#------------
#Summary of Model fit and class size all models
rTime <- summaryLCAR2(Modellist=MAllScalesCyclesNEU, level1 = 1:7, level2 = NA, level3 = NA)
rTime$Fits[[7]] %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit NEU with covariate Time (cycle)", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(2,5,7), border_left = T) %>% 
  row_spec(4:5, bold = TRUE, italic = TRUE) %>% print()

#4 cl
MAllScalesCyclesNEUcl4 <- reshape2::melt(MAllScalesCyclesNEU[[4]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesCyclesNEUcl4, nclass = 4)
MAllScalesCyclesNEUcl4$Class <- factor(MAllScalesCyclesNEUcl4$Class, 
                                       levels = c("class 2: ", "class 4: ", "class 1: ", "class 3: "), 
                                       labels = classes4NEU)
#5 cl
MAllScalesCyclesNEUcl5 <- reshape2::melt(MAllScalesCyclesNEU[[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesCyclesNEUcl5, nclass = 5)
MAllScalesCyclesNEUcl5$Class <- factor(MAllScalesCyclesNEUcl5$Class, 
                                       levels = c("class 4: ", "class 1: ", "class 5: ", "class 3: ", "class 2: "), 
                                       labels = classes5)

lc5 <- rbind(cbind(Nclass = "Classes 4", MAllScalesCyclesNEUcl4),
             cbind(Nclass = "Classes 5", MAllScalesCyclesNEUcl5))
labels_x <- NULL
for (each in levels(lc5$param)){
  labels_x[each] <- paste(attr(ISC_lvRlca[[each]], "label"), "-", each)
}
labels_x <- unlist(labels_x)
pc5 <- lc5 %>% group_by(Nclass, Class, param) %>% filter(value == max(value)) %>% 
  ggplot() + 
  geom_point(aes(x = param, y = value, group = Class, color = Class), size = 2) +
  geom_line(aes(param, value, group = Class, color = Class)) + 
  theme_bw() +
  labs(y="Response probabilities", color = "Classes")  +
  ggtitle("Highest probabilities for 4 and 5 latent classes, \ncontrolling by cycles, NEU countries") +
  facet_grid(Nclass ~ ., switch = "y") +
  theme(legend.position = "top", axis.title.x = element_blank(), 
        strip.text.y = element_text(size = 8), 
        axis.text.y = element_text(size = 8), 
        legend.title = element_text(size = 8), axis.text.x = element_text(angle = 90, vjust = 0, hjust = 1, size = 8)) +
  guides(color=guide_legend(nrow=3,byrow=TRUE)) +
  scale_y_continuous(breaks = c(0.25,0.5,0.75)) +
  scale_x_discrete(labels = str_wrap(labels_x,35))
print(pc5)

# logit coefficients 
# [,1]       [,2]       [,3]
# (Intercept) -0.34374226  0.8752519  1.0967501
# cycle        0.08937802 -0.5660542 -0.6568241
#-------------
cat('\n')
cat('\n')
cat('### 5.2.1 NEU - Cycle as covariate, 4 latent classes  \n')
cat('\n')
cat('\n')
#------------5.2.1 Results covariate cycle with 4-5 classes----

graphclass(MAllScalesCyclesNEUcl4, nclass = 4, title = paste0("LCA all cycles NEU with 4 classes"))

#-----------
cat('\n')
cat('\n')
cat('### 5.2.2 NEU - Cycle as covariate, 5 latent classes  \n')
cat('\n')
cat('\n')
#------------5.2.2 Results covariate cycle with 4-5 classes----
graphclass(MAllScalesCyclesNEUcl5, nclass = 5, title = paste0("LCA all cycles NEU with 5 classes"))

cat('\n')
cat('\n')
cat('## {-}  \n')
cat('\n')
cat('\n')

rTime$Size[[7]] %>% filter(NClass == 4) %>% 
  rename_with(~ classes4NEU[which(c("P.2", "P.4", "P.1", "P.3") == .x)], .cols = c("P.2", "P.4", "P.1", "P.3")) %>% 
  dplyr::select("NClass", all_of(classes4NEU)) %>% 
  kbl(caption = "Size of each 4 latent class with all scales controlling by cycle, NEU countries", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11, full_width = FALSE) %>% 
  column_spec (c(2), border_left = T) %>%
  column_spec(2:5, width = "10em") %>% print()

rTime$Size[[7]] %>% filter(NClass == 5) %>% 
  rename_with(~ classes5[which(c("P.4", "P.1", "P.5", "P.3", "P.2") == .x)], .cols = c("P.4", "P.1", "P.5", "P.3", "P.2")) %>% 
  dplyr::select("NClass", all_of(classes5)) %>% 
  kbl(caption = "Size of each 5 latent class with all scales controlling by cycle, NEU countries", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11, full_width = FALSE) %>% 
  column_spec (c(2), border_left = T) %>%
  column_spec(2:6, width = "10em") %>% print()

# plot prediction of change 
pidmat <- cbind(1,c(1:3))
exb4 <- exp(pidmat %*% MAllScalesCyclesNEU[[4]]$coeff)
odd4<- reshape2::melt((cbind(1,exb4)/(1+rowSums(exb4))))
odd4$Var2<-factor(ifelse(odd4$Var2==2,1,
                         ifelse(odd4$Var2==4,2,
                                ifelse(odd4$Var2==1,3,
                                       ifelse(odd4$Var2==3,4,5)))), levels = 1:5, labels = classes5)
exb5 <- exp(pidmat %*% MAllScalesCyclesNEU[[5]]$coeff)
odd5<- reshape2::melt((cbind(1,exb5)/(1+rowSums(exb5))))
odd5$Var2<-factor(ifelse(odd5$Var2==4,1,
                         ifelse(odd5$Var2==1,2,
                                ifelse(odd5$Var2==5,3,
                                       ifelse(odd5$Var2==3,4,
                                              ifelse(odd5$Var2==2,5,NA))))), labels = classes5)
odd<-rbind(cbind(NClass = "Classes 4", odd4),
           cbind(NClass = "Classes 5", odd5))
g1 <- odd %>% 
  mutate(Var2 = factor(Var2)) %>% 
  ggplot(aes(x=factor(Var1), y=value)) +  
  geom_line(aes(group = Var2, linetype = Var2, color = Var2), size = 1) +
  facet_wrap(NClass ~ .) +
  scale_x_discrete(labels = c("1999", "2009", "2016")) +
  scale_y_continuous(limits = c(0,0.75)) +
  theme_bw() + 
  labs(title = "Time as a predictor of attitude with 4 and 5 classes NEU", x = "Year", 
       y = "Probability of latent class membership") +
  theme(legend.position = "bottom", legend.title = element_blank(),
        title = element_text(size = 9), legend.key.size = unit(0.3,"cm")) + 
  guides(linetype = guide_legend(nrow=3,byrow=TRUE))
print(g1)
#----------------

cat('\n')
cat('\n')
cat("# 6. All items with Covariates COUNTRY and CYCLE  \n")
cat('\n')
cat('\n')
cat('## 6.1 European countries {.tabset .tabset-pills} \n')
cat('\n')
cat('\n')
rCntryTime <- summaryLCAR2(Modellist=MAllScalesCountryCycleNumEU, level1 = 1:7, level2 = NA, level3 = NA)
rCntryTime$Fits[[7]] %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit EU with covariates Time and Country", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(2,5,7), border_left = T) %>% 
  row_spec(4:5, bold = TRUE, italic = TRUE) %>% print()

#------------6.1 Results covariate country and cycle with 4-5-6 classes----
#4 cl
MAllScalesCountryCycleNumEUcl4 <- reshape2::melt(MAllScalesCountryCycleNumEU[[4]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesCountryCycleNumEUcl4, nclass = 4)
MAllScalesCountryCycleNumEUcl4$Class <- factor(MAllScalesCountryCycleNumEUcl4$Class, 
                                    levels = c("class 4: ", "class 3: ", "class 2: ", "class 1: "), 
                                    labels = classes4)
#5 cl
MAllScalesCountryCycleNumEUcl5 <- reshape2::melt(MAllScalesCountryCycleNumEU[[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesCountryCycleNumEUcl5, nclass = 5)
MAllScalesCountryCycleNumEUcl5$Class <- factor(MAllScalesCountryCycleNumEUcl5$Class, 
                                          levels = c("class 3: ", "class 2: ", "class 1: ", "class 5: ", "class 4: "), 
                                          labels = classes5)

lc5 <- rbind(cbind(Nclass = "Classes 4", MAllScalesCountryCycleNumEUcl4),
             cbind(Nclass = "Classes 5", MAllScalesCountryCycleNumEUcl5))
labels_x <- NULL
for (each in levels(lc5$param)){
  labels_x[each] <- paste(attr(ISC_lvRlca[[each]], "label"), "-", each)
}
labels_x <- unlist(labels_x)
pc5 <- lc5 %>% group_by(Nclass, Class, param) %>% filter(value == max(value)) %>% 
  ggplot() + 
  geom_point(aes(x = param, y = value, group = Class, color = Class), size = 2) +
  geom_line(aes(param, value, group = Class, color = Class)) + 
  theme_bw() +
  ggtitle("Highest probabilities for 4 and 5 latent classes, \ncontrolling by cycles and EU countries") +
  labs(y="Response probabilities", color = "Classes")  +
  facet_grid(Nclass ~ ., switch = "y") +
  theme(legend.position = "top", axis.title.x = element_blank(), 
        strip.text.y = element_text(size = 8), axis.text.y = element_text(size = 8), 
        legend.title = element_text(size = 8), axis.text.x = element_text(angle = 90, vjust = 0, hjust = 1, size = 7)) +
  guides(color=guide_legend(nrow=3,byrow=TRUE)) +
  scale_y_continuous(breaks = c(0.25,0.5,0.75)) +
  scale_x_discrete(labels = str_wrap(labels_x,30))
print(pc5)
#----------------
cat('\n')
cat('\n')
cat('### 6.1.1 EU - Country and Cycle as covariates, 4 latent classes  \n')
cat('\n')
cat('\n')
#----------------
graphclass(MAllScalesCountryCycleNumEUcl4, nclass = 4, title = paste0("LCA all cycles EU with 4 classes"))

rCntryTime$Size[[7]] %>% filter(NClass == 4) %>% 
  rename_with(~ classes4[which(c("P.4", "P.3", "P.2", "P.1") == .x)], .cols = c("P.4", "P.3", "P.2", "P.1")) %>% 
  dplyr::select("NClass", all_of(classes4)) %>% 
  kbl(caption = "Size of each 4 latent class with all scales controlling by cycle and EU countries", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11, full_width = FALSE) %>% 
  column_spec (c(2), border_left = T) %>%
  column_spec(2:5, width = "10em") %>% print()

nas <- rownames(MAllScalesCountryCycleNumEU[[4]]$coeff)
cnty<- str_remove(nas[!grepl("cycle|(Intercept)",nas)], "COUNTRY")
g1 <- list()
for(cnt in 0:length(cnty)){
  if(cnt == 0) { pidmat <- cbind(1, c(1:3), rep(0,3), replicate(length(cnty)-1,rep(0,3)), 
                                 c(1:3)*rep(0,3), replicate(length(cnty)-1,rep(0,3))) }  
  else if(cnt == 1) { pidmat <- cbind(1, c(1:3), rep(1,3), replicate(length(cnty)-1,rep(0,3)), 
                               c(1:3)*rep(1,3), replicate(length(cnty)-1,rep(0,3))) } 
  else if(cnt == length(cnty)) { pidmat <- cbind(1, c(1:3), replicate(length(cnty)-1,rep(0,3)), rep(1,3), 
                                      replicate(length(cnty)-1,rep(0,3)),c(1:3)*rep(1,3)) } 
  else  { pidmat <- cbind(1, c(1:3), replicate(cnt-1,rep(0,3)), rep(1,3), replicate(length(cnty)-cnt,rep(0,3)), 
                             replicate(cnt-1,rep(0,3)), c(1:3)*rep(1,3), replicate(length(cnty)-cnt,rep(0,3)))}
  exb <- exp(pidmat %*% MAllScalesCountryCycleNumEU[[4]]$coeff)
  odd<- reshape2::melt((cbind(1,exb)/(1+rowSums(exb))))
  odd$Var2<-factor(ifelse(odd$Var2==4,1,
                           ifelse(odd$Var2==3,2,
                                  ifelse(odd$Var2==2,3,
                                         ifelse(odd$Var2==1,4,NA)))), levels = 1:4, labels = classes4)
  g1[[cnt+1]] <- odd %>%  #study order of (cbind(1,exb)/(1+rowSums(exb))) with names assigned to assign labels
    mutate(Var2 = factor(Var2, labels = classes4)) %>%
    ggplot(aes(x=factor(Var1), y=value)) +
    geom_line(aes(group = Var2, linetype = Var2, color = Var2), size = 1) +
    theme_bw() + 
    scale_x_discrete(labels = c("1999", "2009", "2016")) +
    scale_y_continuous(limits = c(0,0.75), breaks = c(0,0.25,0.5,0.75)) +
    labs(title = ifelse(cnt==0, "BFL",paste(cnty[cnt]))) + #add reference country name
    theme(legend.position = "none", legend.title = element_blank(), 
          plot.title = element_text(size = 9, margin = margin(b=-10,l=0)),
          axis.title = element_blank(), axis.text = element_text(size = 6)) 
}
leg <- reshape2::melt((cbind(1,exb)/(1+rowSums(exb)))) %>%  #study order of (cbind(1,exb)/(1+rowSums(exb))) with names assigned to assign labels
  mutate(Var2 = factor(Var2, labels = str_wrap(classes4,30))) %>%
  ggplot(aes(x=factor(Var1), y=value)) +
  geom_line(aes(group = Var2, linetype = Var2, color = Var2), size = 1) +
  theme_bw() + theme(legend.position = "bottom", legend.title = element_blank(), legend.key.size = unit(0.3, "cm")) +
  guides(linetype=guide_legend(nrow=4,byrow=TRUE)) 
g1[[(length(cnty)+2)]]<-g_legend(leg)
invisible(do.call(grid.arrange, c(g1, ncol=3, top = "Time as a predictor of attitude with 4 classes by EU countries", 
                       left = "Probability of latent class membership")))
#----------------
cat('\n')
cat('\n')
cat('### 6.1.2 EU - Country and Cycle as covariates, 5 latent classes  \n')
cat('\n')
cat('\n')
#----------------
#5 classes
graphclass(MAllScalesCountryCycleNumEUcl5, nclass = 5, title = paste0("LCA all cycles EU with 5 classes"))

rCntryTime$Size[[7]] %>% filter(NClass == 5) %>% 
  rename_with(~ classes5[which(c("P.3", "P.2", "P.1", "P.5", "P.4") == .x)], .cols = c("P.3", "P.2", "P.1", "P.5", "P.4")) %>% 
  dplyr::select("NClass", all_of(classes5)) %>% 
  kbl(caption = "Size of each 5 latent class with all scales controlling by cycle and EU countries", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11, full_width = FALSE) %>% 
  column_spec (c(2), border_left = T) %>%
  column_spec(2:6, width = "10em") %>% print()

nas <- rownames(MAllScalesCountryCycleNumEU[[5]]$coeff)
cnty<- str_remove(nas[!grepl("cycle|(Intercept)",nas)], "COUNTRY")
g2 <- list()
for(cnt in 0:length(cnty)){
  if(cnt == 0) { pidmat <- cbind(1, c(1:3), rep(0,3), replicate(length(cnty)-1,rep(0,3)), 
                                 c(1:3)*rep(0,3), replicate(length(cnty)-1,rep(0,3))) }  
  else if(cnt == 1) { pidmat <- cbind(1, c(1:3), rep(1,3), replicate(length(cnty)-1,rep(0,3)), 
                                 c(1:3)*rep(1,3), replicate(length(cnty)-1,rep(0,3))) } 
  else if(cnt == length(cnty)) { pidmat <- cbind(1, c(1:3), replicate(length(cnty)-1,rep(0,3)), rep(1,3), 
                                                 replicate(length(cnty)-1,rep(0,3)),c(1:3)*rep(1,3)) } 
  else  { pidmat <- cbind(1, c(1:3), replicate(cnt-1,rep(0,3)), rep(1,3), replicate(length(cnty)-cnt,rep(0,3)), 
                          replicate(cnt-1,rep(0,3)), c(1:3)*rep(1,3), replicate(length(cnty)-cnt,rep(0,3)))}
  exb <- exp(pidmat %*% MAllScalesCountryCycleNumEU[[5]]$coeff)
  odd<- reshape2::melt((cbind(1,exb)/(1+rowSums(exb))))
  odd$Var2<-factor(ifelse(odd$Var2==3,1,
                          ifelse(odd$Var2==2,2,
                                 ifelse(odd$Var2==1,3,
                                        ifelse(odd$Var2==5,4,
                                               ifelse(odd$Var2==4,5,NA))))), levels = 1:5, labels = classes5)
  g2[[cnt+1]] <- odd %>%  #study order of (cbind(1,exb)/(1+rowSums(exb))) with names assigned to assign labels
    mutate(Var2 = factor(Var2, labels = classes5)) %>%
    ggplot(aes(x=factor(Var1), y=value)) +
    geom_line(aes(group = Var2, linetype = Var2, color = Var2), size = 1) +
    theme_bw() + 
    scale_x_discrete(labels = c("1999", "2009", "2016")) +
    scale_y_continuous(limits = c(0,0.75), breaks = c(0,0.25,0.5,0.75)) +
    labs(title = ifelse(cnt==0, "BFL",paste(cnty[cnt]))) +
    theme(legend.position = "none", title = element_text(size = 7),
          axis.title = element_blank(), axis.text = element_text(size = 6), 
          plot.title = element_text(size = 9, margin = margin(b=-10,l=0)))
}
leg <- reshape2::melt((cbind(1,exb)/(1+rowSums(exb)))) %>%  #study order of (cbind(1,exb)/(1+rowSums(exb))) with names assigned to assign labels
  mutate(Var2 = factor(Var2, labels = str_wrap(classes5,30))) %>%
  ggplot(aes(x=factor(Var1), y=value)) +
  geom_line(aes(group = Var2, linetype = Var2, color = Var2), size = 1) +
  theme_bw() + 
  theme(legend.position = "bottom", legend.title = element_blank(), legend.key.size = unit(0.2, "cm")) +
  guides(linetype=guide_legend(nrow=7,byrow=TRUE)) 

g2[[(length(cnty)+2)]] <- g_legend(leg)
invisible(do.call(grid.arrange, c(g2, ncol=3, top = "Time as a predictor of attitude with 5 classes by EU countries", 
                       left = "Probability of latent class membership")))
#----------------
cat('\n')
cat('\n')
cat('## 6.2 Non-European countries {.tabset .tabset-pills} \n')
cat('\n')
cat('\n')
rCntryTime <- summaryLCAR2(Modellist=MAllScalesCountryCycleNEU, level1 = 1:7, level2 = NA, level3 = NA)
rCntryTime$Fits[[7]] %>% dplyr::select(-Chisq, -npar) %>% 
  kbl(caption = "Model fit NEU with covariates Time and Country", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(2,5,7), border_left = T) %>% 
  row_spec(4:5, bold = TRUE, italic = TRUE) %>% print()

#---------------
#4 cl
MAllScalesCountryCycleNumNEUcl4 <- reshape2::melt(MAllScalesCountryCycleNEU[[4]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesCountryCycleNumNEUcl4, nclass = 4)
MAllScalesCountryCycleNumNEUcl4$Class <- factor(MAllScalesCountryCycleNumNEUcl4$Class, 
                                                levels = c("class 1: ", "class 4: ", "class 2: ", "class 3: "), 
                                                labels = classes4NEU)
#5 cl
MAllScalesCountryCycleNumNEUcl5 <- reshape2::melt(MAllScalesCountryCycleNEU[[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesCountryCycleNumNEUcl5, nclass = 5)
MAllScalesCountryCycleNumNEUcl5$Class <- factor(MAllScalesCountryCycleNumNEUcl5$Class, 
                                                levels = c("class 3: ", "class 5: ", "class 2: ", "class 4: ", "class 1: "), 
                                                labels = classes5NEU3)

lc5 <- rbind(cbind(Nclass = "Classes 4", MAllScalesCountryCycleNumNEUcl4),
             cbind(Nclass = "Classes 5", MAllScalesCountryCycleNumNEUcl5))
labels_x <- NULL
for (each in levels(lc5$param)){
  labels_x[each] <- paste(attr(ISC_lvRlca[[each]], "label"), "-", each)
}
labels_x <- unlist(labels_x)
pc5 <- lc5 %>% group_by(Nclass, Class, param) %>% filter(value == max(value)) %>% 
  ggplot() + 
  geom_point(aes(x = param, y = value, group = Class, color = Class), size = 2) +
  geom_line(aes(param, value, group = Class, color = Class)) + 
  theme_bw() +
  ggtitle("Highest probabilities for 4 and 5 latent classes, \ncontrolling by cycles and NEU countries") +
  labs(y="Response probabilities", color = "Classes")  +
  facet_grid(Nclass ~ ., switch = "y") +
  theme(legend.position = "top", axis.title.x = element_blank(), 
        strip.text.y = element_text(size = 8), axis.text.y = element_text(size = 8), 
        legend.title = element_text(size = 8), axis.text.x = element_text(angle = 90, vjust = 0, hjust = 1, size = 7)) +
  guides(fill=guide_legend(nrow=3,byrow=TRUE)) +
  scale_y_continuous(breaks = c(0.25,0.5,0.75)) +
  scale_x_discrete(labels = str_wrap(labels_x,30))
print(pc5)

#------------6.2.1 Results covariate country and cycle with 4-5-6 classes----
cat('\n')
cat('\n')
cat('### 6.2.1 NEU - Country and Cycle as covariates, 4 latent classes  \n')
cat('\n')
cat('\n')
#----------------
#4 cl
graphclass(MAllScalesCountryCycleNumNEUcl4, nclass = 4, title = paste0("LCA all cycles NEU with 4 classes"))

rCntryTime$Size[[7]] %>% filter(NClass == 4) %>% 
  rename_with(~ classes4NEU[which(c("P.1", "P.4", "P.2", "P.3") == .x)], .cols = c("P.1", "P.4", "P.2", "P.3")) %>% 
  dplyr::select("NClass", all_of(classes4NEU)) %>% 
  kbl(caption = "Size of each 4 latent class with all scales controlling by cycle and NEU countries", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11, full_width = FALSE) %>% 
  column_spec (c(2), border_left = T) %>%
  column_spec(2:5, width = "10em") %>% print()

nas <- rownames(MAllScalesCountryCycleNEU[[4]]$coeff)
cnty<- str_remove(nas[!grepl("cycle|(Intercept)",nas)], "COUNTRY")
g1 <- list()
for(cnt in 0:length(cnty)){
  if(cnt == 0) { pidmat <- cbind(1, c(1:3), rep(0,3), replicate(length(cnty)-1,rep(0,3)), 
                                 c(1:3)*rep(0,3), replicate(length(cnty)-1,rep(0,3))) }  
  else if(cnt == 1) { pidmat <- cbind(1, c(1:3), rep(1,3), replicate(length(cnty)-1,rep(0,3)), 
                                      c(1:3)*rep(1,3), replicate(length(cnty)-1,rep(0,3))) } 
  else if(cnt == length(cnty)) { pidmat <- cbind(1, c(1:3), replicate(length(cnty)-1,rep(0,3)), rep(1,3), 
                                                 replicate(length(cnty)-1,rep(0,3)),c(1:3)*rep(1,3)) } 
  else  { pidmat <- cbind(1, c(1:3), replicate(cnt-1,rep(0,3)), rep(1,3), replicate(length(cnty)-cnt,rep(0,3)), 
                          replicate(cnt-1,rep(0,3)), c(1:3)*rep(1,3), replicate(length(cnty)-cnt,rep(0,3)))}
  exb <- exp(pidmat %*% MAllScalesCountryCycleNEU[[4]]$coeff)
  odd<- reshape2::melt((cbind(1,exb)/(1+rowSums(exb))))
  odd$Var2<-factor(ifelse(odd$Var2==1,1,
                          ifelse(odd$Var2==4,2,
                                 ifelse(odd$Var2==2,3,
                                        ifelse(odd$Var2==3,4,NA)))), levels = 1:4, labels = classes4NEU)
  g1[[cnt+1]] <- odd %>%  #study order of (cbind(1,exb)/(1+rowSums(exb))) with names assigned to assign labels
    mutate(Var2 = factor(Var2, labels = classes4NEU)) %>%
    ggplot(aes(x=factor(Var1), y=value)) +
    geom_line(aes(group = Var2, linetype = Var2, color = Var2), size = 1) +
    theme_bw() + 
    scale_x_discrete(labels = c("1999", "2009", "2016")) +
    scale_y_continuous(limits = c(0,0.75), breaks = c(0,0.25,0.5,0.75)) +
    labs(title = ifelse(cnt==0, "CHL",paste(cnty[cnt]))) +
    theme(legend.position = "none", legend.title = element_blank(), 
          plot.title = element_text(size = 7, margin = margin(b=-10,l=0)),
          axis.title = element_blank(), axis.text = element_text(size = 6)) +
    guides(linetype=guide_legend(nrow=1,byrow=TRUE)) 
}
leg <- reshape2::melt((cbind(1,exb)/(1+rowSums(exb)))) %>%  #study order of (cbind(1,exb)/(1+rowSums(exb))) with names assigned to assign labels
  mutate(Var2 = factor(Var2, labels = str_wrap(classes4NEU,30))) %>%
  ggplot(aes(x=factor(Var1), y=value)) +
  geom_line(aes(group = Var2, linetype = Var2, color = Var2), size = 1) +
  theme_bw() + theme(legend.position = "bottom", legend.title = element_blank(), legend.key.size = unit(0.3, "cm"))+
  guides(linetype=guide_legend(nrow=5,byrow=TRUE)) 

g1[[(length(cnty)+2)]]<-g_legend(leg)
invisible(do.call(grid.arrange, c(g1, ncol=3, top = "Time as a predictor of attitude with 4 classes by NEU countries", 
                       left = "Probability of latent class membership")))
#----------------
cat('\n')
cat('\n')
cat('### 6.2.2 NEU - Country and Cycle as covariates, 5 latent classes  \n')
cat('\n')
cat('\n')
#----------------
#5 cl
graphclass(MAllScalesCountryCycleNumNEUcl5, nclass = 5, title = paste0("LCA all cycles NEU with 5 classes"))

rCntryTime$Size[[7]] %>% filter(NClass == 5) %>% 
  rename_with(~ classes5NEU3[which(c("P.3", "P.5", "P.2", "P.4", "P.1") == .x)], .cols = c("P.3", "P.5", "P.2", "P.4", "P.1")) %>% 
  dplyr::select("NClass", all_of(classes5NEU3)) %>% 
  kbl(caption = "Size of each 5 latent class with all scales controlling by cycle and NEU countries", row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11, full_width = FALSE) %>% 
  column_spec (c(2), border_left = T) %>%
  column_spec(2:6, width = "10em") %>% print()

nas <- rownames(MAllScalesCountryCycleNEU[[5]]$coeff)
cnty<- str_remove(nas[!grepl("cycle|(Intercept)",nas)], "COUNTRY")
g1 <- list()
for(cnt in 0:length(cnty)){
  if(cnt == 0) { pidmat <- cbind(1, c(1:3), rep(0,3), replicate(length(cnty)-1,rep(0,3)), 
                                 c(1:3)*rep(0,3), replicate(length(cnty)-1,rep(0,3))) }  
  else if(cnt == 1) { pidmat <- cbind(1, c(1:3), rep(1,3), replicate(length(cnty)-1,rep(0,3)), 
                                      c(1:3)*rep(1,3), replicate(length(cnty)-1,rep(0,3))) } 
  else if(cnt == length(cnty)) { pidmat <- cbind(1, c(1:3), replicate(length(cnty)-1,rep(0,3)), rep(1,3), 
                                                 replicate(length(cnty)-1,rep(0,3)),c(1:3)*rep(1,3)) } 
  else  { pidmat <- cbind(1, c(1:3), replicate(cnt-1,rep(0,3)), rep(1,3), replicate(length(cnty)-cnt,rep(0,3)), 
                          replicate(cnt-1,rep(0,3)), c(1:3)*rep(1,3), replicate(length(cnty)-cnt,rep(0,3)))}
  exb <- exp(pidmat %*% MAllScalesCountryCycleNEU[[5]]$coeff)
  odd<- reshape2::melt((cbind(1,exb)/(1+rowSums(exb))))
  odd$Var2<-factor(ifelse(odd$Var2==3,1,
                          ifelse(odd$Var2==5,2,
                                 ifelse(odd$Var2==2,3,
                                        ifelse(odd$Var2==4,4,
                                               ifelse(odd$Var2==1,5,NA))))), levels = 1:5, labels = classes5NEU3)
  g1[[cnt+1]] <- odd  %>%  #study order of (cbind(1,exb)/(1+rowSums(exb))) with names assigned to assign labels
    mutate(Var2 = factor(Var2, labels = classes5NEU3)) %>%
    ggplot(aes(x=factor(Var1), y=value)) +
    geom_line(aes(group = Var2, linetype = Var2, color = Var2), size = 1) +
    theme_bw() + 
    scale_x_discrete(labels = c("1999", "2009", "2016")) +
    scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.5,0.75)) +
    labs(title = ifelse(cnt==0, "CHL",paste(cnty[cnt]))) +
    theme(legend.position = "none", legend.title = element_blank(), 
          plot.title = element_text(size = 7, margin = margin(b=-10,l=0)),
          axis.title = element_blank(), axis.text = element_text(size = 6)) +
    guides(linetype=guide_legend(nrow=1,byrow=TRUE)) 
}
leg <- reshape2::melt((cbind(1,exb)/(1+rowSums(exb)))) %>%  #study order of (cbind(1,exb)/(1+rowSums(exb))) with names assigned to assign labels
  mutate(Var2 = factor(Var2, labels = str_wrap(classes5NEU3,30))) %>%
  ggplot(aes(x=factor(Var1), y=value)) +
  geom_line(aes(group = Var2, linetype = Var2, color = Var2), size = 1) +
  theme_bw() + theme(legend.position = "bottom", legend.title = element_blank(), legend.key.size = unit(0.3, "cm"))+
  guides(linetype=guide_legend(nrow=5,byrow=TRUE)) 

g1[[(length(cnty)+2)]]<-g_legend(leg)
invisible(do.call(grid.arrange, c(g1, ncol=3, top = "Time as a predictor of attitude with 5 classes by NEU countries", 
                       left = "Probability of latent class membership")))
#----------------
cat('\n')
cat('\n')
cat("# 7. All items by COUNTRY and by CYCLE  \n")
cat('\n')
cat('\n')
cat('## 7.1 European countries \n')
cat('\n')
cat('\n')
rBYCntryEU <- summaryLCAR2(Modellist=MAllScalesBYCountryEU, level1 = 14:26, level2 = 1:3, level3 = 1:7)
cat('\n')
cat('\n')
cat('### 7.1.1 EU - Conditional probabilities by country an cycle {.tabset .tabset-pills}   \n')
cat('\n')
cat('\n')
#----------------7.1.1 k=14----
t=list()
k=14
cat('\n')
cat('\n')
cat(paste("####", names(MAllScalesBYCountryEU)[[k]]))
cat('\n')
cat('\n')
rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[1]] %>% filter(NClass == 5) %>% 
  bind_rows(rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[2]] %>% filter(NClass == 5)) %>% 
  bind_rows(rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[3]] %>% filter(NClass == 5)) %>% 
  dplyr::select(-Chisq,-npar) %>% 
  kbl(caption = paste("Model fit",names(MAllScalesBYCountryEU)[[k]]), row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% print()
cat('\n')
cat('\n')

MAllScalesBYCountryc1cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[1]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc1cl5, nclass = 5)
MAllScalesBYCountryc1cl5$Class <- factor(MAllScalesBYCountryc1cl5$Class, 
                                         levels = c("class 5: ", "class 2: ", "class 4: ", "class 3: ", "class 1: "), 
                                         labels = classes5)
s1<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[1]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.5, P.2, P.4, P.3, P.1))
colnames(s1) <- c("param",classes5)
cl1<- rbind(s1, reshape2::dcast(MAllScalesBYCountryc1cl5 %>% group_by(param, Class) %>% 
                                  filter(value == max(value)), param ~ Class))

MAllScalesBYCountryc2cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[2]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc2cl5, nclass = 5)
MAllScalesBYCountryc2cl5$Class <- factor(MAllScalesBYCountryc2cl5$Class, 
                                         levels = c("class 2: ", "class 5: ", "class 1: ", "class 3: ", "class 4: "), 
                                         labels = classes5)
s2<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[2]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.2, P.5, P.1, P.3, P.4))
colnames(s2) <- c("param",classes5)
cl2<- rbind(s2, reshape2::dcast(MAllScalesBYCountryc2cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

MAllScalesBYCountryc3cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[3]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc3cl5, nclass = 5)
MAllScalesBYCountryc3cl5$Class <- factor(MAllScalesBYCountryc3cl5$Class, 
                                         levels = c("class 3: ", "class 2: ", "class 1: ", "class 5: ", "class 4: "), 
                                         labels = classes5)
s3<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[3]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.3, P.2, P.1, P.5, P.4))
colnames(s3) <- c("param",classes5)
cl3<- rbind(s3, reshape2::dcast(MAllScalesBYCountryc3cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

t[[k]] <- left_join(left_join(cl1, cl2, by = "param"), cl3, by = "param")
t[[k]] %>% kbl(digits = 2, caption = paste("Highest conditional probabilities to each class",names(MAllScalesBYCountryEU)[[k]]),
                        col.names = c("", rep(classes5,3))) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(2,7,12), border_left = T) %>% 
  add_header_above(c(paste(names(MAllScalesBYCountryEU)[[k]]), "1999" = 5, "2009" = 5, "2016" = 5)) %>% 
  row_spec(1, bold = TRUE, italic = TRUE) %>% print()



#----------------
#----------------7.1.2 k=15----
t=list()
k=15
cat('\n')
cat('\n')
cat(paste("####", names(MAllScalesBYCountryEU)[[k]]))
cat('\n')
cat('\n')
rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[1]] %>% filter(NClass == 5) %>% 
  bind_rows(rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[2]] %>% filter(NClass == 5)) %>% 
  bind_rows(rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[3]] %>% filter(NClass == 5)) %>% 
  dplyr::select(-Chisq,-npar) %>% 
  kbl(caption = paste("Model fit",names(MAllScalesBYCountryEU)[[k]]), row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% print()
cat('\n')
cat('\n')

MAllScalesBYCountryc1cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[1]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc1cl5, nclass = 5)
MAllScalesBYCountryc1cl5$Class <- factor(MAllScalesBYCountryc1cl5$Class, 
                                         levels = c("class 2: ", "class 3: ", "class 5: ", "class 1: ", "class 4: "), 
                                         labels = classes5)
s1<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[1]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.2, P.3, P.5, P.1, P.4))
colnames(s1) <- c("param",classes5)
cl1<- rbind(s1, reshape2::dcast(MAllScalesBYCountryc1cl5 %>% group_by(param, Class) %>% 
                                  filter(value == max(value)), param ~ Class))

MAllScalesBYCountryc2cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[2]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc2cl5, nclass = 5)
MAllScalesBYCountryc2cl5$Class <- factor(MAllScalesBYCountryc2cl5$Class, 
                                         levels = c("class 3: ", "class 1: ", "class 2: ", "class 4: ", "class 5: "), 
                                         labels = classes5)
s2<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[2]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.3, P.1, P.2, P.4, P.5))
colnames(s2) <- c("param",classes5)
cl2<- rbind(s2, reshape2::dcast(MAllScalesBYCountryc2cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

MAllScalesBYCountryc3cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[3]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc3cl5, nclass = 5)
MAllScalesBYCountryc3cl5$Class <- factor(MAllScalesBYCountryc3cl5$Class, 
                                         levels = c("class 3: ", "class 5: ", "class 4: ", "class 2: ", "class 1: "), 
                                         labels = classes5)
s3<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[3]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.3, P.5, P.4, P.2, P.1))
colnames(s3) <- c("param",classes5)
cl3<- rbind(s3, reshape2::dcast(MAllScalesBYCountryc3cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

t[[k]] <- left_join(left_join(cl1, cl2, by = "param"), cl3, by = "param")
t[[k]] %>% kbl(digits = 2, caption = paste("Highest conditional probabilities to each class",names(MAllScalesBYCountryEU)[[k]]),
               col.names = c("", rep(classes5,3))) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(2,7,12), border_left = T) %>% 
  add_header_above(c(paste(names(MAllScalesBYCountryEU)[[k]]), "1999" = 5, "2009" = 5, "2016" = 5)) %>% 
  row_spec(1, bold = TRUE, italic = TRUE) %>% print()
#----------------
#----------------7.1.3 k=16----
t=list()
k=16
cat('\n')
cat('\n')
cat(paste("####", names(MAllScalesBYCountryEU)[[k]]))
cat('\n')
cat('\n')
rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[1]] %>% filter(NClass == 5) %>% 
  bind_rows(rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[2]] %>% filter(NClass == 5)) %>% 
  bind_rows(rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[3]] %>% filter(NClass == 5)) %>% 
  dplyr::select(-Chisq,-npar) %>% 
  kbl(caption = paste("Model fit",names(MAllScalesBYCountryEU)[[k]]), row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% print()
cat('\n')
cat('\n')

MAllScalesBYCountryc1cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[1]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc1cl5, nclass = 5)
MAllScalesBYCountryc1cl5$Class <- factor(MAllScalesBYCountryc1cl5$Class, 
                                         levels = c("class 4: ", "class 1: ", "class 2: ", "class 5: ", "class 3: "), 
                                         labels = classes5)
s1<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[1]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.4, P.1, P.2, P.5, P.3))
colnames(s1) <- c("param",classes5)
cl1<- rbind(s1, reshape2::dcast(MAllScalesBYCountryc1cl5 %>% group_by(param, Class) %>% 
                                  filter(value == max(value)), param ~ Class))

MAllScalesBYCountryc2cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[2]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc2cl5, nclass = 5)
MAllScalesBYCountryc2cl5$Class <- factor(MAllScalesBYCountryc2cl5$Class, 
                                         levels = c("class 1: ", "class 5: ", "class 4: ", "class 3: ", "class 2: "), 
                                         labels = classes5)
s2<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[2]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.1, P.5, P.4, P.3, P.2))
colnames(s2) <- c("param",classes5)
cl2<- rbind(s2, reshape2::dcast(MAllScalesBYCountryc2cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

MAllScalesBYCountryc3cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[3]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc3cl5, nclass = 5)
MAllScalesBYCountryc3cl5$Class <- factor(MAllScalesBYCountryc3cl5$Class, 
                                         levels = c("class 1: ", "class 3: ", "class 4: ", "class 2: ", "class 5: "), 
                                         labels = classes5)
s3<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[3]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.1, P.3, P.4, P.2, P.5))
colnames(s3) <- c("param",classes5)
cl3<- rbind(s3, reshape2::dcast(MAllScalesBYCountryc3cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

t[[k]] <- left_join(left_join(cl1, cl2, by = "param"), cl3, by = "param")
t[[k]] %>% kbl(digits = 2, caption = paste("Highest conditional probabilities to each class",names(MAllScalesBYCountryEU)[[k]]),
               col.names = c("", rep(classes5,3))) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(2,7,12), border_left = T) %>% 
  add_header_above(c(paste(names(MAllScalesBYCountryEU)[[k]]), "1999" = 5, "2009" = 5, "2016" = 5)) %>% 
  row_spec(1, bold = TRUE, italic = TRUE) %>% print()
#----------------
#----------------7.1.4 k=17----
t=list()
k=17
cat('\n')
cat('\n')
cat(paste("####", names(MAllScalesBYCountryEU)[[k]]))
cat('\n')
cat('\n')
rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[1]] %>% filter(NClass == 5) %>% 
  bind_rows(rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[2]] %>% filter(NClass == 5)) %>% 
  bind_rows(rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[3]] %>% filter(NClass == 5)) %>% 
  dplyr::select(-Chisq,-npar) %>% 
  kbl(caption = paste("Model fit",names(MAllScalesBYCountryEU)[[k]]), row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% print()
cat('\n')
cat('\n')

MAllScalesBYCountryc1cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[1]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc1cl5, nclass = 5)
MAllScalesBYCountryc1cl5$Class <- factor(MAllScalesBYCountryc1cl5$Class, 
                                         levels = c("class 3: ", "class 4: ", "class 1: ", "class 5: ", "class 2: "), 
                                         labels = classes5)
s1<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[1]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.3, P.4, P.1, P.5, P.2))
colnames(s1) <- c("param",classes5)
cl1<- rbind(s1, reshape2::dcast(MAllScalesBYCountryc1cl5 %>% group_by(param, Class) %>% 
                                  filter(value == max(value)), param ~ Class))

MAllScalesBYCountryc2cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[2]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc2cl5, nclass = 5)
MAllScalesBYCountryc2cl5$Class <- factor(MAllScalesBYCountryc2cl5$Class, 
                                         levels = c("class 3: ", "class 1: ", "class 4: ", "class 2: ", "class 5: "), 
                                         labels = classes5)
s2<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[2]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.3, P.1, P.4, P.2, P.5))
colnames(s2) <- c("param",classes5)
cl2<- rbind(s2, reshape2::dcast(MAllScalesBYCountryc2cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

MAllScalesBYCountryc3cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[3]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc3cl5, nclass = 5)
MAllScalesBYCountryc3cl5$Class <- factor(MAllScalesBYCountryc3cl5$Class, 
                                         levels = c("class 1: ", "class 3: ", "class 5: ", "class 4: ", "class 2: "), 
                                         labels = classes5)
s3<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[3]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.1, P.3, P.5, P.4, P.2))
colnames(s3) <- c("param",classes5)
cl3<- rbind(s3, reshape2::dcast(MAllScalesBYCountryc3cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

t[[k]] <- left_join(left_join(cl1, cl2, by = "param"), cl3, by = "param")
t[[k]] %>% kbl(digits = 2, caption = paste("Highest conditional probabilities to each class",names(MAllScalesBYCountryEU)[[k]]),
               col.names = c("", rep(classes5,3))) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(2,7,12), border_left = T) %>% 
  add_header_above(c(paste(names(MAllScalesBYCountryEU)[[k]]), "1999" = 5, "2009" = 5, "2016" = 5)) %>% 
  row_spec(1, bold = TRUE, italic = TRUE) %>% print()
#----------------
#----------------7.1.5 k=18----
t=list()
k=18
cat('\n')
cat('\n')
cat(paste("####", names(MAllScalesBYCountryEU)[[k]]))
cat('\n')
cat('\n')
rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[1]] %>% filter(NClass == 5) %>% 
  bind_rows(rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[2]] %>% filter(NClass == 5)) %>% 
  bind_rows(rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[3]] %>% filter(NClass == 5)) %>% 
  dplyr::select(-Chisq,-npar) %>% 
  kbl(caption = paste("Model fit",names(MAllScalesBYCountryEU)[[k]]), row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% print()
cat('\n')
cat('\n')

MAllScalesBYCountryc1cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[1]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc1cl5, nclass = 5)
MAllScalesBYCountryc1cl5$Class <- factor(MAllScalesBYCountryc1cl5$Class, 
                                         levels = c("class 1: ", "class 3: ", "class 2: ", "class 5: ", "class 4: "), 
                                         labels = classes5)
s1<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[1]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.1, P.3, P.2, P.5, P.4))
colnames(s1) <- c("param",classes5)
cl1<- rbind(s1, reshape2::dcast(MAllScalesBYCountryc1cl5 %>% group_by(param, Class) %>% 
                                  filter(value == max(value)), param ~ Class))

MAllScalesBYCountryc2cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[2]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc2cl5, nclass = 5)
MAllScalesBYCountryc2cl5$Class <- factor(MAllScalesBYCountryc2cl5$Class, 
                                         levels = c("class 3: ", "class 2: ", "class 1: ", "class 4: ", "class 5: "), 
                                         labels = classes5)
s2<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[2]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.3, P.2, P.1, P.4, P.5))
colnames(s2) <- c("param",classes5)
cl2<- rbind(s2, reshape2::dcast(MAllScalesBYCountryc2cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

MAllScalesBYCountryc3cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[3]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc3cl5, nclass = 5)
MAllScalesBYCountryc3cl5$Class <- factor(MAllScalesBYCountryc3cl5$Class, 
                                         levels = c("class 4: ", "class 2: ", "class 1: ", "class 3: ", "class 5: "), 
                                         labels = classes5)
s3<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[3]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.4, P.2, P.1, P.3, P.5))
colnames(s3) <- c("param",classes5)
cl3<- rbind(s3, reshape2::dcast(MAllScalesBYCountryc3cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

t[[k]] <- left_join(left_join(cl1, cl2, by = "param"), cl3, by = "param")
t[[k]] %>% kbl(digits = 2, caption = paste("Highest conditional probabilities to each class",names(MAllScalesBYCountryEU)[[k]]),
               col.names = c("", rep(classes5,3))) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(2,7,12), border_left = T) %>% 
  add_header_above(c(paste(names(MAllScalesBYCountryEU)[[k]]), "1999" = 5, "2009" = 5, "2016" = 5)) %>% 
  row_spec(1, bold = TRUE, italic = TRUE) %>% print()
#----------------
#----------------7.1.6 k=19----
t=list()
k=19
cat('\n')
cat('\n')
cat(paste("####", names(MAllScalesBYCountryEU)[[k]]))
cat('\n')
cat('\n')
rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[1]] %>% filter(NClass == 5) %>% 
  bind_rows(rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[2]] %>% filter(NClass == 5)) %>% 
  bind_rows(rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[3]] %>% filter(NClass == 5)) %>% 
  dplyr::select(-Chisq,-npar) %>% 
  kbl(caption = paste("Model fit",names(MAllScalesBYCountryEU)[[k]]), row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% print()
cat('\n')
cat('\n')

MAllScalesBYCountryc1cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[1]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc1cl5, nclass = 5)
MAllScalesBYCountryc1cl5$Class <- factor(MAllScalesBYCountryc1cl5$Class, 
                                         levels = c("class 5: ", "class 3: ", "class 4: ", "class 1: ", "class 2: "), 
                                         labels = classes5)
s1<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[1]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.5, P.3, P.4, P.1, P.2))
colnames(s1) <- c("param",classes5)
cl1<- rbind(s1, reshape2::dcast(MAllScalesBYCountryc1cl5 %>% group_by(param, Class) %>% 
                                  filter(value == max(value)), param ~ Class))

MAllScalesBYCountryc2cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[2]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc2cl5, nclass = 5)
MAllScalesBYCountryc2cl5$Class <- factor(MAllScalesBYCountryc2cl5$Class, 
                                         levels = c("class 4: ", "class 5: ", "class 3: ", "class 1: ", "class 2: "), 
                                         labels = classes5)
s2<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[2]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.4, P.5, P.3, P.1, P.2))
colnames(s2) <- c("param",classes5)
cl2<- rbind(s2, reshape2::dcast(MAllScalesBYCountryc2cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

MAllScalesBYCountryc3cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[3]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc3cl5, nclass = 5)
MAllScalesBYCountryc3cl5$Class <- factor(MAllScalesBYCountryc3cl5$Class, 
                                         levels = c("class 2: ", "class 1: ", "class 4: ", "class 3: ", "class 5: "), 
                                         labels = classes5)
s3<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[3]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.2, P.1, P.4, P.3, P.5))
colnames(s3) <- c("param",classes5)
cl3<- rbind(s3, reshape2::dcast(MAllScalesBYCountryc3cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

t[[k]] <- left_join(left_join(cl1, cl2, by = "param"), cl3, by = "param")
t[[k]] %>% kbl(digits = 2, caption = paste("Highest conditional probabilities to each class",names(MAllScalesBYCountryEU)[[k]]),
               col.names = c("", rep(classes5,3))) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(2,7,12), border_left = T) %>% 
  add_header_above(c(paste(names(MAllScalesBYCountryEU)[[k]]), "1999" = 5, "2009" = 5, "2016" = 5)) %>% 
  row_spec(1, bold = TRUE, italic = TRUE) %>% print()
#----------------
#----------------7.1.7 k=20----
t=list()
k=20
cat('\n')
cat('\n')
cat(paste("####", names(MAllScalesBYCountryEU)[[k]]))
cat('\n')
cat('\n')
rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[1]] %>% filter(NClass == 5) %>% 
  bind_rows(rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[2]] %>% filter(NClass == 5)) %>% 
  bind_rows(rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[3]] %>% filter(NClass == 5)) %>% 
  dplyr::select(-Chisq,-npar) %>% 
  kbl(caption = paste("Model fit",names(MAllScalesBYCountryEU)[[k]]), row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% print()
cat('\n')
cat('\n')

MAllScalesBYCountryc1cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[1]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc1cl5, nclass = 5)
MAllScalesBYCountryc1cl5$Class <- factor(MAllScalesBYCountryc1cl5$Class, 
                                         levels = c("class 1: ", "class 2: ", "class 4: ", "class 5: ", "class 3: "), 
                                         labels = classes5)
s1<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[1]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.1, P.2, P.4, P.5, P.3))
colnames(s1) <- c("param",classes5)
cl1<- rbind(s1, reshape2::dcast(MAllScalesBYCountryc1cl5 %>% group_by(param, Class) %>% 
                                  filter(value == max(value)), param ~ Class))

MAllScalesBYCountryc2cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[2]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc2cl5, nclass = 5)
MAllScalesBYCountryc2cl5$Class <- factor(MAllScalesBYCountryc2cl5$Class, 
                                         levels = c("class 4: ", "class 3: ", "class 2: ", "class 5: ", "class 1: "), 
                                         labels = classes5)
s2<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[2]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.4, P.3, P.2, P.5, P.1))
colnames(s2) <- c("param",classes5)
cl2<- rbind(s2, reshape2::dcast(MAllScalesBYCountryc2cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

MAllScalesBYCountryc3cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[3]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc3cl5, nclass = 5)
MAllScalesBYCountryc3cl5$Class <- factor(MAllScalesBYCountryc3cl5$Class, 
                                         levels = c("class 4: ", "class 2: ", "class 1: ", "class 3: ", "class 5: "), 
                                         labels = classes5)
s3<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[3]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.4, P.2, P.1, P.3, P.5))
colnames(s3) <- c("param",classes5)
cl3<- rbind(s3, reshape2::dcast(MAllScalesBYCountryc3cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

t[[k]] <- left_join(left_join(cl1, cl2, by = "param"), cl3, by = "param")
t[[k]] %>% kbl(digits = 2, caption = paste("Highest conditional probabilities to each class",names(MAllScalesBYCountryEU)[[k]]),
               col.names = c("", rep(classes5,3))) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(2,7,12), border_left = T) %>% 
  add_header_above(c(paste(names(MAllScalesBYCountryEU)[[k]]), "1999" = 5, "2009" = 5, "2016" = 5)) %>% 
  row_spec(1, bold = TRUE, italic = TRUE) %>% print()
#----------------
#----------------7.1.8 k=21----
t=list()
k=21
cat('\n')
cat('\n')
cat(paste("####", names(MAllScalesBYCountryEU)[[k]]))
cat('\n')
cat('\n')
rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[1]] %>% filter(NClass == 5) %>% 
  bind_rows(rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[2]] %>% filter(NClass == 5)) %>% 
  bind_rows(rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[3]] %>% filter(NClass == 5)) %>% 
  dplyr::select(-Chisq,-npar) %>% 
  kbl(caption = paste("Model fit",names(MAllScalesBYCountryEU)[[k]]), row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% print()
cat('\n')
cat('\n')

MAllScalesBYCountryc1cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[1]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc1cl5, nclass = 5)
MAllScalesBYCountryc1cl5$Class <- factor(MAllScalesBYCountryc1cl5$Class, 
                                         levels = c("class 3: ", "class 5: ", "class 2: ", "class 4: ", "class 1: "), 
                                         labels = classes5)
s1<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[1]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.3, P.5, P.2, P.4, P.1))
colnames(s1) <- c("param",classes5)
cl1<- rbind(s1, reshape2::dcast(MAllScalesBYCountryc1cl5 %>% group_by(param, Class) %>% 
                                  filter(value == max(value)), param ~ Class))

MAllScalesBYCountryc2cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[2]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc2cl5, nclass = 5)
MAllScalesBYCountryc2cl5$Class <- factor(MAllScalesBYCountryc2cl5$Class, 
                                         levels = c("class 5: ", "class 4: ", "class 2: ", "class 3: ", "class 1: "), 
                                         labels = classes5)
s2<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[2]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.5, P.4, P.2, P.3, P.1))
colnames(s2) <- c("param",classes5)
cl2<- rbind(s2, reshape2::dcast(MAllScalesBYCountryc2cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

MAllScalesBYCountryc3cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[3]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc3cl5, nclass = 5)
MAllScalesBYCountryc3cl5$Class <- factor(MAllScalesBYCountryc3cl5$Class, 
                                         levels = c("class 2: ", "class 4: ", "class 3: ", "class 5: ", "class 1: "), 
                                         labels = classes5)
s3<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[3]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.1, P.3, P.4, P.2, P.5))
colnames(s3) <- c("param",classes5)
cl3<- rbind(s3, reshape2::dcast(MAllScalesBYCountryc3cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

t[[k]] <- left_join(left_join(cl1, cl2, by = "param"), cl3, by = "param")
t[[k]] %>% kbl(digits = 2, caption = paste("Highest conditional probabilities to each class",names(MAllScalesBYCountryEU)[[k]]),
               col.names = c("", rep(classes5,3))) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(2,7,12), border_left = T) %>% 
  add_header_above(c(paste(names(MAllScalesBYCountryEU)[[k]]), "1999" = 5, "2009" = 5, "2016" = 5)) %>% 
  row_spec(1, bold = TRUE, italic = TRUE) %>% print()
#----------------
#----------------7.1.9 k=22----
t=list()
k=22
cat('\n')
cat('\n')
cat(paste("####", names(MAllScalesBYCountryEU)[[k]]))
cat('\n')
cat('\n')
rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[1]] %>% filter(NClass == 5) %>% 
  bind_rows(rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[2]] %>% filter(NClass == 5)) %>% 
  bind_rows(rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[3]] %>% filter(NClass == 5)) %>% 
  dplyr::select(-Chisq,-npar) %>% 
  kbl(caption = paste("Model fit",names(MAllScalesBYCountryEU)[[k]]), row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% print()
cat('\n')
cat('\n')

MAllScalesBYCountryc1cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[1]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc1cl5, nclass = 5)
MAllScalesBYCountryc1cl5$Class <- factor(MAllScalesBYCountryc1cl5$Class, 
                                         levels = c("class 2: ", "class 3: ", "class 1: ", "class 5: ", "class 4: "), 
                                         labels = classes5)
s1<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[1]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.2, P.3, P.1, P.5, P.4))
colnames(s1) <- c("param",classes5)
cl1<- rbind(s1, reshape2::dcast(MAllScalesBYCountryc1cl5 %>% group_by(param, Class) %>% 
                                  filter(value == max(value)), param ~ Class))

MAllScalesBYCountryc2cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[2]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc2cl5, nclass = 5)
MAllScalesBYCountryc2cl5$Class <- factor(MAllScalesBYCountryc2cl5$Class, 
                                         levels = c("class 3: ", "class 1: ", "class 4: ", "class 2: ", "class 5: "), 
                                         labels = classes5)
s2<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[2]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.3, P.1, P.4, P.2, P.5))
colnames(s2) <- c("param",classes5)
cl2<- rbind(s2, reshape2::dcast(MAllScalesBYCountryc2cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

MAllScalesBYCountryc3cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[3]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc3cl5, nclass = 5)
MAllScalesBYCountryc3cl5$Class <- factor(MAllScalesBYCountryc3cl5$Class, 
                                         levels = c("class 4: ", "class 3: ", "class 1: ", "class 5: ", "class 2: "), 
                                         labels = classes5)
s3<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[3]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.4, P.3, P.1, P.5, P.2))
colnames(s3) <- c("param",classes5)
cl3<- rbind(s3, reshape2::dcast(MAllScalesBYCountryc3cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

t[[k]] <- left_join(left_join(cl1, cl2, by = "param"), cl3, by = "param")
t[[k]] %>% kbl(digits = 2, caption = paste("Highest conditional probabilities to each class",names(MAllScalesBYCountryEU)[[k]]),
               col.names = c("", rep(classes5,3))) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(2,7,12), border_left = T) %>% 
  add_header_above(c(paste(names(MAllScalesBYCountryEU)[[k]]), "1999" = 5, "2009" = 5, "2016" = 5)) %>% 
  row_spec(1, bold = TRUE, italic = TRUE) %>% print()
#----------------
#----------------7.1.10 k=23----
t=list()
k=23
cat('\n')
cat('\n')
cat(paste("####", names(MAllScalesBYCountryEU)[[k]]))
cat('\n')
cat('\n')
rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[1]] %>% filter(NClass == 5) %>% 
  bind_rows(rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[2]] %>% filter(NClass == 5)) %>% 
  bind_rows(rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[3]] %>% filter(NClass == 5)) %>% 
  dplyr::select(-Chisq,-npar) %>% 
  kbl(caption = paste("Model fit",names(MAllScalesBYCountryEU)[[k]]), row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% print()
cat('\n')
cat('\n')

MAllScalesBYCountryc1cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[1]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc1cl5, nclass = 5)
MAllScalesBYCountryc1cl5$Class <- factor(MAllScalesBYCountryc1cl5$Class, 
                                         levels = c("class 2: ", "class 1: ", "class 5: ", "class 3: ", "class 4: "), 
                                         labels = classes5)
s1<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[1]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.2, P.1, P.5, P.3, P.4))
colnames(s1) <- c("param",classes5)
cl1<- rbind(s1, reshape2::dcast(MAllScalesBYCountryc1cl5 %>% group_by(param, Class) %>% 
                                  filter(value == max(value)), param ~ Class))

MAllScalesBYCountryc2cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[2]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc2cl5, nclass = 5)
MAllScalesBYCountryc2cl5$Class <- factor(MAllScalesBYCountryc2cl5$Class, 
                                         levels = c("class 5: ", "class 3: ", "class 4: ", "class 1: ", "class 2: "), 
                                         labels = classes5)
s2<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[2]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.5, P.3, P.4, P.1, P.2))
colnames(s2) <- c("param",classes5)
cl2<- rbind(s2, reshape2::dcast(MAllScalesBYCountryc2cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

MAllScalesBYCountryc3cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[3]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc3cl5, nclass = 5)
MAllScalesBYCountryc3cl5$Class <- factor(MAllScalesBYCountryc3cl5$Class, 
                                         levels = c("class 4: ", "class 3: ", "class 2: ", "class 5: ", "class 1: "), 
                                         labels = classes5)
s3<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[3]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.4, P.3, P.2, P.5, P.1))
colnames(s3) <- c("param",classes5)
cl3<- rbind(s3, reshape2::dcast(MAllScalesBYCountryc3cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

t[[k]] <- left_join(left_join(cl1, cl2, by = "param"), cl3, by = "param")
t[[k]] %>% kbl(digits = 2, caption = paste("Highest conditional probabilities to each class",names(MAllScalesBYCountryEU)[[k]]),
               col.names = c("", rep(classes5,3))) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(2,7,12), border_left = T) %>% 
  add_header_above(c(paste(names(MAllScalesBYCountryEU)[[k]]), "1999" = 5, "2009" = 5, "2016" = 5)) %>% 
  row_spec(1, bold = TRUE, italic = TRUE) %>% print()
#----------------
#----------------7.1.11 k=24----
t=list()
k=24
cat('\n')
cat('\n')
cat(paste("####", names(MAllScalesBYCountryEU)[[k]]))
cat('\n')
cat('\n')
rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[1]] %>% filter(NClass == 5) %>% 
  bind_rows(rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[2]] %>% filter(NClass == 5)) %>% 
  dplyr::select(-Chisq,-npar) %>% 
  kbl(caption = paste("Model fit",names(MAllScalesBYCountryEU)[[k]]), row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% print()
cat('\n')
cat('\n')

MAllScalesBYCountryc1cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[1]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc1cl5, nclass = 5)
MAllScalesBYCountryc1cl5$Class <- factor(MAllScalesBYCountryc1cl5$Class, 
                                         levels = c("class 1: ", "class 5: ", "class 2: ", "class 4: ", "class 3: "), 
                                         labels = classes5)
s1<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[1]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.1, P.5, P.2, P.4, P.3))
colnames(s1) <- c("param",classes5)
cl1<- rbind(s1, reshape2::dcast(MAllScalesBYCountryc1cl5 %>% group_by(param, Class) %>% 
                                  filter(value == max(value)), param ~ Class))

MAllScalesBYCountryc2cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[2]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc2cl5, nclass = 5)
MAllScalesBYCountryc2cl5$Class <- factor(MAllScalesBYCountryc2cl5$Class, 
                                         levels = c("class 5: ", "class 2: ", "class 3: ", "class 4: ", "class 1: "), 
                                         labels = classes5)
s2<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[2]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.5, P.2, P.3, P.4, P.1))
colnames(s2) <- c("param",classes5)
cl2<- rbind(s2, reshape2::dcast(MAllScalesBYCountryc2cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

t[[k]] <- left_join(cl1, cl2, by = "param")
t[[k]] %>% kbl(digits = 2, caption = paste("Highest conditional probabilities to each class",names(MAllScalesBYCountryEU)[[k]]),
               col.names = c("", rep(classes5,2))) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(2,7), border_left = T) %>% 
  add_header_above(c(paste(names(MAllScalesBYCountryEU)[[k]]), "2009" = 5, "2016" = 5)) %>% 
  row_spec(1, bold = TRUE, italic = TRUE) %>% print()
#----------------
#----------------7.1.12 k=25----
t=list()
k=25
cat('\n')
cat('\n')
cat(paste("####", names(MAllScalesBYCountryEU)[[k]]))
cat('\n')
cat('\n')
rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[1]] %>% filter(NClass == 5) %>% 
  bind_rows(rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[2]] %>% filter(NClass == 5)) %>% 
  dplyr::select(-Chisq,-npar) %>% 
  kbl(caption = paste("Model fit",names(MAllScalesBYCountryEU)[[k]]), row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% print()
cat('\n')
cat('\n')

MAllScalesBYCountryc1cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[1]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc1cl5, nclass = 5)
MAllScalesBYCountryc1cl5$Class <- factor(MAllScalesBYCountryc1cl5$Class, 
                                         levels = c("class 5: ", "class 2: ", "class 4: ", "class 3: ", "class 1: "), 
                                         labels = classes5)
s1<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[1]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.5, P.2, P.4, P.3, P.1))
colnames(s1) <- c("param",classes5)
cl1<- rbind(s1, reshape2::dcast(MAllScalesBYCountryc1cl5 %>% group_by(param, Class) %>% 
                                  filter(value == max(value)), param ~ Class))

MAllScalesBYCountryc2cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[2]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc2cl5, nclass = 5)
MAllScalesBYCountryc2cl5$Class <- factor(MAllScalesBYCountryc2cl5$Class, 
                                         levels = c("class 3: ", "class 2: ", "class 1: ", "class 5: ", "class 4: "), 
                                         labels = classes5)
s2<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[2]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.3, P.2, P.1, P.5, P.4))
colnames(s2) <- c("param",classes5)
cl2<- rbind(s2, reshape2::dcast(MAllScalesBYCountryc2cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

t[[k]] <- left_join(cl1, cl2, by = "param")
t[[k]] %>% kbl(digits = 2, caption = paste("Highest conditional probabilities to each class",names(MAllScalesBYCountryEU)[[k]]),
               col.names = c("", rep(classes5,2))) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(2,7), border_left = T) %>% 
  add_header_above(c(paste(names(MAllScalesBYCountryEU)[[k]]), "2009" = 5, "2016" = 5)) %>% 
  row_spec(1, bold = TRUE, italic = TRUE) %>% print()
#----------------
#----------------7.1.13 k=26----
t=list()
k=26
cat('\n')
cat('\n')
cat(paste("####", names(MAllScalesBYCountryEU)[[k]]))
cat('\n')
cat('\n')
rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[1]] %>% filter(NClass == 5) %>% 
  bind_rows(rBYCntryEU$Fits[[names(MAllScalesBYCountryEU)[[k]]]][[2]] %>% filter(NClass == 5)) %>% 
  dplyr::select(-Chisq,-npar) %>% 
  kbl(caption = paste("Model fit",names(MAllScalesBYCountryEU)[[k]]), row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% print()
cat('\n')
cat('\n')

MAllScalesBYCountryc1cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[1]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc1cl5, nclass = 5)
MAllScalesBYCountryc1cl5$Class <- factor(MAllScalesBYCountryc1cl5$Class, 
                                         levels = c("class 3: ", "class 4: ", "class 1: ", "class 2: ", "class 5: "), 
                                         labels = classes5)
s1<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[1]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.3, P.4, P.1, P.2, P.5))
colnames(s1) <- c("param",classes5)
cl1<- rbind(s1, reshape2::dcast(MAllScalesBYCountryc1cl5 %>% group_by(param, Class) %>% 
                                  filter(value == max(value)), param ~ Class))

MAllScalesBYCountryc2cl5 <- reshape2::melt(MAllScalesBYCountryEU[[k]][[2]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc2cl5, nclass = 5)
MAllScalesBYCountryc2cl5$Class <- factor(MAllScalesBYCountryc2cl5$Class, 
                                         levels = c("class 1: ", "class 2: ", "class 5: ", "class 4: ", "class 3: "), 
                                         labels = classes5)
s2<-cbind("Sizes",rBYCntryEU$Size[[names(MAllScalesBYCountryEU)[[k]]]][[2]] %>% 
            filter(NClass == 5) %>% dplyr::select(P.1, P.2, P.5, P.4, P.3))
colnames(s2) <- c("param",classes5)
cl2<- rbind(s2, reshape2::dcast(MAllScalesBYCountryc2cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

t[[k]] <- left_join(cl1, cl2, by = "param")
t[[k]] %>% kbl(digits = 2, caption = paste("Highest conditional probabilities to each class",names(MAllScalesBYCountryEU)[[k]]),
               col.names = c("", rep(classes5,2))) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(2,7), border_left = T) %>% 
  add_header_above(c(paste(names(MAllScalesBYCountryEU)[[k]]), "2009" = 5, "2016" = 5)) %>% 
  row_spec(1, bold = TRUE, italic = TRUE) %>% print()
#----------------
cat('\n')
cat('\n')
cat('## 7.2 Non-European countries  \n')
cat('\n')
cat('\n')
cat('\n')
rBYCntry <- summaryLCAR2(Modellist=MAllScalesBYCountryNEU, level1 = 9:16, level2 = 1:3, level3 = 1:7)
cat('\n')
cat('\n')
cat('### 7.2.1 NEU - Conditional probabilities by country an cycle {.tabset .tabset-pills}  \n')
cat('\n')
cat('\n')
#----------------7.2.1 k=1----
t <- list()
k=9
cat('\n')
cat('\n')
cat(paste("####", names(MAllScalesBYCountryNEU)[[k]]))
cat('\n')
cat('\n')
rBYCntry$Fits[[names(MAllScalesBYCountryNEU)[[k]]]][[1]] %>% filter(NClass == 5) %>% 
  bind_rows(rBYCntry$Fits[[names(MAllScalesBYCountryNEU)[[k]]]][[2]] %>% filter(NClass == 5)) %>% 
  bind_rows(rBYCntry$Fits[[names(MAllScalesBYCountryNEU)[[k]]]][[3]] %>% filter(NClass == 5)) %>% 
  dplyr::select(-Chisq,-npar) %>% 
  kbl(caption = paste("Model fit",names(MAllScalesBYCountryNEU)[[k]]), row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% print()
cat('\n')
cat('\n')


MAllScalesBYCountryc1cl5 <- reshape2::melt(MAllScalesBYCountryNEU[[k]][[1]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc1cl5, nclass = 5)
MAllScalesBYCountryc1cl5$Class <- factor(MAllScalesBYCountryc1cl5$Class, 
                                         levels = c("class 1: ", "class 4: ", "class 5: ", "class 2: ", "class 3: "), 
                                         labels = classes5)
s1<-cbind("Sizes",rBYCntry$Size[[names(MAllScalesBYCountryNEU)[[k]]]][[1]] %>% filter(NClass == 5) %>% dplyr::select(P.1, P.4, P.5, P.2, P.3))
colnames(s1) <- c("param",classes5)
cl1<- rbind(s1, reshape2::dcast(MAllScalesBYCountryc1cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

MAllScalesBYCountryc2cl5 <- reshape2::melt(MAllScalesBYCountryNEU[[k]][[2]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc2cl5, nclass = 5)
MAllScalesBYCountryc2cl5$Class <- factor(MAllScalesBYCountryc2cl5$Class, 
                                         levels = c("class 5: ", "class 2: ", "class 4: ", "class 3: ", "class 1: "), 
                                         labels = classes5)
s2<-cbind("Sizes",rBYCntry$Size[[names(MAllScalesBYCountryNEU)[[k]]]][[2]] %>% filter(NClass == 5) %>% dplyr::select(P.5, P.2, P.4, P.3, P.1))
colnames(s2) <- c("param",classes5)
cl2<- rbind(s2, reshape2::dcast(MAllScalesBYCountryc2cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

MAllScalesBYCountryc3cl5 <- reshape2::melt(MAllScalesBYCountryNEU[[k]][[3]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc3cl5, nclass = 5)
MAllScalesBYCountryc3cl5$Class <- factor(MAllScalesBYCountryc3cl5$Class, 
                                         levels = c("class 3: ", "class 2: ", "class 4: ", "class 1: ", "class 5: "), 
                                         labels = classes5)
s3<-cbind("Sizes",rBYCntry$Size[[names(MAllScalesBYCountryNEU)[[k]]]][[3]] %>% filter(NClass == 5) %>% dplyr::select(P.3, P.2, P.4, P.1, P.5))
colnames(s3) <- c("param",classes5)
cl3<- rbind(s2, reshape2::dcast(MAllScalesBYCountryc3cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

t[[k]] <- left_join(left_join(cl1, cl2, by="param"), cl3, by="param")
t[[k]] %>% kbl(digits = 2, caption = paste("Highest conditional probabilities to each class", names(MAllScalesBYCountryNEU)[[k]]),
               col.names = c("", rep(classes5,3))) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(2,7,12), border_left = T) %>% 
  add_header_above(c(paste(names(MAllScalesBYCountryNEU)[[k]]), "1999" = 5, "2009" = 5, "2016" = 5)) %>% 
  row_spec(1, bold = TRUE, italic = TRUE) %>% print()

#----------------
#----------------7.2.2 k=2----
t <- list()
k=10
cat('\n')
cat('\n')
cat(paste("####", names(MAllScalesBYCountryNEU)[[k]]))
cat('\n')
cat('\n')
rBYCntry$Fits[[names(MAllScalesBYCountryNEU)[[k]]]][[1]] %>% filter(NClass == 5) %>% 
  bind_rows(rBYCntry$Fits[[names(MAllScalesBYCountryNEU)[[k]]]][[2]] %>% filter(NClass == 5)) %>% 
  bind_rows(rBYCntry$Fits[[names(MAllScalesBYCountryNEU)[[k]]]][[3]] %>% filter(NClass == 5)) %>% 
  dplyr::select(-Chisq,-npar) %>% 
  kbl(caption = paste("Model fit",names(MAllScalesBYCountryNEU)[[k]]), row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% print()
cat('\n')
cat('\n')


MAllScalesBYCountryc1cl5 <- reshape2::melt(MAllScalesBYCountryNEU[[k]][[1]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc1cl5, nclass = 5)
MAllScalesBYCountryc1cl5$Class <- factor(MAllScalesBYCountryc1cl5$Class, 
                                         levels = c("class 3: ", "class 5: ", "class 4: ", "class 1: ", "class 2: "), 
                                         labels = classes5)
s1<-cbind("Sizes",rBYCntry$Size[[names(MAllScalesBYCountryNEU)[[k]]]][[1]] %>% filter(NClass == 5) %>% 
            dplyr::select(P.3, P.5, P.4, P.1, P.2))
colnames(s1) <- c("param",classes5)
cl1<- rbind(s1, reshape2::dcast(MAllScalesBYCountryc1cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

MAllScalesBYCountryc2cl5 <- reshape2::melt(MAllScalesBYCountryNEU[[k]][[2]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc2cl5, nclass = 5)
MAllScalesBYCountryc2cl5$Class <- factor(MAllScalesBYCountryc2cl5$Class, 
                                         levels = c("class 2: ", "class 1: ", "class 4: ", "class 5: ", "class 3: "), 
                                         labels = classes5)
s2<-cbind("Sizes",rBYCntry$Size[[names(MAllScalesBYCountryNEU)[[k]]]][[2]] %>% filter(NClass == 5) %>% 
            dplyr::select(P.2, P.1, P.4, P.5, P.3))
colnames(s2) <- c("param",classes5)
cl2<- rbind(s2, reshape2::dcast(MAllScalesBYCountryc2cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

MAllScalesBYCountryc3cl5 <- reshape2::melt(MAllScalesBYCountryNEU[[k]][[3]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc3cl5, nclass = 5)
MAllScalesBYCountryc3cl5$Class <- factor(MAllScalesBYCountryc3cl5$Class, 
                                         levels = c("class 5: ", "class 4: ", "class 2: ", "class 3: ", "class 1: "), 
                                         labels = classes5)
s3<-cbind("Sizes",rBYCntry$Size[[names(MAllScalesBYCountryNEU)[[k]]]][[3]] %>% filter(NClass == 5) %>% 
            dplyr::select(P.5, P.4, P.2, P.3, P.1))
colnames(s3) <- c("param",classes5)
cl3<- rbind(s2, reshape2::dcast(MAllScalesBYCountryc3cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

t[[k]] <- left_join(left_join(cl1, cl2, by="param"), cl3, by="param")
t[[k]] %>% kbl(digits = 2, caption = paste("Highest conditional probabilities to each class", names(MAllScalesBYCountryNEU)[[k]]),
               col.names = c("", rep(classes5,3))) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(2,7,12), border_left = T) %>% 
  add_header_above(c(paste(names(MAllScalesBYCountryNEU)[[k]]), "1999" = 5, "2009" = 5, "2016" = 5)) %>% 
  row_spec(1, bold = TRUE, italic = TRUE) %>% print()

#----------------
#----------------7.2.3 k=3----
t <- list()
k=11
cat('\n')
cat('\n')
cat(paste("####", names(MAllScalesBYCountryNEU)[[k]]))
cat('\n')
cat('\n')
rBYCntry$Fits[[names(MAllScalesBYCountryNEU)[[k]]]][[1]] %>% filter(NClass == 5) %>% 
  bind_rows(rBYCntry$Fits[[names(MAllScalesBYCountryNEU)[[k]]]][[2]] %>% filter(NClass == 5)) %>% 
  bind_rows(rBYCntry$Fits[[names(MAllScalesBYCountryNEU)[[k]]]][[3]] %>% filter(NClass == 5)) %>% 
  dplyr::select(-Chisq,-npar) %>% 
  kbl(caption = paste("Model fit",names(MAllScalesBYCountryNEU)[[k]]), row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% print()
cat('\n')
cat('\n')

MAllScalesBYCountryc1cl5 <- reshape2::melt(MAllScalesBYCountryNEU[[k]][[1]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc1cl5, nclass = 5)
MAllScalesBYCountryc1cl5$Class <- factor(MAllScalesBYCountryc1cl5$Class, 
                                         levels = c("class 3: ", "class 5: ", "class 4: ", "class 1: ", "class 2: "), 
                                         labels = classes5)
s1<-cbind("Sizes",rBYCntry$Size[[names(MAllScalesBYCountryNEU)[[k]]]][[1]] %>% filter(NClass == 5) %>% 
            dplyr::select(P.3, P.5, P.4, P.1, P.2))
colnames(s1) <- c("param",classes5)
cl1<- rbind(s1, reshape2::dcast(MAllScalesBYCountryc1cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

MAllScalesBYCountryc2cl5 <- reshape2::melt(MAllScalesBYCountryNEU[[k]][[2]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc2cl5, nclass = 5)
MAllScalesBYCountryc2cl5$Class <- factor(MAllScalesBYCountryc2cl5$Class, 
                                         levels = c("class 4: ", "class 3: ", "class 5: ", "class 2: ", "class 1: "), 
                                         labels = classes5)
s2<-cbind("Sizes",rBYCntry$Size[[names(MAllScalesBYCountryNEU)[[k]]]][[2]] %>% filter(NClass == 5) %>% 
            dplyr::select(P.4, P.3, P.5, P.2, P.1))
colnames(s2) <- c("param",classes5)
cl2<- rbind(s2, reshape2::dcast(MAllScalesBYCountryc2cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

MAllScalesBYCountryc3cl5 <- reshape2::melt(MAllScalesBYCountryNEU[[k]][[3]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc3cl5, nclass = 5)
MAllScalesBYCountryc3cl5$Class <- factor(MAllScalesBYCountryc3cl5$Class, 
                                         levels = c("class 3: ", "class 5: ", "class 2: ", "class 1: ", "class 4: "), 
                                         labels = classes5)
s3<-cbind("Sizes",rBYCntry$Size[[names(MAllScalesBYCountryNEU)[[k]]]][[3]] %>% filter(NClass == 5) %>% 
            dplyr::select(P.3, P.5, P.2, P.1, P.4))
colnames(s3) <- c("param",classes5)
cl3<- rbind(s2, reshape2::dcast(MAllScalesBYCountryc3cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

t[[k]] <- left_join(left_join(cl1, cl2, by="param"), cl3, by="param")
t[[k]] %>% kbl(digits = 2, caption = paste("Highest conditional probabilities to each class", names(MAllScalesBYCountryNEU)[[k]]),
               col.names = c("", rep(classes5,3))) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(2,7,12), border_left = T) %>% 
  add_header_above(c(paste(names(MAllScalesBYCountryNEU)[[k]]), "1999" = 5, "2009" = 5, "2016" = 5)) %>% 
  row_spec(1, bold = TRUE, italic = TRUE) %>% print()

#----------------
#----------------7.2.4 k=4----
t <- list()
k=12
cat('\n')
cat('\n')
cat(paste("####", names(MAllScalesBYCountryNEU)[[k]]))
cat('\n')
cat('\n')
rBYCntry$Fits[[names(MAllScalesBYCountryNEU)[[k]]]][[1]] %>% filter(NClass == 5) %>% 
  bind_rows(rBYCntry$Fits[[names(MAllScalesBYCountryNEU)[[k]]]][[2]] %>% filter(NClass == 5)) %>% 
  bind_rows(rBYCntry$Fits[[names(MAllScalesBYCountryNEU)[[k]]]][[3]] %>% filter(NClass == 5)) %>% 
  dplyr::select(-Chisq,-npar) %>% 
  kbl(caption = paste("Model fit",names(MAllScalesBYCountryNEU)[[k]]), row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% print()
cat('\n')
cat('\n')


MAllScalesBYCountryc1cl5 <- reshape2::melt(MAllScalesBYCountryNEU[[k]][[1]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc1cl5, nclass = 5)
MAllScalesBYCountryc1cl5$Class <- factor(MAllScalesBYCountryc1cl5$Class, 
                                         levels = c("class 5: ", "class 1: ", "class 2: ", "class 3: ", "class 4: "), 
                                         labels = classes5)
s1<-cbind("Sizes",rBYCntry$Size[[names(MAllScalesBYCountryNEU)[[k]]]][[1]] %>% filter(NClass == 5) %>% 
            dplyr::select(P.5, P.1, P.2, P.3, P.4))
colnames(s1) <- c("param",classes5)
cl1<- rbind(s1, reshape2::dcast(MAllScalesBYCountryc1cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

MAllScalesBYCountryc2cl5 <- reshape2::melt(MAllScalesBYCountryNEU[[k]][[2]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc2cl5, nclass = 5)
MAllScalesBYCountryc2cl5$Class <- factor(MAllScalesBYCountryc2cl5$Class, 
                                         levels = c("class 1: ", "class 2: ", "class 3: ", "class 4: ", "class 5: "), 
                                         labels = classes5)
s2<-cbind("Sizes",rBYCntry$Size[[names(MAllScalesBYCountryNEU)[[k]]]][[2]] %>% filter(NClass == 5) %>% 
            dplyr::select(P.1, P.2, P.3, P.4, P.5))
colnames(s2) <- c("param",classes5)
cl2<- rbind(s2, reshape2::dcast(MAllScalesBYCountryc2cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

MAllScalesBYCountryc3cl5 <- reshape2::melt(MAllScalesBYCountryNEU[[k]][[3]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc3cl5, nclass = 5)
MAllScalesBYCountryc3cl5$Class <- factor(MAllScalesBYCountryc3cl5$Class, 
                                         levels = c("class 5: ", "class 4: ", "class 2: ", "class 3: ", "class 1: "), 
                                         labels = classes5)
s3<-cbind("Sizes",rBYCntry$Size[[names(MAllScalesBYCountryNEU)[[k]]]][[3]] %>% filter(NClass == 5) %>% 
            dplyr::select(P.5, P.4, P.2, P.3, P.1))
colnames(s3) <- c("param",classes5)
cl3<- rbind(s2, reshape2::dcast(MAllScalesBYCountryc3cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

t[[k]] <- left_join(left_join(cl1, cl2, by="param"), cl3, by="param")
t[[k]] %>% kbl(digits = 2, caption = paste("Highest conditional probabilities to each class", names(MAllScalesBYCountryNEU)[[k]]),
               col.names = c("", rep(classes5,3))) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(2,7,12), border_left = T) %>% 
  add_header_above(c(paste(names(MAllScalesBYCountryNEU)[[k]]), "1999" = 5, "2009" = 5, "2016" = 5)) %>% 
  row_spec(1, bold = TRUE, italic = TRUE) %>% print()

#----------------
#----------------7.2.5 k=5----
t <- list()
k=13
cat('\n')
cat('\n')
cat(paste("####", names(MAllScalesBYCountryNEU)[[k]]))
cat('\n')
cat('\n')
rBYCntry$Fits[[names(MAllScalesBYCountryNEU)[[k]]]][[1]] %>% filter(NClass == 5) %>% 
  bind_rows(rBYCntry$Fits[[names(MAllScalesBYCountryNEU)[[k]]]][[2]] %>% filter(NClass == 5)) %>% 
  dplyr::select(-Chisq,-npar) %>% 
  kbl(caption = paste("Model fit",names(MAllScalesBYCountryNEU)[[k]]), row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% print()
cat('\n')
cat('\n')


MAllScalesBYCountryc1cl5 <- reshape2::melt(MAllScalesBYCountryNEU[[k]][[1]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc1cl5, nclass = 5)
MAllScalesBYCountryc1cl5$Class <- factor(MAllScalesBYCountryc1cl5$Class, 
                                         levels = c("class 5: ", "class 3: ", "class 4: ", "class 1: ", "class 2: "), 
                                         labels = classes5)
s1<-cbind("Sizes",rBYCntry$Size[[names(MAllScalesBYCountryNEU)[[k]]]][[1]] %>% filter(NClass == 5) %>% 
            dplyr::select(P.5, P.3, P.4, P.1, P.2))
colnames(s1) <- c("param",classes5)
cl1<- rbind(s1, reshape2::dcast(MAllScalesBYCountryc1cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

MAllScalesBYCountryc2cl5 <- reshape2::melt(MAllScalesBYCountryNEU[[k]][[2]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc2cl5, nclass = 5)
MAllScalesBYCountryc2cl5$Class <- factor(MAllScalesBYCountryc2cl5$Class, 
                                         levels = c("class 2: ", "class 1: ", "class 3: ", "class 5: ", "class 4: "), 
                                         labels = classes5)
s2<-cbind("Sizes",rBYCntry$Size[[names(MAllScalesBYCountryNEU)[[k]]]][[2]] %>% filter(NClass == 5) %>% 
            dplyr::select(P.2, P.1, P.3, P.5, P.4))
colnames(s2) <- c("param",classes5)
cl2<- rbind(s2, reshape2::dcast(MAllScalesBYCountryc2cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

t[[k]] <- left_join(cl1, cl2, by="param")
t[[k]] %>% kbl(digits = 2, caption = paste("Highest conditional probabilities to each class", names(MAllScalesBYCountryNEU)[[k]]),
               col.names = c("", rep(classes5,2))) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(2,7), border_left = T) %>% 
  add_header_above(c(paste(names(MAllScalesBYCountryNEU)[[k]]), "2009" = 5, "2016" = 5)) %>% 
  row_spec(1, bold = TRUE, italic = TRUE) %>% print()

#----------------
#----------------7.2.6 k=6----
t <- list()
k=14
cat('\n')
cat('\n')
cat(paste("####", names(MAllScalesBYCountryNEU)[[k]]))
cat('\n')
cat('\n')
rBYCntry$Fits[[names(MAllScalesBYCountryNEU)[[k]]]][[1]] %>% filter(NClass == 5) %>% 
  bind_rows(rBYCntry$Fits[[names(MAllScalesBYCountryNEU)[[k]]]][[2]] %>% filter(NClass == 5)) %>% 
  dplyr::select(-Chisq,-npar) %>% 
  kbl(caption = paste("Model fit",names(MAllScalesBYCountryNEU)[[k]]), row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% print()
cat('\n')
cat('\n')

MAllScalesBYCountryc1cl5 <- reshape2::melt(MAllScalesBYCountryNEU[[k]][[1]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc1cl5, nclass = 5)
MAllScalesBYCountryc1cl5$Class <- factor(MAllScalesBYCountryc1cl5$Class, 
                                         levels = c("class 4: ", "class 3: ", "class 2: ", "class 1: ", "class 5: "), 
                                         labels = classes5)
s1<-cbind("Sizes",rBYCntry$Size[[names(MAllScalesBYCountryNEU)[[k]]]][[1]] %>% filter(NClass == 5) %>% 
            dplyr::select(P.4, P.3, P.2, P.1, P.5))
colnames(s1) <- c("param",classes5)
cl1<- rbind(s1, reshape2::dcast(MAllScalesBYCountryc1cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

MAllScalesBYCountryc2cl5 <- reshape2::melt(MAllScalesBYCountryNEU[[k]][[2]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc2cl5, nclass = 5)
MAllScalesBYCountryc2cl5$Class <- factor(MAllScalesBYCountryc2cl5$Class, 
                                         levels = c("class 2: ", "class 3: ", "class 4: ", "class 1: ", "class 5: "), 
                                         labels = classes5)
s2<-cbind("Sizes",rBYCntry$Size[[names(MAllScalesBYCountryNEU)[[k]]]][[2]] %>% filter(NClass == 5) %>% 
            dplyr::select(P.2, P.3, P.4, P.1, P.5))
colnames(s2) <- c("param",classes5)
cl2<- rbind(s2, reshape2::dcast(MAllScalesBYCountryc2cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

t[[k]] <- left_join(cl1, cl2, by="param")
t[[k]] %>% kbl(digits = 2, caption = paste("Highest conditional probabilities to each class", names(MAllScalesBYCountryNEU)[[k]]),
               col.names = c("", rep(classes5,2))) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(2,7), border_left = T) %>% 
  add_header_above(c(paste(names(MAllScalesBYCountryNEU)[[k]]), "2009" = 5, "2016" = 5)) %>% 
  row_spec(1, bold = TRUE, italic = TRUE) %>% print()

#----------------
#----------------7.2.7 k=7----
t <- list()
k=15
cat('\n')
cat('\n')
cat(paste("####", names(MAllScalesBYCountryNEU)[[k]]))
cat('\n')
cat('\n')
rBYCntry$Fits[[names(MAllScalesBYCountryNEU)[[k]]]][[1]] %>% filter(NClass == 5) %>% 
  bind_rows(rBYCntry$Fits[[names(MAllScalesBYCountryNEU)[[k]]]][[2]] %>% filter(NClass == 5)) %>% 
  dplyr::select(-Chisq,-npar) %>% 
  kbl(caption = paste("Model fit",names(MAllScalesBYCountryNEU)[[k]]), row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% print()
cat('\n')
cat('\n')

MAllScalesBYCountryc1cl5 <- reshape2::melt(MAllScalesBYCountryNEU[[k]][[1]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc1cl5, nclass = 5)
MAllScalesBYCountryc1cl5$Class <- factor(MAllScalesBYCountryc1cl5$Class, 
                                         levels = c("class 2: ", "class 4: ", "class 5: ", "class 3: ", "class 1: "), 
                                         labels = classes5)
s1<-cbind("Sizes",rBYCntry$Size[[names(MAllScalesBYCountryNEU)[[k]]]][[1]] %>% filter(NClass == 5) %>% 
            dplyr::select(P.2, P.4, P.5, P.3, P.1))
colnames(s1) <- c("param",classes5)
cl1<- rbind(s1, reshape2::dcast(MAllScalesBYCountryc1cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

MAllScalesBYCountryc2cl5 <- reshape2::melt(MAllScalesBYCountryNEU[[k]][[2]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc2cl5, nclass = 5)
MAllScalesBYCountryc2cl5$Class <- factor(MAllScalesBYCountryc2cl5$Class, 
                                         levels = c("class 3: ", "class 4: ", "class 1: ", "class 2: ", "class 5: "), 
                                         labels = classes5)
s2<-cbind("Sizes",rBYCntry$Size[[names(MAllScalesBYCountryNEU)[[k]]]][[2]] %>% filter(NClass == 5) %>% 
            dplyr::select(P.3, P.4, P.1, P.2, P.5))
colnames(s2) <- c("param",classes5)
cl2<- rbind(s2, reshape2::dcast(MAllScalesBYCountryc2cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

t[[k]] <- left_join(cl1, cl2, by="param")
t[[k]] %>% kbl(digits = 2, caption = paste("Highest conditional probabilities to each class", names(MAllScalesBYCountryNEU)[[k]]),
               col.names = c("", rep(classes5,2))) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(2,7), border_left = T) %>% 
  add_header_above(c(paste(names(MAllScalesBYCountryNEU)[[k]]), "2009" = 5, "2016" = 5)) %>% 
  row_spec(1, bold = TRUE, italic = TRUE) %>% print()

#----------------
#----------------7.2.8 k=8----
t <- list()
k=16
cat('\n')
cat('\n')
cat(paste("####", names(MAllScalesBYCountryNEU)[[k]]))
cat('\n')
cat('\n')
rBYCntry$Fits[[names(MAllScalesBYCountryNEU)[[k]]]][[1]] %>% filter(NClass == 5) %>% 
  bind_rows(rBYCntry$Fits[[names(MAllScalesBYCountryNEU)[[k]]]][[2]] %>% filter(NClass == 5)) %>% 
  dplyr::select(-Chisq,-npar) %>% 
  kbl(caption = paste("Model fit",names(MAllScalesBYCountryNEU)[[k]]), row.names = FALSE) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(4,7,9), border_left = T) %>% print()
cat('\n')
cat('\n')

MAllScalesBYCountryc1cl5 <- reshape2::melt(MAllScalesBYCountryNEU[[k]][[1]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc1cl5, nclass = 5)
MAllScalesBYCountryc1cl5$Class <- factor(MAllScalesBYCountryc1cl5$Class, 
                                         levels = c("class 5: ", "class 1: ", "class 4: ", "class 2: ", "class 3: "), 
                                         labels = classes5)
s1<-cbind("Sizes",rBYCntry$Size[[names(MAllScalesBYCountryNEU)[[k]]]][[1]] %>% filter(NClass == 5) %>% 
            dplyr::select(P.5, P.1, P.4, P.2, P.3))
colnames(s1) <- c("param",classes5)
cl1<- rbind(s1, reshape2::dcast(MAllScalesBYCountryc1cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

MAllScalesBYCountryc2cl5 <- reshape2::melt(MAllScalesBYCountryNEU[[k]][[2]][[5]]$probs, level=2) %>% 
  rename_with(~ c("param", "category", "Class")[which(c("L2", "Var2", "Var1") == .x)], .cols = c("L2", "Var2", "Var1")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
#graphclass(MAllScalesBYCountryc2cl5, nclass = 5)
MAllScalesBYCountryc2cl5$Class <- factor(MAllScalesBYCountryc2cl5$Class, 
                                         levels = c("class 2: ", "class 5: ", "class 1: ", "class 3: ", "class 4: "), 
                                         labels = classes5)
s2<-cbind("Sizes",rBYCntry$Size[[names(MAllScalesBYCountryNEU)[[k]]]][[2]] %>% filter(NClass == 5) %>% 
            dplyr::select(P.2, P.5, P.1, P.3, P.4))
colnames(s2) <- c("param",classes5)
cl2<- rbind(s2, reshape2::dcast(MAllScalesBYCountryc2cl5 %>% group_by(param, Class) %>% filter(value == max(value)), param ~ Class))

t[[k]] <- left_join(cl1, cl2, by="param")
t[[k]] %>% kbl(digits = 2, caption = paste("Highest conditional probabilities to each class", names(MAllScalesBYCountryNEU)[[k]]),
               col.names = c("", rep(classes5,2))) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), font_size = 11) %>% 
  column_spec (c(2,7), border_left = T) %>% 
  add_header_above(c(paste(names(MAllScalesBYCountryNEU)[[k]]), "2009" = 5, "2016" = 5)) %>% 
  row_spec(1, bold = TRUE, italic = TRUE) %>% print()

#----------------
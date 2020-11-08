library(lme4)
library(lmerTest)

#Recodification and standardization of variables 
indexquest <- sapply(ISC_lv[,c("Ethn_Equal", "Gend_Equal", "Immi_Equal","HOMELIT", "SHOMELIT", "S_HOMLIT", "S_HISCED", "HISEI", "S_NISB", "NISB")], 
                     function(x) (x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))*100 ) #indicator (min 0 - max 100)
for (fs in colnames(indexquest)) {
  ISC_lv[, fs] <- indexquest[ , fs]
}

#Unify equal variables through cycles
ISC_lv <- ISC_lv %>%  group_by(cycle) %>% 
  mutate(T_AGE = ifelse(!is.na(AGE), AGE, 
                        ifelse(!is.na(SAGE), SAGE,
                               ifelse(!is.na(S_AGE), S_AGE, NA))),
         T_GENDER = ifelse(!is.na(GENDER), GENDER, 
                           ifelse(!is.na(SGENDER), SGENDER,
                                  ifelse(!is.na(S_GENDER), S_GENDER, NA))),
         T_HOMELIT = ifelse(!is.na(HOMELIT), HOMELIT, 
                           ifelse(!is.na(SHOMELIT), SHOMELIT,
                                  ifelse(!is.na(S_HOMLIT), S_HOMLIT, NA))),
         T_RELIG = ifelse(!is.na(RELIG), RELIG, 
                          ifelse(!is.na(S_RELIG), S_RELIG, NA)),
         T_HISEI = ifelse(!is.na(HISEI), HISEI, 
                          ifelse(!is.na(S_HISCED), S_HISCED, NA)),
         T_NISB = ifelse(!is.na(NISB), NISB, 
                          ifelse(!is.na(S_NISB), S_NISB, NA))
         )

  
  # by(ISC_lv[,c("Ethn_Equal", "Gend_Equal", "Immi_Equal", "T_AGE", "T_GENDER", "T_RELIG", "T_HOMELIT", "T_HISEI", "T_NISB")], ISC_lv[,"cycle"], summary)
  # by(ISC_lv[,"T_GENDER"], ISC_lv[,"cycle"], table)
  # str(ISC_lv$T_HOMELIT)

  #BSGBRN1 IMMIG S_IMMIG  

ds_ml1 <- ISC_lv %>% 
  dplyr::select(cycle, COUNTRY, Ethn_Equal, Gend_Equal, Immi_Equal) %>% na.omit() #, T_AGE, T_GENDER, T_HOMELIT, T_RELIG, T_HISEI, T_NISB)
table(ds_ml1$cycle,ds_ml1$COUNTRY)

modelNull <- lmer(Ethn_Equal ~  (1|COUNTRY) + 
                    (1|COUNTRY:cycle), 
                  data=ds_ml1, REML=FALSE)
summary(modelNull)
nullvar <- as.data.frame(VarCorr(modelNull))[,c("grp","vcov","sdcor")]
colnames(nullvar) <-  c("grp","vcov0","sdcor0")
nullvar %>% mutate(icc0=round(vcov0/sum(vcov0)*100,2)) %>% 
  dplyr::select(grp,vcov0,sdcor0,icc0)

cyc_ml <- unique(ds_ml1$cycle)
meast_ml <- NULL
for (cyml in cyc_ml) {
  modelNull <- lmer(Ethn_Equal ~  (1|COUNTRY), 
                    data=ds_ml1[ds_ml1$cycle == cyml,], REML=FALSE)
  summary(modelNull)
  nullvar <- as.data.frame(VarCorr(modelNull))[,c("grp","vcov","sdcor")]
  colnames(nullvar) <-  c("grp","vcov0","sdcor0")
  meast <- nullvar %>% mutate(icc0=round(vcov0/sum(vcov0)*100,2)) %>% 
    dplyr::select(grp,vcov0,sdcor0,icc0) %>% mutate(Group = cyml)
  meast_ml <- rbind(meast_ml,meast)
}
meast_ml

cnt_ml <- unique(ds_ml1$COUNTRY)
stdl_ml <- NULL
for (cml in cnt_ml) {
  modelNull <- lmer(Ethn_Equal ~  (1|cycle), 
                    data=ds_ml1[ds_ml1$COUNTRY == cml,], REML=FALSE)
  summary(modelNull)
  nullvar <- as.data.frame(VarCorr(modelNull))[,c("grp","vcov","sdcor")]
  colnames(nullvar) <-  c("grp","vcov0","sdcor0")
  stdl <- nullvar %>% mutate(icc0=round(vcov0/sum(vcov0)*100,2)) %>% 
    dplyr::select(grp,vcov0,sdcor0,icc0) %>% mutate(Group = cml)
  stdl_ml <- rbind(stdl_ml,stdl)
}
stdl_ml


ds_filtrada1$Fit0 <- predict(modelNull)

model1a <- lmer(Trust ~ eduyrs + 
                  (1|cntry) + 
                  (1|cntry:essround),
                data=ds_filtrada1, REML=FALSE)
nullvar %>% left_join(as.data.frame(VarCorr(model1a))[,c("grp","vcov","sdcor")]) %>% 
  mutate(varexp = round((vcov0-vcov)/sum(vcov0)*100,2))
anova(model1a)
ranova(model1a)
library(lme4)
library(lmerTest)
library(sjPlot)
setwd("C:/Users/pamel/OneDrive - KU Leuven/Master in Statistics/Master Thesis/Data_analysis")

#Recodification and standardization of variables 
by(ISC_lv[,c("Ethn_Equal", "Gend_Equal", "Immi_Equal","HOMELIT", "SHOMELIT", "S_HOMLIT", "S_HISCED", "HISEI", "S_NISB", "NISB")], 
   ISC_lv[,"COUNTRY"],summary)

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
                          ifelse(!is.na(S_NISB), S_NISB, NA)),
         SENWGT = ifelse(!is.na(SENWGT_Gc1), SENWGT_Gc1,
                         ifelse(!is.na(SENWGT_Gc2), SENWGT_Gc2,
                                ifelse(!is.na(SENWGT_Gc3), SENWGT_Gc3, NA)))
         )

# by(ISC_lv[, c("Ethn_Equal", "Gend_Equal", "Immi_Equal","T_HOMELIT", "T_HISEI", "T_NISB")], 
#    ISC_lv[, c("cycle","COUNTRY")],summary) 

  # by(ISC_lv[,c("Ethn_Equal", "Gend_Equal", "Immi_Equal", "T_AGE", "T_GENDER", "T_RELIG", "T_HOMELIT", "T_HISEI", "T_NISB")], ISC_lv[,"cycle"], summary)
  # by(ISC_lv[,"T_GENDER"], ISC_lv[,"cycle"], table)
  # str(ISC_lv$T_HOMELIT)

  #BSGBRN1 IMMIG S_IMMIG  

ds_ml0 <- ISC_lv %>% 
  dplyr::select(cycle, IDSCHOOL, COUNTRY, SENWGT, Ethn_Equal, Gend_Equal, Immi_Equal) #%>% na.omit() #, T_AGE, T_GENDER, T_HOMELIT, T_RELIG, T_HISEI, T_NISB)
table(ds_ml0$cycle,ds_ml0$COUNTRY)


mNullEthn <- lmer(Ethn_Equal ~  (1|cycle) + (1|cycle:COUNTRY) + (1|cycle:COUNTRY:IDSCHOOL), 
                  data=ds_ml0, weights=SENWGT, REML=FALSE)
mNullGndr <- lmer(Gend_Equal ~  (1|cycle) + (1|cycle:COUNTRY) + (1|cycle:COUNTRY:IDSCHOOL), 
                  data=ds_ml0, weights=SENWGT, REML=FALSE)
mNullImmi <- lmer(Immi_Equal ~  (1|cycle) + (1|cycle:COUNTRY) + (1|cycle:COUNTRY:IDSCHOOL), 
                  data=ds_ml0, weights=SENWGT, REML=FALSE)
t1 <- tab_model(mNullEthn, mNullGndr, mNullImmi)

modelNullEthnC1 <- lmer(Ethn_Equal ~ (1|COUNTRY) + (1|COUNTRY:IDSCHOOL),
                    data=ds_ml0[ds_ml0$cycle == "C1",], weights=SENWGT,  REML=FALSE)
modelNullGndrC1 <- lmer(Gend_Equal ~ (1|COUNTRY) + (1|COUNTRY:IDSCHOOL),
                    data=ds_ml0[ds_ml0$cycle == "C1",], weights=SENWGT,  REML=FALSE)
modelNullImmiC1 <- lmer(Immi_Equal ~ (1|COUNTRY) + (1|COUNTRY:IDSCHOOL),
                    data=ds_ml0[ds_ml0$cycle == "C1",], weights=SENWGT,  REML=FALSE)

modelNullEthnC2 <- lmer(Ethn_Equal ~ (1|COUNTRY) + (1|COUNTRY:IDSCHOOL),
                    data=ds_ml0[ds_ml0$cycle == "C2",], weights=SENWGT,  REML=FALSE)
modelNullGndrC2 <- lmer(Gend_Equal ~ (1|COUNTRY) + (1|COUNTRY:IDSCHOOL),
                    data=ds_ml0[ds_ml0$cycle == "C2",], weights=SENWGT,  REML=FALSE)
modelNullImmiC2 <- lmer(Immi_Equal ~ (1|COUNTRY) + (1|COUNTRY:IDSCHOOL),
                    data=ds_ml0[ds_ml0$cycle == "C2",], weights=SENWGT,  REML=FALSE)

modelNullEthnC3 <- lmer(Ethn_Equal ~ (1|COUNTRY) + (1|COUNTRY:IDSCHOOL),
                    data=ds_ml0[ds_ml0$cycle == "C3",], weights=SENWGT,  REML=FALSE)
modelNullGndrC3 <- lmer(Gend_Equal ~ (1|COUNTRY) + (1|COUNTRY:IDSCHOOL),
                    data=ds_ml0[ds_ml0$cycle == "C3",], weights=SENWGT,  REML=FALSE)
modelNullImmiC3 <- lmer(Immi_Equal ~ (1|COUNTRY) + (1|COUNTRY:IDSCHOOL),
                    data=ds_ml0[ds_ml0$cycle == "C3",], weights=SENWGT,  REML=FALSE)
t2 <- tab_model(modelNullEthnC1, modelNullEthnC2, modelNullEthnC3, dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                title = "Attitudes toward equal rights for all ethnic/racial groups")

t3 <- tab_model(modelNullGndrC1, modelNullGndrC2, modelNullGndrC3, dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                title = "Attitudes toward gender equality")

t4 <- tab_model(modelNullImmiC1, modelNullImmiC2, modelNullImmiC3, dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                title = "Attitudes toward equal rights for immigrants")


# cat('## Two level model: School nested in countries by cycles')
# cat('\n')
# cat('\n')
# cntryless2 <- ds_ml0 %>% group_by(COUNTRY) %>% count(cycle) %>% count(COUNTRY) %>% filter(n == 1) %>% select(COUNTRY) %>% pull()
# ds_ml0cntrygreat1 <- ds_ml0 %>% filter(!COUNTRY %in% cntryless2)
# cntrynull <- lapply(split(ds_ml0cntrygreat1,ds_ml0cntrygreat1$COUNTRY),
#        lmer, formula = Ethn_Equal ~ (1|cycle) + (1|cycle:IDSCHOOL), REML=FALSE )#,
#        #weights=list(split(ds_ml0cntrygreat1$SENWGT,ds_ml0cntrygreat1$COUNTRY)))
# print(tab_model(cntrynull[1:4], dv.labels = sort(unique(ds_ml0cntrygreat1$COUNTRY))[1:4]))
# print(tab_model(cntrynull[5:8], dv.labels = sort(unique(ds_ml0cntrygreat1$COUNTRY))[5:8]))
# print(tab_model(cntrynull[9:12], dv.labels = sort(unique(ds_ml0cntrygreat1$COUNTRY))[9:12]))
# print(tab_model(cntrynull[13:16], dv.labels = sort(unique(ds_ml0cntrygreat1$COUNTRY))[13:16]))
# print(tab_model(cntrynull[17:length(unique(ds_ml0cntrygreat1$COUNTRY))], dv.labels = sort(unique(ds_ml0cntrygreat1$COUNTRY))[17:length(unique(ds_ml0cntrygreat1$COUNTRY))]))


########Model 1 ###############
ds_ml1 <- ISC_lv %>%
  dplyr::select(cycle, COUNTRY, IDSCHOOL, SENWGT, Ethn_Equal, Gend_Equal, Immi_Equal,
                T_AGE, T_GENDER, T_HOMELIT, T_RELIG, T_HISEI, T_NISB) # %>% na.omit() #, T_HOMELIT, T_RELIG, T_HISEI, T_NISB)

mM1Ethn <- lmer(Ethn_Equal ~ T_AGE + T_GENDER + T_HOMELIT +
                    (1|cycle) + (1|cycle:COUNTRY) + (1|cycle:COUNTRY:IDSCHOOL),
                  data=ds_ml1, weights=SENWGT, REML=FALSE)
mM1Gndr <- lmer(Gend_Equal ~ T_AGE + T_GENDER + T_HOMELIT +
                    (1|cycle) + (1|cycle:COUNTRY) + (1|cycle:COUNTRY:IDSCHOOL),
                  data=ds_ml1, weights=SENWGT, REML=FALSE)
mM1Immi <- lmer(Immi_Equal ~ T_AGE + T_GENDER + T_HOMELIT +
                    (1|cycle) + (1|cycle:COUNTRY) + (1|cycle:COUNTRY:IDSCHOOL),
                  data=ds_ml1, weights=SENWGT, REML=FALSE)
t5 <- tab_model(mM1Ethn, mM1Gndr, mM1Immi)

modelM1EthnC1 <- lmer(Ethn_Equal ~ T_AGE + T_GENDER + T_HOMELIT +
                        (1|COUNTRY) + (1|COUNTRY:IDSCHOOL),
                        data=ds_ml1[ds_ml1$cycle == "C1",], weights=SENWGT,  REML=FALSE)
modelM1GndrC1 <- lmer(Gend_Equal ~ T_AGE + T_GENDER + T_HOMELIT +
                        (1|COUNTRY) + (1|COUNTRY:IDSCHOOL),
                        data=ds_ml1[ds_ml1$cycle == "C1",], weights=SENWGT,  REML=FALSE)
modelM1ImmiC1 <- lmer(Immi_Equal ~ T_AGE + T_GENDER + T_HOMELIT +
                        (1|COUNTRY) + (1|COUNTRY:IDSCHOOL),
                        data=ds_ml1[ds_ml1$cycle == "C1",], weights=SENWGT,  REML=FALSE)

modelM1EthnC2 <- lmer(Ethn_Equal ~ T_AGE + T_GENDER + T_HOMELIT + T_RELIG + T_HISEI + T_NISB +
                        (1|COUNTRY) + (1|COUNTRY:IDSCHOOL),
                        data=ds_ml1[ds_ml1$cycle == "C2",], weights=SENWGT,  REML=FALSE)
modelM1GndrC2 <- lmer(Gend_Equal ~ T_AGE + T_GENDER + T_HOMELIT + T_RELIG + T_HISEI + T_NISB +
                        (1|COUNTRY) + (1|COUNTRY:IDSCHOOL),
                        data=ds_ml1[ds_ml1$cycle == "C2",], weights=SENWGT,  REML=FALSE)
modelM1ImmiC2 <- lmer(Immi_Equal ~ T_AGE + T_GENDER + T_HOMELIT + T_RELIG + T_HISEI + T_NISB +
                        (1|COUNTRY) + (1|COUNTRY:IDSCHOOL),
                        data=ds_ml1[ds_ml1$cycle == "C2",], weights=SENWGT,  REML=FALSE)

modelM1EthnC3 <- lmer(Ethn_Equal ~ T_AGE + T_GENDER + T_HOMELIT + T_RELIG + T_HISEI + T_NISB +
                        (1|COUNTRY) + (1|COUNTRY:IDSCHOOL),
                        data=ds_ml1[ds_ml1$cycle == "C3",], weights=SENWGT,  REML=FALSE)
modelM1GndrC3 <- lmer(Gend_Equal ~ T_AGE + T_GENDER + T_HOMELIT + T_RELIG + T_HISEI + T_NISB +
                        (1|COUNTRY) + (1|COUNTRY:IDSCHOOL),
                        data=ds_ml1[ds_ml1$cycle == "C3",], weights=SENWGT,  REML=FALSE)
modelM1ImmiC3 <- lmer(Immi_Equal ~ T_AGE + T_GENDER + T_HOMELIT + T_RELIG + T_HISEI + T_NISB +
                        (1|COUNTRY) + (1|COUNTRY:IDSCHOOL),
                        data=ds_ml1[ds_ml1$cycle == "C3",], weights=SENWGT,  REML=FALSE)

b11 <- tab_model(modelM1EthnC1, modelM1EthnC2, modelM1EthnC3, dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                title = "Attitudes toward equal rights for all ethnic/racial groups")
b21 <- tab_model(modelM1GndrC1, modelM1GndrC2, modelM1GndrC3, dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                title = "Attitudes toward gender equality")
b31 <- tab_model(modelM1ImmiC1, modelM1ImmiC2, modelM1ImmiC3, dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                title = "Attitudes toward equal rights for immigrants ")


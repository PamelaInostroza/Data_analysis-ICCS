library(lme4)
library(lmerTest)

ds_ml0 <- ISC_lvR %>% 
  dplyr::select(cycle, IDSCHOOL, COUNTRY, SENWGT, Ethn_Equal, Gend_Equal, Immi_Equal) 
table(ds_ml0$cycle,ds_ml0$COUNTRY)

L3NullImmi <- lmer(Immi_Equal ~  (1|cycle) + (1|cycle:COUNTRY) + (1|cycle:COUNTRY:IDSCHOOL), 
                   data=ds_ml0, weights=SENWGT, REML=FALSE)
L3NullGndr <- lmer(Gend_Equal ~  (1|cycle) + (1|cycle:COUNTRY) + (1|cycle:COUNTRY:IDSCHOOL), 
                  data=ds_ml0, weights=SENWGT, REML=FALSE)
L3NullEthn <- lmer(Ethn_Equal ~  (1|cycle) + (1|cycle:COUNTRY) + (1|cycle:COUNTRY:IDSCHOOL), 
                   data=ds_ml0, weights=SENWGT, REML=FALSE)
t1 <- tab_model(L3NullImmi, L3NullGndr, L3NullEthn,collapse.ci = TRUE, p.style = "stars")

L2NullImmiC1 <- lmer(Immi_Equal ~ (1|COUNTRY) + (1|COUNTRY:IDSCHOOL),
                    data=ds_ml0[ds_ml0$cycle == "C1",], weights=SENWGT,  REML=FALSE)
L2NullImmiC2 <- lmer(Immi_Equal ~ (1|COUNTRY) + (1|COUNTRY:IDSCHOOL),
                     data=ds_ml0[ds_ml0$cycle == "C2",], weights=SENWGT,  REML=FALSE)
L2NullImmiC3 <- lmer(Immi_Equal ~ (1|COUNTRY) + (1|COUNTRY:IDSCHOOL),
                     data=ds_ml0[ds_ml0$cycle == "C3",], weights=SENWGT,  REML=FALSE)

L2NullGndrC1 <- lmer(Gend_Equal ~ (1|COUNTRY) + (1|COUNTRY:IDSCHOOL),
                     data=ds_ml0[ds_ml0$cycle == "C1",], weights=SENWGT,  REML=FALSE)
L2NullGndrC2 <- lmer(Gend_Equal ~ (1|COUNTRY) + (1|COUNTRY:IDSCHOOL),
                    data=ds_ml0[ds_ml0$cycle == "C2",], weights=SENWGT,  REML=FALSE)
L2NullGndrC3 <- lmer(Gend_Equal ~ (1|COUNTRY) + (1|COUNTRY:IDSCHOOL),
                     data=ds_ml0[ds_ml0$cycle == "C3",], weights=SENWGT,  REML=FALSE)

L2NullEthnC1 <- lmer(Ethn_Equal ~ (1|COUNTRY) + (1|COUNTRY:IDSCHOOL),
                     data=ds_ml0[ds_ml0$cycle == "C1",], weights=SENWGT,  REML=FALSE)
L2NullEthnC2 <- lmer(Ethn_Equal ~ (1|COUNTRY) + (1|COUNTRY:IDSCHOOL),
                     data=ds_ml0[ds_ml0$cycle == "C2",], weights=SENWGT,  REML=FALSE)
L2NullEthnC3 <- lmer(Ethn_Equal ~ (1|COUNTRY) + (1|COUNTRY:IDSCHOOL),
                     data=ds_ml0[ds_ml0$cycle == "C3",], weights=SENWGT,  REML=FALSE)


t2 <- tab_model(L2NullImmiC1, L2NullImmiC2, L2NullImmiC3, dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                collapse.ci = TRUE, p.style = "stars",
                title = "Attitudes toward equal rights for immigrants")

t3 <- tab_model(L2NullGndrC1, L2NullGndrC2, L2NullGndrC3, dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                collapse.ci = TRUE, p.style = "stars",
                title = "Attitudes toward gender equality")

t4 <- tab_model(L2NullEthnC1, L2NullEthnC2, L2NullEthnC3, dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                collapse.ci = TRUE, p.style = "stars",
                title = "Attitudes toward equal rights for all ethnic/racial groups")


########Model 1 ###############
ds_ml1 <- ISC_lvR %>%
  dplyr::select(cycle, COUNTRY, IDSCHOOL, SENWGT, Ethn_Equal, Gend_Equal, Immi_Equal, all_of(Man_cate), all_of(Man_cont)) 

Man_cate2 <- Man_cate[!grepl(paste0(c("T_HIGHEDFA", "T_HISCED"), collapse = "|"), Man_cate)]
Man_cont2 <- Man_cont[!grepl(paste0(c("T_NISB", "T_CITRESP"), collapse = "|"), Man_cont)]

form <- paste0(paste(Man_cate2, collapse = " + "), " + ", paste(Man_cont2, collapse = " + "))

L3M1Immi <- lmer(as.formula(paste("Immi_Equal ~", form, " +
                   (1|cycle) + (1|cycle:COUNTRY) + (1|cycle:COUNTRY:IDSCHOOL)")),
                 data=ds_ml1, weights=SENWGT, REML=FALSE)
L3M1Gndr <- lmer(as.formula(paste("Gend_Equal ~", form, " +
                    (1|cycle) + (1|cycle:COUNTRY) + (1|cycle:COUNTRY:IDSCHOOL)")),
                  data=ds_ml1, weights=SENWGT, REML=FALSE)
L3M1Ethn <- lmer(as.formula(paste("Ethn_Equal ~", form, " +
                    (1|cycle) + (1|cycle:COUNTRY) + (1|cycle:COUNTRY:IDSCHOOL)")),
                 data=ds_ml1, weights=SENWGT, REML=FALSE)
t5 <- tab_model(L3M1Immi, L3M1Gndr, L3M1Ethn, collapse.ci = TRUE, p.style = "stars")

Man_cate3 <- Man_cate2[!grepl(paste0(c("T_HIGHEDEXP", "T_RELIG", "T_PROTES1"), collapse = "|"), Man_cate2)]
Man_cont3 <- Man_cont2[!grepl(paste0(c("T_NISB", "T_HISEI", "T_PROTES", "T_CNTATT", "T_ELECPART", "T_LEGACT", "T_WIDEPART", "T_CITRESP"), collapse = "|"), Man_cont2)]
form1 <- paste0(paste(Man_cate3, collapse = " + "), " + ", paste(Man_cont3, collapse = " + "))

L2M1ImmiC1 <- lmer(as.formula(paste("Immi_Equal ~ ", form1, " +
                        (1|COUNTRY) + (1|COUNTRY:IDSCHOOL)")),
                   data=ds_ml1[ds_ml1$cycle == "C1",], weights=SENWGT,  REML=FALSE)
L2M1ImmiC2 <- lmer(as.formula(paste("Immi_Equal ~ ", form, " +
                        (1|COUNTRY) + (1|COUNTRY:IDSCHOOL)")),
                   data=ds_ml1[ds_ml1$cycle == "C2",], weights=SENWGT,  REML=FALSE)
L2M1ImmiC3 <- lmer(as.formula(paste("Immi_Equal ~ ", form, " +
                        (1|COUNTRY) + (1|COUNTRY:IDSCHOOL)")),
                   data=ds_ml1[ds_ml1$cycle == "C3",], weights=SENWGT,  REML=FALSE)

L2M1GndrC1 <- lmer(as.formula(paste("Gend_Equal ~ ", form1, " +
                        (1|COUNTRY) + (1|COUNTRY:IDSCHOOL)")),
                        data=ds_ml1[ds_ml1$cycle == "C1",], weights=SENWGT,  REML=FALSE)
L2M1GndrC2 <- lmer(as.formula(paste("Gend_Equal ~ ", form, " +
                        (1|COUNTRY) + (1|COUNTRY:IDSCHOOL)")),
                        data=ds_ml1[ds_ml1$cycle == "C2",], weights=SENWGT,  REML=FALSE)
L2M1GndrC3 <- lmer(as.formula(paste("Gend_Equal ~ ", form, " +
                        (1|COUNTRY) + (1|COUNTRY:IDSCHOOL)")),
                   data=ds_ml1[ds_ml1$cycle == "C3",], weights=SENWGT,  REML=FALSE)

L2M1EthnC1 <- lmer(as.formula(paste("Ethn_Equal ~ ", form1, " +
                        (1|COUNTRY) + (1|COUNTRY:IDSCHOOL)")),
                   data=ds_ml1[ds_ml1$cycle == "C1",], weights=SENWGT,  REML=FALSE)
L2M1EthnC2 <- lmer(as.formula(paste("Ethn_Equal ~ ", form, " +
                        (1|COUNTRY) + (1|COUNTRY:IDSCHOOL)")),
                   data=ds_ml1[ds_ml1$cycle == "C2",], weights=SENWGT,  REML=FALSE)
L2M1EthnC3 <- lmer(as.formula(paste("Ethn_Equal ~ ", form, " +
                        (1|COUNTRY) + (1|COUNTRY:IDSCHOOL)")),
                        data=ds_ml1[ds_ml1$cycle == "C3",], weights=SENWGT,  REML=FALSE)

b11 <- tab_model(L2M1ImmiC1, L2M1ImmiC2, L2M1ImmiC3, dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                 collapse.ci = TRUE, p.style = "stars",
                 title = "Attitudes toward equal rights for immigrants ")
b21 <- tab_model(L2M1GndrC1, L2M1GndrC2, L2M1GndrC3, dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                 collapse.ci = TRUE, p.style = "stars",
                title = "Attitudes toward gender equality")
b31 <- tab_model(L2M1EthnC1, L2M1EthnC2, L2M1EthnC3, dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                 collapse.ci = TRUE, p.style = "stars",
                 title = "Attitudes toward equal rights for all ethnic/racial groups")


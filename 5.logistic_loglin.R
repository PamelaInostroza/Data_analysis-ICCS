library(survey)
library(jtools)

options(survey.lonely.psu="adjust")

ds_ll0 <- ISC_lvR %>% 
  dplyr::select(all_of(Id), all_of(sampleID), all_of(Scales), all_of(Scalesb), all_of(Man_cate), all_of(Man_cont)) 

Man_cateSignf <- Man_cate[!grepl(paste0(c("T_HIGHEDFA", "T_HISCED"), collapse = "|"), Man_cate)]

#Selection of variables to be used in models for availability
Man_cateC2C3 <- Man_cateSignf[!grepl(paste0(c("T_HIGHEDFA", "T_HISCED"), collapse = "|"), Man_cateSignf)]
form <- paste0(paste(Man_cateC2C3, collapse = " + "))
#Special selection for cycle 1 for availability
Man_cateC1 <- Man_cateSignf[!grepl(paste0(c("T_HIGHEDEXP", "T_RELIG", "T_HISCED", "T_PROTES1"), collapse = "|"), Man_cateSignf)]
formC1 <- paste0(paste(Man_cateC1, collapse = " + "))

survey.designC1 <- svydesign(ids = ~IDCL, weights = ~SENWGT, data=ds_ll0[ds_ll0$cycle == "C1",], strata = ~IDJK, nest = TRUE)
survey.designC2 <- svydesign(ids = ~IDCL, weights = ~SENWGT, data=ds_ll0[ds_ll0$cycle == "C2",], strata = ~IDJK, nest = TRUE)
survey.designC3 <- svydesign(ids = ~IDCL, weights = ~SENWGT, data=ds_ll0[ds_ll0$cycle == "C3",], strata = ~IDJK, nest = TRUE)

###########################################################
#############Log linear regression / Poisson ###############
###########################################################
logl_IMMIEQ1C1 <- svyglm(as.formula(paste("as.numeric(T_IMMIEQ1) ~", formC1 )), 
                    data = ds_ll0[ds_ll0$cycle == "C1",], family = poisson, design = survey.designC1)
logl_IMMIEQ1C2 <- svyglm(as.formula(paste("as.numeric(T_IMMIEQ1) ~", form )), 
                    data = ds_ll0[ds_ll0$cycle == "C2",], family = poisson, design = survey.designC2)
logl_IMMIEQ1C3 <- svyglm(as.formula(paste("as.numeric(T_IMMIEQ1) ~", form )), 
                    data = ds_ll0[ds_ll0$cycle == "C3",], family = poisson, design = survey.designC3)
ll11 <- tab_model(logl_IMMIEQ1C1, logl_IMMIEQ1C2, logl_IMMIEQ1C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_IMMIEQ1)$label, "Moving/<Immigrants> ")))
#plot_summs(logl_IMMIEQ1C1, logl_IMMIEQ1C2, logl_IMMIEQ1C3, scale = TRUE, exp = TRUE)

logl_IMMIEQ2C1 <- svyglm(as.formula(paste("as.numeric(T_IMMIEQ2) ~", formC1 )), 
                    data = ds_ll0[ds_ll0$cycle == "C1",], family = poisson, design = survey.designC1)
logl_IMMIEQ2C2 <- svyglm(as.formula(paste("as.numeric(T_IMMIEQ2) ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C2",], family = poisson, design = survey.designC2)
logl_IMMIEQ2C3 <- svyglm(as.formula(paste("as.numeric(T_IMMIEQ2) ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C3",], family = poisson, design = survey.designC3)
ll12 <- tab_model(logl_IMMIEQ2C1, logl_IMMIEQ2C2, logl_IMMIEQ2C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_IMMIEQ2)$label, "Moving/<Immigrants> ")))

logl_IMMIEQ3C1 <- svyglm(as.formula(paste("as.numeric(T_IMMIEQ3) ~", formC1 )), 
                    data = ds_ll0[ds_ll0$cycle == "C1",], family = poisson, design = survey.designC1)
logl_IMMIEQ3C2 <- svyglm(as.formula(paste("as.numeric(T_IMMIEQ3) ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C2",], family = poisson, design = survey.designC2)
logl_IMMIEQ3C3 <- svyglm(as.formula(paste("as.numeric(T_IMMIEQ3) ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C3",], family = poisson, design = survey.designC3)
ll13 <- tab_model(logl_IMMIEQ3C1, logl_IMMIEQ3C2, logl_IMMIEQ3C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_IMMIEQ3)$label, "Moving/<Immigrants> ")))

logl_IMMIEQ4C1 <- svyglm(as.formula(paste("as.numeric(T_IMMIEQ4) ~", formC1 )), 
                    data = ds_ll0[ds_ll0$cycle == "C1",], family = poisson, design = survey.designC1)
logl_IMMIEQ4C2 <- svyglm(as.formula(paste("as.numeric(T_IMMIEQ4) ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C2",], family = poisson, design = survey.designC2)
logl_IMMIEQ4C3 <- svyglm(as.formula(paste("as.numeric(T_IMMIEQ4) ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C3",], family = poisson, design = survey.designC3)
ll14 <- tab_model(logl_IMMIEQ4C1, logl_IMMIEQ4C2, logl_IMMIEQ4C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_IMMIEQ4)$label, "Moving/<Immigrants> ")))

logl_IMMIEQ5C1 <- svyglm(as.formula(paste("as.numeric(T_IMMIEQ5) ~", formC1 )), 
                    data = ds_ll0[ds_ll0$cycle == "C1",], family = poisson, design = survey.designC1)
logl_IMMIEQ5C2 <- svyglm(as.formula(paste("as.numeric(T_IMMIEQ5) ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C2",], family = poisson, design = survey.designC2)
logl_IMMIEQ5C3 <- svyglm(as.formula(paste("as.numeric(T_IMMIEQ5) ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C3",], family = poisson, design = survey.designC3)
ll15 <- tab_model(logl_IMMIEQ5C1, logl_IMMIEQ5C2, logl_IMMIEQ5C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_IMMIEQ5)$label, "Moving/<Immigrants> ")))


logl_GNDREQ1C1 <- svyglm(as.formula(paste("as.numeric(T_GNDREQ1) ~", formC1 )), 
                    data = ds_ll0[ds_ll0$cycle == "C1",], family = poisson, design = survey.designC1)
logl_GNDREQ1C2 <- svyglm(as.formula(paste("as.numeric(T_GNDREQ1) ~", form )), 
                    data = ds_ll0[ds_ll0$cycle == "C2",], family = poisson, design = survey.designC2)
logl_GNDREQ1C3 <- svyglm(as.formula(paste("as.numeric(T_GNDREQ1) ~", form )), 
                    data = ds_ll0[ds_ll0$cycle == "C3",], family = poisson, design = survey.designC3)
ll21 <- tab_model(logl_GNDREQ1C1, logl_GNDREQ1C2, logl_GNDREQ1C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_GNDREQ1)$label, "Rights and Responsibilities/Roles women and men/")))

logl_GNDREQ2C1 <- svyglm(as.formula(paste("as.numeric(T_GNDREQ2) ~", formC1 )), 
                    data = ds_ll0[ds_ll0$cycle == "C1",], family = poisson, design = survey.designC1)
logl_GNDREQ2C2 <- svyglm(as.formula(paste("as.numeric(T_GNDREQ2) ~", form )), 
                    data = ds_ll0[ds_ll0$cycle == "C2",], family = poisson, design = survey.designC2)
logl_GNDREQ2C3 <- svyglm(as.formula(paste("as.numeric(T_GNDREQ2) ~", form )), 
                    data = ds_ll0[ds_ll0$cycle == "C3",], family = poisson, design = survey.designC3)
ll22 <- tab_model(logl_GNDREQ2C1, logl_GNDREQ2C2, logl_GNDREQ2C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_GNDREQ2)$label, "Rights and Responsibilities/Roles women and men/")))

logl_GNDREQ3C1 <- svyglm(as.formula(paste("as.numeric(T_GNDREQ3) ~", formC1 )), 
                    data = ds_ll0[ds_ll0$cycle == "C1",], family = poisson, design = survey.designC1)
logl_GNDREQ3C2 <- svyglm(as.formula(paste("as.numeric(T_GNDREQ3) ~", form )), 
                    data = ds_ll0[ds_ll0$cycle == "C2",], family = poisson, design = survey.designC2)
logl_GNDREQ3C3 <- svyglm(as.formula(paste("as.numeric(T_GNDREQ3) ~", form )), 
                    data = ds_ll0[ds_ll0$cycle == "C3",], family = poisson, design = survey.designC3)
ll23 <- tab_model(logl_GNDREQ3C1, logl_GNDREQ3C2, logl_GNDREQ3C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_GNDREQ3)$label, "Rights and Responsibilities/Roles women and men/")))

logl_GNDREQ4C1 <- svyglm(as.formula(paste("as.numeric(T_GNDREQ4) ~", formC1 )), 
                    data = ds_ll0[ds_ll0$cycle == "C1",], family = poisson, design = survey.designC1)
logl_GNDREQ4C2 <- svyglm(as.formula(paste("as.numeric(T_GNDREQ4) ~", form )), 
                    data = ds_ll0[ds_ll0$cycle == "C2",], family = poisson, design = survey.designC2)
logl_GNDREQ4C3 <- svyglm(as.formula(paste("as.numeric(T_GNDREQ4) ~", form )), 
                    data = ds_ll0[ds_ll0$cycle == "C3",], family = poisson, design = survey.designC3)
ll24 <- tab_model(logl_GNDREQ4C1, logl_GNDREQ4C2, logl_GNDREQ4C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_GNDREQ4)$label, "Rights and Responsibilities/Roles women and men/")))

logl_GNDREQ5C1 <- svyglm(as.formula(paste("as.numeric(T_GNDREQ5) ~", formC1 )), 
                    data = ds_ll0[ds_ll0$cycle == "C1",], family = poisson, design = survey.designC1)
logl_GNDREQ5C2 <- svyglm(as.formula(paste("as.numeric(T_GNDREQ5) ~", form )), 
                    data = ds_ll0[ds_ll0$cycle == "C2",], family = poisson, design = survey.designC2)
logl_GNDREQ5C3 <- svyglm(as.formula(paste("as.numeric(T_GNDREQ5) ~", form )), 
                    data = ds_ll0[ds_ll0$cycle == "C3",], family = poisson, design = survey.designC3)
ll25 <- tab_model(logl_GNDREQ5C1, logl_GNDREQ5C2, logl_GNDREQ5C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_GNDREQ5)$label, "Rights and Responsibilities/Roles women and men/")))

logl_GNDREQ6C1 <- svyglm(as.formula(paste("as.numeric(T_GNDREQ6) ~", formC1 )), 
                    data = ds_ll0[ds_ll0$cycle == "C1",], family = poisson, design = survey.designC1)
logl_GNDREQ6C2 <- svyglm(as.formula(paste("as.numeric(T_GNDREQ6) ~", form )), 
                    data = ds_ll0[ds_ll0$cycle == "C2",], family = poisson, design = survey.designC2)
logl_GNDREQ6C3 <- svyglm(as.formula(paste("as.numeric(T_GNDREQ6) ~", form )), 
                    data = ds_ll0[ds_ll0$cycle == "C3",], family = poisson, design = survey.designC3)
ll26 <- tab_model(logl_GNDREQ6C1, logl_GNDREQ6C2, logl_GNDREQ6C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_GNDREQ6)$label, "Rights and Responsibilities/Roles women and men/")))

logl_GNDREQ7C3 <- svyglm(as.formula(paste("as.numeric(T_GNDREQ7) ~", form )), 
                    data = ds_ll0[ds_ll0$cycle == "C3",], family = poisson, design = survey.designC3)
ll27 <- tab_model(logl_GNDREQ7C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_GNDREQ7)$label, "Rights and Responsibilities/Roles women and men/")))


logl_ETHNEQ1C1 <- svyglm(as.formula(paste("as.numeric(T_ETHNEQ1) ~", formC1 )), 
                    data = ds_ll0[ds_ll0$cycle == "C1",], family = poisson, design = survey.designC1)
logl_ETHNEQ1C2 <- svyglm(as.formula(paste("as.numeric(T_ETHNEQ1) ~", form )), 
                    data = ds_ll0[ds_ll0$cycle == "C2",], family = poisson, design = survey.designC2)
logl_ETHNEQ1C3 <- svyglm(as.formula(paste("as.numeric(T_ETHNEQ1) ~", form )), 
                    data = ds_ll0[ds_ll0$cycle == "C3",], family = poisson, design = survey.designC3)
ll31 <- tab_model(logl_ETHNEQ1C1, logl_ETHNEQ1C2, logl_ETHNEQ1C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_ETHNEQ1)$label, "Rights and Responsibilities/Rights and responsibilities/")))

logl_ETHNEQ2C1 <- svyglm(as.formula(paste("as.numeric(T_ETHNEQ2) ~", formC1 )), 
                    data = ds_ll0[ds_ll0$cycle == "C1",], family = poisson, design = survey.designC1)
logl_ETHNEQ2C2 <- svyglm(as.formula(paste("as.numeric(T_ETHNEQ2) ~", form )), 
                    data = ds_ll0[ds_ll0$cycle == "C2",], family = poisson, design = survey.designC2)
logl_ETHNEQ2C3 <- svyglm(as.formula(paste("as.numeric(T_ETHNEQ2) ~", form )), 
                    data = ds_ll0[ds_ll0$cycle == "C3",], family = poisson, design = survey.designC3)
ll32 <- tab_model(logl_ETHNEQ2C1, logl_ETHNEQ2C2, logl_ETHNEQ2C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_ETHNEQ2)$label, "Rights and Responsibilities/Rights and responsibilities/")))

logl_ETHNEQ3C1 <- svyglm(as.formula(paste("as.numeric(T_ETHNEQ3) ~", formC1 )), 
                    data = ds_ll0[ds_ll0$cycle == "C1",], family = poisson, design = survey.designC1)
logl_ETHNEQ3C2 <- svyglm(as.formula(paste("as.numeric(T_ETHNEQ3) ~", form )), 
                    data = ds_ll0[ds_ll0$cycle == "C2",], family = poisson, design = survey.designC2)
logl_ETHNEQ3C3 <- svyglm(as.formula(paste("as.numeric(T_ETHNEQ3) ~", form )), 
                    data = ds_ll0[ds_ll0$cycle == "C3",], family = poisson, design = survey.designC3)
ll33 <- tab_model(logl_ETHNEQ3C1, logl_ETHNEQ3C2, logl_ETHNEQ3C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_ETHNEQ3)$label, "Rights and Responsibilities/Rights and responsibilities/")))

logl_ETHNEQ4C1 <- svyglm(as.formula(paste("as.numeric(T_ETHNEQ4) ~", formC1 )), 
                    data = ds_ll0[ds_ll0$cycle == "C1",], family = poisson, design = survey.designC1)
logl_ETHNEQ4C2 <- svyglm(as.formula(paste("as.numeric(T_ETHNEQ4) ~", form )), 
                    data = ds_ll0[ds_ll0$cycle == "C2",], family = poisson, design = survey.designC2)
logl_ETHNEQ4C3 <- svyglm(as.formula(paste("as.numeric(T_ETHNEQ4) ~", form )), 
                    data = ds_ll0[ds_ll0$cycle == "C3",], family = poisson, design = survey.designC3)
ll34 <- tab_model(logl_ETHNEQ4C1, logl_ETHNEQ4C2, logl_ETHNEQ4C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_ETHNEQ4)$label, "Rights and Responsibilities/Rights and responsibilities/")))

logl_ETHNEQ5C2 <- svyglm(as.formula(paste("as.numeric(T_ETHNEQ5) ~", form )), 
                    data = ds_ll0[ds_ll0$cycle == "C2",], family = poisson, design = survey.designC2)
logl_ETHNEQ5C3 <- svyglm(as.formula(paste("as.numeric(T_ETHNEQ5) ~", form )), 
                    data = ds_ll0[ds_ll0$cycle == "C3",], family = poisson, design = survey.designC3)
ll35 <- tab_model(logl_ETHNEQ5C2, logl_ETHNEQ5C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_ETHNEQ5)$label, "Rights and Responsibilities/Rights and responsibilities/")))
rm(logl_IMMIEQ5C3,logl_IMMIEQ5C2,logl_IMMIEQ5C1,logl_IMMIEQ4C3,logl_IMMIEQ4C2,logl_IMMIEQ4C1,logl_IMMIEQ3C3,logl_IMMIEQ3C2,logl_IMMIEQ3C1,
   logl_IMMIEQ2C3,logl_IMMIEQ2C2,logl_IMMIEQ2C1,logl_IMMIEQ1C3,logl_IMMIEQ1C2,logl_IMMIEQ1C1,
   logl_ETHNEQ5C3,logl_ETHNEQ5C2,logl_ETHNEQ4C3,logl_ETHNEQ4C2,logl_ETHNEQ4C1,logl_ETHNEQ3C3,logl_ETHNEQ3C2,logl_ETHNEQ3C1,
   logl_ETHNEQ2C3,logl_ETHNEQ2C2,logl_ETHNEQ2C1,logl_ETHNEQ1C3,logl_ETHNEQ1C2,logl_ETHNEQ1C1,
   logl_GNDREQ7C3,logl_GNDREQ6C3,logl_GNDREQ6C2,logl_GNDREQ6C1,logl_GNDREQ5C3,logl_GNDREQ5C2,logl_GNDREQ5C1,logl_GNDREQ4C3,logl_GNDREQ4C2,logl_GNDREQ4C1,logl_GNDREQ3C3,logl_GNDREQ3C2,logl_GNDREQ3C1,
   logl_GNDREQ2C3,logl_GNDREQ2C2,logl_GNDREQ2C1,logl_GNDREQ1C3,logl_GNDREQ1C2,logl_GNDREQ1C1)

###########################################################
#############     Ordinal regression  #####################
###########################################################

fit.solr1<-svyolr(as.formula(paste("T_GNDREQ1 ~", formC1 )), design = survey.designC1)
tab_model(fit.solr1, collapse.ci = TRUE, p.style = "stars")

ord_IMMIEQ1C1 <- svyolr(as.formula(paste("T_IMMIEQ1 ~", formC1 )), design = survey.designC1)
ord_IMMIEQ1C2 <- svyolr(as.formula(paste("T_IMMIEQ1 ~", form )), design = survey.designC2)
ord_IMMIEQ1C3 <- svyolr(as.formula(paste("T_IMMIEQ1 ~", form )), design = survey.designC3)
or11 <- tab_model(ord_IMMIEQ1C1, ord_IMMIEQ1C2, ord_IMMIEQ1C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_IMMIEQ1)$label, "Moving/<Immigrants> ")))

ord_IMMIEQ2C1 <- svyolr(as.formula(paste("T_IMMIEQ2 ~", formC1 )), design = survey.designC1)
ord_IMMIEQ2C2 <- svyolr(as.formula(paste("T_IMMIEQ2 ~", form )), design = survey.designC2)
ord_IMMIEQ2C3 <- svyolr(as.formula(paste("T_IMMIEQ2 ~", form )), design = survey.designC3)
or12 <- tab_model(ord_IMMIEQ2C1, ord_IMMIEQ2C2, ord_IMMIEQ2C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_IMMIEQ2)$label, "Moving/<Immigrants> ")))

ord_IMMIEQ3C1 <- svyolr(as.formula(paste("T_IMMIEQ3 ~", formC1 )), design = survey.designC1)
ord_IMMIEQ3C2 <- svyolr(as.formula(paste("T_IMMIEQ3 ~", form )), design = survey.designC2)
ord_IMMIEQ3C3 <- svyolr(as.formula(paste("T_IMMIEQ3 ~", form )), design = survey.designC3)
or13 <- tab_model(ord_IMMIEQ3C1, ord_IMMIEQ3C2, ord_IMMIEQ3C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_IMMIEQ3)$label, "Moving/<Immigrants> ")))

ord_IMMIEQ4C1 <- svyolr(as.formula(paste("T_IMMIEQ4 ~", formC1 )), design = survey.designC1)
ord_IMMIEQ4C2 <- svyolr(as.formula(paste("T_IMMIEQ4 ~", form )), design = survey.designC2)
ord_IMMIEQ4C3 <- svyolr(as.formula(paste("T_IMMIEQ4 ~", form )), design = survey.designC3)
or14 <- tab_model(ord_IMMIEQ4C1, ord_IMMIEQ4C2, ord_IMMIEQ4C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_IMMIEQ4)$label, "Moving/<Immigrants> ")))

ord_IMMIEQ5C1 <- svyolr(as.formula(paste("T_IMMIEQ5 ~", formC1 )), design = survey.designC1)
ord_IMMIEQ5C2 <- svyolr(as.formula(paste("T_IMMIEQ5 ~", form )), design = survey.designC2)
ord_IMMIEQ5C3 <- svyolr(as.formula(paste("T_IMMIEQ5 ~", form )), design = survey.designC3)
or15 <- tab_model(ord_IMMIEQ5C1, ord_IMMIEQ5C2, ord_IMMIEQ5C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_IMMIEQ5)$label, "Moving/<Immigrants> ")))

ord_GNDREQ1C1 <- svyolr(as.formula(paste("T_GNDREQ1 ~", formC1 )), design = survey.designC1)
ord_GNDREQ1C2 <- svyolr(as.formula(paste("T_GNDREQ1 ~", form )), design = survey.designC2)
ord_GNDREQ1C3 <- svyolr(as.formula(paste("T_GNDREQ1 ~", form )), design = survey.designC3)
or21 <- tab_model(ord_GNDREQ1C1, ord_GNDREQ1C2, ord_GNDREQ1C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_GNDREQ1)$label, "Rights and Responsibilities/Roles women and men/")))

ord_GNDREQ2C1 <- svyolr(as.formula(paste("T_GNDREQ2 ~", formC1 )), design = survey.designC1)
ord_GNDREQ2C2 <- svyolr(as.formula(paste("T_GNDREQ2 ~", form )), design = survey.designC2)
ord_GNDREQ2C3 <- svyolr(as.formula(paste("T_GNDREQ2 ~", form )), design = survey.designC3)
or22 <- tab_model(ord_GNDREQ2C1, ord_GNDREQ2C2, ord_GNDREQ2C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_GNDREQ2)$label, "Rights and Responsibilities/Roles women and men/")))

ord_GNDREQ3C1 <- svyolr(as.formula(paste("T_GNDREQ3 ~", formC1 )), design = survey.designC1)
ord_GNDREQ3C2 <- svyolr(as.formula(paste("T_GNDREQ3 ~", form )), design = survey.designC2)
ord_GNDREQ3C3 <- svyolr(as.formula(paste("T_GNDREQ3 ~", form )), design = survey.designC3)
or23 <- tab_model(ord_GNDREQ3C1, ord_GNDREQ3C2, ord_GNDREQ3C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_GNDREQ3)$label, "Rights and Responsibilities/Roles women and men/")))

ord_GNDREQ4C1 <- svyolr(as.formula(paste("T_GNDREQ4 ~", formC1 )), design = survey.designC1)
ord_GNDREQ4C2 <- svyolr(as.formula(paste("T_GNDREQ4 ~", form )), design = survey.designC2)
ord_GNDREQ4C3 <- svyolr(as.formula(paste("T_GNDREQ4 ~", form )), design = survey.designC3)
or24 <- tab_model(ord_GNDREQ4C1, ord_GNDREQ4C2, ord_GNDREQ4C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_GNDREQ4)$label, "Rights and Responsibilities/Roles women and men/")))

ord_GNDREQ5C1 <- svyolr(as.formula(paste("T_GNDREQ5 ~", formC1 )), design = survey.designC1)
ord_GNDREQ5C2 <- svyolr(as.formula(paste("T_GNDREQ5 ~", form )), design = survey.designC2)
ord_GNDREQ5C3 <- svyolr(as.formula(paste("T_GNDREQ5 ~", form )), design = survey.designC3)
or25 <- tab_model(ord_GNDREQ5C1, ord_GNDREQ5C2, ord_GNDREQ5C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_GNDREQ5)$label, "Rights and Responsibilities/Roles women and men/")))

ord_GNDREQ6C1 <- svyolr(as.formula(paste("T_GNDREQ6 ~", formC1 )), design = survey.designC1)
ord_GNDREQ6C2 <- svyolr(as.formula(paste("T_GNDREQ6 ~", form )), design = survey.designC2)
ord_GNDREQ6C3 <- svyolr(as.formula(paste("T_GNDREQ6 ~", form )), design = survey.designC3)
or26 <- tab_model(ord_GNDREQ6C1, ord_GNDREQ6C2, ord_GNDREQ6C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_GNDREQ6)$label, "Rights and Responsibilities/Roles women and men/")))

ord_GNDREQ7C3 <- svyolr(as.formula(paste("T_GNDREQ7 ~", form )), design = survey.designC3)
or27 <- tab_model(ord_GNDREQ7C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_GNDREQ7)$label, "Rights and Responsibilities/Roles women and men/")))


ord_ETHNEQ1C1 <- svyolr(as.formula(paste("T_ETHNEQ1 ~", formC1 )), design = survey.designC1)
ord_ETHNEQ1C2 <- svyolr(as.formula(paste("T_ETHNEQ1 ~", form )), design = survey.designC2)
ord_ETHNEQ1C3 <- svyolr(as.formula(paste("T_ETHNEQ1 ~", form )), design = survey.designC3)
or31 <- tab_model(ord_ETHNEQ1C1, ord_ETHNEQ1C2, ord_ETHNEQ1C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_ETHNEQ1)$label, "Rights and Responsibilities/Rights and responsibilities/")))

ord_ETHNEQ2C1 <- svyolr(as.formula(paste("T_ETHNEQ2 ~", formC1 )), design = survey.designC1)
ord_ETHNEQ2C2 <- svyolr(as.formula(paste("T_ETHNEQ2 ~", form )), design = survey.designC2)
ord_ETHNEQ2C3 <- svyolr(as.formula(paste("T_ETHNEQ2 ~", form )), design = survey.designC3)
or32 <- tab_model(ord_ETHNEQ2C1, ord_ETHNEQ2C2, ord_ETHNEQ2C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_ETHNEQ2)$label, "Rights and Responsibilities/Rights and responsibilities/")))

ord_ETHNEQ3C1 <- svyolr(as.formula(paste("T_ETHNEQ3 ~", formC1 )), design = survey.designC1)
ord_ETHNEQ3C2 <- svyolr(as.formula(paste("T_ETHNEQ3 ~", form )), design = survey.designC2)
ord_ETHNEQ3C3 <- svyolr(as.formula(paste("T_ETHNEQ3 ~", form )), design = survey.designC3)
or33 <- tab_model(ord_ETHNEQ3C1, ord_ETHNEQ3C2, ord_ETHNEQ3C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_ETHNEQ3)$label, "Rights and Responsibilities/Rights and responsibilities/")))

ord_ETHNEQ4C1 <- svyolr(as.formula(paste("T_ETHNEQ4 ~", formC1 )), design = survey.designC1)
ord_ETHNEQ4C2 <- svyolr(as.formula(paste("T_ETHNEQ4 ~", form )), design = survey.designC2)
ord_ETHNEQ4C3 <- svyolr(as.formula(paste("T_ETHNEQ4 ~", form )), design = survey.designC3)
or34 <- tab_model(ord_ETHNEQ4C1, ord_ETHNEQ4C2, ord_ETHNEQ4C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_ETHNEQ4)$label, "Rights and Responsibilities/Rights and responsibilities/")))

ord_ETHNEQ5C2 <- svyolr(as.formula(paste("T_ETHNEQ5 ~", form )), design = survey.designC2)
ord_ETHNEQ5C3 <- svyolr(as.formula(paste("T_ETHNEQ5 ~", form )), design = survey.designC3)
or35 <- tab_model(ord_ETHNEQ5C2, ord_ETHNEQ5C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_ETHNEQ5)$label, "Rights and Responsibilities/Rights and responsibilities/")))

##############################################################
###################Logistic regression########################
##############################################################

logit_IMMIEQ1C1 <- svyglm(as.formula(paste("bT_IMMIEQ1 ~", formC1 )), 
                      data = ds_ll0[ds_ll0$cycle == "C1",], family = binomial, design = survey.designC1)
logit_IMMIEQ1C2 <- svyglm(as.formula(paste("bT_IMMIEQ1 ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C2",], family = binomial, design = survey.designC2)
logit_IMMIEQ1C3 <- svyglm(as.formula(paste("bT_IMMIEQ1 ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C3",], family = binomial, design = survey.designC3)
ll11 <- tab_model(logit_IMMIEQ1C1, logit_IMMIEQ1C2, logit_IMMIEQ1C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_IMMIEQ1)$label, "Moving/<Immigrants> ")))

logit_IMMIEQ2C1 <- svyglm(as.formula(paste("bT_IMMIEQ2 ~", formC1 )), 
                      data = ds_ll0[ds_ll0$cycle == "C1",], family = binomial, design = survey.designC1)
logit_IMMIEQ2C2 <- svyglm(as.formula(paste("bT_IMMIEQ2 ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C2",], family = binomial, design = survey.designC2)
logit_IMMIEQ2C3 <- svyglm(as.formula(paste("bT_IMMIEQ2 ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C3",], family = binomial, design = survey.designC3)
ll12 <- tab_model(logit_IMMIEQ2C1, logit_IMMIEQ2C2, logit_IMMIEQ2C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_IMMIEQ2)$label, "Moving/<Immigrants> ")))

logit_IMMIEQ3C1 <- svyglm(as.formula(paste("bT_IMMIEQ3 ~", formC1 )), 
                      data = ds_ll0[ds_ll0$cycle == "C1",], family = binomial, design = survey.designC1)
logit_IMMIEQ3C2 <- svyglm(as.formula(paste("bT_IMMIEQ3 ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C2",], family = binomial, design = survey.designC2)
logit_IMMIEQ3C3 <- svyglm(as.formula(paste("bT_IMMIEQ3 ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C3",], family = binomial, design = survey.designC3)
ll13 <- tab_model(logit_IMMIEQ3C1, logit_IMMIEQ3C2, logit_IMMIEQ3C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_IMMIEQ3)$label, "Moving/<Immigrants> ")))

logit_IMMIEQ4C1 <- svyglm(as.formula(paste("bT_IMMIEQ4 ~", formC1 )), 
                      data = ds_ll0[ds_ll0$cycle == "C1",], family = binomial, design = survey.designC1)
logit_IMMIEQ4C2 <- svyglm(as.formula(paste("bT_IMMIEQ4 ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C2",], family = binomial, design = survey.designC2)
logit_IMMIEQ4C3 <- svyglm(as.formula(paste("bT_IMMIEQ4 ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C3",], family = binomial, design = survey.designC3)
ll14 <- tab_model(logit_IMMIEQ4C1, logit_IMMIEQ4C2, logit_IMMIEQ4C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_IMMIEQ4)$label, "Moving/<Immigrants> ")))

logit_IMMIEQ5C1 <- svyglm(as.formula(paste("bT_IMMIEQ5 ~", formC1 )), 
                      data = ds_ll0[ds_ll0$cycle == "C1",], family = binomial, design = survey.designC1)
logit_IMMIEQ5C2 <- svyglm(as.formula(paste("bT_IMMIEQ5 ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C2",], family = binomial, design = survey.designC2)
logit_IMMIEQ5C3 <- svyglm(as.formula(paste("bT_IMMIEQ5 ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C3",], family = binomial, design = survey.designC3)
ll15 <- tab_model(logit_IMMIEQ5C1, logit_IMMIEQ5C2, logit_IMMIEQ5C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_IMMIEQ5)$label, "Moving/<Immigrants> ")))


logit_GNDREQ1C1 <- svyglm(as.formula(paste("bT_GNDREQ1 ~", formC1 )), 
                      data = ds_ll0[ds_ll0$cycle == "C1",], family = binomial, design = survey.designC1)
logit_GNDREQ1C2 <- svyglm(as.formula(paste("bT_GNDREQ1 ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C2",], family = binomial, design = survey.designC2)
logit_GNDREQ1C3 <- svyglm(as.formula(paste("bT_GNDREQ1 ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C3",], family = binomial, design = survey.designC3)
ll21 <- tab_model(logit_GNDREQ1C1, logit_GNDREQ1C2, logit_GNDREQ1C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_GNDREQ1)$label, "Rights and Responsibilities/Roles women and men/")))

logit_GNDREQ2C1 <- svyglm(as.formula(paste("bT_GNDREQ2 ~", formC1 )), 
                      data = ds_ll0[ds_ll0$cycle == "C1",], family = binomial, design = survey.designC1)
logit_GNDREQ2C2 <- svyglm(as.formula(paste("bT_GNDREQ2 ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C2",], family = binomial, design = survey.designC2)
logit_GNDREQ2C3 <- svyglm(as.formula(paste("bT_GNDREQ2 ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C3",], family = binomial, design = survey.designC3)
ll22 <- tab_model(logit_GNDREQ2C1, logit_GNDREQ2C2, logit_GNDREQ2C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_GNDREQ2)$label, "Rights and Responsibilities/Roles women and men/")))

logit_GNDREQ3C1 <- svyglm(as.formula(paste("bT_GNDREQ3 ~", formC1 )), 
                      data = ds_ll0[ds_ll0$cycle == "C1",], family = binomial, design = survey.designC1)
logit_GNDREQ3C2 <- svyglm(as.formula(paste("bT_GNDREQ3 ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C2",], family = binomial, design = survey.designC2)
logit_GNDREQ3C3 <- svyglm(as.formula(paste("bT_GNDREQ3 ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C3",], family = binomial, design = survey.designC3)
ll23 <- tab_model(logit_GNDREQ3C1, logit_GNDREQ3C2, logit_GNDREQ3C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_GNDREQ3)$label, "Rights and Responsibilities/Roles women and men/")))

logit_GNDREQ4C1 <- svyglm(as.formula(paste("bT_GNDREQ4 ~", formC1 )), 
                      data = ds_ll0[ds_ll0$cycle == "C1",], family = binomial, design = survey.designC1)
logit_GNDREQ4C2 <- svyglm(as.formula(paste("bT_GNDREQ4 ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C2",], family = binomial, design = survey.designC2)
logit_GNDREQ4C3 <- svyglm(as.formula(paste("bT_GNDREQ4 ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C3",], family = binomial, design = survey.designC3)
ll24 <- tab_model(logit_GNDREQ4C1, logit_GNDREQ4C2, logit_GNDREQ4C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_GNDREQ4)$label, "Rights and Responsibilities/Roles women and men/")))

logit_GNDREQ5C1 <- svyglm(as.formula(paste("bT_GNDREQ5 ~", formC1 )), 
                      data = ds_ll0[ds_ll0$cycle == "C1",], family = binomial, design = survey.designC1)
logit_GNDREQ5C2 <- svyglm(as.formula(paste("bT_GNDREQ5 ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C2",], family = binomial, design = survey.designC2)
logit_GNDREQ5C3 <- svyglm(as.formula(paste("bT_GNDREQ5 ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C3",], family = binomial, design = survey.designC3)
ll25 <- tab_model(logit_GNDREQ5C1, logit_GNDREQ5C2, logit_GNDREQ5C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_GNDREQ5)$label, "Rights and Responsibilities/Roles women and men/")))

logit_GNDREQ6C1 <- svyglm(as.formula(paste("bT_GNDREQ6 ~", formC1 )), 
                      data = ds_ll0[ds_ll0$cycle == "C1",], family = binomial, design = survey.designC1)
logit_GNDREQ6C2 <- svyglm(as.formula(paste("bT_GNDREQ6 ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C2",], family = binomial, design = survey.designC2)
logit_GNDREQ6C3 <- svyglm(as.formula(paste("bT_GNDREQ6 ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C3",], family = binomial, design = survey.designC3)
ll26 <- tab_model(logit_GNDREQ6C1, logit_GNDREQ6C2, logit_GNDREQ6C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_GNDREQ6)$label, "Rights and Responsibilities/Roles women and men/")))

logit_GNDREQ7C3 <- svyglm(as.formula(paste("bT_GNDREQ7 ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C3",], family = binomial, design = survey.designC3)
ll27 <- tab_model(logit_GNDREQ7C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_GNDREQ7)$label, "Rights and Responsibilities/Roles women and men/")))

logit_ETHNEQ1C1 <- svyglm(as.formula(paste("bT_ETHNEQ1 ~", formC1 )), 
                      data = ds_ll0[ds_ll0$cycle == "C1",], family = binomial, design = survey.designC1)
logit_ETHNEQ1C2 <- svyglm(as.formula(paste("bT_ETHNEQ1 ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C2",], family = binomial, design = survey.designC2)
logit_ETHNEQ1C3 <- svyglm(as.formula(paste("bT_ETHNEQ1 ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C3",], family = binomial, design = survey.designC3)
ll31 <- tab_model(logit_ETHNEQ1C1, logit_ETHNEQ1C2, logit_ETHNEQ1C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_ETHNEQ1)$label, "Rights and Responsibilities/Rights and responsibilities/")))

logit_ETHNEQ2C1 <- svyglm(as.formula(paste("bT_ETHNEQ2 ~", formC1 )), 
                      data = ds_ll0[ds_ll0$cycle == "C1",], family = binomial, design = survey.designC1)
logit_ETHNEQ2C2 <- svyglm(as.formula(paste("bT_ETHNEQ2 ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C2",], family = binomial, design = survey.designC2)
logit_ETHNEQ2C3 <- svyglm(as.formula(paste("bT_ETHNEQ2 ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C3",], family = binomial, design = survey.designC3)
ll32 <- tab_model(logit_ETHNEQ2C1, logit_ETHNEQ2C2, logit_ETHNEQ2C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_ETHNEQ2)$label, "Rights and Responsibilities/Rights and responsibilities/")))

logit_ETHNEQ3C1 <- svyglm(as.formula(paste("bT_ETHNEQ3 ~", formC1 )), 
                      data = ds_ll0[ds_ll0$cycle == "C1",], family = binomial, design = survey.designC1)
logit_ETHNEQ3C2 <- svyglm(as.formula(paste("bT_ETHNEQ3 ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C2",], family = binomial, design = survey.designC2)
logit_ETHNEQ3C3 <- svyglm(as.formula(paste("bT_ETHNEQ3 ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C3",], family = binomial, design = survey.designC3)
ll33 <- tab_model(logit_ETHNEQ3C1, logit_ETHNEQ3C2, logit_ETHNEQ3C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_ETHNEQ3)$label, "Rights and Responsibilities/Rights and responsibilities/")))

logit_ETHNEQ4C1 <- svyglm(as.formula(paste("bT_ETHNEQ4 ~", formC1 )), 
                      data = ds_ll0[ds_ll0$cycle == "C1",], family = binomial, design = survey.designC1)
logit_ETHNEQ4C2 <- svyglm(as.formula(paste("bT_ETHNEQ4 ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C2",], family = binomial, design = survey.designC2)
logit_ETHNEQ4C3 <- svyglm(as.formula(paste("bT_ETHNEQ4 ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C3",], family = binomial, design = survey.designC3)
ll34 <- tab_model(logit_ETHNEQ4C1, logit_ETHNEQ4C2, logit_ETHNEQ4C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_ETHNEQ4)$label, "Rights and Responsibilities/Rights and responsibilities/")))

logit_ETHNEQ5C2 <- svyglm(as.formula(paste("bT_ETHNEQ5 ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C2",], family = binomial, design = survey.designC2)
logit_ETHNEQ5C3 <- svyglm(as.formula(paste("bT_ETHNEQ5 ~", form )), 
                      data = ds_ll0[ds_ll0$cycle == "C3",], family = binomial, design = survey.designC3)
ll35 <- tab_model(logit_ETHNEQ5C2, logit_ETHNEQ5C3, collapse.ci = TRUE, p.style = "stars",
                  dv.labels = c("ICCS 2009", "ICCS 2016"),
                  title = c(str_remove(attributes(ds_ll0$T_ETHNEQ5)$label, "Rights and Responsibilities/Rights and responsibilities/")))

##############################################################
################Multinomial regression########################
##############################################################

# library(nnet)
# mlt_GNDREQ1C1 <- multinom(as.formula(paste("as.numeric(bT_GNDREQ1) ~", form )),
#                         data = ds_ll0[ds_ll0$cycle == "C1",], weights=SENWGT)
# mlt_GNDREQ1C2 <- multinom(as.formula(paste("as.numeric(bT_GNDREQ1) ~", form )),
#                         data = ds_ll0[ds_ll0$cycle == "C2",], weights=SENWGT)
# mlt_GNDREQ1C2 <- multinom(as.formula(paste("as.numeric(bT_GNDREQ1) ~", form )),
#                         data = ds_ll0[ds_ll0$cycle == "C3",], weights=SENWGT)

# mlt_GNDREQ2C1 <- multinom(as.formula(paste("as.numeric(bT_GNDREQ2) ~", form )),
#                         data = ds_ll0[ds_ll0$cycle == "C1",], weights=SENWGT)
# ml11 <- tab_model(mlt_GNDREQ1, collapse.ci = TRUE, p.style = "stars", auto.label = FALSE,
#           dv.labels = c(str_remove(attributes(ds_ll0$T_GNDREQ1)$label, "Rights and Responsibilities/Roles women and men/")))
# 
# ml12 <- tab_model(mlt_GNDREQ2, collapse.ci = TRUE, p.style = "stars", auto.label = FALSE,
#           dv.labels = c(str_remove(attributes(ds_ll0$T_GNDREQ2)$label, "Rights and Responsibilities/Roles women and men/")))
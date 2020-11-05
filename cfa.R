library(lavaan)
library(lavaan.survey)
library(semPlot)


if (any(grepl("99", years$year))){
  model99<-'
    Gend_Equal =~ BS4G1 + BS4G4 + BS4G6 + BS4G9 + BS4G11 + BS4G13
    Immi_Equal =~ BS4H1 + BS4H2 + BS4H3 + BS4H4 + BS4H5
    Ethn_Equal =~ BS4G2 + BS4G5 + BS4G8 + BS4G12
    '
  cat('## CIVED 1999  \n')
  #############1999#########
  index99 <- Scales %>% filter(item != "index") %>% dplyr::select(CIVED_1999) %>% na.omit() %>% pull()
  ds99 <- ICCS %>% filter(!COUNTRY %in% c("CHL", "PER")) %>% dplyr::select(all_of(index99), IDSTUD, TOTWGT, COUNTRY, GENDER) %>% 
    mutate(GENDER = as.character(GENDER)) %>% na.omit()  
  survey.design99 <- svydesign(ids=~IDSTUD, prob=~TOTWGT, data=ds99)
  
  cfa99 <- lavaan(model99, data = ds99, auto.fix.first = TRUE, auto.var = TRUE, int.ov.free = TRUE, auto.cov.lv.x = TRUE, 
                  estimator = "MLM", cluster = "COUNTRY", meanstructure = TRUE)
  survey.fit99 <- lavaan.survey(lavaan.fit = cfa99, survey.design = survey.design99)
  
  cnt99 <- unique(ds99$COUNTRY)
  meast99 <- NULL
  for (c99 in cnt99) {
    CNTcfa <- lavaan(model09, data = ds99[ds99$COUNTRY == c99,], auto.fix.first = TRUE, auto.var = TRUE, int.ov.free = TRUE, auto.cov.lv.x = TRUE, 
                     estimator = "MLM", meanstructure = TRUE)
    survey.CNTfit <- lavaan.survey(lavaan.fit = CNTcfa, survey.design = survey.design09)
    meas <- fitMeasures(survey.CNTfit, c("chisq","df", "cfi", "tli","rmsea", "srmr"), output = "matrix")
    meast99 <- cbind(meast99, meas)
  }
  meast99 <- as.data.frame(t(meast99))
  rownames(meast99) <- cnt99
  
  cat('### CFA - ICCS 1999, all countries')
  cat('  \n')
  cat('  \n')
  meas99 <- fitMeasures(survey.fit99, c("chisq","df","cfi", "tli","rmsea", "srmr"), 
                        output = "matrix")
  knitr::kable(round(as.data.frame(t(meas99)), 3)) %>% print() 
  
  cat('  \n')
  cat('### CFA - ICCS 1999, by countries')
  cat('  \n')
  knitr::kable(round(meast99, 3)) %>% print() 
  cat('  \n')
  cat('  \n')
  #print(modindices(survey.fit99,sort=T)[1:10,])
  invisible(semPaths(survey.fit99,"model", "std", "lisrel", edge.label.cex = 0.5, intercepts = FALSE, groups = "latent", 
                     pastel = TRUE, title = FALSE, optimizeLatRes = TRUE, nCharNodes = 10))
  title("CFA measurement model", line = 1)
  cat('### Invariance between COUNTRY')
  cat('  \n')
  cat('  \n')
  inv.conf99 <- lavaan(model99, data = ds99, 
                       auto.fix.first = TRUE, int.ov.free = TRUE, meanstructure = TRUE, auto.var = TRUE, auto.cov.lv.x = TRUE,  
                       estimator = "MLM", group = "COUNTRY")
  inv.conf99 <- lavaan.survey(lavaan.fit = inv.conf99, survey.design = survey.design99)
  inv.metr99 <- lavaan(model99, data = ds99, 
                       auto.fix.first = TRUE, int.ov.free = TRUE, meanstructure = TRUE, auto.var = TRUE, auto.cov.lv.x = TRUE,  
                       estimator = "MLM", group = "COUNTRY", group.equal = c("loadings"))
  inv.metr99 <- lavaan.survey(lavaan.fit = inv.metr99, survey.design = survey.design99)
  inv.scal99 <- lavaan(model99, data = ds99, 
                       auto.fix.first = TRUE, int.ov.free = TRUE, meanstructure = TRUE, auto.var = TRUE, auto.cov.lv.x = TRUE,  
                       estimator = "MLM", group = "COUNTRY", group.equal = c("loadings","intercepts"))
  inv.scal99 <- lavaan.survey(lavaan.fit = inv.scal99, survey.design = survey.design99)
  inv.stri99 <- lavaan(model99, data = ds99, 
                       auto.fix.first = TRUE, int.ov.free = TRUE, meanstructure = TRUE, auto.var = TRUE, auto.cov.lv.x = TRUE,  
                       estimator = "MLM", group = "COUNTRY", group.equal = c("loadings","intercepts","lv.variances"))
  inv.stri99 <- lavaan.survey(lavaan.fit = inv.stri99, survey.design = survey.design99)
  
  invarCNT <- data.frame(round(rbind(Configural = fitMeasures(inv.conf99, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                     Metric = fitMeasures(inv.metr99, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                     Scalar = fitMeasures(inv.scal99, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                     Strict = fitMeasures(inv.stri99, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea"))),3))
  
  print(knitr::kable(invarCNT))
  cat('  \n')
  cat('  \n')
  
  cat('### Invariance between GENDER')
  cat('  \n')
  cat('  \n')
  inv.conf99 <- lavaan(model99, data = ds99, 
                       auto.fix.first = TRUE, int.ov.free = TRUE, meanstructure = TRUE, auto.var = TRUE, auto.cov.lv.x = TRUE,  
                       estimator = "MLM", group = "GENDER")
  inv.conf99 <- lavaan.survey(lavaan.fit = inv.conf99, survey.design = survey.design99)
  inv.metr99 <- lavaan(model99, data = ds99, 
                       auto.fix.first = TRUE, int.ov.free = TRUE, meanstructure = TRUE, auto.var = TRUE, auto.cov.lv.x = TRUE,  
                       estimator = "MLM", group = "GENDER", group.equal = c("loadings"))
  inv.metr99 <- lavaan.survey(lavaan.fit = inv.metr99, survey.design = survey.design99)
  inv.scal99 <- lavaan(model99, data = ds99, 
                       auto.fix.first = TRUE, int.ov.free = TRUE, meanstructure = TRUE, auto.var = TRUE, auto.cov.lv.x = TRUE,  
                       estimator = "MLM", group = "GENDER", group.equal = c("loadings","intercepts"))
  inv.scal99 <- lavaan.survey(lavaan.fit = inv.scal99, survey.design = survey.design99)
  inv.stri99 <- lavaan(model99, data = ds99, 
                       auto.fix.first = TRUE, int.ov.free = TRUE, meanstructure = TRUE, auto.var = TRUE, auto.cov.lv.x = TRUE,  
                       estimator = "MLM", group = "GENDER", group.equal = c("loadings","intercepts","lv.variances"))
  inv.stri99 <- lavaan.survey(lavaan.fit = inv.stri99, survey.design = survey.design99)
  
  invarGNDR <- data.frame(round(rbind(Configural = fitMeasures(inv.conf99, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                      Metric = fitMeasures(inv.metr99, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                      Scalar = fitMeasures(inv.scal99, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                      Strict = fitMeasures(inv.stri99, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea"))),3))
  
  print(knitr::kable(invarGNDR))
  cat('  \n')
  cat('  \n')
}
if (any(grepl("09", years$year))){
  model09<-'
    Gend_Equal =~ IS2P24A + IS2P24B + IS2P24C + IS2P24D + IS2P24E + IS2P24F
    Immi_Equal =~ IS2P26A + IS2P26B + IS2P26C + IS2P26D + IS2P26E
    Ethn_Equal =~ IS2P25A + IS2P25B + IS2P25C + IS2P25D + IS2P25E
  '

  #############2009#########
  cat('## ICCS 2009  \n')
  index09 <- Scales %>% filter(item != "index") %>% dplyr::select(ICCS_2009) %>% na.omit() %>% pull()
  ds09 <- ICCS %>% filter(!COUNTRY %in% c("CHL", "PER")) %>% dplyr::select(all_of(index09), IDSTUD, TOTWGTS, COUNTRY, SGENDER) %>% 
    mutate(SGENDER = as.character(SGENDER)) %>% na.omit()  
  survey.design09 <- svydesign(ids=~IDSTUD, prob=~TOTWGTS, data=ds09)
  
  cfa09 <- lavaan(model09, data = ds09, auto.fix.first = TRUE, auto.var = TRUE, int.ov.free = TRUE, auto.cov.lv.x = TRUE, 
                  estimator = "MLM", cluster = "COUNTRY", meanstructure = TRUE)
  survey.fit09 <- lavaan.survey(lavaan.fit = cfa09, survey.design = survey.design09)

  cnt09 <- unique(ds09$COUNTRY)
  meast09 <- NULL
  for (c09 in cnt09) {
    CNTcfa <- lavaan(model09, data = ds09[ds09$COUNTRY == c09,], auto.fix.first = TRUE, auto.var = TRUE, int.ov.free = TRUE, auto.cov.lv.x = TRUE, 
                     estimator = "MLM", meanstructure = TRUE)
    survey.CNTfit <- lavaan.survey(lavaan.fit = CNTcfa, survey.design = survey.design09)
    meas <- fitMeasures(survey.CNTfit, c("chisq","df", "cfi", "tli","rmsea", "srmr"), output = "matrix")
    meast09 <- cbind(meast09, meas)
  }
  meast09 <- as.data.frame(t(meast09))
  rownames(meast09) <- cnt09

  cat('### CFA - ICCS 2009, all countries')
  cat('  \n')
  cat('  \n')
  meas09 <- fitMeasures(survey.fit09, c("chisq","df","cfi", "tli","rmsea", "srmr"), 
                        output = "matrix")
  knitr::kable(round(as.data.frame(t(meas09)), 3)) %>% print() 
  
  cat('  \n')
  cat('### CFA - ICCS 2009, by countries')
  cat('  \n')
  knitr::kable(round(meast09, 3)) %>% print() 
  cat('  \n')
  cat('  \n')
  #print(modindices(survey.fit09,sort=T)[1:10,])
  invisible(semPaths(survey.fit09,"model", "std", "lisrel", edge.label.cex = 0.5, intercepts = FALSE, groups = "latent", 
                     pastel = TRUE, title = FALSE, optimizeLatRes = TRUE, nCharNodes = 10))
  title("CFA measurement model", line = 2)
  cat('  \n')
  cat('  \n')
  cat('### Invariance between COUNTRY')
  cat('  \n')
  cat('  \n')
  inv.conf09 <- lavaan(model09, data = ds09, 
                       auto.fix.first = TRUE, int.ov.free = TRUE, meanstructure = TRUE, auto.var = TRUE, auto.cov.lv.x = TRUE,  
                       estimator = "MLM", group = "COUNTRY")
  inv.conf09 <- lavaan.survey(lavaan.fit = inv.conf09, survey.design = survey.design09)
  inv.metr09 <- lavaan(model09, data = ds09, 
                       auto.fix.first = TRUE, int.ov.free = TRUE, meanstructure = TRUE, auto.var = TRUE, auto.cov.lv.x = TRUE,  
                       estimator = "MLM", group = "COUNTRY", group.equal = c("loadings"))
  inv.metr09 <- lavaan.survey(lavaan.fit = inv.metr09, survey.design = survey.design09)
  inv.scal09 <- lavaan(model09, data = ds09, 
                       auto.fix.first = TRUE, int.ov.free = TRUE, meanstructure = TRUE, auto.var = TRUE, auto.cov.lv.x = TRUE,  
                       estimator = "MLM", group = "COUNTRY", group.equal = c("loadings","intercepts"))
  inv.scal09 <- lavaan.survey(lavaan.fit = inv.scal09, survey.design = survey.design09)
  inv.stri09 <- lavaan(model09, data = ds09, 
                       auto.fix.first = TRUE, int.ov.free = TRUE, meanstructure = TRUE, auto.var = TRUE, auto.cov.lv.x = TRUE,  
                       estimator = "MLM", group = "COUNTRY", group.equal = c("loadings","intercepts","lv.variances"))
  inv.stri09 <- lavaan.survey(lavaan.fit = inv.stri09, survey.design = survey.design09)
  
  invarCNT <- data.frame(round(rbind(Configural = fitMeasures(inv.conf09, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                  Metric = fitMeasures(inv.metr09, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                  Scalar = fitMeasures(inv.scal09, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                  Strict = fitMeasures(inv.stri09, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea"))),3))
  knitr::kable(invarCNT) %>% print()
  cat('  \n') 
  cat('  \n')
  cat('### Invariance between GENDER')
  cat('  \n')
  cat('  \n')
  inv.conf09 <- lavaan(model09, data = ds09, 
                       auto.fix.first = TRUE, int.ov.free = TRUE, meanstructure = TRUE, auto.var = TRUE, auto.cov.lv.x = TRUE,  
                       estimator = "MLM", group = "SGENDER")
  inv.conf09 <- lavaan.survey(lavaan.fit = inv.conf09, survey.design = survey.design09)
  inv.metr09 <- lavaan(model09, data = ds09, 
                       auto.fix.first = TRUE, int.ov.free = TRUE, meanstructure = TRUE, auto.var = TRUE, auto.cov.lv.x = TRUE,  
                       estimator = "MLM", group = "SGENDER", group.equal = c("loadings"))
  inv.metr09 <- lavaan.survey(lavaan.fit = inv.metr09, survey.design = survey.design09)
  inv.scal09 <- lavaan(model09, data = ds09, 
                       auto.fix.first = TRUE, int.ov.free = TRUE, meanstructure = TRUE, auto.var = TRUE, auto.cov.lv.x = TRUE,  
                       estimator = "MLM", group = "SGENDER", group.equal = c("loadings","intercepts"))
  inv.scal09 <- lavaan.survey(lavaan.fit = inv.scal09, survey.design = survey.design09)
  inv.stri09 <- lavaan(model09, data = ds09, 
                       auto.fix.first = TRUE, int.ov.free = TRUE, meanstructure = TRUE, auto.var = TRUE, auto.cov.lv.x = TRUE,  
                       estimator = "MLM", group = "SGENDER", group.equal = c("loadings","intercepts","lv.variances"))
  inv.stri09 <- lavaan.survey(lavaan.fit = inv.stri09, survey.design = survey.design09)
  
  invarGNDR <- data.frame(round(rbind(Configural = fitMeasures(inv.conf09, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                     Metric = fitMeasures(inv.metr09, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                     Scalar = fitMeasures(inv.scal09, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                     Strict = fitMeasures(inv.stri09, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea"))),3))
  knitr::kable(invarGNDR) %>% print()
  cat('  \n')
  cat('  \n')
}
if (any(grepl("16", years$year))){
  model16<-'
    Gend_Equal =~ IS3G24A + IS3G24B + IS3G24C + IS3G24D + IS3G24E + IS3G24F
    Immi_Equal =~ ES3G04A + ES3G04B + ES3G04C + ES3G04D + ES3G04E
    Ethn_Equal =~ IS3G25A + IS3G25B + IS3G25C + IS3G25D + IS3G25E
  '
  #############2016#########
  cat('## ICCS 2016  \n')
  cat('  \n')
  index16 <- Scales %>% filter(item != "index") %>% dplyr::select(ICCS_2016) %>% na.omit() %>% pull()
  ds16 <- ICCS %>% filter(!COUNTRY %in% c("CHL", "PER")) %>% dplyr::select(all_of(index16), IDSTUD, TOTWGTS, COUNTRY, S_GENDER) %>% 
    mutate(S_GENDER = as.character(S_GENDER)) %>% na.omit()  
  survey.design16 <- svydesign(ids=~IDSTUD, prob=~TOTWGTS, data=ds16)
  
  cfa16 <- lavaan(model16, data = ds16, auto.fix.first = TRUE, auto.var = TRUE, int.ov.free = TRUE, auto.cov.lv.x = TRUE, 
                  estimator = "MLM", cluster = "COUNTRY", meanstructure = TRUE)
  
  survey.fit16 <- lavaan.survey(lavaan.fit = cfa16, survey.design = survey.design16)
  cnt16 <- unique(ds16$COUNTRY)
  meast16 <- NULL
  for (c16 in cnt16) {
    CNTcfa <- lavaan(model16, data = ds16[ds16$COUNTRY == c16,], auto.fix.first = TRUE, auto.var = TRUE, int.ov.free = TRUE, auto.cov.lv.x = TRUE, 
                     estimator = "MLM", meanstructure = TRUE)
    survey.CNTfit <- lavaan.survey(lavaan.fit = CNTcfa, survey.design = survey.design09)
    meas <- fitMeasures(survey.CNTfit, c("chisq","df", "cfi", "tli","rmsea", "srmr"), output = "matrix")
    meast16 <- cbind(meast16, meas)
  }
  meast16 <- as.data.frame(t(meast16))
  rownames(meast16) <- cnt16
  
  cat('### CFA - ICCS 2016, all countries')
  cat('  \n')
  cat('  \n')
  meas16 <- fitMeasures(survey.fit16, c("chisq","df","cfi", "tli","rmsea", "srmr"), 
                        output = "matrix")
  knitr::kable(round(as.data.frame(t(meas16)), 3)) %>% print() 
  
  cat('  \n')
  cat('### CFA - ICCS 2016, by countries')
  cat('  \n')
  knitr::kable(round(meast16, 3)) %>% print() 
  cat('  \n')
  cat('  \n')
  #print(modindices(survey.fit16,sort=T)[1:10,])
  invisible(semPaths(survey.fit16,"model", "std", "lisrel", edge.label.cex = 0.5, intercepts = FALSE, groups = "latent", 
                     pastel = TRUE, title = FALSE, optimizeLatRes = TRUE, nCharNodes = 10))
  title("CFA measurement model", line = 1)
  cat('  \n')
  cat('  \n')
  cat('### Invariance between COUNTRY')
  cat('  \n')
  cat('  \n')
  inv.conf16 <- lavaan(model16, data = ds16, 
                       auto.fix.first = TRUE, int.ov.free = TRUE, meanstructure = TRUE, auto.var = TRUE, auto.cov.lv.x = TRUE,  
                       estimator = "MLM", group = "COUNTRY")
  inv.conf16 <- lavaan.survey(lavaan.fit = inv.conf16, survey.design = survey.design16)
  inv.metr16 <- lavaan(model16, data = ds16, 
                       auto.fix.first = TRUE, int.ov.free = TRUE, meanstructure = TRUE, auto.var = TRUE, auto.cov.lv.x = TRUE,  
                       estimator = "MLM", group = "COUNTRY", group.equal = c("loadings"))
  inv.metr16 <- lavaan.survey(lavaan.fit = inv.metr16, survey.design = survey.design16)
  inv.scal16 <- lavaan(model16, data = ds16, 
                       auto.fix.first = TRUE, int.ov.free = TRUE, meanstructure = TRUE, auto.var = TRUE, auto.cov.lv.x = TRUE,  
                       estimator = "MLM", group = "COUNTRY", group.equal = c("loadings","intercepts"))
  inv.scal16 <- lavaan.survey(lavaan.fit = inv.scal16, survey.design = survey.design16)
  inv.stri16 <- lavaan(model16, data = ds16, 
                       auto.fix.first = TRUE, int.ov.free = TRUE, meanstructure = TRUE, auto.var = TRUE, auto.cov.lv.x = TRUE,  
                       estimator = "MLM", group = "COUNTRY", group.equal = c("loadings","intercepts","lv.variances"))
  inv.stri16 <- lavaan.survey(lavaan.fit = inv.stri16, survey.design = survey.design16)
  
  invarCNT <- data.frame(round(rbind(Configural = fitMeasures(inv.conf16, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                  Metric = fitMeasures(inv.metr16, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                  Scalar = fitMeasures(inv.scal16, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                  Strict = fitMeasures(inv.stri16, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea"))),3))
  knitr::kable(invarCNT) %>% print()
  cat('  \n')
  cat('  \n')
  cat('### Invariance between GENDER')
  cat('  \n')
  cat('  \n')
  inv.conf16 <- lavaan(model16, data = ds16, 
                       auto.fix.first = TRUE, int.ov.free = TRUE, meanstructure = TRUE, auto.var = TRUE, auto.cov.lv.x = TRUE,  
                       estimator = "MLM", group = "S_GENDER")
  inv.conf16 <- lavaan.survey(lavaan.fit = inv.conf16, survey.design = survey.design16)
  inv.metr16 <- lavaan(model16, data = ds16, 
                       auto.fix.first = TRUE, int.ov.free = TRUE, meanstructure = TRUE, auto.var = TRUE, auto.cov.lv.x = TRUE,  
                       estimator = "MLM", group = "S_GENDER", group.equal = c("loadings"))
  inv.metr16 <- lavaan.survey(lavaan.fit = inv.metr16, survey.design = survey.design16)
  inv.scal16 <- lavaan(model16, data = ds16, 
                       auto.fix.first = TRUE, int.ov.free = TRUE, meanstructure = TRUE, auto.var = TRUE, auto.cov.lv.x = TRUE,  
                       estimator = "MLM", group = "S_GENDER", group.equal = c("loadings","intercepts"))
  inv.scal16 <- lavaan.survey(lavaan.fit = inv.scal16, survey.design = survey.design16)
  inv.stri16 <- lavaan(model16, data = ds16, 
                       auto.fix.first = TRUE, int.ov.free = TRUE, meanstructure = TRUE, auto.var = TRUE, auto.cov.lv.x = TRUE,  
                       estimator = "MLM", group = "S_GENDER", group.equal = c("loadings","intercepts","lv.variances"))
  inv.stri16 <- lavaan.survey(lavaan.fit = inv.stri16, survey.design = survey.design16)
  
  invarGNDR <- data.frame(round(rbind(Configural = fitMeasures(inv.conf16, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                  Metric = fitMeasures(inv.metr16, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                  Scalar = fitMeasures(inv.scal16, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                  Strict = fitMeasures(inv.stri16, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea"))),3))
  knitr::kable(invarGNDR) %>% print()
  cat('  \n')
  cat('  \n')
}
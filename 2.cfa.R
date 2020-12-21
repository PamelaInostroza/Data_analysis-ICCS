library(lavaan)
library(lavaan.survey)
library(semPlot)
library(sjlabelled)
memory.limit(15000)
options(survey.lonely.psu="adjust")

InverseCod <- c("BS4G6","BS4G9","BS4G13","IS2P24C","IS2P24D","IS2P24F","IS3G24C","IS3G24D","IS3G24F")
#Cycles 2-3 vars reverse whole scale
C1vars <- VarsToUse %>%  filter(Domain == "Scales" & Dataset %in% c("ISG","ISE")) %>% select(VariableC1) %>% na.omit() %>% pull()
C1vars <- C1vars[grepl(paste0(InverseCod, collapse = "|"), C1vars)]
C2vars <- VarsToUse %>%  filter(Domain == "Scales" & Dataset %in% c("ISG","ISE")) %>% select(VariableC2) %>% na.omit() %>% pull()
C2vars <- C2vars[!grepl(paste0(InverseCod, collapse = "|"), C2vars)]
C3vars <- VarsToUse %>%  filter(Domain == "Scales" & Dataset %in% c("ISG","ISE")) %>% select(VariableC3) %>% na.omit() %>% pull()
C3vars <- C3vars[!grepl(paste0(InverseCod, collapse = "|"), C3vars)]

torecod <- c(C1vars, C2vars, C3vars)
if(!any(colnames(ISC_cfa) %in% paste0(c(C1vars, C2vars, C3vars),"."))){
  dat <- data.frame(psych::reverse.code(keys = rep(-1,length(torecod)), 
                                        items = ISC_cfa[torecod], 
                                        mini = rep(1,length(torecod)), 
                                        maxi = rep(4,length(torecod))))
  colnames(dat) <- colnames(ISC_cfa[, torecod])
}

#Copy labels of old variables
label <- NULL
for (i in 1:length(torecod)) {
  label[i] <- eval(parse(text=paste0("get_label(ISC_cfa$",torecod[i],")")))
}

#Add (r) to label for inverse coded variables
labelr <- NULL
for (j in 1:length(InverseCod)) {
  labelr[j] <- paste0(eval(parse(text=paste0("get_label(ISC_cfa$",InverseCod[j],")"))),"(r)")
}

ISC_cfa <- cbind(ISC_cfa[,!colnames(ISC_cfa) %in% torecod], dat)

ISC_cfa[torecod] <- set_label(ISC_cfa[torecod], label)
ISC_cfa[InverseCod] <- set_label(ISC_cfa[InverseCod], labelr)
ISC_cfa[Scales] <-  set_labels(ISC_cfa[Scales], labels = c("strongly disagree" = 1, "disagree" = 2, "agree" = 3, "strongly agree" = 4))


if (any(grepl("99", years$year))){
  model99E <-'
  Gend_Equal =~ BS4G1 + BS4G4 + BS4G6 + BS4G9 + BS4G11 + BS4G13
  Immi_Equal =~ BS4H1 + BS4H2 + BS4H3 + BS4H4 + BS4H5
  Ethn_Equal =~ BS4G2 + BS4G5 + BS4G8 + BS4G12
  BS4H1 ~~  BS4H4
  BS4G9 ~~ BS4G13
  BS4G6 ~~ BS4G13
  BS4G6 ~~ BS4G9
  '
  model99nE <-'
  Gend_Equal =~ BS4G1 + BS4G4 + BS4G6 + BS4G9 + BS4G11 + BS4G13
  Immi_Equal =~ BS4H1 + BS4H2 + BS4H3 + BS4H4 + BS4H5
  Ethn_Equal =~ BS4G2 + BS4G5 + BS4G8 + BS4G12
  BS4H1 ~~  BS4H4
  BS4G9 ~~ BS4G13
  BS4G6 ~~ BS4G13
  BS4G6 ~~ BS4G9
  '    
  
  cat('## CIVED 1999  \n')
  #############1999#########
  index99 <- Itemdesc %>% filter(item != "index") %>% dplyr::select(CIVED_1999) %>% na.omit() %>% pull()

  ds991 <- ISC_cfa %>% filter(!is.na(TOTWGT_Gc1)) %>% 
    dplyr::select(all_of(index99), all_of(Id), IDJK, IDCL, SENWGT_Gc1, GENDER) %>% 
    mutate(GENDER = as.character(GENDER)) 
  ds99 <- ds991 %>% mutate_at(.funs = as.numeric, .vars = index99)
  
  #European countries
  ds99E <- ds99 %>% filter(!COUNTRY %in% c(CNTne,CNT2cne))
  survey.design99E <- svydesign(ids= ~ IDCL, weights = ~ SENWGT_Gc1, strata = ~ IDJK, nest = TRUE, data = ds99E)
  cfa99E <- cfa(model99E, data = ds99E, cluster = c("COUNTRY", "IDSCHOOL"), missing = "fiml")
  survey.fit99E <- lavaan.survey(lavaan.fit = cfa99E, survey.design = survey.design99E, estimator= "MLMVS")
  
  #cfa <- cfa(model99E, data = ds99E, cluster = c("COUNTRY", "IDSCHOOL"))
  #summary(cfa, fit.measures=TRUE)
  #print(modindices(cfa, sort=T)[1:10,])
  
  #Non European countries
  ds99nE <- ds99 %>% filter(COUNTRY %in% c(CNTne,CNT2cne))
  survey.design99nE <- svydesign(ids= ~ IDCL, weights = ~ SENWGT_Gc1, strata = ~ IDJK, nest = TRUE, data = ds99nE)
  cfa99nE <- cfa(model99nE, data = ds99nE, cluster = c("COUNTRY", "IDSCHOOL"), missing = "fiml")
  survey.fit99nE <- lavaan.survey(lavaan.fit = cfa99nE, survey.design = survey.design99nE, estimator= "MLMVS")
  
  #cfa <- cfa(model99nE, data = ds99nE, cluster = c("COUNTRY", "IDSCHOOL"))
  #summary(cfa, fit.measures=TRUE)
  #print(modindices(cfa,sort=T)[1:10,])
  
  #Factor scores for latent variables
  p99E <- cbind(ds99E, predict(cfa99E)) #Prediction of factor scores should be based on survey design but not possible to obtain in R using FIML
  p99nE <- cbind(ds99nE, predict(cfa99nE))
  p99 <- p99E %>% bind_rows(p99nE) %>% mutate(cycle = "C1") %>% 
    dplyr::select(all_of(Id), all_of(Newscales))
  rm(cfa99E, cfa99nE) #Remove fit to save space in disk
  
  #Fit for each country separately
  cnt99 <- unique(ds99$COUNTRY)
  meast99 <- NULL
  stdl99 <- NULL
  for (c99 in cnt99) {
    dscNT <- ds99[ds99$COUNTRY == c99,]
    survey.cnt99 <- svydesign(ids = ~ IDCL, weights = ~ SENWGT_Gc1, strata = ~ IDJK, nest = TRUE, data = dscNT)
    if(!c99 %in% c(CNTne,CNT2cne)) model <- model99E else model <- model99nE
    CNTcfa <- cfa(model, cluster = c("IDSCHOOL"), data = dscNT)
    survey.CNTfit <- lavaan.survey(lavaan.fit = CNTcfa, survey.design = survey.cnt99, estimator= "MLMVS")
    meas <- fitMeasures(survey.CNTfit, c("chisq","df", "cfi", "tli","rmsea", "srmr"), output = "matrix")
    meas <- rbind(n = nobs(survey.CNTfit), meas)
    meast99 <- cbind(meast99, meas)
    stdl <- standardizedSolution(survey.CNTfit) %>% 
      filter(op == "=~") %>% 
      mutate(cntry = c99)
    stdl99 <- rbind(stdl99, stdl)
  }
  meast99 <- as.data.frame(t(meast99))
  rownames(meast99) <- cnt99
  rm(CNTcfa)
  
  cat('### CFA - ICCS 1999')
  cat('  \n')
  cat('  \n')
  tmeasE <- t(fitMeasures(survey.fit99E, c("chisq","df","cfi", "tli","rmsea", "srmr"), 
                          output = "matrix"))
  tmeasnE <- t(fitMeasures(survey.fit99nE, c("chisq","df","cfi", "tli","rmsea", "srmr"), 
                           output = "matrix"))
  meas99 <- rbind(data.frame(Quest = "European", n = nobs(survey.fit99E), round(tmeasE, 3)),
                  data.frame(Quest = "Non-european", n = nobs(survey.fit99nE), round(tmeasnE, 3)))
  knitr::kable(meas99) %>% print() 
  
  cat('  \n')
  cat('### CFA - ICCS 1999, by countries')
  cat('  \n')
  knitr::kable(meast99,digits = 3) %>% print() 
  cat('  \n')
  cat('  \n')
  invisible(semPaths(survey.fit99E,"model", "std", "lisrel", edge.label.cex = 0.6, intercepts = FALSE, groups = "latent", 
                     pastel = TRUE, title = FALSE, nCharNodes = 10, nDigits = 1))
  title("CFA measurement European model", line = 2)
  invisible(semPaths(survey.fit99nE,"model", "std", "lisrel", edge.label.cex = 0.6, intercepts = FALSE, groups = "latent", 
                     pastel = TRUE, title = FALSE, nCharNodes = 10, nDigits = 1))
  title("CFA measurement Non European model", line = 2)
  cat('  \n')
  cat('  \n')
  
  labels <- data.frame(label = tolower(sjlabelled::get_label(ds991))) 
  labels <- labels %>% filter(!str_detect(rownames(labels), c("IDSTUD|IDSCHOOL|COUNTRY|TOTWGT|GENDER"))) %>% 
    mutate(variable = rownames(.))
  stdl99 <- stdl99 %>% mutate(rhs = factor(rhs, levels = labels$variable, labels = labels$label)) %>% 
    mutate(Model = ifelse(cntry %in% c(CNTne,CNT2cne), "Non-European", "European"))
  l1 <- stdl99 %>% data.frame() %>% 
    ggplot(aes(x = est.std, y = rhs, color = reorder(cntry, desc(cntry)))) +
    geom_linerange(aes(xmin = ci.lower, xmax = ci.upper), position = position_dodge(0.4)) +
    geom_jitter(position = position_dodge(0.4)) +
    facet_grid(lhs~Model, scales = "free") +
    geom_text(aes(label=cntry),hjust=0, vjust=1, position = position_dodge(0.4), size = 2) +
    theme(legend.position = "none", axis.line.y = element_blank()) +
    ggtitle("Loading distribution of scales - ICCS 1999") +
    ylab("") +
    xlab("Loadings with Confidence Interval") +
    scale_y_discrete(labels = function(x) str_wrap(x, 25)) +
    scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(0,1), labels = function(x) sprintf("%.1f", x))
  print(l1)

  cat('  \n')
  cat('  \n')
  cat('### Invariance between COUNTRY')
  cat('  \n')
  cat('  \n')
  inv.conf99E <- cfa(model99E, data = ds99E, cluster = "IDSCHOOL", group = "COUNTRY")
  inv.conf99E <- lavaan.survey(lavaan.fit = inv.conf99E, survey.design = survey.design99E, estimator= "MLMVS")
  inv.metr99E <- cfa(model99E, data = ds99E, cluster = "IDSCHOOL", group = "COUNTRY", group.equal = c("loadings"))
  inv.metr99E <- lavaan.survey(lavaan.fit = inv.metr99E, survey.design = survey.design99E, estimator= "MLMVS")
  inv.scal99E <- cfa(model99E, data = ds99E, cluster = "IDSCHOOL", group = "COUNTRY", group.equal = c("loadings","intercepts"))
  inv.scal99E <- lavaan.survey(lavaan.fit = inv.scal99E, survey.design = survey.design99E, estimator= "MLMVS")
  inv.stri99E <- cfa(model99E, data = ds99E, cluster = "IDSCHOOL", group = "COUNTRY", group.equal = c("loadings","intercepts","lv.variances"))
  inv.stri99E <- lavaan.survey(lavaan.fit = inv.stri99E, survey.design = survey.design99E, estimator= "MLMVS")
  invarCNT1 <- data.frame(Quest = "European", round(rbind(Configural = fitMeasures(inv.conf99E, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                     Metric = fitMeasures(inv.metr99E, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                     Scalar = fitMeasures(inv.scal99E, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                     Strict = fitMeasures(inv.stri99E, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea"))),3))

  inv.conf99nE <- cfa(model99nE, data = ds99nE, cluster = "IDSCHOOL", group = "COUNTRY")
  inv.conf99nE <- lavaan.survey(lavaan.fit = inv.conf99nE, survey.design = survey.design99nE, estimator= "MLMVS")
  inv.metr99nE <- cfa(model99nE, data = ds99nE, cluster = "IDSCHOOL", group = "COUNTRY", group.equal = c("loadings"))
  inv.metr99nE <- lavaan.survey(lavaan.fit = inv.metr99nE, survey.design = survey.design99nE, estimator= "MLMVS")
  inv.scal99nE <- cfa(model99nE, data = ds99nE, cluster = "IDSCHOOL", group = "COUNTRY", group.equal = c("loadings","intercepts"))
  inv.scal99nE <- lavaan.survey(lavaan.fit = inv.scal99nE, survey.design = survey.design99nE, estimator= "MLMVS")
  inv.stri99nE <- cfa(model99nE, data = ds99nE, cluster = "IDSCHOOL", group = "COUNTRY", group.equal = c("loadings","intercepts","lv.variances"))
  inv.stri99nE <- lavaan.survey(lavaan.fit = inv.stri99nE, survey.design = survey.design99nE, estimator= "MLMVS")
  
  invarCNT2 <- data.frame(Quest = "Non-European", round(rbind(Configural = fitMeasures(inv.conf99nE, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                      Metric = fitMeasures(inv.metr99nE, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                      Scalar = fitMeasures(inv.scal99nE, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                      Strict = fitMeasures(inv.stri99nE, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea"))),3))
  
  invarCNT <- invarCNT1 %>% mutate(Invariance = rownames(invarCNT1))  %>% relocate(Invariance, .before = npar) %>%
    mutate(D_tli = tli-lag(tli),
           D_cfi = cfi-lag(cfi),
           D_rmsea = rmsea-lag(rmsea)) %>%
    bind_rows(invarCNT2 %>% mutate(Invariance = rownames(invarCNT2))  %>% relocate(Invariance, .before = npar) %>%
                mutate(D_tli = tli-lag(tli),
                       D_cfi = cfi-lag(cfi),
                       D_rmsea = rmsea-lag(rmsea))) %>%
    knitr::kable() %>% print()
  cat('  \n')
  cat('  \n')
  rm(inv.conf99E, inv.metr99E, inv.scal99E, inv.stri99E, inv.conf99nE, inv.metr99nE, inv.scal99nE, inv.stri99nE) #Remove to save space in disk

  cat('### Invariance between GENDER')
  cat('  \n')
  cat('  \n')
  inv.conf99E <- cfa(model99E, data = ds99E, cluster = "IDSCHOOL", group = "GENDER")
  inv.conf99E <- lavaan.survey(lavaan.fit = inv.conf99E, survey.design = survey.design99E, estimator= "MLMVS")
  inv.metr99E <- cfa(model99E, data = ds99E, cluster = "IDSCHOOL", group = "GENDER", group.equal = c("loadings"))
  inv.metr99E <- lavaan.survey(lavaan.fit = inv.metr99E, survey.design = survey.design99E, estimator= "MLMVS")
  inv.scal99E <- cfa(model99E, data = ds99E, cluster = "IDSCHOOL", group = "GENDER", group.equal = c("loadings","intercepts"))
  inv.scal99E <- lavaan.survey(lavaan.fit = inv.scal99E, survey.design = survey.design99E, estimator= "MLMVS")
  inv.stri99E <- cfa(model99E, data = ds99E, cluster = "IDSCHOOL", group = "GENDER", group.equal = c("loadings","intercepts","lv.variances"))
  inv.stri99E <- lavaan.survey(lavaan.fit = inv.stri99E, survey.design = survey.design99E, estimator= "MLMVS")

  invarGNDR1 <- data.frame(Quest = "European", round(rbind(Configural = fitMeasures(inv.conf99E, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                      Metric = fitMeasures(inv.metr99E, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                      Scalar = fitMeasures(inv.scal99E, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                      Strict = fitMeasures(inv.stri99E, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea"))),3))
  inv.conf99nE <- cfa(model99nE, data = ds99nE, cluster = "IDSCHOOL", group = "GENDER")
  inv.conf99nE <- lavaan.survey(lavaan.fit = inv.conf99nE, survey.design = survey.design99nE, estimator= "MLMVS")
  inv.metr99nE <- cfa(model99nE, data = ds99nE, cluster = "IDSCHOOL", group = "GENDER", group.equal = c("loadings"))
  inv.metr99nE <- lavaan.survey(lavaan.fit = inv.metr99nE, survey.design = survey.design99nE, estimator= "MLMVS")
  inv.scal99nE <- cfa(model99nE, data = ds99nE, cluster = "IDSCHOOL", group = "GENDER", group.equal = c("loadings","intercepts"))
  inv.scal99nE <- lavaan.survey(lavaan.fit = inv.scal99nE, survey.design = survey.design99nE, estimator= "MLMVS")
  inv.stri99nE <- cfa(model99nE, data = ds99nE, cluster = "IDSCHOOL", group = "GENDER", group.equal = c("loadings","intercepts","lv.variances"))
  inv.stri99nE <- lavaan.survey(lavaan.fit = inv.stri99nE, survey.design = survey.design99nE, estimator= "MLMVS")
  
  invarGNDR2 <- data.frame(Quest = "Non-European", round(rbind(Configural = fitMeasures(inv.conf99nE, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                      Metric = fitMeasures(inv.metr99nE, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                      Scalar = fitMeasures(inv.scal99nE, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                      Strict = fitMeasures(inv.stri99nE, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea"))),3))
  invarGNDR <- invarGNDR1 %>% mutate(Invariance = rownames(invarGNDR1))  %>% relocate(Invariance, .before = npar) %>%
    mutate(D_tli = tli-lag(tli),
           D_cfi = cfi-lag(cfi),
           D_rmsea = rmsea-lag(rmsea)) %>%
    bind_rows(invarGNDR2 %>% mutate(Invariance = rownames(invarGNDR2))  %>% relocate(Invariance, .before = npar) %>%
                mutate(D_tli = tli-lag(tli),
                       D_cfi = cfi-lag(cfi),
                       D_rmsea = rmsea-lag(rmsea))) %>%
    knitr::kable() %>% print()
  cat('  \n')
  cat('  \n')
  rm(inv.conf99E, inv.metr99E, inv.scal99E, inv.stri99E, inv.conf99nE, inv.metr99nE, inv.scal99nE, inv.stri99nE) #Remove to save space in disk

  }
if (any(grepl("09", years$year))){
  model09E<-'
    Gend_Equal =~ IS2P24A + IS2P24B + IS2P24C + IS2P24D + IS2P24E
    Immi_Equal =~ IS2P26A + IS2P26B + IS2P26C + IS2P26D + IS2P26E
    Ethn_Equal =~ IS2P25A + IS2P25B + IS2P25C + IS2P25D + IS2P25E
    IS2P24C ~~ IS2P24D
    IS2P25A ~~ IS2P25B
    IS2P26A ~~ IS2P26D
    IS2P24A ~~ IS2P24B
  '
  model09nE<-'
    Gend_Equal =~ IS2P24A + IS2P24B + IS2P24C + IS2P24D + IS2P24E
    Immi_Equal =~ IS2P26A + IS2P26B + IS2P26C + IS2P26D + IS2P26E
    Ethn_Equal =~ IS2P25A + IS2P25B + IS2P25C + IS2P25D + IS2P25E
    IS2P24C ~~ IS2P24D
    IS2P25A ~~ IS2P25B
    IS2P26A ~~ IS2P26D
  '
  #############2009#########
  cat('## ICCS 2009  \n')
  index09 <- Itemdesc %>% filter(item != "index") %>% dplyr::select(ICCS_2009) %>% na.omit() %>% pull()
  
  ds091 <- ISC_cfa %>% filter(!is.na(TOTWGT_Gc2)) %>% 
    dplyr::select(all_of(index09), all_of(Id), IDJK, IDCL, SENWGT_Gc2, SGENDER) %>% 
    mutate(GENDER = as.character(SGENDER)) 
  ds09 <- ds091 %>% mutate_at(.funs = as.numeric, .vars = index09)
  
  #European countries
  ds09E <- ds09 %>% filter(!COUNTRY %in% c(CNTne,CNT2cne))
  survey.design09E <- svydesign(ids= ~ IDCL, weights = ~ SENWGT_Gc2, strata = ~ IDJK, nest = TRUE, data = ds09E)
  cfa09E <- cfa(model09E, data = ds09E, cluster = c("COUNTRY", "IDSCHOOL"), missing = "fiml")
  survey.fit09E <- lavaan.survey(lavaan.fit = cfa09E, survey.design = survey.design09E, estimator= "MLMVS")
  
  # cfa <- cfa(model09E, data = ds09E, cluster = c("COUNTRY", "IDSCHOOL"))
  # summary(cfa, fit.measures=TRUE)
  # print(modindices(cfa, sort=T)[1:10,])
   
  #Non European countries
  ds09nE <- ds09 %>% filter(COUNTRY %in% c(CNTne,CNT2cne))
  survey.design09nE <- svydesign(ids= ~ IDCL, weights = ~ SENWGT_Gc2, strata = ~ IDJK, nest = TRUE, data = ds09nE)
  cfa09nE <- cfa(model09nE, data = ds09nE, cluster = c("COUNTRY", "IDSCHOOL"), missing = "fiml")
  survey.fit09nE <- lavaan.survey(lavaan.fit = cfa09nE, survey.design = survey.design09nE, estimator= "MLMVS")
  
  # cfa <- cfa(model09nE, data = ds09nE, cluster = c("COUNTRY", "IDSCHOOL"))
  # summary(cfa, fit.measures=TRUE)
  # print(modindices(cfa,sort=T)[1:10,])

  #Factor scores for latent variables
  p09E <- cbind(ds09E, predict(cfa09E)) #Prediction of factor scores should be based on survey design but not possible to obtain in R using FIML
  p09nE <- cbind(ds09nE, predict(cfa09nE))
  p09 <- p09E %>% bind_rows(p09nE) %>% mutate(cycle = "C2") %>% 
    dplyr::select(all_of(Id), all_of(Newscales))
  rm(cfa09E, cfa09nE) #Remove fit to save space in disk
  
  #Fit for each country separately
  cnt09 <- unique(ds09$COUNTRY)
  meast09 <- NULL
  stdl09 <- NULL
  for (c09 in cnt09) {
    dscNT <- ds09[ds09$COUNTRY == c09,]
    survey.cnt09 <- svydesign(ids = ~ IDCL, weights = ~ SENWGT_Gc2, strata = ~ IDJK, nest = TRUE, data = dscNT)
    if(!c09 %in% c(CNTne,CNT2cne)) model <- model09E else model <- model09nE
    CNTcfa <- cfa(model, cluster = c("IDSCHOOL"), data = dscNT)
    survey.CNTfit <- lavaan.survey(lavaan.fit = CNTcfa, survey.design = survey.cnt09, estimator= "MLMVS")
    meas <- fitMeasures(survey.CNTfit, c("chisq","df", "cfi", "tli","rmsea", "srmr"), output = "matrix")
    meas <- rbind(n = nobs(survey.CNTfit), meas)
    meast09 <- cbind(meast09, meas)
    stdl <- standardizedSolution(survey.CNTfit) %>% 
      filter(op == "=~") %>% 
      mutate(cntry = c09)
    stdl09 <- rbind(stdl09, stdl)
  }
  meast09 <- as.data.frame(t(meast09))
  rownames(meast09) <- cnt09
  rm(CNTcfa)
  
  cat('### CFA - ICCS 2009')
  cat('  \n')
  cat('  \n')
  tmeasE <- t(fitMeasures(survey.fit09E, c("chisq","df","cfi", "tli","rmsea", "srmr"), 
                          output = "matrix"))
  tmeasnE <- t(fitMeasures(survey.fit09nE, c("chisq","df","cfi", "tli","rmsea", "srmr"), 
                           output = "matrix"))
  meas09 <- rbind(data.frame(Quest = "European", n = nobs(survey.fit09E), round(tmeasE, 3)),
                  data.frame(Quest = "Non-european", n = nobs(survey.fit09nE), round(tmeasnE, 3)))
  knitr::kable(meas09) %>% print() 
  
  cat('  \n')
  cat('### CFA - ICCS 2009, by countries')
  cat('  \n')
  knitr::kable(meast09,digits = 3) %>% print() 
  cat('  \n')
  cat('  \n')
  invisible(semPaths(survey.fit09E,"model", "std", "lisrel", edge.label.cex = 0.6, intercepts = FALSE, groups = "latent", 
                     pastel = TRUE, title = FALSE, nCharNodes = 10, nDigits = 1))
  title("CFA measurement European model", line = 2)
  invisible(semPaths(survey.fit09nE,"model", "std", "lisrel", edge.label.cex = 0.6, intercepts = FALSE, groups = "latent", 
                     pastel = TRUE, title = FALSE, nCharNodes = 10, nDigits = 1))
  title("CFA measurement Non European model", line = 2)
  cat('  \n')
  cat('  \n')
  
  labels <- data.frame(label = tolower(sjlabelled::get_label(ds091))) 
  labels <- labels %>% filter(!str_detect(rownames(labels), c("IDSTUD|IDSCHOOL|COUNTRY|TOTWGT|GENDER"))) %>% 
    mutate(variable = rownames(.))
  stdl09 <- stdl09 %>% mutate(rhs = factor(rhs, levels = labels$variable, labels = labels$label)) %>% 
    mutate(Model = ifelse(cntry %in% c(CNTne,CNT2cne), "Non-European", "European"))
  l1 <- stdl09 %>% data.frame() %>% 
    ggplot(aes(x = est.std, y = rhs, color = reorder(cntry, desc(cntry)))) +
    geom_linerange(aes(xmin = ci.lower, xmax = ci.upper), position = position_dodge(0.4)) +
    geom_jitter(position = position_dodge(0.4)) +
    facet_grid(lhs~Model, scales = "free") +
    geom_text(aes(label=cntry),hjust=0, vjust=1, position = position_dodge(0.4), size = 2) +
    theme(legend.position = "none", axis.line.y = element_blank()) +
    ggtitle("Loading distribution of scales - ICCS 2009") +
    ylab("") +
    xlab("Loadings with Confidence Interval") +
    scale_y_discrete(labels = function(x) str_wrap(x, 25)) +
    scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(0,1), labels = function(x) sprintf("%.1f", x))
  print(l1)
  
  cat('  \n')
  cat('  \n')
  cat('### Invariance between COUNTRY')
  cat('  \n')
  cat('  \n')
  inv.conf09E <- cfa(model09E, data = ds09E, cluster = "IDSCHOOL", group = "COUNTRY")
  inv.conf09E <- lavaan.survey(lavaan.fit = inv.conf09E, survey.design = survey.design09E, estimator= "MLMVS")
  inv.metr09E <- cfa(model09E, data = ds09E, cluster = "IDSCHOOL", group = "COUNTRY", group.equal = c("loadings"))
  inv.metr09E <- lavaan.survey(lavaan.fit = inv.metr09E, survey.design = survey.design09E, estimator= "MLMVS")
  inv.scal09E <- cfa(model09E, data = ds09E, cluster = "IDSCHOOL", group = "COUNTRY", group.equal = c("loadings","intercepts"))
  inv.scal09E <- lavaan.survey(lavaan.fit = inv.scal09E, survey.design = survey.design09E, estimator= "MLMVS")
  inv.stri09E <- cfa(model09E, data = ds09E, cluster = "IDSCHOOL", group = "COUNTRY", group.equal = c("loadings","intercepts","lv.variances"))
  inv.stri09E <- lavaan.survey(lavaan.fit = inv.stri09E, survey.design = survey.design09E, estimator= "MLMVS")
  invarCNT1 <- data.frame(Quest = "European", round(rbind(Configural = fitMeasures(inv.conf09E, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                                          Metric = fitMeasures(inv.metr09E, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                                          Scalar = fitMeasures(inv.scal09E, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                                          Strict = fitMeasures(inv.stri09E, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea"))),3))
  
  inv.conf09nE <- cfa(model09nE, data = ds09nE, cluster = "IDSCHOOL", group = "COUNTRY")
  inv.conf09nE <- lavaan.survey(lavaan.fit = inv.conf09nE, survey.design = survey.design09nE, estimator= "MLMVS")
  inv.metr09nE <- cfa(model09nE, data = ds09nE, cluster = "IDSCHOOL", group = "COUNTRY", group.equal = c("loadings"))
  inv.metr09nE <- lavaan.survey(lavaan.fit = inv.metr09nE, survey.design = survey.design09nE, estimator= "MLMVS")
  inv.scal09nE <- cfa(model09nE, data = ds09nE, cluster = "IDSCHOOL", group = "COUNTRY", group.equal = c("loadings","intercepts"))
  inv.scal09nE <- lavaan.survey(lavaan.fit = inv.scal09nE, survey.design = survey.design09nE, estimator= "MLMVS")
  inv.stri09nE <- cfa(model09nE, data = ds09nE, cluster = "IDSCHOOL", group = "COUNTRY", group.equal = c("loadings","intercepts","lv.variances"))
  inv.stri09nE <- lavaan.survey(lavaan.fit = inv.stri09nE, survey.design = survey.design09nE, estimator= "MLMVS")
  
  invarCNT2 <- data.frame(Quest = "Non-European", round(rbind(Configural = fitMeasures(inv.conf09nE, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                                              Metric = fitMeasures(inv.metr09nE, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                                              Scalar = fitMeasures(inv.scal09nE, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                                              Strict = fitMeasures(inv.stri09nE, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea"))),3))
  
  invarCNT <- invarCNT1 %>% mutate(Invariance = rownames(invarCNT1))  %>% relocate(Invariance, .before = npar) %>%
    mutate(D_tli = tli-lag(tli),
           D_cfi = cfi-lag(cfi),
           D_rmsea = rmsea-lag(rmsea)) %>%
    bind_rows(invarCNT2 %>% mutate(Invariance = rownames(invarCNT2))  %>% relocate(Invariance, .before = npar) %>%
                mutate(D_tli = tli-lag(tli),
                       D_cfi = cfi-lag(cfi),
                       D_rmsea = rmsea-lag(rmsea))) %>%
    knitr::kable() %>% print()
  cat('  \n')
  cat('  \n')
  rm(inv.conf09E, inv.metr09E, inv.scal09E, inv.stri09E, inv.conf09nE, inv.metr09nE, inv.scal09nE, inv.stri09nE) #Remove to save space in disk
  
  cat('### Invariance between GENDER')
  cat('  \n')
  cat('  \n')
  inv.conf09E <- cfa(model09E, data = ds09E, cluster = "IDSCHOOL", group = "GENDER")
  inv.conf09E <- lavaan.survey(lavaan.fit = inv.conf09E, survey.design = survey.design09E, estimator= "MLMVS")
  inv.metr09E <- cfa(model09E, data = ds09E, cluster = "IDSCHOOL", group = "GENDER", group.equal = c("loadings"))
  inv.metr09E <- lavaan.survey(lavaan.fit = inv.metr09E, survey.design = survey.design09E, estimator= "MLMVS")
  inv.scal09E <- cfa(model09E, data = ds09E, cluster = "IDSCHOOL", group = "GENDER", group.equal = c("loadings","intercepts"))
  inv.scal09E <- lavaan.survey(lavaan.fit = inv.scal09E, survey.design = survey.design09E, estimator= "MLMVS")
  inv.stri09E <- cfa(model09E, data = ds09E, cluster = "IDSCHOOL", group = "GENDER", group.equal = c("loadings","intercepts","lv.variances"))
  inv.stri09E <- lavaan.survey(lavaan.fit = inv.stri09E, survey.design = survey.design09E, estimator= "MLMVS")
  
  invarGNDR1 <- data.frame(Quest = "European", round(rbind(Configural = fitMeasures(inv.conf09E, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                                           Metric = fitMeasures(inv.metr09E, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                                           Scalar = fitMeasures(inv.scal09E, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                                           Strict = fitMeasures(inv.stri09E, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea"))),3))
  inv.conf09nE <- cfa(model09nE, data = ds09nE, cluster = "IDSCHOOL", group = "GENDER")
  inv.conf09nE <- lavaan.survey(lavaan.fit = inv.conf09nE, survey.design = survey.design09nE, estimator= "MLMVS")
  inv.metr09nE <- cfa(model09nE, data = ds09nE, cluster = "IDSCHOOL", group = "GENDER", group.equal = c("loadings"))
  inv.metr09nE <- lavaan.survey(lavaan.fit = inv.metr09nE, survey.design = survey.design09nE, estimator= "MLMVS")
  inv.scal09nE <- cfa(model09nE, data = ds09nE, cluster = "IDSCHOOL", group = "GENDER", group.equal = c("loadings","intercepts"))
  inv.scal09nE <- lavaan.survey(lavaan.fit = inv.scal09nE, survey.design = survey.design09nE, estimator= "MLMVS")
  inv.stri09nE <- cfa(model09nE, data = ds09nE, cluster = "IDSCHOOL", group = "GENDER", group.equal = c("loadings","intercepts","lv.variances"))
  inv.stri09nE <- lavaan.survey(lavaan.fit = inv.stri09nE, survey.design = survey.design09nE, estimator= "MLMVS")
  
  invarGNDR2 <- data.frame(Quest = "Non-European", round(rbind(Configural = fitMeasures(inv.conf09nE, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                                               Metric = fitMeasures(inv.metr09nE, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                                               Scalar = fitMeasures(inv.scal09nE, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                                               Strict = fitMeasures(inv.stri09nE, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea"))),3))
  invarGNDR <- invarGNDR1 %>% mutate(Invariance = rownames(invarGNDR1))  %>% relocate(Invariance, .before = npar) %>%
    mutate(D_tli = tli-lag(tli),
           D_cfi = cfi-lag(cfi),
           D_rmsea = rmsea-lag(rmsea)) %>%
    bind_rows(invarGNDR2 %>% mutate(Invariance = rownames(invarGNDR2))  %>% relocate(Invariance, .before = npar) %>%
                mutate(D_tli = tli-lag(tli),
                       D_cfi = cfi-lag(cfi),
                       D_rmsea = rmsea-lag(rmsea))) %>%
    knitr::kable() %>% print()
  cat('  \n')
  cat('  \n')
  rm(inv.conf09E, inv.metr09E, inv.scal09E, inv.stri09E, inv.conf09nE, inv.metr09nE, inv.scal09nE, inv.stri09nE) #Remove to save space in disk
  
}
if (any(grepl("16", years$year))){
    model16E<-'
      Gend_Equal =~ IS3G24A + IS3G24B + IS3G24C + IS3G24D + IS3G24E
      Immi_Equal =~ ES3G04A + ES3G04B + ES3G04C + ES3G04D + ES3G04E
      Ethn_Equal =~ IS3G25A + IS3G25B + IS3G25C + IS3G25D + IS3G25E
      IS3G24A ~~ IS3G24B
      IS3G25A ~~ IS3G25B
      ES3G04A ~~ ES3G04D
    '
    model16nE<-'
      Gend_Equal =~ IS3G24A + IS3G24B + IS3G24C + IS3G24D + IS3G24E
      Ethn_Equal =~ IS3G25A + IS3G25B + IS3G25C + IS3G25D + IS3G25E
      IS3G24C ~~ IS3G24D
      IS3G25A ~~ IS3G25B
      IS3G24A ~~ IS3G24B
    '
  #############2016#########
  cat('## ICCS 2016  \n')
    index16 <- Itemdesc %>% filter(item != "index") %>% dplyr::select(ICCS_2016) %>% na.omit() %>% pull()
    
    ds161 <- ISC_cfa %>% filter(!is.na(TOTWGT_Gc3)) %>% 
      dplyr::select(all_of(index16), all_of(Id), IDJK, IDCL, SENWGT_Gc3, S_GENDER) %>% 
      mutate(GENDER = as.character(S_GENDER)) 
    ds16 <- ds161 %>% mutate_at(.funs = as.numeric, .vars = index16)
    
    #European countries
    ds16E <- ds16 %>% filter(!COUNTRY %in% c(CNTne,CNT2cne))
    survey.design16E <- svydesign(ids= ~ IDCL, weights = ~ SENWGT_Gc3, strata = ~ IDJK, nest = TRUE, data = ds16E)
    cfa16E <- cfa(model16E, data = ds16E, cluster = c("COUNTRY", "IDSCHOOL"), missing = "fiml")
    survey.fit16E <- lavaan.survey(lavaan.fit = cfa16E, survey.design = survey.design16E, estimator= "MLMVS")
    
    #cfa <- cfa(model16E, data = ds16E, cluster = c("COUNTRY", "IDSCHOOL"))
    #summary(cfa, fit.measures=TRUE)
    #print(modindices(cfa, sort=T)[1:10,])
    
    #Non European countries
    ds16nE <- ds16 %>% filter(COUNTRY %in% c(CNTne,CNT2cne))
    survey.design16nE <- svydesign(ids= ~ IDCL, weights = ~ SENWGT_Gc3, strata = ~ IDJK, nest = TRUE, data = ds16nE)
    cfa16nE <- cfa(model16nE, data = ds16nE, cluster = c("COUNTRY", "IDSCHOOL"), missing = "fiml")
    survey.fit16nE <- lavaan.survey(lavaan.fit = cfa16nE, survey.design = survey.design16nE, estimator= "MLMVS")
    
    # cfa <- cfa(model16nE, data = ds16nE, cluster = c("COUNTRY", "IDSCHOOL"))
    # summary(cfa, fit.measures=TRUE)
    # print(modindices(cfa,sort=T)[1:10,])

    #Factor scores for latent variables
    p16E <- cbind(ds16E, predict(cfa16E)) #Prediction of factor scores should be based on survey design but not possible to obtain in R using FIML
    p16nE <- cbind(ds16nE, predict(cfa16nE))
    p16 <- p16E %>% bind_rows(p16nE) %>% mutate(cycle = "C3") %>% 
      dplyr::select(all_of(Id), all_of(Newscales))
    rm(cfa16E, cfa16nE) #Remove fit to save space in disk
    
    #Fit for each country separately
    cnt16 <- unique(ds16$COUNTRY)
    meast16 <- NULL
    stdl16 <- NULL
    for (c16 in cnt16) {
      dscNT <- ds16[ds16$COUNTRY == c16,]
      survey.cnt16 <- svydesign(ids = ~ IDCL, weights = ~ SENWGT_Gc3, strata = ~ IDJK, nest = TRUE, data = dscNT)
      if(!c16 %in% c(CNTne,CNT2cne)) model <- model16E else model <- model16nE
      CNTcfa <- cfa(model, cluster = c("IDSCHOOL"), data = dscNT)
      survey.CNTfit <- lavaan.survey(lavaan.fit = CNTcfa, survey.design = survey.cnt16, estimator= "MLMVS")
      meas <- fitMeasures(survey.CNTfit, c("chisq","df", "cfi", "tli","rmsea", "srmr"), output = "matrix")
      meas <- rbind(n = nobs(survey.CNTfit), meas)
      meast16 <- cbind(meast16, meas)
      stdl <- standardizedSolution(survey.CNTfit) %>% 
        filter(op == "=~") %>% 
        mutate(cntry = c16)
      stdl16 <- rbind(stdl16, stdl)
    }
    meast16 <- as.data.frame(t(meast16))
    rownames(meast16) <- cnt16
    rm(CNTcfa)
    
    cat('### CFA - ICCS 2016')
    cat('  \n')
    cat('  \n')
    tmeasE <- t(fitMeasures(survey.fit16E, c("chisq","df","cfi", "tli","rmsea", "srmr"), 
                            output = "matrix"))
    tmeasnE <- t(fitMeasures(survey.fit16nE, c("chisq","df","cfi", "tli","rmsea", "srmr"), 
                             output = "matrix"))
    meas16 <- rbind(data.frame(Quest = "European", n = nobs(survey.fit16E), round(tmeasE, 3)),
                    data.frame(Quest = "Non-european", n = nobs(survey.fit16nE), round(tmeasnE, 3)))
    knitr::kable(meas16) %>% print() 
    
    cat('  \n')
    cat('### CFA - ICCS 2016, by countries')
    cat('  \n')
    knitr::kable(meast16,digits = 3) %>% print() 
    cat('  \n')
    cat('  \n')
    invisible(semPaths(survey.fit16E,"model", "std", "lisrel", edge.label.cex = 0.6, intercepts = FALSE, groups = "latent", 
                       pastel = TRUE, title = FALSE, nCharNodes = 10, nDigits = 1))
    title("CFA measurement European model", line = 2)
    invisible(semPaths(survey.fit16nE,"model", "std", "lisrel", edge.label.cex = 0.6, intercepts = FALSE, groups = "latent", 
                       pastel = TRUE, title = FALSE, nCharNodes = 10, nDigits = 1))
    title("CFA measurement Non European model", line = 2)
    cat('  \n')
    cat('  \n')
    
    labels <- data.frame(label = tolower(str_remove(sjlabelled::get_label(ds161), "Moving/<Immigrants> | Rights and Responsibilities/Rights and responsibilities/|Rights and Responsibilities/Roles women and men/"))) 
    labels <- labels %>% filter(!str_detect(rownames(labels), c("IDSTUD|IDSCHOOL|COUNTRY|TOTWGT|GENDER"))) %>% 
      mutate(variable = rownames(.))
    stdl16 <- stdl16 %>% mutate(rhs = factor(rhs, levels = labels$variable, labels = labels$label)) %>% 
      mutate(Model = ifelse(cntry %in% c(CNTne,CNT2cne), "Non-European", "European"))
    l1 <- stdl16 %>% data.frame() %>% 
      ggplot(aes(x = est.std, y = rhs, color = reorder(cntry, desc(cntry)))) +
      geom_linerange(aes(xmin = ci.lower, xmax = ci.upper), position = position_dodge(0.4)) +
      geom_jitter(position = position_dodge(0.4)) +
      facet_grid(lhs~Model, scales = "free") +
      geom_text(aes(label=cntry),hjust=0, vjust=1, position = position_dodge(0.4), size = 2) +
      theme(legend.position = "none", axis.line.y = element_blank()) +
      ggtitle("Loading distribution of scales - ICCS 2016") +
      ylab("") +
      xlab("Loadings with Confidence Interval") +
      scale_y_discrete(labels = function(x) str_wrap(x, 25)) +
      scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(0,1), labels = function(x) sprintf("%.1f", x))
    print(l1)
    
    cat('  \n')
    cat('  \n')
    cat('### Invariance between COUNTRY')
    cat('  \n')
    cat('  \n')
    inv.conf16E <- cfa(model16E, data = ds16E, cluster = "IDSCHOOL", group = "COUNTRY")
    inv.conf16E <- lavaan.survey(lavaan.fit = inv.conf16E, survey.design = survey.design16E, estimator= "MLMVS")
    inv.metr16E <- cfa(model16E, data = ds16E, cluster = "IDSCHOOL", group = "COUNTRY", group.equal = c("loadings"))
    inv.metr16E <- lavaan.survey(lavaan.fit = inv.metr16E, survey.design = survey.design16E, estimator= "MLMVS")
    inv.scal16E <- cfa(model16E, data = ds16E, cluster = "IDSCHOOL", group = "COUNTRY", group.equal = c("loadings","intercepts"))
    inv.scal16E <- lavaan.survey(lavaan.fit = inv.scal16E, survey.design = survey.design16E, estimator= "MLMVS")
    inv.stri16E <- cfa(model16E, data = ds16E, cluster = "IDSCHOOL", group = "COUNTRY", group.equal = c("loadings","intercepts","lv.variances"))
    inv.stri16E <- lavaan.survey(lavaan.fit = inv.stri16E, survey.design = survey.design16E, estimator= "MLMVS")
    invarCNT1 <- data.frame(Quest = "European", round(rbind(Configural = fitMeasures(inv.conf16E, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                                            Metric = fitMeasures(inv.metr16E, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                                            Scalar = fitMeasures(inv.scal16E, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                                            Strict = fitMeasures(inv.stri16E, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea"))),3))
    
    inv.conf16nE <- cfa(model16nE, data = ds16nE, cluster = "IDSCHOOL", group = "COUNTRY")
    inv.conf16nE <- lavaan.survey(lavaan.fit = inv.conf16nE, survey.design = survey.design16nE, estimator= "MLMVS")
    inv.metr16nE <- cfa(model16nE, data = ds16nE, cluster = "IDSCHOOL", group = "COUNTRY", group.equal = c("loadings"))
    inv.metr16nE <- lavaan.survey(lavaan.fit = inv.metr16nE, survey.design = survey.design16nE, estimator= "MLMVS")
    inv.scal16nE <- cfa(model16nE, data = ds16nE, cluster = "IDSCHOOL", group = "COUNTRY", group.equal = c("loadings","intercepts"))
    inv.scal16nE <- lavaan.survey(lavaan.fit = inv.scal16nE, survey.design = survey.design16nE, estimator= "MLMVS")
    inv.stri16nE <- cfa(model16nE, data = ds16nE, cluster = "IDSCHOOL", group = "COUNTRY", group.equal = c("loadings","intercepts","lv.variances"))
    inv.stri16nE <- lavaan.survey(lavaan.fit = inv.stri16nE, survey.design = survey.design16nE, estimator= "MLMVS")
    
    invarCNT2 <- data.frame(Quest = "Non-European", round(rbind(Configural = fitMeasures(inv.conf16nE, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                                                Metric = fitMeasures(inv.metr16nE, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                                                Scalar = fitMeasures(inv.scal16nE, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                                                Strict = fitMeasures(inv.stri16nE, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea"))),3))
    
    invarCNT <- invarCNT1 %>% mutate(Invariance = rownames(invarCNT1))  %>% relocate(Invariance, .before = npar) %>%
      mutate(D_tli = tli-lag(tli),
             D_cfi = cfi-lag(cfi),
             D_rmsea = rmsea-lag(rmsea)) %>%
      bind_rows(invarCNT2 %>% mutate(Invariance = rownames(invarCNT2))  %>% relocate(Invariance, .before = npar) %>%
                  mutate(D_tli = tli-lag(tli),
                         D_cfi = cfi-lag(cfi),
                         D_rmsea = rmsea-lag(rmsea))) %>%
      knitr::kable() %>% print()
    cat('  \n')
    cat('  \n')
    rm(inv.conf16E, inv.metr16E, inv.scal16E, inv.stri16E, inv.conf16nE, inv.metr16nE, inv.scal16nE, inv.stri16nE) #Remove to save space in disk
    
    cat('### Invariance between GENDER')
    cat('  \n')
    cat('  \n')
    inv.conf16E <- cfa(model16E, data = ds16E, cluster = "IDSCHOOL", group = "GENDER")
    inv.conf16E <- lavaan.survey(lavaan.fit = inv.conf16E, survey.design = survey.design16E, estimator= "MLMVS")
    inv.metr16E <- cfa(model16E, data = ds16E, cluster = "IDSCHOOL", group = "GENDER", group.equal = c("loadings"))
    inv.metr16E <- lavaan.survey(lavaan.fit = inv.metr16E, survey.design = survey.design16E, estimator= "MLMVS")
    inv.scal16E <- cfa(model16E, data = ds16E, cluster = "IDSCHOOL", group = "GENDER", group.equal = c("loadings","intercepts"))
    inv.scal16E <- lavaan.survey(lavaan.fit = inv.scal16E, survey.design = survey.design16E, estimator= "MLMVS")
    inv.stri16E <- cfa(model16E, data = ds16E, cluster = "IDSCHOOL", group = "GENDER", group.equal = c("loadings","intercepts","lv.variances"))
    inv.stri16E <- lavaan.survey(lavaan.fit = inv.stri16E, survey.design = survey.design16E, estimator= "MLMVS")
    
    invarGNDR1 <- data.frame(Quest = "European", round(rbind(Configural = fitMeasures(inv.conf16E, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                                             Metric = fitMeasures(inv.metr16E, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                                             Scalar = fitMeasures(inv.scal16E, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                                             Strict = fitMeasures(inv.stri16E, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea"))),3))
    inv.conf16nE <- cfa(model16nE, data = ds16nE, cluster = "IDSCHOOL", group = "GENDER")
    inv.conf16nE <- lavaan.survey(lavaan.fit = inv.conf16nE, survey.design = survey.design16nE, estimator= "MLMVS")
    inv.metr16nE <- cfa(model16nE, data = ds16nE, cluster = "IDSCHOOL", group = "GENDER", group.equal = c("loadings"))
    inv.metr16nE <- lavaan.survey(lavaan.fit = inv.metr16nE, survey.design = survey.design16nE, estimator= "MLMVS")
    inv.scal16nE <- cfa(model16nE, data = ds16nE, cluster = "IDSCHOOL", group = "GENDER", group.equal = c("loadings","intercepts"))
    inv.scal16nE <- lavaan.survey(lavaan.fit = inv.scal16nE, survey.design = survey.design16nE, estimator= "MLMVS")
    inv.stri16nE <- cfa(model16nE, data = ds16nE, cluster = "IDSCHOOL", group = "GENDER", group.equal = c("loadings","intercepts","lv.variances"))
    inv.stri16nE <- lavaan.survey(lavaan.fit = inv.stri16nE, survey.design = survey.design16nE, estimator= "MLMVS")
    
    invarGNDR2 <- data.frame(Quest = "Non-European", round(rbind(Configural = fitMeasures(inv.conf16nE, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                                                 Metric = fitMeasures(inv.metr16nE, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                                                 Scalar = fitMeasures(inv.scal16nE, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                                                 Strict = fitMeasures(inv.stri16nE, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea"))),3))
    invarGNDR <- invarGNDR1 %>% mutate(Invariance = rownames(invarGNDR1))  %>% relocate(Invariance, .before = npar) %>%
      mutate(D_tli = tli-lag(tli),
             D_cfi = cfi-lag(cfi),
             D_rmsea = rmsea-lag(rmsea)) %>%
      bind_rows(invarGNDR2 %>% mutate(Invariance = rownames(invarGNDR2))  %>% relocate(Invariance, .before = npar) %>%
                  mutate(D_tli = tli-lag(tli),
                         D_cfi = cfi-lag(cfi),
                         D_rmsea = rmsea-lag(rmsea))) %>%
      knitr::kable() %>% print()
    cat('  \n')
    cat('  \n')
    rm(inv.conf16E, inv.metr16E, inv.scal16E, inv.stri16E, inv.conf16nE, inv.metr16nE, inv.scal16nE, inv.stri16nE) #Remove to save space in disk
}

cat('  \n')
cat('  \n')
cat('## Summary indexes, all countries, all cycles')
cat('  \n')
cat('  \n')

pall <- plyr::rbind.fill(p99, p09, p16)
ISC_cfa <- left_join(ISC_cfa, pall, by = all_of(Id)) 
set_label(ISC_cfa$Gend_Equal) <- c("Attitudes towards Gender equality")
set_label(ISC_cfa$Immi_Equal) <- c("Attitudes towards Immigrants rights")
set_label(ISC_cfa$Ethn_Equal) <- c("Attitudes towards Minorities rights")


mg <- ISC_cfa %>% dplyr::select(cycle, all_of(Newscales)) %>% group_by(cycle) %>% 
  summarise_at(Newscales, list(~ mean(., na.rm = TRUE))) %>% 
  mutate(cycle = as.factor(cycle)) %>% data.frame()

for(i in 1:length(Newscales)){
  
  vargra <- Newscales[i]
  s2 <- ISC_cfa %>% dplyr::select("cycle", "COUNTRY", vargra) %>% na.omit() %>% 
    ggplot(aes_string(x = vargra, y = paste0("reorder(COUNTRY, desc(COUNTRY))"), group = paste0("interaction(cycle, COUNTRY)"), fill = "COUNTRY")) +
    geom_boxplot() +
    facet_grid(.~ cycle)+
    geom_vline(aes_string(xintercept = vargra), mg, linetype="dotted", size = 0.8) +
    ggtitle(eval(parse(text=paste0("attributes(ISC_cfa$",vargra,")$label")))) +
    ylab("Distribution of Scores CFA") +
    xlab(paste0(vargra)) +
    scale_color_brewer(palette="Accent") +
    theme(legend.position = "none")
  print(s2)
}

ISC_cfa <- ISC_cfa %>% select(all_of(Id), all_of(Scales), all_of(Newscales))

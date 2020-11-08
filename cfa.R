library(lavaan)
library(lavaan.survey)
library(semPlot)


recod <- c("BS4G6", "IS2P24C", "IS3G24C", "BS4G9", "IS2P24D", "IS3G24D","BS4G13", "IS2P24F", "IS3G24F")
dat <- data.frame(psych::reverse.code(keys = rep(-1,length(recod)), 
                                      items = ISC[,recod], 
                                      mini = rep(1,length(recod)), 
                                      maxi = rep(4,length(recod))))
colnames(dat) <- paste(recod,"_r",sep = "")
ISC <- cbind(ISC, dat)
ISC$BS4G6_r <- sjlabelled::set_label(ISC$BS4G6_r, paste0(sjlabelled::get_label(ISC$BS4G6),"(r)"))
ISC$BS4G9_r <- sjlabelled::set_label(ISC$BS4G9_r, paste0(sjlabelled::get_label(ISC$BS4G9),"(r)"))
ISC$BS4G13_r <- sjlabelled::set_label(ISC$BS4G13_r, paste0(sjlabelled::get_label(ISC$BS4G13),"(r)"))
ISC$IS2P24C_r <- sjlabelled::set_label(ISC$IS2P24C_r, paste0(sjlabelled::get_label(ISC$IS2P24C),"(r)"))
ISC$IS2P24D_r <- sjlabelled::set_label(ISC$IS2P24D_r, paste0(sjlabelled::get_label(ISC$IS2P24D),"(r)"))
ISC$IS2P24F_r <- sjlabelled::set_label(ISC$IS2P24F_r, paste0(sjlabelled::get_label(ISC$IS2P24F),"(r)"))
ISC$IS3G24C_r <- sjlabelled::set_label(ISC$IS3G24C_r, paste0(sjlabelled::get_label(ISC$IS3G24C),"(r)"))
ISC$IS3G24D_r <- sjlabelled::set_label(ISC$IS3G24D_r, paste0(sjlabelled::get_label(ISC$IS3G24D),"(r)"))
ISC$IS3G24F_r <- sjlabelled::set_label(ISC$IS3G24F_r, paste0(sjlabelled::get_label(ISC$IS3G24F),"(r)"))

Scalesr <- Scales %>% 
  mutate(CIVED_1999 = ifelse(CIVED_1999 %in% c("BS4G6","BS4G9","BS4G13"), paste0(CIVED_1999,"_r"), CIVED_1999),
          ICCS_2009 = ifelse(ICCS_2009 %in% c("IS2P24C","IS2P24D","IS2P24F"), paste0(ICCS_2009 ,"_r"), ICCS_2009),
          ICCS_2016 = ifelse(ICCS_2016 %in% c("IS3G24C","IS3G24D","IS3G24F"), paste0(ICCS_2016 ,"_r"), ICCS_2016)) 
  
if (any(grepl("99", years$year))){
  model99<-'
    Gend_Equal =~ BS4G1 + BS4G4 + BS4G6_r + BS4G9_r + BS4G11 + BS4G13_r
    Immi_Equal =~ BS4H1 + BS4H2 + BS4H3 + BS4H4 + BS4H5
    Ethn_Equal =~ BS4G2 + BS4G5 + BS4G8 + BS4G12
    BS4H1 ~~  BS4H4
    BS4G9_r ~~ BS4G13_r
    '
  cat('## CIVED 1999  \n')
  #############1999#########
  index99 <- Scalesr %>% filter(item != "index") %>% dplyr::select(CIVED_1999) %>% na.omit() %>% pull()

  ds991 <- ISC %>% filter(!is.na(TOTWGT_Gc1)) %>% dplyr::select(all_of(index99), IDSTUD, TOTWGT_Gc1, COUNTRY, GENDER) %>% 
    mutate(GENDER = as.character(GENDER)) 
  ds99 <- ds991 %>% mutate_at(.funs = as.numeric, .vars = index99) %>%  na.omit()

  survey.design99 <- svydesign(ids=~IDSTUD, prob=~TOTWGT_Gc1, data=ds99)
  
  cfa99 <- cfa(model99, data = ds99)
  survey.fit99 <- lavaan.survey(lavaan.fit = cfa99, survey.design = survey.design99)
  #print(modindices(survey.fit99,sort=T)[1:10,])
  p99 <- cbind(ds99, predict(cfa99))
  p99 <- p99 %>% mutate(cycle = "C1") %>% dplyr::select(cycle, IDSTUD, COUNTRY,Gend_Equal, Immi_Equal, Ethn_Equal)
  
  cnt99 <- unique(ds99$COUNTRY)
  meast99 <- NULL
  stdl99 <- NULL
  for (c99 in cnt99) {
    survey.cnt99 <- svydesign(ids=~IDSTUD, prob=~TOTWGT_Gc1, data=ds99[ds99$COUNTRY == c99,])
    
    CNTcfa <- cfa(model99, data = ds99[ds99$COUNTRY == c99,])
    survey.CNTfit <- lavaan.survey(lavaan.fit = CNTcfa, survey.design = survey.cnt99)
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
  
  cat('### CFA - ICCS 1999, all countries')
  cat('  \n')
  cat('  \n')
  meas99 <- fitMeasures(survey.fit99, c("chisq","df","cfi", "tli","rmsea", "srmr"), 
                        output = "matrix")
  knitr::kable(cbind(n = nobs(survey.fit99), round(as.data.frame(t(meas99)), 3))) %>% print() 
  
  cat('  \n')
  cat('### CFA - ICCS 1999, by countries')
  cat('  \n')
  knitr::kable(meast99,digits = 3) %>% print() 
  cat('  \n')
  cat('  \n')
  invisible(semPaths(survey.fit99,"model", "std", "lisrel", edge.label.cex = 0.5, intercepts = FALSE, groups = "latent", 
                     pastel = TRUE, title = FALSE, optimizeLatRes = TRUE, nCharNodes = 10))
  title("CFA measurement model", line = 2)
  cat('  \n')
  cat('  \n')
  
  labels <- data.frame(label = tolower(sjlabelled::get_label(ds991))) 
  labels <- labels %>% filter(!str_detect(rownames(labels), c("IDSTUD|IDSCHOOL|COUNTRY|TOTWGT|GENDER"))) %>% 
    mutate(variable = rownames(.))
  stdl99 <- stdl99 %>% mutate(rhs = factor(rhs, levels = labels$variable, labels = labels$label))
  l1 <- stdl99 %>% data.frame() %>% 
    ggplot(aes(x = est.std, y = rhs, color = reorder(cntry, desc(cntry)))) +
    geom_linerange(aes(xmin = ci.lower, xmax = ci.upper), position = position_dodge(0.4)) +
    geom_jitter(position = position_dodge(0.4)) +
    facet_wrap(. ~ lhs, scales = "free", ncol = 1)+
    geom_text(aes(label=cntry),hjust=0, vjust=1, position = position_dodge(0.4), size = 2) +
    theme(legend.position = "none") +
    ggtitle("Loading distribution of scales - ICCS 1999") +
    ylab("") +
    xlab("Loadings with Confidence Interval") +
    scale_y_discrete(labels = function(x) str_wrap(x, 25) )
  print(l1)
  cat('  \n')
  cat('  \n')
  cat('### Invariance between COUNTRY')
  cat('  \n')
  cat('  \n')
  inv.conf99 <- cfa(model99, data = ds99, group = "COUNTRY")
  inv.conf99 <- lavaan.survey(lavaan.fit = inv.conf99, survey.design = survey.design99)
  inv.metr99 <- cfa(model99, data = ds99, group = "COUNTRY", group.equal = c("loadings"))
  inv.metr99 <- lavaan.survey(lavaan.fit = inv.metr99, survey.design = survey.design99)
  inv.scal99 <- cfa(model99, data = ds99, group = "COUNTRY", group.equal = c("loadings","intercepts"))
  inv.scal99 <- lavaan.survey(lavaan.fit = inv.scal99, survey.design = survey.design99)
  inv.stri99 <- cfa(model99, data = ds99, group = "COUNTRY", group.equal = c("loadings","intercepts","lv.variances"))
  inv.stri99 <- lavaan.survey(lavaan.fit = inv.stri99, survey.design = survey.design99)
  
  invarCNT <- data.frame(round(rbind(Configural = fitMeasures(inv.conf99, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                     Metric = fitMeasures(inv.metr99, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                     Scalar = fitMeasures(inv.scal99, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                     Strict = fitMeasures(inv.stri99, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea"))),3))
  invarCNT <- invarCNT %>% mutate(Invariance = rownames(invarCNT)) %>% relocate(Invariance, .before = npar) %>% 
    mutate(D_tli = tli-lag(tli),
           D_cfi = cfi-lag(cfi),
           D_rmsea = rmsea-lag(rmsea)) %>% 
    knitr::kable() %>% print()
  cat('  \n')
  cat('  \n')
  
  cat('### Invariance between GENDER')
  cat('  \n')
  cat('  \n')
  inv.conf99 <- cfa(model99, data = ds99, group = "GENDER")
  inv.conf99 <- lavaan.survey(lavaan.fit = inv.conf99, survey.design = survey.design99)
  inv.metr99 <- cfa(model99, data = ds99, group = "GENDER", group.equal = c("loadings"))
  inv.metr99 <- lavaan.survey(lavaan.fit = inv.metr99, survey.design = survey.design99)
  inv.scal99 <- cfa(model99, data = ds99, group = "GENDER", group.equal = c("loadings","intercepts"))
  inv.scal99 <- lavaan.survey(lavaan.fit = inv.scal99, survey.design = survey.design99)
  inv.stri99 <- cfa(model99, data = ds99, group = "GENDER", group.equal = c("loadings","intercepts","lv.variances"))
  inv.stri99 <- lavaan.survey(lavaan.fit = inv.stri99, survey.design = survey.design99)
  
  invarGNDR <- data.frame(round(rbind(Configural = fitMeasures(inv.conf99, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                      Metric = fitMeasures(inv.metr99, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                      Scalar = fitMeasures(inv.scal99, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                      Strict = fitMeasures(inv.stri99, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea"))),3))
  invarGNDR <- invarGNDR %>% mutate(Invariance = rownames(invarGNDR)) %>% relocate(Invariance, .before = npar) %>% 
    mutate(D_tli = tli-lag(tli),
           D_cfi = cfi-lag(cfi),
           D_rmsea = rmsea-lag(rmsea)) %>% 
    knitr::kable() %>% print()
  cat('  \n')
  cat('  \n')

}
if (any(grepl("09", years$year))){
  model09<-'
    Gend_Equal =~ IS2P24A + IS2P24B + IS2P24C_r + IS2P24D_r + IS2P24E + IS2P24F_r
    Immi_Equal =~ IS2P26A + IS2P26B + IS2P26C + IS2P26D + IS2P26E
    Ethn_Equal =~ IS2P25A + IS2P25B + IS2P25C + IS2P25D + IS2P25E
    IS2P24A ~~ IS2P24B
    IS2P25A ~~ IS2P25B
    IS2P26A ~~ IS2P26D
  '
  # model09<-'
  #   Gend_Equal =~ IS2P24A + IS2P24B + IS2P24E
  #   Immi_Equal =~ IS2P26A + IS2P26C + IS2P26D + IS2P26E
  #   Ethn_Equal =~ IS2P25A + IS2P25B + IS2P25C  + IS2P25E
  #   IS2P25A ~~ IS2P25B
  # '

  #############2009#########
  cat('## ICCS 2009  \n')
  index09 <- Scalesr %>% filter(item != "index") %>% dplyr::select(ICCS_2009) %>% na.omit() %>% pull()
  ds091 <- ISC %>% filter(!is.na(TOTWGT_Gc2)) %>% dplyr::select(all_of(index09), IDSTUD, IDSCHOOL,TOTWGT_Gc2, COUNTRY, SGENDER) %>% 
    mutate(SGENDER = as.character(SGENDER)) 
  ds09 <- ds091 %>% mutate_at(.funs = as.numeric, .vars = index09) %>% na.omit()
  
  # psych::pairwiseCount(ds09[ds09$COUNTRY == c09,all_of(index09)])
  # psych::pairwiseDescribe(ds09[,all_of(index09)],quant=c(.1,.25,.5,.75,.9))
  # 
  # ds09 %>%  dplyr::select(IS2P24A , IS2P24B , IS2P24E, IS2P26A , IS2P26C , IS2P26D , IS2P26E, IS2P25A , IS2P25B , IS2P25C  , IS2P25E, COUNTRY) %>% 
  #   group_by(COUNTRY) %>% 
  #   summarise_all(funs(sum(!is.na(.))))
  # ds09 %>% 
  #   group_by(COUNTRY, IDSTUD) %>%  
  #   dplyr::select(IS2P25A , IS2P25B , IS2P25C  , IS2P25E ) %>% 
  #   filter_all(all_vars(!is.na(.))) %>% 
  #   full_join(
  # ds09 %>% 
  #   group_by(COUNTRY, IDSTUD) %>%  
  #   dplyr::select(IS2P24A , IS2P24B , IS2P24E ) %>% 
  #   filter_all(all_vars(!is.na(.))) , by = c("COUNTRY", "IDSTUD")) %>%
  #   full_join( 
  # ds09 %>% 
  #   group_by(COUNTRY, IDSTUD) %>%  
  #   dplyr::select(IS2P26A , IS2P26C , IS2P26D , IS2P26E) %>% 
  #   filter_all(all_vars(!is.na(.))) , by = c("COUNTRY", "IDSTUD"))  %>% group_by(COUNTRY) %>%  count()
  survey.design09 <- svydesign(ids=~IDSTUD, prob=~TOTWGT_Gc2, data=ds09)
  
  # CV09 <- cov(ds09[,all_of(index09)], use = "na.or.complete")
  # cfa09 <- cfa(model09, sample.cov = CV09, sample.nobs = nrow(ds09))

  cfa09 <- cfa(model09, data = ds09)
  survey.fit09 <- lavaan.survey(lavaan.fit = cfa09, survey.design = survey.design09)
  #print(modindices(survey.fit09,sort=T)[1:10,])
  p09 <- cbind(ds09, predict(cfa09))
  p09 <- p09 %>% mutate(cycle = "C2") %>% dplyr::select(cycle, IDSTUD, COUNTRY,Gend_Equal, Immi_Equal, Ethn_Equal)
  
  cnt09 <- unique(ds09$COUNTRY)
  meast09 <- NULL
  stdl09 <- NULL
  for (c09 in cnt09) {
    survey.cnt09 <- svydesign(ids=~IDSTUD, prob=~TOTWGT_Gc2, data=ds09[ds09$COUNTRY == c09,])
    ds09cfa <- ds09 %>%  dplyr::select(all_of(index09), COUNTRY) %>% mutate(COUNTRY = as.numeric(factor(COUNTRY)))
    # write.table(ds09[ds09$COUNTRY == c09,], "CFADNK.dat", na = ".", quote = FALSE, row.names = FALSE, sep = "\t", col.names=FALSE)
    # CV09 <- cov(ds09[ds09$COUNTRY == c09, all_of(index09)], use = "na.or.complete")
    # CNTcfa <- cfa(model09, sample.cov = CV09, sample.nobs = 4355)
    CNTcfa <- cfa(model09, data = ds09[ds09$COUNTRY == c09,])
    survey.CNTfit <- lavaan.survey(lavaan.fit = CNTcfa, survey.design = survey.cnt09)
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

  cat('### CFA - ICCS 2009, all countries')
  cat('  \n')
  cat('  \n')
  meas09 <- fitMeasures(survey.fit09, c("chisq","df","cfi", "tli","rmsea", "srmr"), 
                        output = "matrix")
  knitr::kable(cbind(n = nobs(survey.fit09), round(as.data.frame(t(meas09)), 3))) %>% print() 
  
  cat('  \n')
  cat('### CFA - ICCS 2009, by countries')
  cat('  \n')
  knitr::kable(meast09,digits = 3) %>% print() 
  cat('  \n')
  cat('  \n')
 
  invisible(semPaths(survey.fit09,"model", "std", "lisrel", edge.label.cex = 0.5, intercepts = FALSE, groups = "latent", 
                     pastel = TRUE, title = FALSE, optimizeLatRes = TRUE, nCharNodes = 10))
  title("CFA measurement model", line = 2)
  cat('  \n')
  cat('  \n')
  
  labels <- data.frame(label = tolower(sjlabelled::get_label(ds091))) 
  labels <- labels %>% filter(!str_detect(rownames(labels), c("IDSTUD|IDSCHOOL|COUNTRY|TOTWGT|GENDER"))) %>% 
    mutate(variable = rownames(.))
  stdl09 <- stdl09 %>% mutate(rhs = factor(rhs, levels = labels$variable, labels = labels$label))
  l2 <- stdl09 %>% data.frame() %>% 
  ggplot(aes(x = est.std, y = rhs, color = reorder(cntry, desc(cntry)))) +
    geom_linerange(aes(xmin = ci.lower, xmax = ci.upper), position = position_dodge(0.4)) +
    geom_jitter(position = position_dodge(0.4)) +
    facet_wrap(. ~ lhs, scales = "free", ncol = 1)+
    geom_text(aes(label=cntry),hjust=0, vjust=1, position = position_dodge(0.4), size = 2) +
    theme(legend.position = "none") +
    ggtitle("Loading distribution of scales - ICCS 2009") +
    ylab("") +
    xlab("Loadings with Confidence Interval") +
    scale_y_discrete(labels = function(x) str_wrap(x, 25) )
  print(l2)
  cat('  \n')
  cat('  \n')
  cat('### Invariance between COUNTRY')
  cat('  \n')
  cat('  \n')
  inv.conf09 <- cfa(model09, data = ds09, group = "COUNTRY")
  inv.conf09 <- lavaan.survey(lavaan.fit = inv.conf09, survey.design = survey.design09)
  inv.metr09 <- cfa(model09, data = ds09, group = "COUNTRY", group.equal = c("loadings"))
  inv.metr09 <- lavaan.survey(lavaan.fit = inv.metr09, survey.design = survey.design09)
  inv.scal09 <- cfa(model09, data = ds09, group = "COUNTRY", group.equal = c("loadings","intercepts"))
  inv.scal09 <- lavaan.survey(lavaan.fit = inv.scal09, survey.design = survey.design09)
  inv.stri09 <- cfa(model09, data = ds09, group = "COUNTRY", group.equal = c("loadings","intercepts","lv.variances"))
  inv.stri09 <- lavaan.survey(lavaan.fit = inv.stri09, survey.design = survey.design09)
  
  invarCNT <- data.frame(round(rbind(Configural = fitMeasures(inv.conf09, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                  Metric = fitMeasures(inv.metr09, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                  Scalar = fitMeasures(inv.scal09, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                  Strict = fitMeasures(inv.stri09, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea"))),3))
  invarCNT <- invarCNT %>% mutate(Invariance = rownames(invarCNT)) %>% relocate(Invariance, .before = npar) %>% 
    mutate(D_tli = tli-lag(tli),
           D_cfi = cfi-lag(cfi),
           D_rmsea = rmsea-lag(rmsea)) %>% 
    knitr::kable() %>% print()
  cat('  \n') 
  cat('  \n')
  cat('### Invariance between GENDER')
  cat('  \n')
  cat('  \n')
  inv.conf09 <- cfa(model09, data = ds09, group = "SGENDER")
  inv.conf09 <- lavaan.survey(lavaan.fit = inv.conf09, survey.design = survey.design09)
  inv.metr09 <- cfa(model09, data = ds09, group = "SGENDER", group.equal = c("loadings"))
  inv.metr09 <- lavaan.survey(lavaan.fit = inv.metr09, survey.design = survey.design09)
  inv.scal09 <- cfa(model09, data = ds09, group = "SGENDER", group.equal = c("loadings","intercepts"))
  inv.scal09 <- lavaan.survey(lavaan.fit = inv.scal09, survey.design = survey.design09)
  inv.stri09 <- cfa(model09, data = ds09, group = "SGENDER", group.equal = c("loadings","intercepts","lv.variances"))
  inv.stri09 <- lavaan.survey(lavaan.fit = inv.stri09, survey.design = survey.design09)
  
  invarGNDR <- data.frame(round(rbind(Configural = fitMeasures(inv.conf09, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                     Metric = fitMeasures(inv.metr09, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                     Scalar = fitMeasures(inv.scal09, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                     Strict = fitMeasures(inv.stri09, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea"))),3))
  invarGNDR <- invarGNDR %>% mutate(Invariance = rownames(invarGNDR)) %>% relocate(Invariance, .before = npar) %>% 
    mutate(D_tli = tli-lag(tli),
           D_cfi = cfi-lag(cfi),
           D_rmsea = rmsea-lag(rmsea)) %>% 
    knitr::kable() %>% print()
  cat('  \n')
  cat('  \n')

  
}
if (any(grepl("16", years$year))){
  model16<-'
    Gend_Equal =~ IS3G24A + IS3G24B + IS3G24C_r + IS3G24D_r + IS3G24E + IS3G24F_r
    Immi_Equal =~ ES3G04A + ES3G04B + ES3G04C + ES3G04D + ES3G04E
    Ethn_Equal =~ IS3G25A + IS3G25B + IS3G25C + IS3G25D + IS3G25E
    IS3G24A ~~ IS3G24B
    IS3G25A ~~ IS3G25B
    ES3G04A ~~ ES3G04D
  '
  #############2016#########
  cat('## ICCS 2016  \n')
  cat('  \n')
  index16 <- Scalesr %>% filter(item != "index") %>% dplyr::select(ICCS_2016) %>% na.omit() %>% pull()
  ds161 <- ISC %>% filter(!is.na(TOTWGT_Gc3)) %>% dplyr::select(all_of(index16), IDSTUD, TOTWGT_Gc3, COUNTRY, S_GENDER) %>% 
    mutate(S_GENDER = as.character(S_GENDER)) 
  ds16 <- ds161 %>% mutate_at(.funs = as.numeric, .vars = index16) %>% na.omit()
  survey.design16 <- svydesign(ids=~IDSTUD, prob=~TOTWGT_Gc3, data=ds16)
  
  cfa16 <- cfa(model16, data = ds16)
  survey.fit16 <- lavaan.survey(lavaan.fit = cfa16, survey.design = survey.design16)
  #print(modindices(survey.fit16,sort=T)[1:10,])
  p16 <- cbind(ds16, predict(cfa16))
  p16 <- p16 %>% mutate(cycle = "C3") %>% dplyr::select(cycle, IDSTUD, COUNTRY,Gend_Equal, Immi_Equal, Ethn_Equal)
  
  cnt16 <- unique(ds16$COUNTRY)
  meast16 <- NULL
  stdl16 <- NULL
  for (c16 in cnt16) {
    survey.cnt16 <- svydesign(ids=~IDSTUD, prob=~TOTWGT_Gc3, data=ds16[ds16$COUNTRY == c16,])
    
    CNTcfa <- cfa(model16, data = ds16[ds16$COUNTRY == c16,])
    survey.CNTfit <- lavaan.survey(lavaan.fit = CNTcfa, survey.design = survey.cnt16)
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
  
  cat('### CFA - ICCS 2016, all countries')
  cat('  \n')
  cat('  \n')
  meas16 <- fitMeasures(survey.fit16, c("chisq","df","cfi", "tli","rmsea", "srmr"), 
                        output = "matrix")
  knitr::kable(cbind(n = nobs(survey.fit16), round(as.data.frame(t(meas16)), 3))) %>% print() 
  
  cat('  \n')
  cat('### CFA - ICCS 2016, by countries')
  cat('  \n')
  knitr::kable(meast16, digits = 3) %>% print() 
  cat('  \n')
  cat('  \n')
  #print(modindices(survey.fit16,sort=T)[1:10,])
  invisible(semPaths(survey.fit16,"model", "std", "lisrel", edge.label.cex = 0.5, intercepts = FALSE, groups = "latent", 
                     pastel = TRUE, title = FALSE, optimizeLatRes = TRUE, nCharNodes = 10))
  title("CFA measurement model", line = 2)
  cat('  \n')
  cat('  \n')
  
  labels <- data.frame(label = tolower(sjlabelled::get_label(ds161))) 
  labels <- labels %>% filter(!str_detect(rownames(labels), c("IDSTUD|IDSCHOOL|COUNTRY|TOTWGT|GENDER"))) %>% 
    mutate(variable = rownames(.),
           label = str_remove(label, "rights and responsibilities/rights and responsibilities/|rights and responsibilities/roles women and men/|moving/"))
  stdl16 <- stdl16 %>% mutate(rhs = factor(rhs, levels = labels$variable, labels = labels$label))
  l3 <- stdl16 %>% data.frame() %>% 
    ggplot(aes(x = est.std, y = rhs, color = reorder(cntry, desc(cntry)))) +
    geom_linerange(aes(xmin = ci.lower, xmax = ci.upper), position = position_dodge(0.4)) +
    geom_jitter(position = position_dodge(0.4)) +
    facet_wrap(. ~ lhs, scales = "free", ncol = 1)+
    geom_text(aes(label=cntry),hjust=0, vjust=1, position = position_dodge(0.4), size = 2) +
    theme(legend.position = "none") +
    ggtitle("Loading distribution of scales - ICCS 2016") +
    ylab("") +
    xlab("Loadings with Confidence Interval") +
    scale_y_discrete(labels = function(x) str_wrap(x, 30) )
  print(l3)
  cat('  \n')
  cat('  \n')
  cat('### Invariance between COUNTRY')
  cat('  \n')
  cat('  \n')
  inv.conf16 <- cfa(model16, data = ds16, group = "COUNTRY")
  inv.conf16 <- lavaan.survey(lavaan.fit = inv.conf16, survey.design = survey.design16)
  inv.metr16 <- cfa(model16, data = ds16, group = "COUNTRY", group.equal = c("loadings"))
  inv.metr16 <- lavaan.survey(lavaan.fit = inv.metr16, survey.design = survey.design16)
  inv.scal16 <- cfa(model16, data = ds16, group = "COUNTRY", group.equal = c("loadings","intercepts"))
  inv.scal16 <- lavaan.survey(lavaan.fit = inv.scal16, survey.design = survey.design16)
  inv.stri16 <- cfa(model16, data = ds16, group = "COUNTRY", group.equal = c("loadings","intercepts","lv.variances"))
  inv.stri16 <- lavaan.survey(lavaan.fit = inv.stri16, survey.design = survey.design16)
  
  invarCNT <- data.frame(round(rbind(Configural = fitMeasures(inv.conf16, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                  Metric = fitMeasures(inv.metr16, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                  Scalar = fitMeasures(inv.scal16, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                  Strict = fitMeasures(inv.stri16, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea"))),3))
  invarCNT <- invarCNT %>% mutate(Invariance = rownames(invarCNT)) %>% relocate(Invariance, .before = npar) %>% 
    mutate(D_tli = tli-lag(tli),
           D_cfi = cfi-lag(cfi),
           D_rmsea = rmsea-lag(rmsea)) %>% 
    knitr::kable() %>% print()
  cat('  \n')
  cat('  \n')
  cat('### Invariance between GENDER')
  cat('  \n')
  cat('  \n')
  inv.conf16 <- cfa(model16, data = ds16, group = "S_GENDER")
  inv.conf16 <- lavaan.survey(lavaan.fit = inv.conf16, survey.design = survey.design16)
  inv.metr16 <- cfa(model16, data = ds16, group = "S_GENDER", group.equal = c("loadings"))
  inv.metr16 <- lavaan.survey(lavaan.fit = inv.metr16, survey.design = survey.design16)
  inv.scal16 <- cfa(model16, data = ds16, group = "S_GENDER", group.equal = c("loadings","intercepts"))
  inv.scal16 <- lavaan.survey(lavaan.fit = inv.scal16, survey.design = survey.design16)
  inv.stri16 <- cfa(model16, data = ds16, group = "S_GENDER", group.equal = c("loadings","intercepts","lv.variances"))
  inv.stri16 <- lavaan.survey(lavaan.fit = inv.stri16, survey.design = survey.design16)
  
  invarGNDR <- data.frame(round(rbind(Configural = fitMeasures(inv.conf16, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                  Metric = fitMeasures(inv.metr16, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                  Scalar = fitMeasures(inv.scal16, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea")),
                                  Strict = fitMeasures(inv.stri16, c("npar", "logl","chisq", "df", "tli", "cfi", "rmsea"))),3))
  invarGNDR <- invarGNDR %>% mutate(Invariance = rownames(invarGNDR)) %>% relocate(Invariance, .before = npar) %>% 
    mutate(D_tli = tli-lag(tli),
           D_cfi = cfi-lag(cfi),
           D_rmsea = rmsea-lag(rmsea)) %>% 
    knitr::kable() %>% print()
  cat('  \n')
  cat('  \n')
}

cat('  \n')
cat('  \n')
cat('## Summary indexes, all countries, all cycles')
cat('  \n')
cat('  \n')

pall <- rbind(p99, p09, p16)
library(CTT)
pall$Gend_Equal  <- score.transform(pall$Gend_Equal, mu.new = 50, sd.new = 10, normalize = FALSE)$new.scores
pall$Immi_Equal  <- score.transform(pall$Immi_Equal, mu.new = 50, sd.new = 10, normalize = FALSE)$new.scores
pall$Ethn_Equal  <- score.transform(pall$Ethn_Equal, mu.new = 50, sd.new = 10, normalize = FALSE)$new.scores
ISC_lv <- left_join(ISC, pall, by = c("cycle", "COUNTRY", "IDSTUD"))
save(ISC_lv, file = "Data_analysis/ICCSAll_lv.RData")

mg <- ISC %>% dplyr::select(cycle, Gend_Equal, Immi_Equal, Ethn_Equal) %>% group_by(cycle) %>% 
  summarise_at(c("Ethn_Equal","Gend_Equal", "Immi_Equal"), list(~ mean(., na.rm = TRUE))) %>% mutate(cycle = as.factor(cycle)) %>% data.frame()

s1 <- ISC %>% dplyr::select(cycle, COUNTRY, Gend_Equal) %>% na.omit() %>% 
  ggplot(aes(x = Gend_Equal, y = reorder(COUNTRY, desc(COUNTRY)), group = interaction(cycle,COUNTRY), fill = COUNTRY)) +
  geom_boxplot() +
  facet_grid(.~ cycle)+
  geom_vline(aes_string(xintercept = "Gend_Equal"), mg, linetype="dotted", size = 0.8) +
  ggtitle("Attitudes toward gender equality") +
  ylab("Distribution of Scores CFA") +
  xlab(paste0("Gend_Equal")) +
  scale_color_brewer(palette="Accent") +
  theme(legend.position = "none")
print(s1)
s2 <- ISC %>% dplyr::select(cycle, COUNTRY, Immi_Equal) %>% na.omit() %>% 
  ggplot(aes(x = Immi_Equal, y = reorder(COUNTRY, desc(COUNTRY)), group = interaction(cycle,COUNTRY), fill = COUNTRY)) +
  geom_boxplot() +
  facet_grid(.~ cycle)+
  geom_vline(aes_string(xintercept = "Immi_Equal"), mg, linetype="dotted", size = 0.8) +
  ggtitle("Attitudes toward equal rights for immigrants") +
  ylab("Distribution of Scores CFA") +
  xlab(paste0("Immi_Equal")) +
  scale_color_brewer(palette="Accent") +
  theme(legend.position = "none")
print(s2)
s3 <- ISC %>% dplyr::select(cycle, COUNTRY, Ethn_Equal) %>% na.omit() %>% 
  ggplot(aes(x = Ethn_Equal, y = reorder(COUNTRY, desc(COUNTRY)), group = interaction(cycle, COUNTRY), fill = COUNTRY)) +
  geom_boxplot() +
  facet_grid(.~ cycle) +
  geom_vline(aes_string(xintercept = "Ethn_Equal"), mg, linetype="dotted", size = 0.8) +
  ggtitle("Attitudes toward equal rights for all ethnic/racial groups") +
  ylab("Distribution of Scores CFA") +
  xlab(paste0("Ethn_Equal")) +
  scale_color_brewer(palette="Accent") +
  theme(legend.position = "none")
print(s3)
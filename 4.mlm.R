library(lme4)
library(lmerTest)
library(sjPlot)

#---European countries---
ds_ml <- ISC_lvR %>% filter(!COUNTRY %in% c(CNTne, CNT2cne)) %>% 
  dplyr::select(cycle, IDSCHOOL, COUNTRY, SENWGT, all_of(Indicfa), all_of(Man_cate), all_of(Man_cont))
knitr::kable(table(ds_ml$cycle,ds_ml$COUNTRY), caption = "European countries included in the analysis") %>% print()


Man_cate2 <- Man_cate[!grepl(paste0(c("T_HIGHEDFA", "T_HISCED"), collapse = "|"), Man_cate)]
Man_cont2 <- Man_cont[!grepl(paste0(c("T_NISB", "T_CITRESP"), collapse = "|"), Man_cont)]
formReg <- paste0(paste(Man_cate2, collapse = " + "), " + ", paste(Man_cont2, collapse = " + "))

Man_cate3 <- Man_cate2[!grepl(paste0(c("T_HIGHEDEXP", "T_RELIG", "T_PROTES1"), collapse = "|"), Man_cate2)]
Man_cont3 <- Man_cont2[!grepl(paste0(c("T_NISB", "T_HISEI", "T_PROTES", "T_CNTATT", "T_ELECPART", "T_LEGACT", "T_WIDEPART", "T_CITRESP"), collapse = "|"), Man_cont2)]
formReg1 <- paste0(paste(Man_cate3, collapse = " + "), " + ", paste(Man_cont3, collapse = " + "))

# attr(ds_ml$Ethn_Equal, "label") <- NULL
# attr(ds_ml$Gend_Equal, "label") <- NULL
# attr(ds_ml$Immi_Equal, "label") <- NULL
attr(ds_ml$Ethn_Equal, "class") <- NULL
attr(ds_ml$Gend_Equal, "class") <- NULL
attr(ds_ml$Immi_Equal, "class") <- NULL

#--- Nested in cycles----------
# -----------------------------#
##### 2 cycles 2009/2016########
# -----------------------------#
ds_ml0 <- ds_ml %>% filter(cycle %in% c("C2", "C3"))

  # Null model 
  L3 <- list()
  for(i in 1:length(Indicfa)){
    form <- as.formula(paste0(Indicfa[i],"~ (1|cycle) + (1|cycle:COUNTRY) + (1|cycle:COUNTRY:IDSCHOOL)"))
    L3[[i]] <- lmer(form, data=ds_ml0, weights=SENWGT, REML=FALSE)
  }
  t2cNull <- tab_model(L3, collapse.ci = TRUE, p.style = "stars") 
  
  # Model 1
  Lr3 <- list()
  for(i in 1:length(Indicfa)){
    form <- as.formula(paste0(Indicfa[i],"~ ", formReg, "+ (1|cycle) + (1|cycle:COUNTRY) + (1|cycle:COUNTRY:IDSCHOOL)"))
    Lr3[[i]] <- lmer(form, data=ds_ml0, weights=SENWGT, REML=FALSE)
  }
  t2cMod1 <- tab_model(Lr3, collapse.ci = TRUE, p.style = "stars")
  rm(L3, Lr3)
  
# -----------------------------#
###  3 cycles 1999/2009/2016 ###
# -----------------------------#

  # Null model 
  L3 <- list()
  for(i in 1:length(Indicfa)){
    form <- as.formula(paste0(Indicfa[i],"~ (1|cycle) + (1|cycle:COUNTRY) + (1|cycle:COUNTRY:IDSCHOOL)"))
    L3[[i]] <- lmer(form, data=ds_ml, weights=SENWGT, REML=FALSE)
  }
  t3cNull <- tab_model(L3, collapse.ci = TRUE, p.style = "stars") 
  
  # Model 1
  Lr3 <- list()
  for(i in 1:length(Indicfa)){
    form <- as.formula(paste0(Indicfa[i],"~ ", formReg1, "+ (1|cycle) + (1|cycle:COUNTRY) + (1|cycle:COUNTRY:IDSCHOOL)"))
    Lr3[[i]] <- lmer(form, data=ds_ml, weights=SENWGT, REML=FALSE)
  }
  t3cMod1 <- tab_model(Lr3, collapse.ci = TRUE, p.style = "stars")
  rm(L3, Lr3)
  # -----------------------------#
  ### By cycles 1999/2009/2016 ###
  # -----------------------------#
  # Null model 
  Lr2 <- list(GEND = list(), IMMI = list(), ETHN = list())
  trNull <- list(GEND = list(), IMMI = list(), ETHN = list())
  for(k in 1:length(Indicfa)){
    for(j in 1:3){
      if(j==1) form <- as.formula(paste0(Indicfa[k],"~ (1|COUNTRY) + (1|COUNTRY:IDSCHOOL)"))
      else form <- as.formula(paste0(Indicfa[k],"~ (1|COUNTRY) + (1|COUNTRY:IDSCHOOL)"))
      Lr2[[k]][[j]] <- lmer(form, data=ds_ml[ds_ml$cycle == paste0("C",j),], weights=SENWGT, REML=FALSE)
    }  
    trNull[[k]] <- tab_model(Lr2[[k]], dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                           collapse.ci = TRUE, p.style = "stars", title = sjlabelled::get_label(eval(parse(text=paste0("ISC_lvR$",Indicfa[k])))))
  }
  rm(Lr2)
  # Model 1
  Lr2 <- list(GEND = list(), IMMI = list(), ETHN = list())
  trMod1 <- list(GEND = list(), IMMI = list(), ETHN = list())
  for(k in 1:length(Indicfa)){
    for(j in 1:3){
      if(j==1) form <- as.formula(paste0(Indicfa[k],"~ ", formReg1, "+ (1|COUNTRY) + (1|COUNTRY:IDSCHOOL)"))
      else form <- as.formula(paste0(Indicfa[k],"~ ", formReg, "+ (1|COUNTRY) + (1|COUNTRY:IDSCHOOL)"))
      Lr2[[k]][[j]] <- lmer(form, data=ds_ml[ds_ml$cycle == paste0("C",j),], weights=SENWGT, REML=FALSE)
    }  
    trMod1[[k]] <- tab_model(Lr2[[k]], dv.labels = c("CIVED 1999", "ICCS 2009", "ICCS 2016"),
                                   collapse.ci = TRUE, p.style = "stars", title = sjlabelled::get_label(eval(parse(text=paste0("ISC_lvR$",Indicfa[k])))))
  }
  rm(Lr2)
 ###Non european countries
  
  ds_mlne <- ISC_lvR %>% filter(COUNTRY %in% c(CNTne, CNT2cne)) %>% 
    dplyr::select(cycle, IDSCHOOL, COUNTRY, SENWGT, all_of(Indicfa), all_of(Man_cate), all_of(Man_cont))
  knitr::kable(table(ds_mlne$cycle,ds_mlne$COUNTRY), caption = "Non-European countries included in the analysis") %>% print()
  
  
  Man_cate2 <- Man_cate[!grepl(paste0(c("T_HIGHEDFA", "T_HISCED"), collapse = "|"), Man_cate)]
  Man_cont2 <- Man_cont[!grepl(paste0(c("T_NISB", "T_CITRESP"), collapse = "|"), Man_cont)]
  formReg <- paste0(paste(Man_cate2, collapse = " + "), " + ", paste(Man_cont2, collapse = " + "))
  
  Man_cate3 <- Man_cate2[!grepl(paste0(c("T_HIGHEDEXP", "T_RELIG", "T_PROTES1"), collapse = "|"), Man_cate2)]
  Man_cont3 <- Man_cont2[!grepl(paste0(c("T_NISB", "T_HISEI", "T_PROTES", "T_CNTATT", "T_ELECPART", "T_LEGACT", "T_WIDEPART", "T_CITRESP"), collapse = "|"), Man_cont2)]
  formReg1 <- paste0(paste(Man_cate3, collapse = " + "), " + ", paste(Man_cont3, collapse = " + "))
  
  # attr(ds_mlne$Ethn_Equal, "label") <- NULL
  # attr(ds_mlne$Gend_Equal, "label") <- NULL
  # attr(ds_mlne$Immi_Equal, "label") <- NULL
  # 
  attr(ds_mlne$Ethn_Equal, "class") <- NULL
  attr(ds_mlne$Gend_Equal, "class") <- NULL
  attr(ds_mlne$Immi_Equal, "class") <- NULL
  
  #--- Nested in cycles----------
  # -----------------------------#
  ##### 2 cycles 2009/2016########
  # -----------------------------#
  ds_mlne0 <- ds_mlne %>% filter(cycle %in% c("C2", "C3")) 
  
  Indicfane <- Indicfa[!grepl("Immi_Equal", Indicfa)]
  # Null model 
  L3 <- list()
  for(i in 1:length(Indicfane)){
    form <- as.formula(paste0(Indicfane[i],"~ (1|cycle) + (1|cycle:COUNTRY) + (1|cycle:COUNTRY:IDSCHOOL)"))
    L3[[i]] <- lmer(form, data=ds_mlne0, weights=SENWGT, REML=FALSE)
  }
  Nt2cNull <- tab_model(L3, collapse.ci = TRUE, p.style = "stars") 
  
  # Model 1
  Lr3 <- list()
  for(i in 1:length(Indicfane)){
    form <- as.formula(paste0(Indicfane[i],"~ ", formReg, "+ (1|cycle) + (1|cycle:COUNTRY) + (1|cycle:COUNTRY:IDSCHOOL)"))
    Lr3[[i]] <- lmer(form, data=ds_mlne0, weights=SENWGT, REML=FALSE)
  }
  Nt2cMod1 <- tab_model(Lr3, collapse.ci = TRUE, p.style = "stars")
  rm(L3, Lr3)
  
  # -----------------------------#
  ###  3 cycles 1999/2009/2016 ###
  # -----------------------------#
  
  # Null model 
  L3 <- list()
  for(i in 1:length(Indicfane)){
    form <- as.formula(paste0(Indicfane[i],"~ (1|cycle) + (1|cycle:COUNTRY) + (1|cycle:COUNTRY:IDSCHOOL)"))
    L3[[i]] <- lmer(form, data=ds_mlne, weights=SENWGT, REML=FALSE)
  }
  Nt3cNull <- tab_model(L3, collapse.ci = TRUE, p.style = "stars") 
  
  # Model 1
  Lr3 <- list()
  for(i in 1:length(Indicfane)){
    form <- as.formula(paste0(Indicfane[i],"~ ", formReg1, "+ (1|cycle) + (1|cycle:COUNTRY) + (1|cycle:COUNTRY:IDSCHOOL)"))
    Lr3[[i]] <- lmer(form, data=ds_mlne, weights=SENWGT, REML=FALSE)
  }
  Nt3cMod1 <- tab_model(Lr3, collapse.ci = TRUE, p.style = "stars")
  rm(L3, Lr3)
  # -----------------------------#
  ### By cycles 1999/2009/2016 ###
  # -----------------------------#
  # Null model 
  Lr2 <- list(GEND = list(), IMMI = list(), ETHN = list())
  NtrNull <- list(GEND = list(), IMMI = list(), ETHN = list())
  for(k in 1:length(Indicfa)){
    if(Indicfa[k] == "Immi_Equal") {t <- 1:2; dv <- c("CIVED 1999", "ICCS 2009")}else {t <- 1:3; dv <-c("CIVED 1999", "ICCS 2009", "ICCS 2016")}
    for(j in t){
      form <- as.formula(paste0(Indicfa[k],"~ (1|COUNTRY) + (1|COUNTRY:IDSCHOOL)"))
      Lr2[[k]][[j]] <- lmer(form, data=ds_mlne[ds_mlne$cycle == paste0("C",j),], weights=SENWGT, REML=FALSE)
    }  
    NtrNull[[k]] <- tab_model(Lr2[[k]], dv.labels = dv,
                             collapse.ci = TRUE, p.style = "stars", title = sjlabelled::get_label(eval(parse(text=paste0("ISC_lvR$",Indicfa[k])))))
  }
  rm(Lr2)
  # Model 1
  Lr2 <- list(GEND = list(), IMMI = list(), ETHN = list())
  NtrMod1 <- list(GEND = list(), IMMI = list(), ETHN = list())
  for(k in 1:length(Indicfa)){
    if(Indicfa[k] == "Immi_Equal") {t <- 1:2; dv <- c("CIVED 1999", "ICCS 2009")}else {t <- 1:3; dv <-c("CIVED 1999", "ICCS 2009", "ICCS 2016")}
    for(j in t){
      if(j==1) form <- as.formula(paste0(Indicfa[k],"~ ", formReg1, "+ (1|COUNTRY) + (1|COUNTRY:IDSCHOOL)"))
      else form <- as.formula(paste0(Indicfa[k],"~ ", formReg, "+ (1|COUNTRY) + (1|COUNTRY:IDSCHOOL)"))
      Lr2[[k]][[j]] <- lmer(form, data=ds_mlne[ds_mlne$cycle == paste0("C",j),], weights=SENWGT, REML=FALSE)
    }  
    NtrMod1[[k]] <- tab_model(Lr2[[k]], dv.labels = dv,
                             collapse.ci = TRUE, p.style = "stars", title = sjlabelled::get_label(eval(parse(text=paste0("ISC_lvR$",Indicfa[k])))))
  }
  rm(Lr2)
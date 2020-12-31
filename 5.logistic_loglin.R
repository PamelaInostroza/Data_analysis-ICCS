library(survey)
library(jtools)

options(survey.lonely.psu="adjust")

#-------European ------

ds_ll0 <- ISC_lvR %>% filter(!COUNTRY %in% c(CNTne, CNT2cne)) %>% 
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

logl <- vector(mode = 'list', length = length(Scales))
Tlogl <- vector(mode = 'list', length = length(Scales))
for(i in 1:length(Scales)){
   if(Scales[i] == "T_ETHNEQ5") {t <- 2:3; dv <- c( "ICCS 2009", "ICCS 2016")}
   else {t <- 1:3; dv <- c("CIVED 1999", "ICCS 2009", "ICCS 2016")}
    for (j in t) {
      if(j==1) {
         form <- formC1
         Survey.design = survey.designC1
      } else if(j==2) {
         form <- form
         Survey.design = survey.designC2
      } else if(j==3) {
         form <- form
         Survey.design = survey.designC3
      }
      logl[[i]][[j]] <- svyglm(as.formula(paste("as.numeric(",Scales[i],") ~", form )), 
                          data = ds_ll0[ds_ll0$cycle == paste0("C",j),], family = poisson, design = Survey.design)  
    }
   if (Scales[i] == "T_ETHNEQ5") Tlogl[[i]] <- tab_model(logl[[i]][[2]],logl[[i]][[3]], dv.labels = dv,
                                                         collapse.ci = TRUE, p.style = "stars", title = sjlabelled::get_label(eval(parse(text=paste0("ds_ll0$",Scales[i])))))
   else Tlogl[[i]] <- tab_model(logl[[i]], dv.labels = dv,
                            collapse.ci = TRUE, p.style = "stars", title = sjlabelled::get_label(eval(parse(text=paste0("ds_ll0$",Scales[i])))))
   names(Tlogl)[[i]] <- Scales[i]
}
rm(logl)
###########################################################
#############     Ordinal regression  #####################
###########################################################

ordl <- vector(mode = 'list', length = length(Scales))
Tordl <- list()
for(i in 1:length(Scales)){
   if(Scales[i] == "T_ETHNEQ5") {t <- 2:3; dv <- c( "ICCS 2009", "ICCS 2016")}
   else {t <- 1:3; dv <- c("CIVED 1999", "ICCS 2009", "ICCS 2016")}
   for (j in t) {
      if(j==1) {
         form <- formC1
         Survey.design = survey.designC1
      } else if(j==2) {
         form <- form
         Survey.design = survey.designC2
      } else if(j==3) {
         form <- form
         Survey.design = survey.designC3
      }
      
      ordl[[i]][[j]] <- svyolr(as.formula(paste(Scales[i], "~", form )), design = Survey.design)  
   }
   if (Scales[i] == "T_ETHNEQ5") Tordl[[i]] <- tab_model(ordl[[i]][[2]],ordl[[i]][[3]], dv.labels = dv,
                                                         collapse.ci = TRUE, p.style = "stars", title = sjlabelled::get_label(eval(parse(text=paste0("ds_ll0$",Scales[i])))))
   else Tordl[[i]] <- tab_model(ordl[[i]], dv.labels = dv,
                           collapse.ci = TRUE, p.style = "stars", title = str_remove(sjlabelled::get_label(eval(parse(text=paste0("ds_ll0$",Scales[i])))), "Moving/|<|>|Rights and Responsibilities/|Roles women and men/"))
   names(Tordl)[[i]] <- Scales[i]
}
rm(ordl)
##############################################################
###################Logistic regression########################
##############################################################

logit <- vector(mode = 'list', length = length(Scalesb))
Tlogit <- list()
for(i in 1:length(Scalesb)){
   if(Scalesb[i] == "bT_ETHNEQ5") {t <- 2:3; dv <- c( "ICCS 2009", "ICCS 2016")}
   else {t <- 1:3; dv <- c("CIVED 1999", "ICCS 2009", "ICCS 2016")}
   for (j in t) {
      if(j==1) {
         form <- formC1
         Survey.design = survey.designC1
      } else if(j==2) {
         form <- form
         Survey.design = survey.designC2
      } else if(j==3) {
         form <- form
         Survey.design = survey.designC3
      }
      
      logit[[i]][[j]] <- svyglm(as.formula(paste(Scalesb[i], "~", form )),  
                               data = ds_ll0[ds_ll0$cycle == paste0("C",j),], family = binomial, design = Survey.design)  
   }
   if (Scalesb[i] == "bT_ETHNEQ5") Tlogit[[i]] <- tab_model(logit[[i]][[2]],logit[[i]][[3]], dv.labels = dv,
                                                         collapse.ci = TRUE, p.style = "stars", title = sjlabelled::get_label(eval(parse(text=paste0("ds_ll0$",Scales[i])))))
   else Tlogit[[i]] <- tab_model(logit[[i]], dv.labels = dv,
                           collapse.ci = TRUE, p.style = "stars", title = str_remove(sjlabelled::get_label(eval(parse(text=paste0("ds_ll0$",Scales[i])))), "Moving/|<|>|Rights and Responsibilities/|Roles women and men/"))
   names(Tlogit)[[i]] <- Scalesb[i]
}
rm(logit)
#-------Non European ------

ds_ll0ne <- ISC_lvR %>% filter(COUNTRY %in% c(CNTne, CNT2cne)) %>% 
   dplyr::select(all_of(Id), all_of(sampleID), all_of(Scales), all_of(Scalesb), all_of(Man_cate), all_of(Man_cont)) 

Man_cateSignf <- Man_cate[!grepl(paste0(c("T_HIGHEDFA", "T_HISCED"), collapse = "|"), Man_cate)]

#Selection of variables to be used in models for availability
Man_cateC2C3 <- Man_cateSignf[!grepl(paste0(c("T_HIGHEDFA", "T_HISCED"), collapse = "|"), Man_cateSignf)]
form <- paste0(paste(Man_cateC2C3, collapse = " + "))
#Special selection for cycle 1 for availability
Man_cateC1 <- Man_cateSignf[!grepl(paste0(c("T_HIGHEDEXP", "T_RELIG", "T_HISCED", "T_PROTES1"), collapse = "|"), Man_cateSignf)]
formC1 <- paste0(paste(Man_cateC1, collapse = " + "))
ScalesNe <- Scales
survey.designC1 <- svydesign(ids = ~IDCL, weights = ~SENWGT, data=ds_ll0ne[ds_ll0ne$cycle == "C1",], strata = ~IDJK, nest = TRUE)
survey.designC2 <- svydesign(ids = ~IDCL, weights = ~SENWGT, data=ds_ll0ne[ds_ll0ne$cycle == "C2",], strata = ~IDJK, nest = TRUE)
survey.designC3 <- svydesign(ids = ~IDCL, weights = ~SENWGT, data=ds_ll0ne[ds_ll0ne$cycle == "C3",], strata = ~IDJK, nest = TRUE)

###########################################################
#############Log linear regression / Poisson ###############
###########################################################

Nelogl <- vector(mode = 'list', length = length(ScalesNe))
NeTlogl <- list()
for(i in 1:length(ScalesNe)){
   if(ScalesNe[i] == "T_ETHNEQ5") {t <- 2:3; dv <- c( "ICCS 2009", "ICCS 2016")} else {
      t <- 1:3; dv <- c("CIVED 1999", "ICCS 2009", "ICCS 2016")
      if(grepl("T_IMMIEQ[1-9]",ScalesNe[i])) {t <- 1:2; dv <- c("CIVED 1999", "ICCS 2009")} else {t <- 1:3; dv <-c("CIVED 1999", "ICCS 2009", "ICCS 2016")}
   }
   for (j in t) {
      if(j==1) {
         form <- formC1
         Survey.design = survey.designC1
      } else if(j==2) {
         form <- form
         Survey.design = survey.designC2
      } else if(j==3) {
         form <- form
         Survey.design = survey.designC3
      }
      
      Nelogl[[i]][[j]] <- svyglm(as.formula(paste("as.numeric(",ScalesNe[i],") ~", form )), 
                               data = ds_ll0ne[ds_ll0ne$cycle == paste0("C",j),], family = poisson, design = Survey.design)  
   }
   if (ScalesNe[i] == "T_ETHNEQ5") NeTlogl[[i]] <- tab_model(Nelogl[[i]][[2]],Nelogl[[i]][[3]], dv.labels = dv,
                                                         collapse.ci = TRUE, p.style = "stars", title = sjlabelled::get_label(eval(parse(text=paste0("ds_ll0ne$",ScalesNe[i]))))) else NeTlogl[[i]] <- tab_model(Nelogl[[i]], dv.labels = dv,
                           collapse.ci = TRUE, p.style = "stars", title = sjlabelled::get_label(eval(parse(text=paste0("ds_ll0ne$",ScalesNe[i])))))
   names(NeTlogl)[[i]] <- ScalesNe[i]
}
rm(Nelogl)
###########################################################
#############     Ordinal regression  #####################
###########################################################

Neordl <- vector(mode = 'list', length = length(ScalesNe))
NeTordl <- list()
for(k in 1:length(ScalesNe)){
   if(ScalesNe[k] == "T_ETHNEQ5") {
      t <- 2:3 
      dv <- c( "ICCS 2009", "ICCS 2016")} else {
      t <- 1:3 
      dv <- c("CIVED 1999", "ICCS 2009", "ICCS 2016")
      if(grepl("T_IMMIEQ[1-9]",ScalesNe[k])) {t <- 1:2; dv <- c("CIVED 1999", "ICCS 2009")} else {
         t <- 1:3; dv <-c("CIVED 1999", "ICCS 2009", "ICCS 2016")}
      }
   for (j in t) {
      if(j==1) {
         form <- formC1
         Survey.design = survey.designC1
      } else if(j==2) {
         form <- form
         Survey.design = survey.designC2
      } else if(j==3) {
         form <- form
         Survey.design = survey.designC3
      }
      
      Neordl[[k]][[j]] <- svyolr(as.formula(paste(ScalesNe[k], "~", form )), design = Survey.design)  
   }
   if (ScalesNe[k] == "T_ETHNEQ5") NeTordl[[k]] <- tab_model(Neordl[[k]][[2]],Neordl[[k]][[3]], dv.labels = dv,
                                                           collapse.ci = TRUE, p.style = "stars", title = sjlabelled::get_label(eval(parse(text=paste0("ds_ll0ne$",ScalesNe[k])))))
   else NeTordl[[k]] <- tab_model(Neordl[[k]], dv.labels = dv,
                           collapse.ci = TRUE, p.style = "stars", title = str_remove(sjlabelled::get_label(eval(parse(text=paste0("ds_ll0ne$",ScalesNe[k])))), "Moving/|<|>|Rights and Responsibilities/|Roles women and men/"))
   names(NeTordl)[[k]] <- ScalesNe[k]
}
rm(Neordl)
##############################################################
###################Logistic regression########################
##############################################################
ScalesNeb <- Scalesb
Nelogit <- vector(mode = 'list', length = length(ScalesNeb))
NeTlogit <- list()
for(i in 1:length(ScalesNeb)){
   if(ScalesNeb[i] == "bT_ETHNEQ5") {t <- 2:3; dv <- c( "ICCS 2009", "ICCS 2016")}
   else {t <- 1:3; dv <- c("CIVED 1999", "ICCS 2009", "ICCS 2016")
   if(grepl("bT_IMMIEQ[1-9]",ScalesNeb[i])) {t <- 1:2; dv <- c("CIVED 1999", "ICCS 2009")}else {t <- 1:3; dv <-c("CIVED 1999", "ICCS 2009", "ICCS 2016")}
   }
   for (j in t) {
      if(j==1) {
         form <- formC1
         Survey.design = survey.designC1
      } else if(j==2) {
         form <- form
         Survey.design = survey.designC2
      } else if(j==3) {
         form <- form
         Survey.design = survey.designC3
      }
      
      Nelogit[[i]][[j]] <- svyglm(as.formula(paste(ScalesNeb[i], "~", form )),  
                                data = ds_ll0ne[ds_ll0ne$cycle == paste0("C",j),], family = binomial, design = Survey.design)  
   }
   if (ScalesNeb[i] == "bT_ETHNEQ5") NeTlogit[[i]] <- tab_model(Nelogit[[i]][[2]],Nelogit[[i]][[3]], dv.labels = dv,
                                                             collapse.ci = TRUE, p.style = "stars", title = sjlabelled::get_label(eval(parse(text=paste0("ds_ll0ne$",ScalesNeb[i])))))
   else NeTlogit[[i]] <- tab_model(Nelogit[[i]], dv.labels = dv,
                            collapse.ci = TRUE, p.style = "stars", title = str_remove(sjlabelled::get_label(eval(parse(text=paste0("ds_ll0ne$",ScalesNeb[i])))), "Moving/|<|>|Rights and Responsibilities/|Roles women and men/"))
   names(NeTlogit)[[i]] <- ScalesNeb[i]
}
rm(Nelogit)
#--------
##############################################################
################Multinomial regression########################
##############################################################

# library(nnet)

# multl <- vector(mode = 'list', length = length(Scales))
# Tmultl <- list()
# for(i in 1:length(Scales)){
#    if(Scales[i] == "T_ETHNEQ5") {t <- 2:3; dv <- c( "ICCS 2009", "ICCS 2016")}
#    else {t <- 1:3; dv <- c("CIVED 1999", "ICCS 2009", "ICCS 2016")}
#    for (j in t) {
#       if(j==1) {
#          form <- formC1
#          Survey.design = survey.designC1
#       } else if(j==2) {
#          form <- form
#          Survey.design = survey.designC2
#       } else if(j==3) {
#          form <- form
#          Survey.design = survey.designC3
#       }
#       
#       multl[[i]][[j]] <- multinom(as.formula(paste(Scales[i], "~", form )), data = ds_ll0[ds_ll0$cycle == paste0("C",j),], design = Survey.design)  
#    }
#    if (Scales[i] == "T_ETHNEQ5") Tmultl[[i]] <- tab_model(multl[[i]][[2]],multl[[i]][[3]], dv.labels = dv,
#                                                          collapse.ci = TRUE, p.style = "stars", title = sjlabelled::get_label(eval(parse(text=paste0("ds_ll0$",Scales[i])))))
#    else Tmultl[[i]] <- tab_model(multl[[i]][[i]], dv.labels = dv,
#                                 collapse.ci = TRUE, p.style = "stars", title = str_remove(sjlabelled::get_label(eval(parse(text=paste0("ds_ll0$",Scales[i])))), "Moving/|<|>|Rights and Responsibilities/|Roles women and men/"))
#    names(Tmultl)[[i]] <- Scales[i]
# }
# rm(multl)
# 
# 
# des2<-as.svrepdesign(survey.designC3, type="bootstrap" , replicates=10)
# mfit<-withReplicates(des2, quote(coef(multinom(as.formula(paste(Scales[i], "~", form )), weights=.weights, trace=F ))))
# mfitcoef<-data.frame(matrix(attr(attr(mfit, "var"), "means")[-1:-4], nrow=4, ncol=length(Man_cateC2C3), byrow=F))
# names(mfitcoef)<-names(coef(ex1)[-1])
# round(exp(mfitcoef), 3) # odds ratios

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
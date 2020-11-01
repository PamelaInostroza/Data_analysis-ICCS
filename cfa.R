library(lavaan)
library(lavaan.survey)
library(semPlot)
constr_name <- VarsToUse %>% filter(Construct == constr[2]) %>%  dplyr::select(VariableC1) %>% na.omit() %>% pull()

#1st order 
model2<-'
GendEqual =~ BS4G4 + BS4G6 + BS4G9 + BS4G11 + BS4G13
'

ds <- ICCS %>% dplyr::select(constr_name, IDSTUD, TOTWGT, COUNTRY) %>% na.omit()  
survey.design <- svydesign(ids=~IDSTUD, prob=~TOTWGT, data=ds)

lavaan.fit2 <- lavaan(model2, data = ds, auto.fix.first = TRUE,
                      auto.var = TRUE, int.ov.free = TRUE,
                      auto.cov.lv.x = TRUE, estimator = "MLM",
                      cluster = "COUNTRY", meanstructure = TRUE)

survey.fit2 <- lavaan.survey(lavaan.fit = lavaan.fit2, survey.design = survey.design)
print(fitMeasures(survey.fit2, c("chisq","pvalue","cfi", "tli","rmsea", "srmr", "chisq.scaled","pvalue.scaled","cfi.robust","tli.robust","rmsea.robust","srmr_bentler")))
print(modindices(survey.fit2,sort=T)[1:10,])
invisible(semPaths(survey.fit2,"model","std","lisrel", edge.label.cex = 0.8, intercepts = FALSE, groups = "latent", 
                   pastel = TRUE, optimizeLatRes = TRUE))


inv.scalfit2 <- lavaan(model2, data = ds, 
                          auto.fix.first = TRUE,  #factor loading of first indicator set to 1
                          int.ov.free = TRUE,     #intercepts not fixed to 0
                          meanstructure = TRUE,   #the means of the observed variables enter the model, not what user define
                          auto.var = TRUE,        #residual variances and variances of exogeneous latent variables are included in the model and set free
                          auto.cov.lv.x = TRUE,   #covariances of exogeneous latent variables are included in the model and set free
                          estimator = "MLM",
                          group = "COUNTRY",
                          group.label = c("CHL", "ITA"),
                          group.equal = c("loadings","intercepts"))#, group.partial= c("iplylfr_r ~1","ipeqopt_r~1"))
scalfit2 <- lavaan.survey(lavaan.fit = inv.scalfit2, survey.design = survey.design)
print(fitMeasures(scalfit2, c("chisq","pvalue","cfi", "tli","rmsea", "srmr", "chisq.scaled","pvalue.scaled","cfi.robust","tli.robust","rmsea.robust","srmr_bentler")))
invisible(semPaths(scalfit2, "model", "std", "lisrel", edge.label.cex = 1.2, intercepts = FALSE, layout = "tree2",
                   panelGroups = FALSE, ask = FALSE, groups = "latent", pastel = TRUE, exoCov = TRUE, rotation = 1))
mi2 <- modindices(scalfit2, sort = T, free.remove = F)
mi2[mi2$op == "~1",]

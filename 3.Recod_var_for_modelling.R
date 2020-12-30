library(gridExtra)
library(grid)

library(expss)

#Sample variables
ISC_rec$SENWGT <- ifelse(!is.na(ISC_rec$SENWGT_Gc1), ISC_rec$SENWGT_Gc1,
                         ifelse(!is.na(ISC_rec$SENWGT_Gc2), ISC_rec$SENWGT_Gc2,
                                ifelse(!is.na(ISC_rec$SENWGT_Gc3), ISC_rec$SENWGT_Gc3, NA)))
ISC_rec$TOTWGT <- ifelse(!is.na(ISC_rec$TOTWGT_Gc1), ISC_rec$TOTWGT_Gc1,
                         ifelse(!is.na(ISC_rec$TOTWGT_Gc2), ISC_rec$TOTWGT_Gc2,
                                ifelse(!is.na(ISC_rec$TOTWGT_Gc3), ISC_rec$TOTWGT_Gc3, NA)))


#########Continuous variables#################
ISC_rec$T_CONVCITI <- ifelse(!is.na(ISC_rec$CTCONMLE), ISC_rec$CTCONMLE,
                             ifelse(!is.na(ISC_rec$CITCON), ISC_rec$CITCON,
                                    ifelse(!is.na(ISC_rec$S_CITCON), ISC_rec$S_CITCON, NA)))

ISC_rec$T_SOCIMOVE <- ifelse(!is.na(ISC_rec$CTSOCMLE), ISC_rec$CTSOCMLE,
                             ifelse(!is.na(ISC_rec$CITSOC), ISC_rec$CITSOC,
                                    ifelse(!is.na(ISC_rec$S_CITSOC), ISC_rec$S_CITSOC, NA)))

ISC_rec$T_TRUST <- ifelse(!is.na(ISC_rec$TRUSTMLE), ISC_rec$TRUSTMLE,
                          ifelse(!is.na(ISC_rec$INTRUST), ISC_rec$INTRUST,
                                 ifelse(!is.na(ISC_rec$S_INTRUST), ISC_rec$S_INTRUST, NA)))

ISC_rec$T_POLIPART <- ifelse(!is.na(ISC_rec$POLATMLE), ISC_rec$POLATMLE,
                             ifelse(!is.na(ISC_rec$POLPART), ISC_rec$POLPART,
                                    ifelse(!is.na(ISC_rec$S_POLPART), ISC_rec$S_POLPART, NA)))

ISC_rec$T_SCHPART <- ifelse(!is.na(ISC_rec$CONFSMLE), ISC_rec$CONFSMLE,
                            ifelse(!is.na(ISC_rec$PARTSCHL), ISC_rec$PARTSCHL,
                                   ifelse(!is.na(ISC_rec$S_SCHPART), ISC_rec$S_SCHPART, NA)))

ISC_rec$T_TRUST <- ifelse(!is.na(ISC_rec$TRUSTMLE), ISC_rec$TRUSTMLE,
                          ifelse(!is.na(ISC_rec$INTRUST), ISC_rec$INTRUST,
                                 ifelse(!is.na(ISC_rec$S_INTRUST), ISC_rec$S_INTRUST, NA)))
#only 2 cycles
ISC_rec$T_NISB <- ifelse(!is.na(ISC_rec$NISB), ISC_rec$NISB,
                         ifelse(!is.na(ISC_rec$S_NISB), ISC_rec$S_NISB, NA))
ISC_rec$T_PROTES <- ifelse(!is.na(ISC_rec$ILLPROT), ISC_rec$ILLPROT,
                           ifelse(!is.na(ISC_rec$S_ILLACT), ISC_rec$S_ILLACT, NA))
ISC_rec$T_HISEI = ifelse(!is.na(ISC_rec$HISEI), ISC_rec$HISEI,
                         ifelse(!is.na(ISC_rec$S_HISEI), ISC_rec$S_HISEI, NA))
ISC_rec$T_CNTATT = ifelse(!is.na(ISC_rec$ATTCNT), ISC_rec$ATTCNT,
                          ifelse(!is.na(ISC_rec$S_CNTATT), ISC_rec$S_CNTATT, NA))
ISC_rec$T_ELECPART = ifelse(!is.na(ISC_rec$ELECPART), ISC_rec$ELECPART,
                            ifelse(!is.na(ISC_rec$S_ELECPART), ISC_rec$S_ELECPART, NA))
ISC_rec$T_LEGACT = ifelse(!is.na(ISC_rec$LEGPROT), ISC_rec$LEGPROT,
                          ifelse(!is.na(ISC_rec$S_LEGACT), ISC_rec$S_LEGACT, NA))
ISC_rec$T_WIDEPART = ifelse(!is.na(ISC_rec$PARTCOM), ISC_rec$PARTCOM,
                            ifelse(!is.na(ISC_rec$S_COMPART), ISC_rec$S_COMPART, NA))

ISC_rec$T_CITRESP = ISC_rec$S_CITRESP*1

#Age
ISC_rec$T_AGE = ifelse(!is.na(ISC_rec$AGE), ISC_rec$AGE,
                       ifelse(!is.na(ISC_rec$SAGE), ISC_rec$SAGE,
                              ifelse(!is.na(ISC_rec$S_AGE), ISC_rec$S_AGE, NA)))
var_lab(ISC_rec$T_AGE) <- var_lab(ISC_rec$S_AGE)

ISC_rec1 <- ISC_rec #for plots of original data
attr(ISC_rec$Ethn_Equal, "class") <- NULL
attr(ISC_rec$Gend_Equal, "class") <- NULL
attr(ISC_rec$Immi_Equal, "class") <- NULL

#Standardization of continuous variables
Man_cont <- c(VarsToUse %>%  filter(Domain %in% "Contextual" & Dataset == "ISG") %>% select(VariableName) %>% na.omit() %>% pull())
contvars <- c(Indicfa, Man_cont)
ISC_rec <- ISC_rec %>% group_by(cycle) %>%
  mutate(across(all_of(contvars[!grepl(paste0(c("T_AGE"), collapse = "|"), contvars)]),
                function(x) {(x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))*100}))

##Missing labels
var_lab(ISC_rec$Gend_Equal) <- "Positive attitudes toward gender equality"
var_lab(ISC_rec$Immi_Equal) <- "Positive attitudes toward equal rights for immigrants"
var_lab(ISC_rec$Ethn_Equal) <- "Positive attitudes toward equal rights for ethnic/racial groups"

var_lab(ISC_rec$CTCONMLE) <- "IMPORTANCE OF CONVENTIONAL CITIZENSHIP"
var_lab(ISC_rec$CTSOCMLE) <- "IMPORTANCE OF SOCIAL-MOVEMENT-RELATED CITIZENSHIP"
var_lab(ISC_rec$TRUSTMLE) <- "TRUST IN GOVERNMENT RELATED INSTITUTIONS"
var_lab(ISC_rec$POLATMLE) <- "POLITICAL ACTIVITIES"
var_lab(ISC_rec$CONFSMLE) <- "CONFIDENCE IN PARTICIPATING AT SCHOOL"

var_lab(ISC_rec$T_HISEI) <- var_lab(ISC_rec$S_HISEI)
var_lab(ISC_rec$T_CNTATT) <- str_remove(var_lab(ISC_rec$S_CNTATT), " - WLE")
var_lab(ISC_rec$T_CONVCITI) <- str_remove(var_lab(ISC_rec$S_CITCON), " - WLE")
var_lab(ISC_rec$T_CITRESP) <- str_remove(var_lab(ISC_rec$S_CITRESP), " - WLE")
var_lab(ISC_rec$T_SOCIMOVE) <- str_remove(var_lab(ISC_rec$S_CITSOC), " - WLE")
var_lab(ISC_rec$T_ELECPART) <- str_remove(var_lab(ISC_rec$S_ELECPART), " - WLE")
var_lab(ISC_rec$T_PROTES) <- str_remove(var_lab(ISC_rec$S_ILLACT), " - WLE")
var_lab(ISC_rec$T_TRUST) <- str_remove(var_lab(ISC_rec$S_INTRUST), " - WLE")
var_lab(ISC_rec$T_LEGACT) <- str_remove(var_lab(ISC_rec$S_LEGACT), " - WLE")
var_lab(ISC_rec$T_POLIPART) <- str_remove(var_lab(ISC_rec$S_POLPART), " - WLE")
var_lab(ISC_rec$T_WIDEPART) <- str_remove(var_lab(ISC_rec$S_COMPART), " - WLE")
var_lab(ISC_rec$T_SCHPART) <- str_remove(var_lab(ISC_rec$S_SCHPART), " - WLE")
var_lab(ISC_rec$T_NISB) <- var_lab(ISC_rec$S_NISB)


#by(ISC_rec[,contvars], ISC_rec[,"cycle"],summary)

############Categorical variables######################
#Recodification of categorical variables
cattorecd <- VarsToUse %>%  filter(Domain %in% "Background questionnaires" & Dataset == "ISG" &
                                     (!grepl("*[1-9]$", VariableName) | grepl("^T_PROTEST*", VariableName))) %>% #select only new variables that are created
  select(VariableC1, VariableC2, VariableC3, VariableName)
Man_cate <- cattorecd %>% select(VariableName) %>% pull()

#Homogenization of categories of Education parents
# ISC_rec$BSGYFEDr <- factor(ifelse(ISC_rec$BSGYFED == 1, 4,
#                                   ifelse(ISC_rec$BSGYFED == 2, 4,
#                                          ifelse(ISC_rec$BSGYFED == 3, 3,
#                                                 ifelse(ISC_rec$BSGYFED == 4, 3,
#                                                        ifelse(ISC_rec$BSGYFED == 5, 2,
#                                                               ifelse(ISC_rec$BSGYFED == 6, 1,
#                                                                      ifelse(ISC_rec$BSGYFED == 7, 1, NA))))))), level = c(1,2,3,4),
#                            labels = c("ISCED level 5A, 6, 7 or 8", "ISCED level 4 or 5(B)", "ISCED level 3", "ISCED level 2 or below"))
ISC_rec$IS2G03r <- factor(ifelse(ISC_rec$IS2G03 %in% c(5,4), 1,
                                 ifelse(ISC_rec$IS2G03 == 3, 2,
                                        ifelse(ISC_rec$IS2G03 == 2, 3,
                                               ifelse(ISC_rec$IS2G03 == 1, 4, NA)))), levels = c(1,2,3,4),
                          labels = c("ISCED level 2 or below", "ISCED level 3", "ISCED level 4 or 5(B)", "ISCED level 5A, 6, 7 or 8"))
ISC_rec$IS3G03r <- factor(ifelse(ISC_rec$IS3G03 == 4, 1,
                                 ifelse(ISC_rec$IS3G03 == 3, 2,
                                        ifelse(ISC_rec$IS3G03 == 2, 3,
                                               ifelse(ISC_rec$IS3G03 == 1, 4, NA)))), levels = c(1,2,3,4),
                          labels = c("ISCED level 5A, 6, 7 or 8", "ISCED level 4 or 5(B)", "ISCED level 3", "ISCED level 2 or below"))

ISC_rec$T_HIGHEDEXP = factor(ifelse(!is.na(ISC_rec$IS2G03r), ISC_rec$IS2G03r,
                                    ifelse(!is.na(ISC_rec$IS3G03r), ISC_rec$IS3G03r, NA)),
                             labels = c("Primary or below", "Secondary", "Technical", "Bachelor or higher"))
var_lab(ISC_rec$T_HIGHEDEXP) <- "Highest level of education expected"

ISC_rec$HISCEDr <- factor(ifelse(ISC_rec$HISCED %in% c(0, 1), 1,
                                 ifelse(ISC_rec$HISCED == 2, 2,
                                        ifelse(ISC_rec$HISCED == 3, 3,
                                               ifelse(ISC_rec$HISCED == 4, 4,
                                                      ifelse(ISC_rec$HISCED == 5, 5,ISC_rec$HISCED))))), levels = c(1,2,3,4,5),
                          labels = c("Not complete ISCED level 2", "ISCED level 2", "ISCED level 3", "ISCED level 4 or 5", "ISCED level 6,7 or 8"))
ISC_rec$S_HISCEDr <- factor(ifelse(ISC_rec$S_HISCED == 0, 1,
                                   ifelse(ISC_rec$S_HISCED == 1, 2,
                                          ifelse(ISC_rec$S_HISCED == 2, 3,
                                                 ifelse(ISC_rec$S_HISCED == 3, 4,
                                                        ifelse(ISC_rec$S_HISCED == 4, 5,ISC_rec$S_HISCED))))), levels = c(1,2,3,4,5),
                            labels = c("Not complete ISCED level 2", "ISCED level 2", "ISCED level 3", "ISCED level 4 or 5", "ISCED level 6,7 or 8"))

ISC_rec$T_HISCED = factor(ifelse(!is.na(ISC_rec$HISCEDr), ISC_rec$HISCEDr,
                                 ifelse(!is.na(ISC_rec$S_HISCEDr), ISC_rec$S_HISCEDr, NA)),
                          labels = c("Not complete Primary", "Primary", "Secondary", "Technical", "Bachelor or higher"))
var_lab(ISC_rec$T_HISCED) <- var_lab(ISC_rec$S_HISCED)

#Homogenization of categories of Education mother
ISC_rec$BSGEDUMr <- factor(ifelse(ISC_rec$BSGEDUM == 0, NA,
                                  ifelse(ISC_rec$BSGEDUM == 1, 1,
                                         ifelse(ISC_rec$BSGEDUM == 2, 2,
                                                ifelse(ISC_rec$BSGEDUM == 3, 3,
                                                       ifelse(ISC_rec$BSGEDUM == 4, 3,
                                                              ifelse(ISC_rec$BSGEDUM == 5, 4,
                                                                     ifelse(ISC_rec$BSGEDUM == 6, 4,
                                                                            ifelse(ISC_rec$BSGEDUM == 7, 5, NA)))))))), level = c(1,2,3,4,5),
                           labels = c("Not complete ISCED level 2", "ISCED level 2", "ISCED level 3", "ISCED level 4 or 5", "ISCED level 6, 7 or 8"))
ISC_rec$IS2G07r <- factor(ifelse(ISC_rec$IS2G07 %in% c(5,6), 1,
                                 ifelse(ISC_rec$IS2G07 == 4, 2,
                                        ifelse(ISC_rec$IS2G07 == 3, 3,
                                               ifelse(ISC_rec$IS2G07 == 2, 4,
                                                      ifelse(ISC_rec$IS2G07 == 1, 5, ISC_rec$IS2G07))))),
                          levels = c(1,2,3,4,5),
                          labels = c("Not complete ISCED level 2", "ISCED level 2", "ISCED level 3", "ISCED level 4 or 5", "ISCED level 6, 7 or 8"))
ISC_rec$IS3G07r <- factor(ifelse(ISC_rec$IS3G07 == 5, 1,
                                 ifelse(ISC_rec$IS3G07 == 4, 2,
                                        ifelse(ISC_rec$IS3G07 == 3, 3,
                                               ifelse(ISC_rec$IS3G07 == 2, 4,
                                                      ifelse(ISC_rec$IS3G07 == 1, 5, ISC_rec$IS3G07))))),
                          levels = c(1,2,3,4,5),
                          labels = c("Not complete ISCED level 2", "ISCED level 2", "ISCED level 3", "ISCED level 4 or 5", "ISCED level 6, 7 or 8"))
ISC_rec$T_HIGHEDMO = factor(ifelse(!is.na(ISC_rec$BSGEDUMr), ISC_rec$BSGEDUMr,
                                   ifelse(!is.na(ISC_rec$IS2G07r), ISC_rec$IS2G07r,
                                          ifelse(!is.na(ISC_rec$IS3G07r), ISC_rec$IS3G07r, NA))),
                            labels = c("Not complete Primary", "Primary", "Secondary", "Technical", "Bachelor or higher"))
var_lab(ISC_rec$T_HIGHEDMO) <- "Highest level of education completed - mother"

#Homogenization of categories of Education father
ISC_rec$BSGEDUFr <- factor(ifelse(ISC_rec$BSGEDUF == 0, NA,
                                  ifelse(ISC_rec$BSGEDUF == 1, 1,
                                         ifelse(ISC_rec$BSGEDUF == 2, 2,
                                                ifelse(ISC_rec$BSGEDUF == 3, 3,
                                                       ifelse(ISC_rec$BSGEDUF == 4, 3,
                                                              ifelse(ISC_rec$BSGEDUF == 5, 4,
                                                                     ifelse(ISC_rec$BSGEDUF == 6, 4,
                                                                            ifelse(ISC_rec$BSGEDUF == 7, 5, NA)))))))), level = c(1,2,3,4,5),
                           labels = c("Not complete ISCED level 2", "ISCED level 2", "ISCED level 3", "ISCED level 4 or 5", "ISCED level 6, 7 or 8"))
ISC_rec$IS2G09r <- factor(ifelse(ISC_rec$IS2G09 %in% c(5,6), 1,
                                 ifelse(ISC_rec$IS2G09 == 4, 2,
                                        ifelse(ISC_rec$IS2G09 == 3, 3,
                                               ifelse(ISC_rec$IS2G09 == 2, 4,
                                                      ifelse(ISC_rec$IS2G09 == 1, 5, ISC_rec$IS2G09))))),
                          levels = c(1,2,3,4,5),
                          labels = c("Not complete ISCED level 2", "ISCED level 2", "ISCED level 3", "ISCED level 4 or 5", "ISCED level 6, 7 or 8"))
ISC_rec$IS3G09r <- factor(ifelse(ISC_rec$IS3G09 == 5, 1,
                                 ifelse(ISC_rec$IS3G09 == 4, 2,
                                        ifelse(ISC_rec$IS3G09 == 3, 3,
                                               ifelse(ISC_rec$IS3G09 == 2, 4,
                                                      ifelse(ISC_rec$IS3G09 == 1, 5, ISC_rec$IS3G09))))),
                          levels = c(1,2,3,4,5),
                          labels = c("Not complete ISCED level 2", "ISCED level 2", "ISCED level 3", "ISCED level 4 or 5", "ISCED level 6, 7 or 8"))
ISC_rec$T_HIGHEDFA = factor(ifelse(!is.na(ISC_rec$BSGEDUFr), ISC_rec$BSGEDUFr,
                                   ifelse(!is.na(ISC_rec$IS2G09r), ISC_rec$IS2G09r,
                                          ifelse(!is.na(ISC_rec$IS3G09r), ISC_rec$IS3G09r, NA))),
                            labels = c("Not complete Primary", "Primary", "Secondary", "Technical", "Bachelor or higher"))
var_lab(ISC_rec$T_HIGHEDFA) <- "Highest level of education completed - father"

#Homogenization of categories of Religion
ISC_rec$IS2P34r <- factor(ISC_rec$IS2P34, levels = c(0,1), labels = c("No Religion", "Religion"))
ISC_rec$IS3G33r <- factor(ISC_rec$IS3G33, levels = c(0,1), labels = c("No Religion", "Religion"))

ISC_rec$T_RELIG = factor(ifelse(!is.na(ISC_rec$IS2P34r), ISC_rec$IS2P34r,
                                ifelse(!is.na(ISC_rec$IS3G33r), ISC_rec$IS3G33r, NA)),
                         labels = c("No Religion", "Religion"))
var_lab(ISC_rec$T_RELIG) <- "Student religious affiliation"

#Homogenization of categories of Gender
ISC_rec$T_GENDER = factor(ifelse(!is.na(ISC_rec$GENDER), ISC_rec$GENDER,
                                 ifelse(!is.na(ISC_rec$SGENDER), ISC_rec$SGENDER,
                                        ifelse(!is.na(ISC_rec$S_GENDER), ISC_rec$S_GENDER, NA))),
                          labels = c("Boy", "Girl"))
var_lab(ISC_rec$T_GENDER) <- var_lab(ISC_rec$S_GENDER)

#Homogenization of categories of Home literacy
ISC_rec$HOMELITr <- factor(ifelse(ISC_rec$HOMELIT == 1, 0,
                                  ifelse(ISC_rec$HOMELIT == 2, 1,
                                         ifelse(ISC_rec$HOMELIT == 3, 2,
                                                ifelse(ISC_rec$HOMELIT == 4, 3,
                                                       ifelse(ISC_rec$HOMELIT == 5, 4, NA))))), level = c(0,1,2,3,4),
                           labels = c("Less than 10", "Between 11 and 25", "Between 26 and 100", "Between 101 and 200", "More than 200"))
ISC_rec$SHOMELITr <- factor(ifelse(ISC_rec$SHOMELIT == 5, 4, ISC_rec$SHOMELIT), levels = c(0,1,2,3,4),
                            labels = c("Less than 10", "Between 11 and 25", "Between 26 and 100", "Between 101 and 200", "More than 200"))
ISC_rec$S_HOMLITr <- factor(ISC_rec$S_HOMLIT, levels = c(0,1,2,3,4),
                            labels = c("Less than 10", "Between 11 and 25", "Between 26 and 100", "Between 101 and 200", "More than 200"))

ISC_rec$T_HOMELIT = factor(ifelse(!is.na(ISC_rec$HOMELITr), ISC_rec$HOMELITr,
                                  ifelse(!is.na(ISC_rec$SHOMELITr), ISC_rec$SHOMELITr,
                                         ifelse(!is.na(ISC_rec$S_HOMLITr), ISC_rec$S_HOMLITr, NA))),
                           labels = c("Less than 10 books", "Between 11 and 25 books", "Between 26 and 100 books", "Between 101 and 200 books", "More than 200 books"))
var_lab(ISC_rec$T_HOMELIT) <- var_lab(ISC_rec$S_HOMLIT)

#Homogenization of categories of Immigration status
ISC_rec$IMMIGr <- factor(ifelse(ISC_rec$BSGBRN1 == 1, 1, ifelse(ISC_rec$BSGBRN1 == 2, 0, NA)), levels = c(0,1), labels = c("Immigrant", "Native"))
ISC_rec$SIMMIGr <- factor(ifelse(ISC_rec$IMMIG == 3, 1, 0), levels = c(0,1), labels = c("Immigrant", "Native"))
ISC_rec$S_IMMIGr <- factor(ifelse(ISC_rec$S_IMMIG == 3, 1, 0), levels = c(0,1), labels = c("Immigrant", "Native"))

ISC_rec$T_IMMGR = factor(ifelse(!is.na(ISC_rec$IMMIGr), ISC_rec$IMMIGr,
                                ifelse(!is.na(ISC_rec$SIMMIGr), ISC_rec$SIMMIGr,
                                       ifelse(!is.na(ISC_rec$S_IMMIGr), ISC_rec$S_IMMIGr, NA))),
                         labels = c("Immigrant", "Native"))
var_lab(ISC_rec$T_IMMGR) <- var_lab(ISC_rec$S_IMMIG)

#-------------
#Background questionnaire categorical scales
# VarsToUse %>%
#   filter(Domain %in% "Background questionnaires" & Construct == "students’ expected participation in future illegal activities")
# sjlabelled::get_label(ISC_rec$BS5M11)
# sjlabelled::get_labels(ISC_rec$BS5M11)
# table(sjlabelled::as_label(ISC_rec$BS5M11))
# sjlabelled::get_label(ISC_rec$IS2P31H)
# table(sjlabelled::as_label(ISC_rec$IS2P31H))
# sjlabelled::get_labels(ISC_rec$IS2P31H)
# sjlabelled::get_label(ISC_rec$IS3G30J)
# table(sjlabelled::as_label(ISC_rec$IS3G30J))
# sjlabelled::get_labels(ISC_rec$IS3G30J)

vartorec <- c(VarsToUse %>% 
                filter(Domain %in% "Background questionnaires" & Construct == "students’ expected participation in future illegal activities") %>% 
                select(VariableC2) %>%  na.omit() %>% pull(),
              VarsToUse %>% 
                filter(Domain %in% "Background questionnaires" & Construct == "students’ expected participation in future illegal activities") %>% 
                select(VariableC3) %>%  na.omit() %>% pull())
recoded <- data.frame(psych::reverse.code(keys = rep(-1,length(vartorec)), 
                                          items = ISC_rec[vartorec], 
                                          mini = rep(1,length(vartorec)), 
                                          maxi = rep(4,length(vartorec))))
colnames(recoded) <- colnames(ISC_rec[vartorec])
lab1 <- str_remove(var_lab(ISC_rec$IS3G30H), "Participating in Society/Take part in activities to express opinion/")
lab2 <- str_remove(var_lab(ISC_rec$IS3G30I), "Participating in Society/Take part in activities to express opinion/")
lab3 <- str_remove(var_lab(ISC_rec$IS3G30J), "Participating in Society/Take part in activities to express opinion/")
lab4 <- str_remove(var_lab(ISC_rec$IS3G30K), "Participating in Society/Take part in activities to express opinion/")

ISC_rec <- cbind(ISC_rec[,!colnames(ISC_rec) %in% vartorec], recoded[vartorec])

ISC_rec$T_PROTES1 <- factor(ifelse(!is.na(ISC_rec$IS2P31F), ISC_rec$IS2P31F,
                                   ifelse(!is.na(ISC_rec$IS3G30H), ISC_rec$IS3G30H, NA)),
                            levels = c(1,2,3,4),
                            labels = c("I would certainly not do this", "I would probably not do this", "I would probably do this", "I would certainly do this"))
var_lab(ISC_rec$T_PROTES1) <- lab1
ISC_rec$T_PROTES2 <- factor(ifelse(!is.na(ISC_rec$BS5M11), ISC_rec$BS5M10, 
                                   ifelse(!is.na(ISC_rec$IS2P31G), ISC_rec$IS2P31G,
                                          ifelse(!is.na(ISC_rec$IS3G30I), ISC_rec$IS3G30I, NA))),
                            levels = c(1,2,3,4),
                            labels = c("I would certainly not do this", "I would probably not do this", "I would probably do this", "I would certainly do this"))
var_lab(ISC_rec$T_PROTES2) <- lab2
ISC_rec$T_PROTES3 <- factor(ifelse(!is.na(ISC_rec$BS5M11), ISC_rec$BS5M11, 
                                   ifelse(!is.na(ISC_rec$IS2P31H), ISC_rec$IS2P31H,
                                          ifelse(!is.na(ISC_rec$IS3G30J), ISC_rec$IS3G30J, NA))),
                            levels = c(1,2,3,4),
                            labels = c("I would certainly not do this", "I would probably not do this", "I would probably do this", "I would certainly do this"))
var_lab(ISC_rec$T_PROTES3) <- lab3
ISC_rec$T_PROTES4 <- factor(ifelse(!is.na(ISC_rec$BS5M12), ISC_rec$BS5M12, 
                                   ifelse(!is.na(ISC_rec$IS2P31I), ISC_rec$IS2P31I,
                                          ifelse(!is.na(ISC_rec$IS3G30K), ISC_rec$IS3G30K, NA))),
                            levels = c(1,2,3,4),
                            labels = c("I would certainly not do this", "I would probably not do this", "I would probably do this", "I would certainly do this"))
var_lab(ISC_rec$T_PROTES4) <- lab4

# VarsToUse %>% 
#   filter(Domain %in% "Background questionnaires" & Construct == "students’ expected participation in future illegal activities") 
# get_label(ISC_rec$BS5M12)
# get_label(ISC_rec$IS2P31I)
# get_label(ISC_rec$IS3G30K)

#-------------
cat("  \n")
cat("  \n")
cat('## Standardized data  \n')
cat("  \n")
cat("  \n")
for(l in 1:length(contvars)){
  ind_name <- contvars[l]
  mg2 <- ISC_rec %>% dplyr::select(cycle, SENWGT, all_of(ind_name)) %>% group_by(cycle) %>%
    summarise_at(ind_name, list(~ weighted.mean(., SENWGT, na.rm = TRUE))) %>% data.frame()
  
  g62 <- ISC_rec %>%
    ggplot(aes_string(x = ind_name, y = paste("reorder(COUNTRY, desc(COUNTRY))"), group = "COUNTRY", fill = "COUNTRY")) +
    geom_violin(aes(weight = SENWGT), alpha = 0.5) +
    geom_boxplot(aes(weight = SENWGT), width=0.1) +
    geom_vline(aes_string(xintercept = ind_name), mg2, linetype="dotted", size = 0.8) +
    facet_grid(. ~ cycle) +
    ggtitle(str_wrap(tolower(eval(parse(text=paste0("attributes(ISC_rec$", ind_name,")$label")))), 60)) +
    ylab("Distribution of derived scale") +
    xlab(paste0(ind_name)) +
    scale_color_brewer(palette="Accent") +
    theme(legend.position = "none")
  print(g62)
}
cat("  \n")
cat("  \n")
for(i in 1:nrow(cattorecd)){
  var <- all_of(cattorecd %>%  select(VariableName) %>%  slice(i) %>% unlist(., use.names=FALSE))
  t1 <- ISC_rec %>%  select(cycle, COUNTRY, SENWGT, all_of(var)) %>% na.omit()
  g1 <- t1 %>%
    ggplot(aes_string(x = var, y = "..prop..", group = "COUNTRY", fill = "COUNTRY")) +
    geom_bar(aes(weight = SENWGT), alpha = 0.5) +
    facet_grid(COUNTRY ~ cycle, switch = "y") +
    scale_fill_discrete(guide = FALSE) +
    geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), y= ..prop.. ), stat= "count", vjust = -.5, size = 2) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0,1)) +
    scale_x_discrete(label = function(x) str_wrap(x,10)) +
    ggtitle(str_wrap(tolower(eval(parse(text=paste0("attributes(ISC_rec$",var,")$label")))), 60)) +
    ylab("Weighted proportion of responses") +
    xlab(paste0(var))+
    theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
  print(g1)
}
cat("  \n")
cat("  \n")
cat('## Original data  \n')
cat("  \n")
cat("  \n")
for(l in 1:length(contvars)){
  ind_name <- contvars[l]
  mg <- ISC_rec1 %>% dplyr::select(cycle, SENWGT, all_of(ind_name)) %>% group_by(cycle) %>%
    summarise_at(ind_name, list(~ weighted.mean(., SENWGT, na.rm = TRUE))) %>% data.frame()
  
  g6 <- ISC_rec1 %>%
    ggplot(aes_string(x = ind_name, y = paste("reorder(COUNTRY, desc(COUNTRY))"), group = "COUNTRY", fill = "COUNTRY")) +
    geom_violin(aes(weight = SENWGT), alpha = 0.5) +
    geom_boxplot(aes(weight = SENWGT), width=0.1) +
    geom_vline(aes_string(xintercept = ind_name), mg, linetype="dotted", size = 0.8) +
    facet_grid(. ~ cycle, scales = "free_x") +
    ggtitle(str_wrap(tolower(eval(parse(text=paste0("attributes(ISC_rec$", ind_name,")$label")))), 60)) +
    ylab("Distribution of derived scale") +
    xlab(paste0(ind_name)) +
    scale_color_brewer(palette="Accent") +
    theme(legend.position = "none")
  print(g6)
}
cat("  \n")
cat("  \n")
for(i in 1:nrow(cattorecd)){
  vars <- all_of(cattorecd %>%  select(-VariableName) %>%  slice(i) %>% unlist(., use.names=FALSE)) %>% na.omit()
  t1 <- ISC_rec1 %>%  select(cycle, COUNTRY, SENWGT, all_of(vars)) %>%
    mutate_at(vars, haven::as_factor)
  p <- list()
  for (j in 1:length(vars)) {
    p[[j]] <- t1 %>% select(cycle, COUNTRY, SENWGT, vars[j]) %>% na.omit() %>%
      ggplot(aes_string(x = vars[j], y = "..prop..", group = "COUNTRY", fill = "COUNTRY")) +
      geom_bar(aes(weight = SENWGT), alpha = 0.5) +
      facet_grid(COUNTRY ~ ., labeller = label_context, switch = "y", scales = "free_x", drop = TRUE) +
      scale_fill_discrete(guide = FALSE) +
      geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), y= ..prop.. ), stat= "count", vjust = -.5, size = 2) +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0,1)) +
      scale_x_discrete(label = function(x) str_wrap(x,9)) +
      ggtitle(str_wrap(tolower(eval(parse(text=paste0("attributes(ISC_rec1$",vars[j],")$label")))), 30)) +
      xlab(paste0(vars[j]))+
      theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y = element_blank(), text = element_text(size = 9),
            plot.title = element_text(size=10))
  }
  cat("  \n")
  cat("  \n")
  invisible(do.call(grid.arrange, c(p, nrow=1)))
  cat("  \n")
  cat("  \n")
}

###GENDER EQUALITY
ISC_rec$T_GNDREQ1 <- factor(ifelse(!is.na(ISC_rec$BS4G1), ISC_rec$BS4G1,
                                   ifelse(!is.na(ISC_rec$IS2P24A), ISC_rec$IS2P24A,
                                          ifelse(!is.na(ISC_rec$IS3G24A), ISC_rec$IS3G24A, NA))),
                            levels = c(1,2,3,4),
                            labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"))
var_lab(ISC_rec$T_GNDREQ1) <- str_remove(var_lab(ISC_rec$IS3G24A),"Rights and Responsibilities/Roles women and men/")
ISC_rec$T_GNDREQ2 <- factor(ifelse(!is.na(ISC_rec$BS4G4), ISC_rec$BS4G4,
                                   ifelse(!is.na(ISC_rec$IS2P24B), ISC_rec$IS2P24B,
                                          ifelse(!is.na(ISC_rec$IS3G24B), ISC_rec$IS3G24B, NA))),
                            levels = c(1,2,3,4),
                            labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"))
var_lab(ISC_rec$T_GNDREQ2) <- str_remove(var_lab(ISC_rec$IS3G24B),"Rights and Responsibilities/Roles women and men/")
ISC_rec$T_GNDREQ3 <- factor(ifelse(!is.na(ISC_rec$BS4G6), ISC_rec$BS4G6,
                                   ifelse(!is.na(ISC_rec$IS2P24C), ISC_rec$IS2P24C,
                                          ifelse(!is.na(ISC_rec$IS3G24C), ISC_rec$IS3G24C, NA))),
                            levels = c(1,2,3,4),
                            labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"))
var_lab(ISC_rec$T_GNDREQ3) <- str_remove(var_lab(ISC_rec$IS3G24C),"Rights and Responsibilities/Roles women and men/")
ISC_rec$T_GNDREQ4 <- factor(ifelse(!is.na(ISC_rec$BS4G9), ISC_rec$BS4G9,
                                   ifelse(!is.na(ISC_rec$IS2P24D), ISC_rec$IS2P24D,
                                          ifelse(!is.na(ISC_rec$IS3G24D), ISC_rec$IS3G24D, NA))),
                            levels = c(1,2,3,4),
                            labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"))
var_lab(ISC_rec$T_GNDREQ4) <- str_remove(var_lab(ISC_rec$IS3G24D),"Rights and Responsibilities/Roles women and men/")
ISC_rec$T_GNDREQ5 <- factor(ifelse(!is.na(ISC_rec$BS4G11), ISC_rec$BS4G11,
                                   ifelse(!is.na(ISC_rec$IS2P24E), ISC_rec$IS2P24E,
                                          ifelse(!is.na(ISC_rec$IS3G24E), ISC_rec$IS3G24E, NA))),
                            levels = c(1,2,3,4),
                            labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"))
var_lab(ISC_rec$T_GNDREQ5) <- str_remove(var_lab(ISC_rec$IS3G24E),"Rights and Responsibilities/Roles women and men/")
ISC_rec$T_GNDREQ6 <- factor(ifelse(!is.na(ISC_rec$BS4G13), ISC_rec$BS4G13,
                                   ifelse(!is.na(ISC_rec$IS2P24F), ISC_rec$IS2P24F,
                                          ifelse(!is.na(ISC_rec$IS3G24F), ISC_rec$IS3G24F, NA))),
                            levels = c(1,2,3,4),
                            labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"))
var_lab(ISC_rec$T_GNDREQ6) <- str_remove(var_lab(ISC_rec$IS3G24F),"Rights and Responsibilities/Roles women and men/")
ISC_rec$T_GNDREQ7 <- factor(ifelse(!is.na(ISC_rec$IS3G24G), ISC_rec$IS3G24G, NA),
                            levels = c(1,2,3,4),
                            labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"))
var_lab(ISC_rec$T_GNDREQ7) <- str_remove(var_lab(ISC_rec$IS3G24G),"Rights and Responsibilities/Roles women and men/")

###ETHNIC RACE EQUALITY
ISC_rec$T_ETHNEQ1 <- factor(ifelse(!is.na(ISC_rec$BS4G2), ISC_rec$BS4G2,
                                   ifelse(!is.na(ISC_rec$IS2P25A), ISC_rec$IS2P25A,
                                          ifelse(!is.na(ISC_rec$IS3G25A), ISC_rec$IS3G25A, NA))),
                            levels = c(1,2,3,4),
                            labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"))
var_lab(ISC_rec$T_ETHNEQ1) <-  paste0("All ethnic/racial groups ",tolower(str_remove(var_lab(ISC_rec$IS3G25A), 
                                                                                     "Rights and Responsibilities/Rights and responsibilities/")))
ISC_rec$T_ETHNEQ2 <- factor(ifelse(!is.na(ISC_rec$BS4G5), ISC_rec$BS4G5,
                                   ifelse(!is.na(ISC_rec$IS2P25B), ISC_rec$IS2P25B,
                                          ifelse(!is.na(ISC_rec$IS3G25B), ISC_rec$IS3G25B, NA))),
                            levels = c(1,2,3,4),
                            labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"))
var_lab(ISC_rec$T_ETHNEQ2) <- paste0("All ethnic/racial groups ",tolower(str_remove(var_lab(ISC_rec$IS3G25B), 
                                                                                    "Rights and Responsibilities/Rights and responsibilities/")))
ISC_rec$T_ETHNEQ3 <- factor(ifelse(!is.na(ISC_rec$BS4G8), ISC_rec$BS4G8,
                                   ifelse(!is.na(ISC_rec$IS2P25C), ISC_rec$IS2P25C,
                                          ifelse(!is.na(ISC_rec$IS3G25C), ISC_rec$IS3G25C, NA))),
                            levels = c(1,2,3,4),
                            labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"))
var_lab(ISC_rec$T_ETHNEQ3) <- paste0("All ethnic/racial groups ",tolower(str_remove(var_lab(ISC_rec$IS3G25C), 
                                                                                    "Rights and Responsibilities/Rights and responsibilities/")))
ISC_rec$T_ETHNEQ4 <- factor(ifelse(!is.na(ISC_rec$BS4G12), ISC_rec$BS4G12,
                                   ifelse(!is.na(ISC_rec$IS2P25D), ISC_rec$IS2P25D,
                                          ifelse(!is.na(ISC_rec$IS3G25D), ISC_rec$IS3G25D, NA))),
                            levels = c(1,2,3,4),
                            labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"))
var_lab(ISC_rec$T_ETHNEQ4) <- paste0("All ethnic/racial groups ",tolower(str_remove(var_lab(ISC_rec$IS3G25D), 
                                                                                    "Rights and Responsibilities/Rights and responsibilities/")))
ISC_rec$T_ETHNEQ5 <- factor(ifelse(!is.na(ISC_rec$IS2P25E), ISC_rec$IS2P25E,
                                   ifelse(!is.na(ISC_rec$IS3G25E), ISC_rec$IS3G25E, NA)),
                            levels = c(1,2,3,4),
                            labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"))
var_lab(ISC_rec$T_ETHNEQ5) <- paste0("All ethnic/racial groups ",tolower(str_remove(var_lab(ISC_rec$IS3G25E), 
                                                                                    "Rights and Responsibilities/Rights and responsibilities/")))
###IMMIGRANT EQUALITY
ISC_rec$T_IMMIEQ1 <- factor(ifelse(!is.na(ISC_rec$BS4H1), ISC_rec$BS4H1,
                                   ifelse(!is.na(ISC_rec$IS2P26A), ISC_rec$IS2P26A,
                                          ifelse(!is.na(ISC_rec$ES3G04A), ISC_rec$ES3G04A, NA))),
                            levels = c(1,2,3,4),
                            labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"))
var_lab(ISC_rec$T_IMMIEQ1) <- str_remove(str_remove(str_remove(var_lab(ISC_rec$ES3G04A),"<"),">"),"Moving/")
ISC_rec$T_IMMIEQ2 <- factor(ifelse(!is.na(ISC_rec$BS4H2), ISC_rec$BS4H2,
                                   ifelse(!is.na(ISC_rec$IS2P26B), ISC_rec$IS2P26B,
                                          ifelse(!is.na(ISC_rec$ES3G04B), ISC_rec$ES3G04B, NA))),
                            levels = c(1,2,3,4),
                            labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"))
var_lab(ISC_rec$T_IMMIEQ2) <- str_remove(str_remove(str_remove(var_lab(ISC_rec$ES3G04B),"<"),">"),"Moving/")
ISC_rec$T_IMMIEQ3 <- factor(ifelse(!is.na(ISC_rec$BS4H3), ISC_rec$BS4H3,
                                   ifelse(!is.na(ISC_rec$IS2P26C), ISC_rec$IS2P26C,
                                          ifelse(!is.na(ISC_rec$ES3G04C), ISC_rec$ES3G04C, NA))),
                            levels = c(1,2,3,4),
                            labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"))
var_lab(ISC_rec$T_IMMIEQ3) <- str_remove(str_remove(str_remove(var_lab(ISC_rec$ES3G04C),"<"),">"),"Moving/")
ISC_rec$T_IMMIEQ4 <- factor(ifelse(!is.na(ISC_rec$BS4H4), ISC_rec$BS4H4,
                                   ifelse(!is.na(ISC_rec$IS2P26D), ISC_rec$IS2P26D,
                                          ifelse(!is.na(ISC_rec$ES3G04D), ISC_rec$ES3G04D, NA))),
                            levels = c(1,2,3,4),
                            labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"))
var_lab(ISC_rec$T_IMMIEQ4) <- str_remove(str_remove(str_remove(var_lab(ISC_rec$ES3G04D),"<"),">"),"Moving/")
ISC_rec$T_IMMIEQ5 <- factor(ifelse(!is.na(ISC_rec$BS4H5), ISC_rec$BS4H5,
                                   ifelse(!is.na(ISC_rec$IS2P26E), ISC_rec$IS2P26E,
                                          ifelse(!is.na(ISC_rec$ES3G04E), ISC_rec$ES3G04E, NA))),
                            levels = c(1,2,3,4),
                            labels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"))
var_lab(ISC_rec$T_IMMIEQ5) <- str_remove(str_remove(str_remove(var_lab(ISC_rec$ES3G04E),"<"),">"),"Moving/")

# binary Scales
ISC_rec$bT_IMMIEQ1 <- factor(ifelse(ISC_rec$T_IMMIEQ1 %in% c("Strongly disagree", "Disagree"), 1,
                                    ifelse(ISC_rec$T_IMMIEQ1 %in% c("Agree", "Strongly agree"), 2, NA)),
                             labels = c("Disagree", "Agree"))
var_lab(ISC_rec$bT_IMMIEQ1) <- var_lab(ISC_rec$T_IMMIEQ1)

ISC_rec$bT_IMMIEQ2 <- factor(ifelse(ISC_rec$T_IMMIEQ2 %in% c("Strongly disagree", "Disagree"), 1,
                                    ifelse(ISC_rec$T_IMMIEQ2 %in% c("Agree", "Strongly agree"), 2, NA)),
                             labels = c("Disagree", "Agree"))
var_lab(ISC_rec$bT_IMMIEQ2) <- var_lab(ISC_rec$T_IMMIEQ2)

ISC_rec$bT_IMMIEQ3 <- factor(ifelse(ISC_rec$T_IMMIEQ3 %in% c("Strongly disagree", "Disagree"), 1,
                                    ifelse(ISC_rec$T_IMMIEQ3 %in% c("Agree", "Strongly agree"), 2, NA)),
                             labels = c("Disagree", "Agree"))
var_lab(ISC_rec$bT_IMMIEQ3) <- var_lab(ISC_rec$T_IMMIEQ3)

ISC_rec$bT_IMMIEQ4 <- factor(ifelse(ISC_rec$T_IMMIEQ4 %in% c("Strongly disagree", "Disagree"), 1,
                                    ifelse(ISC_rec$T_IMMIEQ4 %in% c("Agree", "Strongly agree"), 2, NA)),
                             labels = c("Disagree", "Agree"))
var_lab(ISC_rec$bT_IMMIEQ4) <- var_lab(ISC_rec$T_IMMIEQ4)

ISC_rec$bT_IMMIEQ5 <- factor(ifelse(ISC_rec$T_IMMIEQ5 %in% c("Strongly disagree", "Disagree"), 1,
                                    ifelse(ISC_rec$T_IMMIEQ5 %in% c("Agree", "Strongly agree"), 2, NA)),
                             labels = c("Disagree", "Agree"))
var_lab(ISC_rec$bT_IMMIEQ5) <- var_lab(ISC_rec$T_IMMIEQ5)

ISC_rec$bT_GNDREQ1 <- factor(ifelse(ISC_rec$T_GNDREQ1 %in% c("Strongly disagree", "Disagree"), 1,
                                    ifelse(ISC_rec$T_GNDREQ1 %in% c("Agree", "Strongly agree"), 2, NA)),
                             labels = c("Disagree", "Agree"))
var_lab(ISC_rec$bT_GNDREQ1) <- var_lab(ISC_rec$T_GNDREQ1)

ISC_rec$bT_GNDREQ2 <- factor(ifelse(ISC_rec$T_GNDREQ2 %in% c("Strongly disagree", "Disagree"), 1,
                                    ifelse(ISC_rec$T_GNDREQ2 %in% c("Agree", "Strongly agree"), 2, NA)),
                             labels = c("Disagree", "Agree"))
var_lab(ISC_rec$bT_GNDREQ2) <- var_lab(ISC_rec$T_GNDREQ2)

ISC_rec$bT_GNDREQ3 <- factor(ifelse(ISC_rec$T_GNDREQ3 %in% c("Strongly disagree", "Disagree"), 1,
                                    ifelse(ISC_rec$T_GNDREQ3 %in% c("Agree", "Strongly agree"), 2, NA)),
                             labels = c("Disagree", "Agree"))
var_lab(ISC_rec$bT_GNDREQ3) <- var_lab(ISC_rec$T_GNDREQ3)

ISC_rec$bT_GNDREQ4 <- factor(ifelse(ISC_rec$T_GNDREQ4 %in% c("Strongly disagree", "Disagree"), 1,
                                    ifelse(ISC_rec$T_GNDREQ4 %in% c("Agree", "Strongly agree"), 2, NA)),
                             labels = c("Disagree", "Agree"))
var_lab(ISC_rec$bT_GNDREQ4) <- var_lab(ISC_rec$T_GNDREQ4)

ISC_rec$bT_GNDREQ5 <- factor(ifelse(ISC_rec$T_GNDREQ5 %in% c("Strongly disagree", "Disagree"), 1,
                                    ifelse(ISC_rec$T_GNDREQ5 %in% c("Agree", "Strongly agree"), 2, NA)),
                             labels = c("Disagree", "Agree"))
var_lab(ISC_rec$bT_GNDREQ5) <- var_lab(ISC_rec$T_GNDREQ5)

ISC_rec$bT_GNDREQ6 <- factor(ifelse(ISC_rec$T_GNDREQ6 %in% c("Strongly disagree", "Disagree"), 1,
                                    ifelse(ISC_rec$T_GNDREQ6 %in% c("Agree", "Strongly agree"), 2, NA)),
                             labels = c("Disagree", "Agree"))
var_lab(ISC_rec$bT_GNDREQ6) <- var_lab(ISC_rec$T_GNDREQ6)

ISC_rec$bT_GNDREQ7 <- factor(ifelse(ISC_rec$T_GNDREQ7 %in% c("Strongly disagree", "Disagree"), 1,
                                    ifelse(ISC_rec$T_GNDREQ7 %in% c("Agree", "Strongly agree"), 2, NA)),
                             labels = c("Disagree", "Agree"))
var_lab(ISC_rec$bT_GNDREQ7) <- var_lab(ISC_rec$T_GNDREQ7)

ISC_rec$bT_ETHNEQ1 <- factor(ifelse(ISC_rec$T_ETHNEQ1 %in% c("Strongly disagree", "Disagree"), 1,
                                    ifelse(ISC_rec$T_ETHNEQ1 %in% c("Agree", "Strongly agree"), 2, NA)),
                             labels = c("Disagree", "Agree"))
var_lab(ISC_rec$bT_ETHNEQ1) <- var_lab(ISC_rec$T_ETHNEQ1)

ISC_rec$bT_ETHNEQ2 <- factor(ifelse(ISC_rec$T_ETHNEQ2 %in% c("Strongly disagree", "Disagree"), 1,
                                    ifelse(ISC_rec$T_ETHNEQ2 %in% c("Agree", "Strongly agree"), 2, NA)),
                             labels = c("Disagree", "Agree"))
var_lab(ISC_rec$bT_ETHNEQ2) <- var_lab(ISC_rec$T_ETHNEQ2)

ISC_rec$bT_ETHNEQ3 <- factor(ifelse(ISC_rec$T_ETHNEQ3 %in% c("Strongly disagree", "Disagree"), 1,
                                    ifelse(ISC_rec$T_ETHNEQ3 %in% c("Agree", "Strongly agree"), 2, NA)),
                             labels = c("Disagree", "Agree"))
var_lab(ISC_rec$bT_ETHNEQ3) <- var_lab(ISC_rec$T_ETHNEQ3)

ISC_rec$bT_ETHNEQ4 <- factor(ifelse(ISC_rec$T_ETHNEQ4 %in% c("Strongly disagree", "Disagree"), 1,
                                    ifelse(ISC_rec$T_ETHNEQ4 %in% c("Agree", "Strongly agree"), 2, NA)),
                             labels = c("Disagree", "Agree"))
var_lab(ISC_rec$bT_ETHNEQ4) <- var_lab(ISC_rec$T_ETHNEQ4)

ISC_rec$bT_ETHNEQ5 <- factor(ifelse(ISC_rec$T_ETHNEQ5 %in% c("Strongly disagree", "Disagree"), 1,
                                    ifelse(ISC_rec$T_ETHNEQ5 %in% c("Agree", "Strongly agree"), 2, NA)),
                             labels = c("Disagree", "Agree"))
var_lab(ISC_rec$bT_ETHNEQ5) <- var_lab(ISC_rec$T_ETHNEQ5)

Scales <- c(VarsToUse %>%  filter(Domain == "Scales" & Dataset %in% c("ISG","ISE")) %>% select(VariableName) %>% na.omit() %>% pull())
Scalesb <- paste0("b",c(VarsToUse %>%  filter(Domain == "Scales" & Dataset %in% c("ISG","ISE")) %>% select(VariableName) %>% na.omit() %>% pull()))

ISC_rec <- ISC_rec %>% select(all_of(Id), all_of(Man_cate), all_of(Man_cont), all_of(Indicfa), SENWGT, TOTWGT, all_of(Scales), all_of(Scalesb))

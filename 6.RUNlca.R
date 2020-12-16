library(poLCA)
library(MplusAutomation)

ds_lc0 <- ISC_lvR %>% 
  dplyr::select(all_of(Id), all_of(sampleID), all_of(Scales), all_of(Scalesb), all_of(Man_cate), all_of(Man_cont)) %>% 
  mutate(Subg = ifelse(COUNTRY %in% c("CHL", "COL", "PER", "DOM", "MEX"), "LA", "EU"))
ds_lc0$IDSTUD <- as.numeric(ds_lc0$IDSTUD)

remlabclass <- function(ces){
  for (each in colnames(ces)){
    if ("labelled" %in% class(ces[[each]])){
      class(ces[[each]]) = c("numeric")
      attr(ces[[each]], "levels") <- NULL
    }
    attr(ces[[each]], "label") <- NULL
  }
  return(ces)
}
ds_lc0 <- remlabclass(ds_lc0)

Man_cate2 <- Man_cate[!grepl(paste0(c("T_HIGHEDFA", "T_HISCED"), collapse = "|"), Man_cate)]
Man_cont2 <- Man_cont[!grepl(paste0(c("T_NISB", "T_PROTES"), collapse = "|"), Man_cont)]
Scalesc <- Scales[!grepl(paste0(c("T_GNDREQ7", "T_ETHNEQ5"), collapse = "|"), Scales)] #Using only scales that are in all cycles
Scalesbc <- Scalesb[!grepl(paste0(c("bT_GNDREQ7", "bT_ETHNEQ5"), collapse = "|"), Scalesb)] #Using only scales that are in all cycles

#------------------------------------------------------------
#-----------------------Mplus code---------------------------
#------------------------------------------------------------
#NOMINAL scale
# #----------------All scales together by cycle--------------------------------------
# for (j in 1:3) { #input file for each cycle 1:3
# 
#   # if(j == 1) Scalesc <- Scales[!grepl(paste0(c("T_GNDREQ7", "T_ETHNEQ5"), collapse = "|"), Scales)] else
#   #   if(j == 2) Scalesc <- Scales[!grepl(paste0(c("T_GNDREQ7"), collapse = "|"), Scales)] else
#   #     Scalesc <- Scales
# 
#       data <- ds_lc0 %>%  filter(cycle == paste0("C",j)) %>%
#       dplyr::select(all_of(Id), all_of(sampleID), all_of(Scalesc), -COUNTRY, -cycle) %>%
#       mutate_if(is.factor, ~ as.numeric(.x)) %>%
#       rename_with(~ gsub("T_ETHNEQ", "ETH", .x, fixed = TRUE)) %>%
#       rename_with(~ gsub("T_GNDREQ", "GND", .x, fixed = TRUE)) %>%
#       rename_with(~ gsub("T_IMMIEQ", "IMM", .x, fixed = TRUE)) %>%
#       data.frame()
#     prepareMplusData(df = data,
#                      filename = paste0("Mplus/DtaC",j,".dat"), interactive =FALSE)
# 
#     cas <- list()
# 
#     lapply(4:7, function(k) { #input file for different number of classes 
#       for(c in 1:k){
#         cas[c] <- paste0("%c#",c,"%")
#       }
#       fileConn <- file(paste0("Mplus/lca_C",j,"cl",sprintf("%d", k),".inp"))
#       writeLines(c(
#         paste0("TITLE: LCA C", j," with ", k ," classes;"),
#         "DATA: ",
#         paste0("FILE = DtaC",j,".dat;"),
#         "",
#         "VARIABLE: ",
#         paste0("NAMES = ", paste(colnames(data), collapse = "\n"),";"),
#         "IDVARIABLE = IDSTUD;",
#         paste0("USEVARIABLES = ", paste(colnames(data)[grepl('^GND|^IMM|^ETH', colnames(data))], collapse = "\n"),";"),
#         paste0("CATEGORICAL = ", paste(colnames(data)[grepl('^GND|^IMM|^ETH', colnames(data))], collapse = "\n"),";"),
#         "MISSING = .;",
#         paste0("CLASSES = ",sprintf("c(%d);", k)),
#         #paste0("CLASSES = g(",length(unique(data$IDCNTRY)),") ",sprintf("c(%d));", k)),
#         #paste0("KNOWNCLASS = g(IDCNTRY =", paste(unique(data$IDCNTRY), collapse = "\n"),");"),
#         "WEIGHT = SENWGT;",
#         "STRATIFICATION = IDJK;",
#         "CLUSTER = IDCL;",
#         " ",
#         "ANALYSIS:",
#         "TYPE = COMPLEX MIXTURE;",
#         "PROCESSORS = 4;",
#         "STARTS = 100 50;",
#         "STITERATIONS = 5;",
#         "STSEED = 288;",
#         "",
#         "MODEL:",
#         "%OVERALL%",
#         " ",
#         #"c ON g",
#         paste(cas, collapse = "\n\n"),
#         " ",
#         "OUTPUT: ",
#         "TECH10",#for bivariate model fit information
#         "TECH11",
#         #"SAMP",
#         #"TECH14;", Time consuming tests
#         #"CINTERVAL",
#         #"SVALUES",
#         ";",
#         "",
#         "SAVEDATA:",
#         paste0("FILE = lca_C", j,"cl", k,".dat;"),
#         "SAVE = CPROBABILITIES;"
# 
#       ), fileConn)
#       close(fileConn)
#     })
# }

#----------------Each scale separately by cycle--------------------------------------------
# for (j in 1:3) { #input file for each cycle 1:3
# 
#   # if(j == 1) Scalesc <- Scales[!grepl(paste0(c("T_GNDREQ7", "T_ETHNEQ5"), collapse = "|"), Scales)] else
#   #   if(j == 2) Scalesc <- Scales[!grepl(paste0(c("T_GNDREQ7"), collapse = "|"), Scales)] else
#   #     Scalesc <- Scales
# 
#     data <- ds_lc0 %>%  filter(cycle == paste0("C",j)) %>%
#       dplyr::select(all_of(Id), all_of(sampleID), all_of(Scalesc), -COUNTRY, -cycle) %>%
#       mutate_if(is.factor, ~ as.numeric(.x)) %>%
#       rename_with(~ gsub("T_ETHNEQ", "ETHN", .x, fixed = TRUE)) %>%
#       rename_with(~ gsub("T_GNDREQ", "GNDR", .x, fixed = TRUE)) %>%
#       rename_with(~ gsub("T_IMMIEQ", "IMMI", .x, fixed = TRUE)) %>%
#       data.frame()
#     prepareMplusData(df = data,
#                      filename = paste0("Mplus/DtaC",j,".dat"),interactive =FALSE)
# 
#     for (l in c("ETHN", "GNDR", "IMMI")) { #3 scales
# 
#       f1 <- as.matrix(dplyr::select(data, starts_with(l)))~1
# 
#       cas <- list()
# 
#       lapply(2:4, function(k) { #input file for different number of classes 2-5
#         for(c in 1:k){
#           cas[c] <- paste0("%c#",c,"%")
#         }
#         fileConn <- file(paste0("Mplus/lca_","C", j, l, "cl", sprintf("%d", k), ".inp"))
#         writeLines(c(
#           paste0("TITLE: LCA C", j," ", l, " ", k ," classes;"),
#           "DATA: ",
#           paste0("FILE = DtaC",j,".dat;"),
#           "",
#           "VARIABLE: ",
#           paste0("NAMES = ", paste(colnames(data), collapse = "\n"),";"),
#           "IDVARIABLE = IDSTUD;",
#           paste0("USEVARIABLES = ", paste(colnames(data)[grepl(paste0('^',l), colnames(data))], collapse = "\n"),";"),
#           paste0("CATEGORICAL = ", paste(colnames(data)[grepl(paste0('^',l), colnames(data))], collapse = "\n"),";"),
#           "MISSING = .;",
#           paste0("CLASSES = ",sprintf("c(%d);", k)),
#           #paste0("CLASSES = g(",length(unique(data$IDCNTRY)),") ",sprintf("c(%d));", k)),
#           #paste0("KNOWNCLASS = g(IDCNTRY =", paste(unique(data$IDCNTRY), collapse = "\n"),");"),
#           "WEIGHT = SENWGT;",
#           "STRATIFICATION = IDJK;",
#           "CLUSTER = IDCL;",
#           " ",
#           "ANALYSIS:",
#           "TYPE = COMPLEX MIXTURE;",
#           "PROCESSORS = 4;",
#           "STARTS = 100 50;",
#           "STITERATIONS = 5;",
#           "STSEED = 288;",
#           "",
#           "MODEL:",
#           "%OVERALL%",
#           " ",
#           #"c ON g",
#           paste(cas, collapse = "\n\n"),
#           " ",
#           "OUTPUT: ",
#           "TECH10",#for bivariate model fit information
#           "TECH11",
#           #"SAMP",
#           #"TECH14;", Time consuming tests
#           #"CINTERVAL",
#           #"SVALUES",
#           ";",
#           "",
#           "SAVEDATA:",
#           paste0("FILE = lca_C", j, l, "cl", k,".dat;"),
#           "SAVE = CPROBABILITIES;"
# 
#         ), fileConn)
#         close(fileConn)
#       })
#     }
# }

#-------------------
#-------------------
####Binomial########
#-------------------
#-------------------
# #----------------All scales together by cycle--------------------------------------
for (j in 1:3) { #input file for each cycle 1:3

  # if(j == 1) Scalesc <- Scales[!grepl(paste0(c("T_GNDREQ7", "T_ETHNEQ5"), collapse = "|"), Scales)] else
  #   if(j == 2) Scalesc <- Scales[!grepl(paste0(c("T_GNDREQ7"), collapse = "|"), Scales)] else
  #     Scalesc <- Scales

      data <- ds_lc0 %>%  filter(cycle == paste0("C",j)) %>%
      dplyr::select(all_of(Id), all_of(sampleID), all_of(Scalesbc), -COUNTRY, -cycle) %>%
      mutate_if(is.factor, ~ as.numeric(.x)) %>%
      rename_with(~ gsub("bT_ETHNEQ", "ETH", .x, fixed = TRUE)) %>%
      rename_with(~ gsub("bT_GNDREQ", "GND", .x, fixed = TRUE)) %>%
      rename_with(~ gsub("bT_IMMIEQ", "IMM", .x, fixed = TRUE)) %>%
      data.frame()
    prepareMplusData(df = data,
                     filename = paste0("Mplus/BDtaC",j,".dat"), interactive =FALSE)

    cas <- list()

    lapply(4:7, function(k) { #input file for different number of classes
      for(c in 1:k){
        cas[c] <- paste0("%c#",c,"%")
      }
      fileConn <- file(paste0("Mplus/Blca_C",j,"cl",sprintf("%d", k),".inp"))
      writeLines(c(
        paste0("TITLE: LCA Binomial C", j," with ", k ," classes;"),
        "DATA: ",
        paste0("FILE = BDtaC",j,".dat;"),
        "",
        "VARIABLE: ",
        paste0("NAMES = ", paste(colnames(data), collapse = "\n"),";"),
        "IDVARIABLE = IDSTUD;",
        paste0("USEVARIABLES = ", paste(colnames(data)[grepl('^GND|^IMM|^ETH', colnames(data))], collapse = "\n"),";"),
        paste0("CATEGORICAL = ", paste(colnames(data)[grepl('^GND|^IMM|^ETH', colnames(data))], collapse = "\n"),";"),
        "MISSING = .;",
        paste0("CLASSES = ",sprintf("c(%d);", k)),
        #paste0("CLASSES = g(",length(unique(data$IDCNTRY)),") ",sprintf("c(%d));", k)),
        #paste0("KNOWNCLASS = g(IDCNTRY =", paste(unique(data$IDCNTRY), collapse = "\n"),");"),
        "WEIGHT = SENWGT;",
        "STRATIFICATION = IDJK;",
        "CLUSTER = IDCL;",
        " ",
        "ANALYSIS:",
        "TYPE = COMPLEX MIXTURE;",
        "PROCESSORS = 4;",
        "STARTS = 100 50;",
        "STITERATIONS = 5;",
        "STSEED = 288;",
        "",
        "MODEL:",
        "%OVERALL%",
        " ",
        #"c ON g",
        paste(cas, collapse = "\n\n"),
        " ",
        "OUTPUT: ",
        "TECH10",#for bivariate model fit information
        "TECH11",
        #"SAMP",
        #"TECH14;", Time consuming tests
        #"CINTERVAL",
        #"SVALUES",
        ";",
        "",
        "SAVEDATA:",
        paste0("FILE = blca_C", j,"cl", k,".dat;"),
        "SAVE = CPROBABILITIES;"

      ), fileConn)
      close(fileConn)
    })
}

#----------------Each scale separately by cycle--------------------------------------------
# for (j in 1:3) { #input file for each cycle 1:3
# 
#   # if(j == 1) Scalesc <- Scales[!grepl(paste0(c("T_GNDREQ7", "T_ETHNEQ5"), collapse = "|"), Scales)] else
#   #   if(j == 2) Scalesc <- Scales[!grepl(paste0(c("T_GNDREQ7"), collapse = "|"), Scales)] else
#   #     Scalesc <- Scales
# 
#     data <- ds_lc0 %>%  filter(cycle == paste0("C",j)) %>%
#       dplyr::select(all_of(Id), all_of(sampleID), all_of(Scalesc), -COUNTRY, -cycle) %>%
#       mutate_if(is.factor, ~ as.numeric(.x)) %>%
#       rename_with(~ gsub("T_ETHNEQ", "ETHN", .x, fixed = TRUE)) %>%
#       rename_with(~ gsub("T_GNDREQ", "GNDR", .x, fixed = TRUE)) %>%
#       rename_with(~ gsub("T_IMMIEQ", "IMMI", .x, fixed = TRUE)) %>%
#       data.frame()
#     prepareMplusData(df = data,
#                      filename = paste0("Mplus/DtaC",j,".dat"),interactive =FALSE)
# 
#     for (l in c("ETHN", "GNDR", "IMMI")) { #3 scales
# 
#       f1 <- as.matrix(dplyr::select(data, starts_with(l)))~1
# 
#       cas <- list()
# 
#       lapply(2:4, function(k) { #input file for different number of classes 2-5
#         for(c in 1:k){
#           cas[c] <- paste0("%c#",c,"%")
#         }
#         fileConn <- file(paste0("Mplus/lca_","C", j, l, "cl", sprintf("%d", k), ".inp"))
#         writeLines(c(
#           paste0("TITLE: LCA C", j," ", l, " ", k ," classes;"),
#           "DATA: ",
#           paste0("FILE = DtaC",j,".dat;"),
#           "",
#           "VARIABLE: ",
#           paste0("NAMES = ", paste(colnames(data), collapse = "\n"),";"),
#           "IDVARIABLE = IDSTUD;",
#           paste0("USEVARIABLES = ", paste(colnames(data)[grepl(paste0('^',l), colnames(data))], collapse = "\n"),";"),
#           paste0("CATEGORICAL = ", paste(colnames(data)[grepl(paste0('^',l), colnames(data))], collapse = "\n"),";"),
#           "MISSING = .;",
#           paste0("CLASSES = ",sprintf("c(%d);", k)),
#           #paste0("CLASSES = g(",length(unique(data$IDCNTRY)),") ",sprintf("c(%d));", k)),
#           #paste0("KNOWNCLASS = g(IDCNTRY =", paste(unique(data$IDCNTRY), collapse = "\n"),");"),
#           "WEIGHT = SENWGT;",
#           "STRATIFICATION = IDJK;",
#           "CLUSTER = IDCL;",
#           " ",
#           "ANALYSIS:",
#           "TYPE = COMPLEX MIXTURE;",
#           "PROCESSORS = 4;",
#           "STARTS = 100 50;",
#           "STITERATIONS = 5;",
#           "STSEED = 288;",
#           "",
#           "MODEL:",
#           "%OVERALL%",
#           " ",
#           #"c ON g",
#           paste(cas, collapse = "\n\n"),
#           " ",
#           "OUTPUT: ",
#           "TECH10",#for bivariate model fit information
#           "TECH11",
#           #"SAMP",
#           #"TECH14;", Time consuming tests
#           #"CINTERVAL",
#           #"SVALUES",
#           ";",
#           "",
#           "SAVEDATA:",
#           paste0("FILE = lca_C", j, l, "cl", k,".dat;"),
#           "SAVE = CPROBABILITIES;"
# 
#         ), fileConn)
#         close(fileConn)
#       })
#     }
# }


#-------------------
#-------------------
####By subgroup########
#Nominal variables
#-------------------
#-------------------
# #----------------All scales together by cycle--------------------------------------
for (j in 1:3) { #input file for each cycle 1:3
  
  # if(j == 1) Scalesc <- Scales[!grepl(paste0(c("T_GNDREQ7", "T_ETHNEQ5"), collapse = "|"), Scales)] else
  #   if(j == 2) Scalesc <- Scales[!grepl(paste0(c("T_GNDREQ7"), collapse = "|"), Scales)] else
  #     Scalesc <- Scales
  LAScalesc <- Scales[!grepl(paste0(c("T_GNDREQ7", "T_ETHNEQ5"), collapse = "|"), Scales)]
  dataLA <- ds_lc0 %>%  filter(cycle == paste0("C",j)) %>%
    dplyr::select(all_of(Id), all_of(sampleID), all_of(Scalesbc), -COUNTRY, -cycle) %>%
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    rename_with(~ gsub("T_ETHNEQ", "ETH", .x, fixed = TRUE)) %>%
    rename_with(~ gsub("T_GNDREQ", "GND", .x, fixed = TRUE)) %>%
    rename_with(~ gsub("T_IMMIEQ", "IMM", .x, fixed = TRUE)) %>%
    data.frame()
  prepareMplusData(df = dataLA,
                   filename = paste0("Mplus/LADtaC",j,".dat"), interactive =FALSE)
  
  cas <- list()
  
  lapply(4:7, function(k) { #input file for different number of classes
    for(c in 1:k){
      cas[c] <- paste0("%c#",c,"%")
    }
    fileConn <- file(paste0("Mplus/LAlca_C",j,"cl",sprintf("%d", k),".inp"))
    writeLines(c(
      paste0("TITLE: LCA Binomial C", j," with ", k ," classes;"),
      "DATA: ",
      paste0("FILE = BDtaC",j,".dat;"),
      "",
      "VARIABLE: ",
      paste0("NAMES = ", paste(colnames(data), collapse = "\n"),";"),
      "IDVARIABLE = IDSTUD;",
      paste0("USEVARIABLES = ", paste(colnames(data)[grepl('^GND|^IMM|^ETH', colnames(data))], collapse = "\n"),";"),
      paste0("CATEGORICAL = ", paste(colnames(data)[grepl('^GND|^IMM|^ETH', colnames(data))], collapse = "\n"),";"),
      "MISSING = .;",
      paste0("CLASSES = ",sprintf("c(%d);", k)),
      #paste0("CLASSES = g(",length(unique(data$IDCNTRY)),") ",sprintf("c(%d));", k)),
      #paste0("KNOWNCLASS = g(IDCNTRY =", paste(unique(data$IDCNTRY), collapse = "\n"),");"),
      "WEIGHT = SENWGT;",
      "STRATIFICATION = IDJK;",
      "CLUSTER = IDCL;",
      " ",
      "ANALYSIS:",
      "TYPE = COMPLEX MIXTURE;",
      "PROCESSORS = 4;",
      "STARTS = 100 50;",
      "STITERATIONS = 5;",
      "STSEED = 288;",
      "",
      "MODEL:",
      "%OVERALL%",
      " ",
      #"c ON g",
      paste(cas, collapse = "\n\n"),
      " ",
      "OUTPUT: ",
      "TECH10",#for bivariate model fit information
      "TECH11",
      #"SAMP",
      #"TECH14;", Time consuming tests
      #"CINTERVAL",
      #"SVALUES",
      ";",
      "",
      "SAVEDATA:",
      paste0("FILE = blca_C", j,"cl", k,".dat;"),
      "SAVE = CPROBABILITIES;"
      
    ), fileConn)
    close(fileConn)
  })
}

#----------------Each scale separately by cycle--------------------------------------------
# for (j in 1:3) { #input file for each cycle 1:3
# 
#   # if(j == 1) Scalesc <- Scales[!grepl(paste0(c("T_GNDREQ7", "T_ETHNEQ5"), collapse = "|"), Scales)] else
#   #   if(j == 2) Scalesc <- Scales[!grepl(paste0(c("T_GNDREQ7"), collapse = "|"), Scales)] else
#   #     Scalesc <- Scales
# 
#     data <- ds_lc0 %>%  filter(cycle == paste0("C",j)) %>%
#       dplyr::select(all_of(Id), all_of(sampleID), all_of(Scalesc), -COUNTRY, -cycle) %>%
#       mutate_if(is.factor, ~ as.numeric(.x)) %>%
#       rename_with(~ gsub("T_ETHNEQ", "ETHN", .x, fixed = TRUE)) %>%
#       rename_with(~ gsub("T_GNDREQ", "GNDR", .x, fixed = TRUE)) %>%
#       rename_with(~ gsub("T_IMMIEQ", "IMMI", .x, fixed = TRUE)) %>%
#       data.frame()
#     prepareMplusData(df = data,
#                      filename = paste0("Mplus/DtaC",j,".dat"),interactive =FALSE)
# 
#     for (l in c("ETHN", "GNDR", "IMMI")) { #3 scales
# 
#       f1 <- as.matrix(dplyr::select(data, starts_with(l)))~1
# 
#       cas <- list()
# 
#       lapply(2:4, function(k) { #input file for different number of classes 2-5
#         for(c in 1:k){
#           cas[c] <- paste0("%c#",c,"%")
#         }
#         fileConn <- file(paste0("Mplus/lca_","C", j, l, "cl", sprintf("%d", k), ".inp"))
#         writeLines(c(
#           paste0("TITLE: LCA C", j," ", l, " ", k ," classes;"),
#           "DATA: ",
#           paste0("FILE = DtaC",j,".dat;"),
#           "",
#           "VARIABLE: ",
#           paste0("NAMES = ", paste(colnames(data), collapse = "\n"),";"),
#           "IDVARIABLE = IDSTUD;",
#           paste0("USEVARIABLES = ", paste(colnames(data)[grepl(paste0('^',l), colnames(data))], collapse = "\n"),";"),
#           paste0("CATEGORICAL = ", paste(colnames(data)[grepl(paste0('^',l), colnames(data))], collapse = "\n"),";"),
#           "MISSING = .;",
#           paste0("CLASSES = ",sprintf("c(%d);", k)),
#           #paste0("CLASSES = g(",length(unique(data$IDCNTRY)),") ",sprintf("c(%d));", k)),
#           #paste0("KNOWNCLASS = g(IDCNTRY =", paste(unique(data$IDCNTRY), collapse = "\n"),");"),
#           "WEIGHT = SENWGT;",
#           "STRATIFICATION = IDJK;",
#           "CLUSTER = IDCL;",
#           " ",
#           "ANALYSIS:",
#           "TYPE = COMPLEX MIXTURE;",
#           "PROCESSORS = 4;",
#           "STARTS = 100 50;",
#           "STITERATIONS = 5;",
#           "STSEED = 288;",
#           "",
#           "MODEL:",
#           "%OVERALL%",
#           " ",
#           #"c ON g",
#           paste(cas, collapse = "\n\n"),
#           " ",
#           "OUTPUT: ",
#           "TECH10",#for bivariate model fit information
#           "TECH11",
#           #"SAMP",
#           #"TECH14;", Time consuming tests
#           #"CINTERVAL",
#           #"SVALUES",
#           ";",
#           "",
#           "SAVEDATA:",
#           paste0("FILE = lca_C", j, l, "cl", k,".dat;"),
#           "SAVE = CPROBABILITIES;"
# 
#         ), fileConn)
#         close(fileConn)
#       })
#     }
# }


#----------------All scales/cycles together--------------------------------------

# dataAll <- ds_lc0 %>%
#   dplyr::select(all_of(Id), all_of(sampleID), all_of(Scalesc), -COUNTRY, -cycle) %>%
#   mutate_if(is.factor, ~ as.numeric(.x)) %>%
#   rename_with(~ gsub("T_ETHNEQ", "ETH", .x, fixed = TRUE)) %>%
#   rename_with(~ gsub("T_GNDREQ", "GND", .x, fixed = TRUE)) %>%
#   rename_with(~ gsub("T_IMMIEQ", "IMM", .x, fixed = TRUE)) %>%
#   data.frame()
# prepareMplusData(df = dataAll,
#                  filename = paste0("Mplus/Dta.dat"),interactive =FALSE)
# 
# cas1 <- list()
# 
# lapply(4:7, function(k) { #input file for different number of classes 
#   for(c in 1:k){
#     cas1[c] <- paste0("%c#",c,"%")
#   }
#   fileConn <- file(paste0("Mplus/lca_cl",sprintf("%d", k),".inp"))
#   writeLines(c(
#     paste0("TITLE: LCA with ", k ," classes;"),
#     "DATA: ",
#     paste0("FILE = Dta.dat;"),
#     "",
#     "VARIABLE: ",
#     paste0("NAMES = ", paste(colnames(dataAll), collapse = "\n"),";"),
#     "IDVARIABLE = IDSTUD;",
#     paste0("USEVARIABLES = ", paste(colnames(dataAll)[grepl('^GND|^IMM|^ETH', colnames(dataAll))], collapse = "\n"),";"),
#     paste0("CATEGORICAL = ", paste(colnames(dataAll)[grepl('^GND|^IMM|^ETH', colnames(dataAll))], collapse = "\n"),";"),
#     "MISSING = .;",
#     paste0("CLASSES = ",sprintf("c(%d);", k)),
#     #paste0("CLASSES = g(",length(unique(dataAll$IDCNTRY)),") ",sprintf("c(%d));", k)),
#     #paste0("KNOWNCLASS = g(IDCNTRY =", paste(unique(dataAll$IDCNTRY), collapse = "\n"),");"),
#     "WEIGHT = SENWGT;",
#     "STRATIFICATION = IDJK;",
#     "CLUSTER = IDCL;",
#     " ",
#     "ANALYSIS:",
#     "TYPE = COMPLEX MIXTURE;",
#     "PROCESSORS = 4;",
#     "STARTS = 100 50;",
#     "STITERATIONS = 5;",
#     "STSEED = 288;",
#     "",
#     "MODEL:",
#     "%OVERALL%",
#     " ",
#     #"c ON g",
#     paste(cas1, collapse = "\n\n"),
#     " ",
#     "OUTPUT: ",
#     "TECH10",#for bivariate model fit information
#     "TECH11",
#     #"SAMP",
#     #"TECH14;", Time consuming tests
#     #"CINTERVAL",
#     #"SVALUES",
#     ";",
#     "",
#     "SAVEDATA:",
#     paste0("FILE = lca_cl", k,".dat;"),
#     "SAVE = CPROBABILITIES;"
# 
#   ), fileConn)
#   close(fileConn)
# })

#----------------All cycles together by scale--------------------------------------
# for (l in c("ETHN", "GNDR", "IMMI")) { #3 scales
#   
#   f1 <- as.matrix(dplyr::select(dataAll, starts_with(l)))~1
#   
#   cas <- list()
#   
#   lapply(2:4, function(k) { #input file for different number of classes 2-5
#     for(c in 1:k){
#       cas[c] <- paste0("%c#",c,"%")
#     }
#     fileConn <- file(paste0("Mplus/lca_", l, "cl", sprintf("%d", k), ".inp"))
#     writeLines(c(
#       paste("TITLE: LCA", l, k ," classes;"),
#       "DATA: ",
#       paste0("FILE = Dta.dat;"),
#       "",
#       "VARIABLE: ", 
#       paste0("NAMES = ", paste(colnames(dataAll), collapse = "\n"),";"),
#       "IDVARIABLE = IDSTUD;",
#       paste0("USEVARIABLES = ", paste(colnames(dataAll)[grepl(paste0('^',l), colnames(dataAll))], collapse = "\n"),";"),
#       paste0("CATEGORICAL = ", paste(colnames(dataAll)[grepl(paste0('^',l), colnames(dataAll))], collapse = "\n"),";"),
#       "MISSING = .;",
#       paste0("CLASSES = ",sprintf("c(%d);", k)),
#       #paste0("CLASSES = g(",length(unique(dataAll$IDCNTRY)),") ",sprintf("c(%d));", k)),
#       #paste0("KNOWNCLASS = g(IDCNTRY =", paste(unique(dataAll$IDCNTRY), collapse = "\n"),");"),
#       "WEIGHT = SENWGT;",
#       "STRATIFICATION = IDJK;",
#       "CLUSTER = IDCL;",
#       " ",
#       "ANALYSIS:",
#       "TYPE = COMPLEX MIXTURE;",
#       "PROCESSORS = 4;",
#       "STARTS = 100 50;",
#       "STITERATIONS = 5;",
#       "STSEED = 288;",
#       "",
#       "MODEL:",
#       "%OVERALL%", 
#       " ",
#       #"c ON g",
#       paste(cas, collapse = "\n\n"),
#       " ",
#       "OUTPUT: ",
#       "TECH10",#for bivariate model fit information
#       "TECH11",
#       #"SAMP",
#       #"TECH14;", Time consuming tests
#       #"CINTERVAL",
#       #"SVALUES",
#       ";",
#       "",
#       "SAVEDATA:",
#       paste0("FILE = lca", l, "cl", k,".dat;"),
#       "SAVE = CPROBABILITIES;"
#       
#     ), fileConn)
#     close(fileConn)
#   })
# }



#runModels(target = "Mplus", replaceOutfile = "never") modifiedDate

#load("LCA_MplusModels.RData")

# MAllScalesBycycleMplus <- readModels(target = "Mplus", recursive = TRUE, filefilter = "lca_c[1-3]{1}cl*")
# MByScalesBycycleMplus <- readModels(target = "Mplus", recursive = TRUE, filefilter = "lca_c[1-3]{1}[a-z]{4}cl*")
MBAllScalesBycycleMplus <- readModels(target = "Mplus", recursive = TRUE, filefilter = "blca_c[1-3]{1}cl*")

# MAllScalesAllcycleMplus <- readModels(target = "Mplus", recursive = TRUE, filefilter = "lca_cl[1-3]{1}")
# MByScalesAllcycleMplus <- readModels(target = "Mplus", recursive = TRUE, filefilter = "lca_[a-z]{4}cl*")

save(MAllScalesBycycleMplus,  MByScalesBycycleMplus, MBAllScalesBycycleMplus, file="LCA_MplusModels.RData")
#     MAllScalesAllcycleMplus, MByScalesAllcycleMplus, file="LCA_MplusModels.RData")



#------------------------------------------------------------
#-----------------poLCA package------------------------------
#------------------------------------------------------------

# #------ All scales by cycle
# Scalesc <- Scales[!grepl(paste0(c("T_GNDREQ7", "T_ETHNEQ5"), collapse = "|"), Scales)]
# MAllScalesbycycle <- list(c1=list(),c2=list(),c3=list())
# for (j in 1:3) { #input file for each cycle 1:3
#   print(paste0("C",j))
#   # if(j == 1) Scalesc <- Scales[!grepl(paste0(c("T_GNDREQ7", "T_ETHNEQ5"), collapse = "|"), Scales)] else
#   #   if(j == 2) Scalesc <- Scales[!grepl(paste0(c("T_GNDREQ7"), collapse = "|"), Scales)] else
#   #     Scalesc <- Scales
# 
#     data <- ds_lc0 %>%  filter(cycle == paste0("C",j)) %>%
#       dplyr::select(all_of(Id), all_of(sampleID), all_of(Scalesc), -COUNTRY, -cycle)
# 
#     f1 <- as.matrix(dplyr::select(data, starts_with("T_")))~1
# 
#     for(i in 4:7){ #4 to 10 classes
#       lc <- poLCA(f1, data, nclass=i, maxiter=3000,
#                   tol=1e-5, na.rm=FALSE,
#                   nrep=5, verbose=FALSE, calc.se=TRUE)
#       MAllScalesbycycle[[j]][[i]] <- lc
#     }
# }
#Binomial---------------------
Scalesbc <- Scalesb[!grepl(paste0(c("bT_GNDREQ7", "bT_ETHNEQ5"), collapse = "|"), Scalesb)]
MBAllScalesbycycle <- list(c1=list(),c2=list(),c3=list())
for (j in 1:3) { #input file for each cycle 1:3
  print(paste0("C",j))
  # if(j == 1) Scalesc <- Scales[!grepl(paste0(c("T_GNDREQ7", "T_ETHNEQ5"), collapse = "|"), Scales)] else
  #   if(j == 2) Scalesc <- Scales[!grepl(paste0(c("T_GNDREQ7"), collapse = "|"), Scales)] else
  #     Scalesc <- Scales

  data <- ds_lc0 %>%  filter(cycle == paste0("C",j)) %>%
    dplyr::select(all_of(Id), all_of(sampleID), all_of(Scalesbc), -COUNTRY, -cycle)

  f1 <- as.matrix(dplyr::select(data, starts_with("bT_"))+1)~1

  for(i in 4:7){ #4 to 10 classes
    lc <- poLCA(f1, data, nclass=i, maxiter=3000,
                tol=1e-5, na.rm=FALSE,
                nrep=5, verbose=FALSE, calc.se=TRUE)
    MBAllScalesbycycle[[j]][[i]] <- lc
  }
}

#------ By scales and cycles
# Mbyscalebycycle <- list(C1 = list(IMMI = list(), GNDR = list(), ETHN = list()),
#                         C2 = list(IMMI = list(), GNDR = list(), ETHN = list()),
#                         C3 = list(IMMI = list(), GNDR = list(), ETHN = list()))
# for (j in 1:3) { #3 cycles
#   print(paste0("C",j))
#   if(j == 1) Scalesc <- Scales[!grepl(paste0(c("T_GNDREQ7", "T_ETHNEQ5"), collapse = "|"), Scales)] else
#     if(j == 2) Scalesc <- Scales[!grepl(paste0(c("T_GNDREQ7"), collapse = "|"), Scales)] else
#       Scalesc <- Scales
# 
#   data <- ds_lc0 %>%  filter(cycle == paste0("C",j)) %>%
#     dplyr::select(all_of(Id), all_of(sampleID), all_of(Scalesc), -COUNTRY, -cycle)
#    for (k in c("IMMI", "GNDR", "ETHN")) { #3 scales
#     print(paste0(k))
#     f1 <- as.matrix(dplyr::select(data, starts_with(paste0("T_",k,"EQ"))))~1
# 
#     for(i in 2:4){#1 to 5 classes
#       lc <- poLCA(f1, data, nclass=i, maxiter=3000,
#                   tol=1e-5, na.rm=FALSE,
#                   nrep=5, verbose=FALSE, calc.se=TRUE, graphs = TRUE)
#       Mbyscalebycycle[[j]][[k]][[i]] <- lc
#     }
#   }
# }

#------ All scales and cycle
# dataAllR <- ds_lc0 %>%  
#   dplyr::select(all_of(Id), all_of(sampleID), all_of(Scalesc), -COUNTRY, -cycle)
#     
# f1 <- as.matrix(dplyr::select(dataAllR, starts_with("T_")))~1
#     
# MAllScalesCycles <- list()
#   for(i in 4:7){ #4 to 10 classes
#     lc <- poLCA(f1, dataAllR, nclass=i, maxiter=3000, 
#                 tol=1e-5, na.rm=FALSE, 
#                 nrep=5, verbose=FALSE, calc.se=TRUE) 
#     MAllScalesCycles[[i]] <- lc
#   }

#------ By scales all cycles
# MAllcyclebyscale <- list(C1 = list(IMMI = list(), GNDR = list(), ETHN = list()),
#                           C2 = list(IMMI = list(), GNDR = list(), ETHN = list()),
#                           C3 = list(IMMI = list(), GNDR = list(), ETHN = list()))
# for (k in c("IMMI", "GNDR", "ETHN")) { #3 scales
#   print(paste0(k))
#   f1 <- as.matrix(dplyr::select(dataAllR, starts_with(paste0("T_",k,"EQ"))))~1
# 
#   for(i in 2:4){#1 to 5 classes
#     lc <- poLCA(f1, dataAllR, nclass=i, maxiter=3000,
#                 tol=1e-5, na.rm=FALSE,
#                 nrep=5, verbose=FALSE, calc.se=TRUE, graphs = TRUE)
#     MAllcyclebyscale[[k]][[i]] <- lc
#   }
# }


# #------------------------------------------------------------
# #-----------------Each scale with country covariate----------
# 
# MbyscaleCov1 <- list(C1 = list(IMMI = list(), GNDR = list(), ETHN = list()), 
#                      C2 = list(IMMI = list(), GNDR = list(), ETHN = list()),
#                      C3 = list(IMMI = list(), GNDR = list(), ETHN = list()))
# for (j in 1:3) { #3 cycles
#   print(paste0("C",j))
#   if(j == 1) Scalesc <- Scales[!grepl(paste0(c("T_GNDREQ7", "T_ETHNEQ5"), collapse = "|"), Scales)] else
#     if(j == 2) Scalesc <- Scales[!grepl(paste0(c("T_GNDREQ7"), collapse = "|"), Scales)] else
#       Scalesc <- Scales
#     
#     data <- ds_lc0 %>%  filter(cycle == paste0("C",j)) %>% 
#       dplyr::select(all_of(Id), all_of(sampleID), all_of(Scalesc), -cycle)
#     for (k in c("IMMI", "GNDR", "ETHN")) { #3 scales
#       print(paste0("C",j,"\n"))
#       print(paste0(k,"\n"))
#       fcov1 <- as.matrix(dplyr::select(data, starts_with(paste0("T_",k,"EQ")))) ~ COUNTRY
#       
#       for(i in 2:5){#1 to 5 classes
#         lc <- poLCA(fcov1, data, nclass=i, maxiter=3000, 
#                     tol=1e-5, na.rm=FALSE, 
#                     nrep=5, verbose=FALSE, calc.se=TRUE, graphs = TRUE) 
#         MbyscaleCov1[[j]][[k]][[i]] <- lc
#       }
#     }
# }

#load("LCA_RModels.RData")

save(MAllScalesbycycle, Mbyscalebycycle, MBAllScalesbycycle, file="LCA_RModels.RData")
#     MAllScalesCycles, MAllcyclebyscale, 
#     MbyscaleCov1, file="LCA_RModels.RData")


# 
# library(lcca)
# lca.lcca<-lcca::lca(
#   f1,
#   nclass=4,
#   data=ds_lc0,
#   flatten.rhos=1
# )
# summary.lca(lca.lcca, show.all=T)
# groups 
# constrain.rhos
# constrain.gammas
# lca.cov

library(mclust)
height_fit_mclust <- Mclust(height)
summary(height_fit_mclust, parameters = TRUE)

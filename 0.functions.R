library(plyr)

rename_cycle <- function(dataset){
  names2 <- colnames(dataset)
  
  for (idv in 1:length(names2)) {
    if (substr(names2[idv],1,3) %in% c("IS2","IS3","ES2","ES3","LS2","LS3","IT2","IT3","IC2","IC3")) {
      names2[idv] <-  paste0(substr(names2[idv],1,2),substr(names2[idv],4,nchar(names2[idv])))
    }
    if (substr(names2[idv],1,2) %in% c()) {
      names2[idv] <-  paste0(substr(names2[idv],1,2),substr(names2[idv],4,nchar(names2[idv])))
    }
  }
  dataset_r <- dataset %>% rename_at(vars(colnames(dataset)), ~ names2)
}


subchunkify <- function(g, name, fig_height = 7, fig_width = 5) {
  g_deparsed <- paste0(deparse(
                        function() {g}
                      ), collapse = '')
  
  sub_chunk <- paste0("
    `","``{r sub_chunk_", name, ", fig.height = ", fig_height, ", fig.width = ", fig_width, ", echo = FALSE}",
                        "\n(", 
                        g_deparsed
                        , ")()",
                        "\n`","``
    ")
  
  cat(knitr::knit(text = knitr::knit_expand(text = sub_chunk), quiet = TRUE))
}



#------Graph polCA models classes----------
graphclass <- function(cmodel = NULL, nclass = NULL, orden = c(1:length(levels(cmodel$param))), title = NULL){
  a <- levels(cmodel$param)
  a <- a[order(a)[orden]]
  labels <- NULL
  for (each in a){
    labels[each] <- paste(str_remove(attr(ISC_lvRlca[[each]], "label"), 
                                     "Rights and Responsibilities/Rights and responsibilities/|Rights and Responsibilities/Roles women and men/|Moving/<Immigrants> |Moving/<Immigrant> "),
                          "-", each)
  }
  
  labels2 <- NULL
  n <- 0
  for (each in levels(cmodel$category)){
    n <- n + 1
    labels2[each] <- paste(each, "-", levels(ISC_lvRlca[[cmodel[cmodel$category == each, "param"][1]]])[n])
  }
  
  cmodel$paramf <- factor(cmodel$param, levels = a, labels = labels)
  cmodel$categoryf <- factor(cmodel$category, levels = levels(cmodel$category), labels = labels2)
  
  zp1 <- ggplot(cmodel,aes(x = paramf, y = value, fill = categoryf)) + 
    geom_bar(stat = "identity", position = "stack") + 
    facet_grid(Class ~ ., labeller = label_wrap_gen(16)) + 
    #scale_fill_brewer(type="seq", palette="Greys") +
    theme_bw() + 
    ggtitle(title) +
    labs(x = "Items", y = "Response probabilities", fill ="Response category") + 
    theme(legend.position = "top", title = element_text(size=9),
          strip.text.y = element_text(size = 8), 
          axis.text.x=element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6),
          axis.text.y=element_text(size = 7),
          axis.title = element_text(size = 7),
          axis.ticks.y=element_blank(),                    
          panel.grid.major.y=element_blank(), legend.title = element_text(size = 8), 
          legend.key.size = unit(0.3, "cm"),
          legend.text = element_text(size = 8)) +
    scale_x_discrete(label = function(x) str_wrap(x,25)) +
    guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
    scale_y_continuous(breaks = c(0.25,0.5,0.75)) +
    geom_hline(yintercept=c(0.25,0.5,0.75), linetype = "dashed", size = 0.3, color = "gray")
  print(zp1)
}

graphclasstogether <- function(cmodel = NULL, nclass = NULL, orden = c(1:length(levels(cmodel$param))), title = NULL){
  a <- levels(cmodel$param)
  a <- a[order(a)[orden]]
  labels <- NULL
  for (each in a){
    labels[each] <- paste(str_remove(attr(ISC_lvRlca[[each]], "label"), 
                                     "Rights and Responsibilities/Rights and responsibilities/|Rights and Responsibilities/Roles women and men/|Moving/<Immigrants> |Moving/<Immigrant> "),
                          "-", each)
  }
  
  labels2 <- NULL
  n <- 0
  for (each in levels(cmodel$category)){
    n <- n + 1
    labels2[each] <- paste(each, "-", levels(ISC_lvRlca[[cmodel[cmodel$category == each, "param"][1]]])[n])
  }
  
  cmodel$paramf <- factor(cmodel$param, levels = a, labels = labels)
  cmodel$categoryf <- factor(cmodel$category, levels = levels(cmodel$category), labels = labels2)
  
  zp1 <- ggplot(cmodel,aes(x = paramf, y = value, fill = categoryf)) + 
    geom_bar(stat = "identity", position = "stack") + 
    facet_grid(Class ~ Cycle) + 
    #scale_fill_brewer(type="seq", palette="Greys") +
    theme_bw() + 
    ggtitle(title) +
    labs(x = "Items", y = "Response probabilities", fill ="Response category") + 
    theme(legend.position = "top", title = element_text(size=9),
           axis.text.x=element_text(angle = 90, vjust = 0, hjust = 1, size = 7),
           axis.text.y=element_text(size = 7),
           axis.title = element_text(size = 7),
           axis.ticks.y=element_blank(),                    
           panel.grid.major.y=element_blank(), legend.title = element_text(size = 8), 
           legend.key.size = unit(0.3, "cm"),
           legend.text = element_text(size = 8)) + 
    scale_x_discrete(label = function(x) str_wrap(x,25)) +
    guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
    scale_y_continuous(breaks = c(0.25,0.5,0.75)) +
    geom_hline(yintercept=c(0.25,0.5,0.75), linetype = "dashed", size = 0.3, color = "gray")
  print(zp1)
}

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

entropy <- function(p) sum(-p * log(p))

#------Summary of polCA models----------
summaryLCAR2 <- function(Modellist, level1 = 1:3, level2 = 1:5, level3 = 1:5,data = "EU"){
  l0 <- 0
  Fits <- vector(mode = 'list', length = length(level1))
  Sizes <- vector(mode = 'list', length = length(level1))
  
  if (all(!is.na(level3))){
  
    for (l1 in level1) {#cycles#country
      
      l0 <- l0 + 1
      if(data == "NEU" & l1 == 3){
        level2 <- 2
      } else{
        if(l0 == l1){
          level2 <- length(names(Modellist[[l1]]))  
        } else level2 <- length(names(Modellist[level1][[l0]]))  
        
      }
      
      for (l2 in 1:level2) {# scales#years
        
        fit <- data.frame(L1 = NULL, L2 = NULL, NClass = NULL, Chisq = NULL, npar = NULL, loglik = NULL, 
                          Gsq = NULL, BIC = NULL, AIC = NULL)
        sizes <-  data.frame(matrix(NA,1,2*max(level3)+3))
        names(sizes) <- c("L1", "L2", "NClass", paste("P", 1:max(level3), sep = "."), 
                          paste("Pse", 1:max(level3), sep = "."))
        
        for(l3 in level3){#nclasses
          
          fit[l3, "L1"] <- names(Modellist[l1])
          fit[l3, "L2"] <- names(Modellist[l1][[1]])[l2]
          fit[l3, "NClass"] <- l3
          fit[l3, "Chisq"] <- Modellist[[l1]][[l2]][[l3]]$Chisq  
          fit[l3, "npar"] <- Modellist[[l1]][[l2]][[l3]]$npar  
          fit[l3, "loglik"] <- Modellist[[l1]][[l2]][[l3]]$llik  
          fit[l3, "Gsq"] <- Modellist[[l1]][[l2]][[l3]]$Gsq  
          fit[l3, "BIC"] <- Modellist[[l1]][[l2]][[l3]]$bic
          fit[l3, "AIC"] <- Modellist[[l1]][[l2]][[l3]]$aic  
          if(l3 != 1) {
            posteriors <- data.frame(Modellist[[l1]][[l2]][[l3]]$posterior, predclass=Modellist[[l1]][[l2]][[l3]]$predclass)
            classification_table <-  as.matrix(ddply(posteriors, .(predclass), function(x) colSums(x[,1:l3])) )
            fit[l3, "ClassErr"] <- round(1-sum(diag(classification_table[,c(2:(l3+1))])) / sum(classification_table[,c(2:(l3+1))]),3)
            
            error_prior <- entropy(Modellist[[l1]][[l2]][[l3]]$P) # Class proportions
            error_post <- mean(apply(Modellist[[l1]][[l2]][[l3]]$posterior, 1, entropy), na.rm = TRUE)
            fit[l3, "R2_entropy"] <- round((error_prior - error_post) / error_prior,3)
          }
          
          
          sizes[l3, "L1"] <- names(Modellist[l1])
          sizes[l3, "L2"] <- names(Modellist[l1][[1]])[l2]
          sizes[l3, "NClass"] <- l3
          sizes[l3, names(sizes) %in% paste("P", 1:l3, sep = ".")] <- round(Modellist[[l1]][[l2]][[l3]]$P,3)
          if(l3 != 1) sizes[l3, names(sizes) %in% paste("Pse", 1:l3, sep = ".")] <- round(Modellist[[l1]][[l2]][[l3]]$P.se,3)
        }
        fit$chg_Gsq <- round((fit$Gsq - fit$Gsq[1])/fit$Gsq[1],3)
   
        Fits[[l0]][[l2]] <- fit %>% dplyr::select(L1, L2, NClass, Chisq, npar, loglik, Gsq, chg_Gsq, BIC, AIC, R2_entropy, ClassErr)  
        Sizes[[l0]][[l2]] <- sizes[level3,] 
      }
      if(data == "NEU" & l1 == 3){
        names(Fits[[l0]]) <- names(Modellist[l1][[1]])[c(1,2)]
        names(Sizes[[l0]]) <- names(Modellist[l1][[1]])[c(1,2)]} else {
          names(Fits[[l0]]) <- names(Modellist[l1][[1]])
          names(Sizes[[l0]]) <- names(Modellist[l1][[1]])    
        }
      
      
      names(Fits) <- names(Modellist)[level1]
      names(Sizes) <- names(Modellist)[level1]
    }
  } else if (all(!is.na(level2) & is.na(level3))){
    for (l11 in level1) {
      l0 <- l0 + 1
      fit <- data.frame(L1 = NULL, NClass = NULL, Chisq = NULL, npar = NULL, loglik = NULL, Gsq = NULL, BIC = NULL, AIC = NULL)
      sizes <-  data.frame(matrix(NA,1,2*max(level2)+2))
      names(sizes) <- c("L1", "NClass", paste("P", 1:max(level2), sep = "."), 
                        paste("Pse", 1:max(level2), sep = "."))
      
      for (l21 in 1:max(level2)) {
        
        fit[l21, "L1"] <- names(Modellist[l11])
        fit[l21, "NClass"] <- l21
        fit[l21, "Chisq"] <- Modellist[[l11]][[l21]]$Chisq  
        fit[l21, "npar"] <- Modellist[[l11]][[l21]]$npar  
        fit[l21, "loglik"] <- Modellist[[l11]][[l21]]$llik  
        fit[l21, "Gsq"] <- Modellist[[l11]][[l21]]$Gsq  
        fit[l21, "BIC"] <- Modellist[[l11]][[l21]]$bic
        fit[l21, "AIC"] <- Modellist[[l11]][[l21]]$aic  
        if(l21 != 1) {
          posteriors <- data.frame(Modellist[[l11]][[l21]]$posterior, predclass=Modellist[[l11]][[l21]]$predclass)
          classification_table <-  as.matrix(ddply(posteriors, .(predclass), function(x) colSums(x[,1:l21])) )
          fit[l21, "ClassErr"] <- round(1-sum(diag(classification_table[,c(2:(l21+1))])) / sum(classification_table[,c(2:(l21+1))]),3)
          
          error_prior <- entropy(Modellist[[l11]][[l21]]$P) # Class proportions
          error_post <- mean(apply(Modellist[[l11]][[l21]]$posterior, 1, entropy), na.rm = TRUE)
          fit[l21, "R2_entropy"] <- round((error_prior - error_post) / error_prior,3)
        }
        sizes[l21, "L1"] <- names(Modellist[l11])
        sizes[l21, "NClass"] <- l21
        sizes[l21, names(sizes) %in% paste("P", 1:l21, sep = ".")] <- round(Modellist[[l11]][[l21]]$P,3)
        if(l21 != 1) sizes[l21, names(sizes) %in% paste("Pse", 1:l21, sep = ".")] <- round(Modellist[[l11]][[l21]]$P.se,3)
      }
      fit$chg_Gsq <- round((fit$Gsq - fit$Gsq[1])/fit$Gsq[1],3)
      
      Fits[[l0]] <- fit %>% dplyr::select(L1, NClass, Chisq, npar, loglik, Gsq, chg_Gsq, BIC, AIC, R2_entropy, ClassErr)  
      Sizes[[l0]] <- sizes[level2,] 
  
      names(Fits) <- names(Modellist)
      names(Sizes) <- names(Modellist)
    }
  } else if (all(is.na(level2) & is.na(level3))){
    
    fit <- data.frame(NClass = NULL, Chisq = NULL, npar = NULL, loglik = NULL, Gsq = NULL, BIC = NULL, AIC = NULL)
    sizes <-  data.frame(matrix(NA,1,2*max(level1)+1))
    names(sizes) <- c("NClass", paste("P", 1:max(level1), sep = "."), 
                      paste("Pse", 1:max(level1), sep = "."))
    
    for (l12 in level1) {
      fit[l12, "NClass"] <- l12
      fit[l12, "Chisq"] <- Modellist[[l12]]$Chisq  
      fit[l12, "npar"] <- Modellist[[l12]]$npar  
      fit[l12, "loglik"] <- Modellist[[l12]]$llik  
      fit[l12, "Gsq"] <- Modellist[[l12]]$Gsq  
      fit[l12, "BIC"] <- Modellist[[l12]]$bic
      fit[l12, "AIC"] <- Modellist[[l12]]$aic  
      if(l12 != 1) {
        posteriors <- data.frame(Modellist[[l12]]$posterior, predclass=Modellist[[l12]]$predclass)
        classification_table <-  as.matrix(ddply(posteriors, .(predclass), function(x) colSums(x[,1:l12])) )
        fit[l12, "ClassErr"] <- round(1-sum(diag(classification_table[,c(2:(l12+1))])) / sum(classification_table[,c(2:(l12+1))]),3)
        
        error_prior <- entropy(Modellist[[l12]]$P) # Class proportions
        error_post <- mean(apply(Modellist[[l12]]$posterior, 1, entropy), na.rm = TRUE)
        fit[l12, "R2_entropy"] <- round((error_prior - error_post) / error_prior,3)  
      }
      sizes[l12, "NClass"] <- l12
      sizes[l12, names(sizes) %in% paste("P", 1:l12, sep = ".")] <- round(Modellist[[l12]]$P,3)
      if(l12 != 1) sizes[l12, names(sizes) %in% paste("Pse", 1:l12, sep = ".")] <- round(Modellist[[l12]]$P.se,3)
    }
    fit$chg_Gsq <- round((fit$Gsq - fit$Gsq[1])/fit$Gsq[1],3)
    
    Fits[[l12]] <- fit %>% dplyr::select(NClass, Chisq, npar, loglik, Gsq, chg_Gsq, BIC, AIC, R2_entropy, ClassErr)  
    Sizes[[l12]] <- sizes[level1,]
  }
  Results <- list(Fits = Fits, Size = Sizes)
  return(Results)
}

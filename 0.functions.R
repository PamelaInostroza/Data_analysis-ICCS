
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
graphclass <- function(cmodel = NULL, nclass = NULL, title = NULL){
  
  labels <- NULL
  for (each in levels(cmodel$param)){
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
  
  cmodel$paramf <- factor(cmodel$param, levels = levels(cmodel$param), labels = labels)
  cmodel$categoryf <- factor(cmodel$category, levels = levels(cmodel$category), labels = labels2)
  
  zp1 <- ggplot(cmodel,aes(x = paramf, y = value, fill = categoryf)) + 
    geom_bar(stat = "identity", position = "stack") + 
    facet_grid(Class ~ .) + 
    scale_fill_brewer(type="seq", palette="Greys", direction = -1) +
    theme_bw() + 
    ggtitle(title) +
    labs(x = "Items", y = "Response probabilities", fill ="Response category") + 
    theme( legend.position = "top",
           axis.text.x=element_text(angle = 90, vjust = 0, hjust = 1, size = 8),
           axis.ticks.y=element_blank(),                    
           panel.grid.major.y=element_blank(), legend.title = element_text(size = 8), 
           legend.key.size = unit(0.5, "cm"),
           legend.text = element_text(size = 8)) + 
    scale_x_discrete(label = function(x) str_wrap(x,35)) +
    guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
    scale_y_continuous(breaks = c(0.25,0.5,0.75)) +
    geom_hline(yintercept=c(0.25,0.5,0.75), linetype = "dashed", size = 0.3, color = "gray")
  print(zp1)
}



#------Summary of polCA models----------
summaryLCAR2 <- function(Modellist, level1 = 1:3, level2 = 1:5, level3 = 1:5){
  if (all(!is.na(level3))){
    for (l1 in level1) {
      for (l2 in level2) {
        fit <- data.frame(L1 = NULL, L2 = NULL, NClass = NULL, Chisq = NULL, npar = NULL, loglik = NULL, Gsq = NULL, BIC = NULL, AIC = NULL)
        sizes <-  data.frame(matrix(NA,1,2*max(level3)+3))
        names(sizes) <- c("L1", "L2", "NClass", paste("P", 1:max(level3), sep = "."), 
                          paste("Pse", 1:max(level3), sep = "."))
        
        for(l3 in level3){
          
          fit[l3, "L1"] <- names(Modellist[l1])
          fit[l3, "L2"] <- names(Modellist[l1][[1]])[l2]
          fit[l3, "NClass"] <- l3
          fit[l3, "Chisq"] <- Modellist[[l1]][[l2]][[l3]]$Chisq  
          fit[l3, "npar"] <- Modellist[[l1]][[l2]][[l3]]$npar  
          fit[l3, "loglik"] <- Modellist[[l1]][[l2]][[l3]]$llik  
          fit[l3, "Gsq"] <- Modellist[[l1]][[l2]][[l3]]$Gsq  
          fit[l3, "BIC"] <- Modellist[[l1]][[l2]][[l3]]$bic
          fit[l3, "AIC"] <- Modellist[[l1]][[l2]][[l3]]$aic  
          
          sizes[l3, "L1"] <- names(Modellist[l1])
          sizes[l3, "L2"] <- names(Modellist[l1][[1]])[l2]
          sizes[l3, "NClass"] <- l3
          sizes[l3, names(sizes) %in% paste("P", 1:l3, sep = ".")] <- round(Modellist[[l1]][[l2]][[l3]]$P,3)
          if(l3 != 1) sizes[l3, names(sizes) %in% paste("Pse", 1:l3, sep = ".")] <- round(Modellist[[l1]][[l2]][[l3]]$P.se,3)
        }
        fit %>% na.omit() %>% knitr::kable(caption = paste("Model fit", names(Modellist[l1]), names(Modellist[l1][[1]])[l2]), row.names = FALSE) %>% print() 
        sizes[level3,] %>% 
          knitr::kable(caption = paste("Size (s.e) of each latent class", names(Modellist[l1]), names(Modellist[l1][[1]])[l2])) %>% print() 
      }
    }
  } else if (all(!is.na(level2) & is.na(level3))){
    for (l11 in level1) {
      fit <- data.frame(L1 = NULL, NClass = NULL, Chisq = NULL, npar = NULL, loglik = NULL, Gsq = NULL, BIC = NULL, AIC = NULL)
      sizes <-  data.frame(matrix(NA,1,2*max(level2)+2))
      names(sizes) <- c("L1", "NClass", paste("P", 1:max(level2), sep = "."), 
                        paste("Pse", 1:max(level2), sep = "."))
      
      for (l21 in level2) {
        
        fit[l21, "L1"] <- names(Modellist[l11])
        fit[l21, "NClass"] <- l21
        fit[l21, "Chisq"] <- Modellist[[l11]][[l21]]$Chisq  
        fit[l21, "npar"] <- Modellist[[l11]][[l21]]$npar  
        fit[l21, "loglik"] <- Modellist[[l11]][[l21]]$llik  
        fit[l21, "Gsq"] <- Modellist[[l11]][[l21]]$Gsq  
        fit[l21, "BIC"] <- Modellist[[l11]][[l21]]$bic
        fit[l21, "AIC"] <- Modellist[[l11]][[l21]]$aic  
        
        sizes[l21, "L1"] <- names(Modellist[l11])
        sizes[l21, "NClass"] <- l21
        sizes[l21, names(sizes) %in% paste("P", 1:l21, sep = ".")] <- round(Modellist[[l11]][[l21]]$P,3)
        if(l21 != 1) sizes[l21, names(sizes) %in% paste("Pse", 1:l21, sep = ".")] <- round(Modellist[[l11]][[l21]]$P.se,3)
      }
      fit %>%  na.omit() %>% knitr::kable(caption = paste("Model fit", names(Modellist[l11])), row.names = FALSE) %>% print() 
      sizes[level2,] %>% knitr::kable(caption = paste("Size (s.e) of each latent class", names(Modellist[l11]))) %>% print() 
    }
  } else if (all(is.na(level2) & is.na(level3))){
    fit <- data.frame(NClass = NULL, Chisq = NULL, npar = NULL, loglik = NULL, Gsq = NULL, BIC = NULL, AIC = NULL)
    sizes <-  data.frame(matrix(NA,1,2*max(level1)+3))
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
      
      sizes[l12, "NClass"] <- l12
      sizes[l12, names(sizes) %in% paste("P", 1:l12, sep = ".")] <- round(Modellist[[l12]]$P,3)
      if(l12 != 1) sizes[l12, names(sizes) %in% paste("Pse", 1:l12, sep = ".")] <- round(Modellist[[l12]]$P.se,3)
    }
    fit %>%  na.omit() %>% knitr::kable(caption = paste("Model fit All cycles and All scales"), row.names = FALSE) %>% print() 
    sizes[level1,] %>% knitr::kable(caption = paste("Size (s.e) of each latent class All merged")) %>% print() 
  }
}


setwd("C:/Users/pamel/OneDrive - KU Leuven/Master in Statistics/Master Thesis/Data_analysis")

  index  <- Scales %>% filter(item == "index")
  index1 <- index %>% dplyr::select(item, CIVED_1999, ICCS_2009, ICCS_2016) %>% 
    pivot_longer(-item) %>% 
    mutate(C = ifelse(value %in% c("IMMIGMLE", "IMMRGHT", "E_IMMRGHT"), index$D3[1], ifelse(value %in% c("WOMRTMLE", "GENEQL", "S_GENEQL"), index$D3[2],
               ifelse(value %in% c("ETHRGHT", "S_ETHRGHT"), index$D3[3], NA)))) #"MINORMLE", is not in CIVED1999
  
  items <- Scales %>% filter(item != "index") %>% dplyr::select(item, CIVED_1999, ICCS_2009, ICCS_2016) %>% 
    pivot_longer(-item) %>% 
    mutate(Construct = ifelse(substr(item,1,1) == "I", paste0(index$D3[1]), 
                              ifelse(substr(item,1,1) == "G", paste0(index$D3[2]), paste0(index$D3[3])))) 
  
 
  for (j in 1:length(unique(items$Construct))){
    cat("  \n")
    cat("  \n")
    cat(paste0('## Construct: ', unique(items$Construct)[j], '  \n'))
    cat("  \n")
    ha <- items %>% filter(Construct == unique(items$Construct)[j])
      for (l in 1:length(unique(ha$item))) {
        
    		hl <- ha %>% filter(item == unique(ha$item)[l]) %>% na.omit()
    		constr_name <- hl$value
    		cat("  \n")
    		cat("  \n")
    		cat(paste('### Items:', paste(constr_name,collapse = ", "),'  \n'))
    		
    		for (i in 1:length(hl$name)){
    			cat("  \n")
    			if (hl[i, "name"] == "CIVED_1999"){
    			  cat('#### Responses Cycle 1 - CivED1999  \n')
    			  data <- ISC %>% dplyr::select(COUNTRY, SENWGT_Gc1, all_of(constr_name[i]), GENDER) %>%
    								mutate_at(c(constr_name[i],"GENDER"), as_factor) %>%
    			          mutate(GENDER = factor(GENDER, levels = c("Male", "Female"), labels = c("Boy", "Girl"))) %>% 
    								na.omit()
    			  
    			  g1 <- data %>%
    					ggplot(aes_string(x = constr_name[i], y = "..prop..", group = "COUNTRY", fill = "COUNTRY")) +
    							geom_bar(aes(weight = SENWGT_Gc1), alpha = 0.5) +
    							geom_bar(aes(weight = SENWGT_Gc1), data = transform(data, GENDER = "Total"), alpha = 0.5) +
    							facet_grid(COUNTRY ~ GENDER, switch = "y") +
    							scale_fill_discrete(guide = FALSE) +
    							ggtitle(str_wrap(tolower(eval(parse(text=paste0("attributes(data$",constr_name[i],")$label")))), 60)) +
    							ylab("Weighted proportion of responses") +
    							xlab(paste0(constr_name[i],"- CIVED_1999"))+
    							geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), y= ..prop.. ), stat= "count", vjust = -.5, size = 2) +
    							geom_text(data = transform(data, GENDER = "Total"), aes(label = scales::percent(..prop.., accuracy = 0.1), y= ..prop.. ), stat= "count", vjust = -.5, size = 2) +
    							scale_y_continuous(labels = scales::percent_format(), limits = c(0,1)) +
    			        scale_x_discrete(label = function(x) str_wrap(x,10)) +
    			        theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) 
    			print(g1)
    			} else if (hl[i, "name"] == "ICCS_2009"){
    			  cat("  \n")
    			  cat("  \n")
    			  cat("#### Responses Cycle 2 - ICCS2009  \n")
    			  data <- ISC %>% dplyr::select(COUNTRY, SENWGT_Gc2, all_of(constr_name[i]), SGENDER) %>%
    								mutate_at(c(constr_name[i],"SGENDER"), as_factor) %>%
    								na.omit()
    			  g2 <- data %>%
    					ggplot(aes_string(x = constr_name[i], y = "..prop..", group = "COUNTRY", fill = "COUNTRY")) +
    							geom_bar(aes(weight = SENWGT_Gc2), alpha = 0.5) +
    							geom_bar(aes(weight = SENWGT_Gc2), data = transform(data, SGENDER = "Total"), alpha = 0.5) +
    							facet_grid(COUNTRY ~ SGENDER, switch = "y") +
    							scale_fill_discrete(guide = FALSE) +
    							ggtitle(str_wrap(tolower(eval(parse(text=paste0("attributes(data$",constr_name[i],")$label")))), 60)) +
    							ylab("Weighted proportion of responses") +
    							xlab(paste0(constr_name[i],"- ICCS2009")) +
    							geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), y= ..prop.. ), stat= "count", vjust = -.5, size = 2) +
    							geom_text(data = transform(data, SGENDER = "Total"), aes(label = scales::percent(..prop.., accuracy = 0.1), y= ..prop.. ), stat= "count", vjust = -.5, size = 2) +
    							scale_y_continuous(labels = scales::percent_format(), limits = c(0,1)) +
    			        scale_x_discrete(label = function(x) str_wrap(tolower(x),10)) +
    			        theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
    			print(g2)
    			} else if (hl[i, "name"] == "ICCS_2016"){
    			  cat("  \n")
    			  cat("  \n")
    			  cat("#### Responses Cycle 3 - ICCS2016  \n")
    			  data <- ISC %>% dplyr::select(COUNTRY, SENWGT_Gc3, all_of(constr_name[i]), S_GENDER) %>%
    								mutate_at(c(constr_name[i],"S_GENDER"), as_factor) %>%
    								na.omit()
    			  g3 <- data %>%
    					ggplot(aes_string(x = str_wrap(constr_name[i],5), y = "..prop..", group = "COUNTRY", fill = "COUNTRY")) +
    							geom_bar(aes(weight = SENWGT_Gc3), alpha = 0.5) +
    							geom_bar(aes(weight = SENWGT_Gc3), data = transform(data, S_GENDER = "Total"), alpha = 0.5) +
    							facet_grid(COUNTRY ~ S_GENDER, switch = "y") +
    							scale_fill_discrete(guide = FALSE) +
    							ggtitle(str_wrap(tolower(eval(parse(text=paste0("attributes(data$",constr_name[i],")$label")))), 60)) +
    							ylab("Weighted proportion of responses") +
    							xlab(paste0(constr_name[i],"- ICCS2016"))+
    							geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), y= ..prop.. ), stat= "count", vjust = -.5, size = 2) +
    							geom_text(data = transform(data, S_GENDER = "Total"), aes(label = scales::percent(..prop.., accuracy = 0.1), y= ..prop.. ), stat= "count", vjust = -.5, size = 2) +
    							scale_y_continuous(labels = scales::percent_format(), limits = c(0,1)) +
    			        scale_x_discrete(label = function(x) str_wrap(x,10)) +
    			        theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
    			print(g3)
    			}  
    		}
      } 
    cat("  \n")
    cat("  \n")
    cat(paste('### Index:', paste(unique(index$D3)[j],collapse = ", "),'  \n'))
    cat("  \n")
    cat("  \n")
    cat("Distribution of the index by country with boxplot. Include dotted line with weighted mean for each subgroup and Total.  \n")
    cat("  \n")
    cat("  \n")
    hai <- index1 %>%  filter(C == unique(index$D3)[j]) 
    for (h in 1:nrow(hai)){
      ind_name <- hai[h,"value"]$value
      if (hai[h, "name"] == "CIVED_1999"){
        
        data <- ISC %>% dplyr::select(COUNTRY, SENWGT_Gc1, all_of(ind_name), GENDER) %>%
          mutate_at(c("GENDER", "COUNTRY"), as_factor) %>%
          mutate(GENDER = factor(GENDER, levels = c("Male","Female"), labels = c("Boy", "Girl"))) %>% 
          na.omit()
        me <- data %>% dplyr::select(SENWGT_Gc1, all_of(ind_name)) %>% summarise_at(ind_name, list(~ weighted.mean(., data$SENWGT_Gc1))) %>% data.frame() %>% pull
        mg <- data %>% dplyr::select(GENDER, SENWGT_Gc1, all_of(ind_name)) %>% group_by(GENDER) %>% 
          summarise_at(ind_name, list(~ weighted.mean(., SENWGT_Gc1))) %>% mutate(GENDER = as.factor(GENDER)) %>% data.frame()
        g4 <- data %>% 
          ggplot(aes_string(x = ind_name, y = paste("reorder(COUNTRY, desc(",ind_name,"), mean)"), group = "COUNTRY", fill = "COUNTRY")) +
          geom_violin(aes(weight = SENWGT_Gc1), alpha = 0.5) +
          geom_violin(aes(weight = SENWGT_Gc1), data = transform(data, GENDER = "Total"), alpha = 0.5) +
          geom_boxplot(aes(weight = SENWGT_Gc1), width=0.1) +
          geom_boxplot(aes(weight = SENWGT_Gc1), data = transform(data, GENDER = "Total"), width=0.1) +
          geom_vline(aes_string(xintercept = ind_name), mg, linetype="dotted", size = 0.8) +
          geom_vline(aes(xintercept = me), data = transform(data, GENDER = "Total"), linetype="dotted", size = 0.8) +
          facet_grid(. ~ GENDER) +
          ggtitle(str_wrap(tolower(eval(parse(text=paste0("attributes(data$", ind_name,")$label")))), 60)) +
          ylab("Distribution of derived scale") +
          xlab(paste0(ind_name,"- CIVED1999")) +
          scale_color_brewer(palette="Accent") +
          theme(legend.position = "none") 
        print(g4)
      } else if (hai[h, "name"] == "ICCS_2009"){
        data <- ISC %>% dplyr::select(COUNTRY, SENWGT_Gc2, all_of(ind_name), SGENDER) %>%
          mutate_at(c("SGENDER"), as_factor) %>%
          na.omit()
        
        me <- data %>% dplyr::select(SENWGT_Gc2, all_of(ind_name)) %>% summarise_at(ind_name, list(~ weighted.mean(., data$SENWGT_Gc2))) %>% data.frame() %>% pull
        mg <- data %>% dplyr::select(SGENDER, SENWGT_Gc2, all_of(ind_name)) %>% group_by(SGENDER) %>% 
          summarise_at(ind_name, list(~ weighted.mean(., SENWGT_Gc2))) %>% mutate(SGENDER = as.factor(SGENDER)) %>% data.frame()
        g5 <- data %>% 
          ggplot(aes_string(x = ind_name, y = paste("reorder(COUNTRY, desc(",ind_name,"), mean)"), group = "COUNTRY", fill = "COUNTRY")) +
          geom_violin(aes(weight = SENWGT_Gc2), alpha = 0.5) +
          geom_violin(aes(weight = SENWGT_Gc2), data = transform(data, SGENDER = "Total"), alpha = 0.5) +
          geom_boxplot(aes(weight = SENWGT_Gc2), width=0.1) +
          geom_boxplot(aes(weight = SENWGT_Gc2), data = transform(data, SGENDER = "Total"), width=0.1) +
          geom_vline(aes_string(xintercept = ind_name), mg, linetype="dotted", size = 0.8) +
          geom_vline(aes(xintercept = me), data = transform(data, SGENDER = "Total"), linetype="dotted", size = 0.8) +
          facet_grid(. ~ SGENDER) +
          ggtitle(str_wrap(tolower(eval(parse(text=paste0("attributes(data$", ind_name,")$label")))), 60)) +
          ylab("Distribution of derived scale") +
          xlab(paste0(ind_name,"- ICCS_2009")) +
          scale_color_brewer(palette="Accent") +
          theme(legend.position = "none")
        print(g5)
      } else if (hai[h, "name"] == "ICCS_2016"){
        data <- ISC %>% dplyr::select(COUNTRY, SENWGT_Gc3, all_of(ind_name), S_GENDER) %>%
          mutate_at(c("S_GENDER"), as_factor) %>%
          na.omit()
        
        me <- data %>% dplyr::select(SENWGT_Gc3, all_of(ind_name)) %>% summarise_at(ind_name, list(~ weighted.mean(., data$SENWGT_Gc3))) %>% data.frame() %>% pull
        mg <- data %>% dplyr::select(S_GENDER, SENWGT_Gc3, all_of(ind_name)) %>% group_by(S_GENDER) %>% 
          summarise_at(ind_name, list(~ weighted.mean(., SENWGT_Gc3))) %>% mutate(S_GENDER = as.factor(S_GENDER)) %>% data.frame()
        g6 <- data %>% 
          ggplot(aes_string(x = ind_name, y = paste("reorder(COUNTRY, desc(",ind_name,"), mean)"), group = "COUNTRY", fill = "COUNTRY")) +
          geom_violin(aes(weight = SENWGT_Gc3), alpha = 0.5) +
          geom_violin(aes(weight = SENWGT_Gc3), data = transform(data, S_GENDER = "Total"), alpha = 0.5) +
          geom_boxplot(aes(weight = SENWGT_Gc3), width=0.1) +
          geom_boxplot(aes(weight = SENWGT_Gc3), data = transform(data, S_GENDER = "Total"), width=0.1) +
          geom_vline(aes_string(xintercept = ind_name), mg, linetype="dotted", size = 0.8) +
          geom_vline(aes(xintercept = me), data = transform(data, S_GENDER = "Total"), linetype="dotted", size = 0.8) +
          facet_grid(. ~ S_GENDER) +
          ggtitle(str_wrap(tolower(eval(parse(text=paste0("attributes(data$", ind_name,")$label")))), 60)) +
          ylab("Distribution of derived scale") +
          xlab(paste0(ind_name,"- ICCS_2016")) +
          scale_color_brewer(palette="Accent") +
          theme(legend.position = "none")
        print(g6)
      } 
    }
  }
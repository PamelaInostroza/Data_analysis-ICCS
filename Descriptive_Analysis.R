for (j in 1:3){
    cat("  \n")
    cat(paste0('## Construct: ', constr[j], '  \n'))
    cat("  \n")
    cat('### Responses Cycle 1 - CivED1999  \n')
    constr_name <- VarsToUse %>% filter(Construct == constr[j]) %>%  dplyr::select(VariableC1) %>% na.omit() %>% pull()
    print(paste("Items:", paste(constr_name,collapse = ", ")))
    for (i in 1:length(constr_name)){
      print(ICCS %>% dplyr::select(COUNTRY,TOTWGT, constr_name[i]) %>%
              mutate_at(constr_name[i], as_factor) %>%
              mutate_at(constr_name[i], str_wrap, 10) %>%
            na.omit() %>%
            ggplot(aes_string(x = constr_name[i], y = "..prop..", group = "COUNTRY", fill = "COUNTRY")) +
            geom_bar(aes(weight = TOTWGT)) +
            facet_grid(. ~ COUNTRY) +
            scale_fill_discrete(guide = FALSE) +
            ggtitle(str_wrap(eval(parse(text=paste0("attributes(ICCS$",constr_name[i],")$label"))), 60)) +
            ylab("Weighted proportion of responses") +
            geom_text(aes(label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) +
            scale_y_continuous(labels = scales::percent_format(), limits = c(0,1)) +
            theme_bw() )
    }
    cat("  \n")
    cat("  \n")
    cat("### Responses Cycle 2 - ICCS2009  \n")
    constr_name <- VarsToUse %>% filter(Construct == constr[j]) %>%  dplyr::select(VariableC2) %>% na.omit() %>% pull()
    print(paste("Items:", paste(constr_name,collapse = ", ")))
    for (i in 1:length(constr_name)){
      print(ICCS %>% dplyr::select(COUNTRY,TOTWGTS, constr_name[i]) %>%
              mutate_at(constr_name[i], as_factor) %>%
              mutate_at(constr_name[i], str_wrap, 10) %>%
              na.omit() %>%
              ggplot(aes_string(x = constr_name[i], y = "..prop..", group = "COUNTRY", fill = "COUNTRY")) +
              geom_bar(aes(weight = TOTWGTS)) +
              facet_grid(. ~ COUNTRY) +
              scale_fill_discrete(guide = FALSE) +
              ggtitle(str_wrap(eval(parse(text=paste0("attributes(ICCS$",constr_name[i],")$label"))), 60)) +
              ylab("Weighted proportion of responses") +
              geom_text(aes(label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) +
              scale_y_continuous(labels = scales::percent_format(), limits = c(0,1)) +
              theme_bw() )
    }
    cat("  \n")
    cat("  \n")
    cat("### Responses Cycle 3 - ICCS2016  \n")
    constr_name <- VarsToUse %>% filter(Construct == constr[j]) %>%  dplyr::select(VariableC3) %>% na.omit() %>% pull()
    print(paste("Items:", paste(constr_name,collapse = ", ")))
    for (i in 1:length(constr_name)){
      print(ICCS %>% dplyr::select(COUNTRY,TOTWGTS, constr_name[i]) %>%
              mutate_at(constr_name[i], as_factor) %>%
              mutate_at(constr_name[i], str_wrap, 10) %>%
              na.omit() %>%
              ggplot(aes_string(x = str_wrap(constr_name[i],5), y = "..prop..", group = "COUNTRY", fill = "COUNTRY")) +
              geom_bar(aes(weight = TOTWGTS)) +
              facet_grid(. ~ COUNTRY) +
              scale_fill_discrete(guide = FALSE) +
              ggtitle(str_wrap(eval(parse(text=paste0("attributes(ICCS$",constr_name[i],")$label"))), 60)) +
              ylab("Weighted proportion of responses") +
              geom_text(aes(label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) +
              scale_y_continuous(labels = scales::percent_format(), limits = c(0,1)) +
              theme_bw() )
    }
    cat("  \n")
    
}

#ISGC2 %>% dplyr::select(COUNTRY, constr_name[i])  %>%  na.omit() %>% table()
#ICCS %>% dplyr::select(COUNTRY, constr_name[i])  %>%  na.omit() %>% table()
#round(proportions(table(ICCS$IS2P26A, ICCS$COUNTRY), 2),2)

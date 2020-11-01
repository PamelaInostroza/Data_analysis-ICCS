# Colnames
c <- colnames(ICCS)
constr1 <- "Attitudes toward equal rights for immigrants"
constr2 <- "Attitudes toward gender equality"
constr3 <- "Attitudes toward equal rights for all ethnic/racial groups"


attimmc1_name <- VarsToUse %>% filter(Construct == constr1) %>%  dplyr::select(VariableC1) %>% pull()
for (i in 1:length(attimmc1_name)){
  print(ICCS %>% dplyr::select(COUNTRY,TOTWGT, attimmc1_name[i]) %>% 
          mutate_at(attimmc1_name[i], as_factor) %>% 
        na.omit() %>% 
        ggplot(aes_string(x = attimmc1_name[i], y = "..prop..", group = "COUNTRY", fill = "COUNTRY")) +
        geom_bar(aes(weight = TOTWGT)) +
        facet_grid(. ~ COUNTRY) +
        scale_fill_discrete(guide = FALSE) +
        ggtitle(eval(parse(text=paste0("attributes(ICCS$",attimmc1_name[i],")$label")))) +
        ylab("Weighted proportion of responses") +
        geom_text(aes(label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) +
        scale_y_continuous(labels = scales::percent_format(), limits = c(0,1)) + 
        theme_bw() )
}

ICCS %>% dplyr::select(BS4H1,COUNTRY, TOTWGT, cycle)
round(proportions(table(ICCS$BS4H1, ICCS$COUNTRY), 2),2)

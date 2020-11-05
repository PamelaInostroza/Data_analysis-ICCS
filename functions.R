
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
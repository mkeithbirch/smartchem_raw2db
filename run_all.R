library(knitr)
library(rmarkdown)

render(knit_root_dir = file.path("..", "."), 
       input = file.path("rmds", "data_processing.Rmd"), 
       output_format = "html_document", 
       output_dir = file.path("reports"), 
       output_file = paste0("data_processing_", format(Sys.time(), format = "%Y%m%d%H%M"), ".html"))

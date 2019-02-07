sc_absdups_plot <- function(sc_run_file) {
  
  absdups_plot <- 
    sc_run_file$dups %>% 
    ggplot(aes(RunTime, abs_diff_conc)) + 
    geom_point(aes(SampleID = SampleID)) + 
    geom_line() + 
    geom_hline(yintercept = 0, color = "grey") +
    ggtitle(label = paste(sc_run_file$method, "- Absolute difference of Dups", sc_run_file$run_date)) 
  
  absdups_plotly <- ggplotly(absdups_plot) 
  
  absdups_plotly
}
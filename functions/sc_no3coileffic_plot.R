sc_no3coileffic_plot <- function(sc_run_file) {
  
  coileffic_plot <- 
    sc_run_file$controls %>% 
    filter(SampleID == "CCN3") %>% 
    ggplot(aes(RunTime, coil_effic)) +
    geom_point() +
    geom_line() +
    geom_smooth(method = "lm", se = F, show.legend = T) +
    geom_hline(yintercept = mean(sc_run_file$controls$coil_effic, na.rm = T), linetype = 2, color = "blue", show.legend = T) +
    ggtitle(label = paste(sc_run_file$method, "- NO3 Module Coil Efficiency", sc_run_file$run_date)) + 
    xlab("Runtime") +
    ylab("Coil efficiency")
  
  coileffic_plotly <- 
    ggplotly(coileffic_plot)
  
  coileffic_plotly
}
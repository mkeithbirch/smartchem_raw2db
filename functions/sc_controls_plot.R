sc_controls_plot <- function(sc_run_file) {

  controls_plot <- 
    sc_run_file$controls %>% 
    ggplot(aes(RunTime, Concentration, color = SampleID)) +
    geom_point() +
    geom_line() +
    geom_smooth(method = "lm", se = F) +
    ggtitle(label = paste(sc_run_file$method, "- Controls", sc_run_file$run_date)) + 
    xlab("Runtime") +
    ylab("Concentration") +
    #theme(legend.position = "top") +
    geom_hline(yintercept = unique(sc_run_file$controls$Nominal), linetype = 2)
  
  controls_plotly <- 
    ggplotly(controls_plot) %>% 
    layout(legend = list(orientation = "h", y = 1.05))
  
  controls_plotly
}
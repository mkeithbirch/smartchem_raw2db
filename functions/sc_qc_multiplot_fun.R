sc_qc_multiplot_fun <- function(sc_list_obj) {
  
  xlims <- c(min(sc_list_obj$controls$RunTime), max(sc_list_obj$controls$RunTime))

  g1 <- sc_list_obj$controls %>% 
    ggplot(aes(RunTime, Concentration, color = SampleID, fill = SampleID)) +
    geom_point() +
    geom_line() +
    geom_smooth(method = "lm") +
    ggtitle(label = paste(sc_list_obj$method, "- Controls", sc_list_obj$run_date)) + 
    xlab("Runtime") +
    ylab("Concentration") +
    theme(legend.position = "top") +
    geom_hline(yintercept = unique(sc_list_obj$controls$Nominal), linetype = 2) +
    xlim(xlims)

  g2 <- sc_list_obj$dups %>% 
    ggplot(aes(RunTime, pct_diff_conc)) + 
    geom_point() + 
    geom_line() + 
    ggtitle(label = paste(sc_list_obj$method, "- Percent difference of Dups",sc_list_obj$run_date)) +
    xlim(xlims)
  
  g3 <- sc_list_obj$dups %>% 
    ggplot(aes(RunTime, diff_conc)) + 
    geom_point() + 
    geom_line() + 
    ggtitle(label = paste(sc_list_obj$method, "- Absolute difference of Dups",sc_list_obj$run_date)) +
    xlim(xlims)

  return(plot_grid(g1 + theme(axis.title.x = element_blank()), 
                  g2 + theme(axis.title.x = element_blank()), 
                  g3, 
                  align = "hv", 
                  ncol = 1, 
                  axis = "t"))
}

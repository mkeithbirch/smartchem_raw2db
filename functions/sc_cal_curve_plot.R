sc_cal_curve_plot <- function(sc_run_file) {

  cal_lm <- lm(sc_run_file[[4]]$Abs ~ sc_run_file[[4]]$Concentration)

  cal_plot <- sc_run_file[[4]] %>% 
    ggplot(aes(x = Concentration, y = Abs)) + 
    geom_point() +
    geom_smooth(method = "lm") + 
    geom_hline(aes(yintercept = sc_run_file$rbl), linetype = "dashed") + # dashed line for RBL
    labs(title = paste("Calibrant Report -", sc_run_file$method, sc_run_file$run_date),
         subtitle = paste("Adj R2 =",signif(summary(cal_lm)$adj.r.squared, 3),
                          "Intercept =",signif(cal_lm$coef[[1]],3 ),
                          "Slope =",signif(cal_lm$coef[[2]], 3),
                          "RBL =",signif(sc_run_file$rbl, 3)))
  cal_plot
}
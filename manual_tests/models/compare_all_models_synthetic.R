generate_synthetic_data(5, 6, 75, "(12)(35)", c(
  "(15342)",
  "(14)(25)",
  "(123)",
  "(135)(24)",
  "(1543)",
  "(12)(345)"
)
, "bf")
mult_plots_info_BF <- generate_multiple_plots_info(".", granularity = 2, data_file_prefix = "bf", n_experiments = 1, lb = 150)

bf_plot <- plot_multilevel(mult_plots_info_BF)

generate_synthetic_data(10, 6, 50,"(2137)", perms_10 <- c(
  "(73)(29)(461)",
  "(1483)(25)(69)",
  "(16)(27)(38)(49)",
  "(194)(268)(357)",
  "(136)(25)(479)",
  "(18)(239)(567)"
), "mh")

mult_plots_info_MH <- generate_multiple_plots_info(".", granularity = 2, opt = "MH", max_iter = 200, data_file_prefix = "mh", n_experiments = 1, lb = 150)



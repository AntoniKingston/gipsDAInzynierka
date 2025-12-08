# setwd("data")
source("generate/synthetic_data_function.R")
source("../manual_tests/models/utils.R")

generate_synthetic_data(5, 6, 75, "(12)(35)", c(
  "(15342)",
  "(14)(25)",
  "(123)",
  "(135)(24)",
  "(1543)",
  "(12)(345)"
)
, "bf")

mult_plots_info_BF <- generate_multiple_plots_info(".", granularity = 50, data_file_prefix = "bf", n_experiments = 5, lb = 30)


gg_BF <- create_multilevel_plot(mult_plots_info_BF)
ggsave("BF_ggplot.png", plot = gg_BF, width = 10, height = 5, dpi = 300)

mult_plots_info_WA <- generate_multiple_plots_info(".", granularity = 50, data_file_prefix = "bf", n_experiments = 5, lb = 30, MAP = FALSE)
gg_WA <- create_multilevel_plot(mult_plots_info_WA)
ggsave("WA_ggplot.png", plot = gg_WA, width = 10, height = 5, dpi = 300)

# generate_synthetic_data(10, 6, 50,"(2137)", perms_10 <- c(
#   "(73)(29)(461)",
#   "(1483)(25)(69)",
#   "(16)(27)(38)(49)",
#   "(194)(268)(357)",
#   "(136)(25)(479)",
#   "(18)(239)(567)"
# ), "mh")
#
# mult_plots_info_MH <- generate_multiple_plots_info(".", granularity = 2, opt = "MH", max_iter = 200, data_file_prefix = "mh", n_experiments = 1, lb = 150)



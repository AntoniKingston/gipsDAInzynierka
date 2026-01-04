# setwd("data")

source("data/generate/scenario_1_gipsLDA.R")
source("data/generate/scenario_2_gipsMultQDA.R")
source("data/generate/scenario_3_gipsQDA.R")
source("data/generate/scenario_4_classicLDA.R")
source("data/generate/scenario_5_classicQDA.R")

# sigma_generate can be classic or qr,
# It determines the method for generating covariance matrices:
# if "classic", they are generated using the Wishart distribution.
# if "qr", they are generated using the QR decomposition method.
generate_all_scenarios_data <- function(
  p, n_classes, n_per_class, perm, perms, output_filename_prefix,
  sigma_generate = "classic", lambda_dist = "exp", ...
) {
generate_scenario_gipslda(p = p, n_classes = n_classes,
                          n_per_class = n_per_class,
                          perm = perm,
                          output_filename = paste0(output_filename_prefix,
                                                   "_scenario_gipslda.csv"),
                          sigma_generate = sigma_generate,
                          lambda_dist = lambda_dist,
                          ...)

generate_scenario_gipsmultqda(p = p, n_classes = n_classes,
                              n_per_class = n_per_class,
                              perm = perm,
                              output_filename = paste0(output_filename_prefix,
                                                       "_scenario_gipsmultqda.csv"),
                              sigma_generate = sigma_generate,
                              lambda_dist = lambda_dist,
                              ...)

generate_scenario_gipsqda(p = p, n_classes = n_classes,
                          n_per_class = n_per_class,
                          perms = perms,
                          output_filename = paste0(output_filename_prefix,
                                                   "_scenario_gipsqda.csv"),
                          sigma_generate = sigma_generate,
                          lambda_dist = lambda_dist,
                          ...)

generate_scenario_lda(p = p, n_classes = n_classes,
                      n_per_class = n_per_class,
                      output_filename = paste0(output_filename_prefix,
                                               "_scenario_lda.csv"),
                      sigma_generate = sigma_generate,
                      lambda_dist = lambda_dist,
                      ...)

generate_scenario_qda(p = p, n_classes = n_classes,
                      n_per_class = n_per_class,
                      output_filename = paste0(output_filename_prefix,
                                               "_scenario_qda.csv"),
                      sigma_generate = sigma_generate,
                      lambda_dist = lambda_dist,
                      ...)
}
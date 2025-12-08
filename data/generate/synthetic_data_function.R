
source("generate/scenario_1_gipsLDA.R")
source("generate/scenario_2_gipsMultQDA.R")
source("generate/scenario_3_gipsQDA.R")
source("generate/scenario_4_classicLDA.R")
source("generate/scenario_5_classicQDA.R")


generate_synthetic_data <- function(p, n_classes, n_per_class, perm,
                                    perms, output_filename_prefix) {

generate_scenario_gipslda(p, n_classes, n_per_class, perm = perm,
                          output_filename = paste0(output_filename_prefix, "_scenario_gipslda.csv"))

generate_scenario_gipsmultqda(p, n_classes, n_per_class, perm = perm,
                              output_filename = paste0(output_filename_prefix, "_scenario_gipsmultqda.csv"))

generate_scenario_gipsqda(p, n_classes, n_per_class,
                          perms = perms,
                          output_filename = paste0(output_filename_prefix, "_scenario_gipsqda.csv"))

generate_scenario_lda(p, n_classes, n_per_class,
                      output_filename = paste0(output_filename_prefix, "_scenario_lda.csv"))

generate_scenario_qda(p, n_classes, n_per_class,
                      output_filename = paste0(output_filename_prefix, "_scenario_qda.csv"))
}

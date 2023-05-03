# Libraries ---------------------------------------------------------------
library(tidyverse)
library(zCompositions)
library(LeyLabRMisc)
library(conflicted)

# JdlC's util functions
source("/ebio/abt3_projects/columbian_gut_metagenome/code/misc_r_functions/misc_jdlc.R")

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")


# Load tables -------------------------------------------------------------
base_dir = "/ebio/abt3_projects/columbian_gut_metagenome"
logratios_dir = file.path(base_dir, "data/metagenome_profile/intermediate_files/2020_04_log_ratios")
Col_ortho_rpk_strat = data.table::fread(file.path(logratios_dir, "Orthologs_Z_strat.tsv.gz"), 
                                        data.table = FALSE)


# Execute functions -------------------------------------------------------
# Impute zeros in a compositionally aware manner
# Remove orthologs absent from all samples
remove_orthologs_df = Col_ortho_rpk_strat %>%
  column_to_rownames("Sample")

remove_orthologs = colSums(remove_orthologs_df)
remove_orthologs = remove_orthologs[remove_orthologs == 0] %>% 
  names()

Col_ortho_noZ_strat = Col_ortho_rpk_strat %>%
  select(-any_of(remove_orthologs)) %>%
  column_to_rownames("Sample") %>%
  cmultRepl(method = "SQ", output = "p-counts")


# Write and report --------------------------------------------------------
fwrite(x = Col_ortho_noZ_strat, 
       file = file.path(logratios_dir, "Orthologs_noZ_strat.tsv.gz"), 
       sep = "\t")

send_email(body = "cmultRepl is done", subject = "cmultRepl")
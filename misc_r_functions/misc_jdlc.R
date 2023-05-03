# Fix column names in Colombian data and filter by contributional diversity ----
# Takes a raw HUMANn2 table
prep_table = function(df_raw, contributional = TRUE){
  require(tidyverse)
  
  # Default name of path/gene/gene family
  default_feat = colnames(df_raw) %>% first()
  
  # Rename columns
  colnames(df_raw) = colnames(df_raw) %>% 
    str_remove_all("_Abundance.*") %>%
    str_replace(default_feat, "Feature") %>%
    str_replace_all("\\-", "\\_") 
  
  # Return (or not) contributional diversity table
  if(contributional == F){
    df = df_raw %>% 
      filter(!str_detect(Feature, "\\|"))
  }
  else{
    df = df_raw
  }
  return(df)
}

# Transpose a fixed HUMANn2 table (after executing prep_table) ----
transpose_df = function(df){
  require(dplyr)
  Tdf = df %>% 
    gather(Sample, Relabund, -Feature) %>% # Convert to long format
    spread(Feature, Relabund) # Return to wide with `Sample` as first column
  Tdf
}

# Filter a table by mean and sd abundance ----
filt_rank_msd = function(df, mean_percentile = 0.5, sd_percentile = 0.5){
  require(dplyr)
  m_ranks = df %>% 
    summarise_if(is.numeric, mean) %>% 
    gather(Feature, mean) %>% 
    mutate(rank_mean = percent_rank(mean))
  
  sd_ranks = df %>% 
    summarise_if(is.numeric, sd) %>% 
    gather(Feature, sd) %>% 
    mutate(rank_sd = percent_rank(sd))
  
  msd_ranks = left_join(m_ranks, sd_ranks, by = "Feature") %>% 
    filter(rank_mean > mean_percentile & rank_sd > sd_percentile) %>% 
    arrange(-rank_mean)
  
  msd_ranks
}

# Transpose to data frame ----
t_df = function(table) {
  as.data.frame(t(table))
}

# Remove outliers
remove_outliers <- function(x, na.rm = TRUE, inner_fence = T, remove = T, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  val = 1.5
  # Use inner fence or outer fence to remove outliers
  if(inner_fence == F) {
    val = 3
  }
  H <- val * IQR(x, na.rm = na.rm)
  y <- x
  # Ir remove == T, remove. Else, cap to outlier limit
  if(remove == T){
    y[x < (qnt[1] - H)] <- NA
    y[x > (qnt[2] + H)] <- NA
    
    (qnt[1] - H)
  } else {
    y[x < (qnt[1] - H)] <- (qnt[1] - H)
    y[x > (qnt[2] + H)] <- (qnt[2] + H)
  }
  y
}

# Create manifest file for switchr env
# Save as RDS file
switchr_manifest = function(out_file){
  require(switchr)
  saveRDS(libManifest(), file = out_file)
}

# Deattach all loaded packages ----
detach_all_packages <- function() {
  # Basic packages
  basic.packages <- c("package:stats","package:graphics","package:grDevices",
                      "package:utils","package:datasets","package:methods",
                      "package:base", "package:switchr")
  # Loaded packages
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  # Packages to unload
  package.list <- setdiff(package.list,basic.packages)
  # Unload
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}

# Print head of very large objects
lhead = function(object, n = 10){
  if(is.vector(object) == TRUE){
    large_head = object[1:n]
  } else if(is.data.frame(object)){
    large_head = object[1:n, 1:n]
  }
  large_head 
}

# Perform common operation on rows (e.g. row means, row sums)
# to be used with `mutate` after a `rowwise` call
# Works only with df of numeric values
row_operation = function(operation = "mean"){
  if(operation == "mean"){
    mean(c_across(everything()))
  } else if(operation == "median"){
    median(c_across(everything()))
  } else if(operation == "sum"){
    sum(c_across(everything()))
  } else if(operation == "min"){
    min(c_across(everything()))
  } else if(operation == "max"){
    max(c_across(everything()))
  } else if(operation == "sd"){
    sd(c_across(everything()))
  }
}

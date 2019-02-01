#'Collect baseline 520 absorbance from a list of ECS data
#'
#'Given a list of ECS data (ie from read_all_folder) creates a matching list of baseline A520 data
#'TODO: Re-write to accept a single file. Until then, you can call ss_a520_baseline(list(datum))
#'@param ecs_data A list of ECS data traces
#'@name ss_a520_baseline
#'@export
require(tidyverse)
require(magrittr)
ss_a520_baseline <- function(ecs_data){
  baseline_A520 <- lapply(ecs_data,function(x) mean(x$Raw_Voltage[0:100]))
  times <- lapply(ecs_data,function(datum) mean(datum$Time[0:100]))
  baseline_A520 %<>% unlist() %>% as_tibble() %>% add_column(Time = unlist(times)) %>% rename(Baseline=value)
  baseline_A520 <- baseline_A520[order(baseline_A520$Time),]
  return(baseline_A520)
  
}
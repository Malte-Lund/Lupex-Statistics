library(here)
source(here("Code","Verisense-Toolbox-master","Verisense_step_algorithm","verisense_count_steps.R"))
myfun =  list(FUN=verisense_count_steps,
              parameters= c(3, 5, 15, -0.5, 3, 4, 0.001, 1.2),
              expected_sample_rate= 15,
              expected_unit="g",
              colnames = c("step_count"),
              outputres = 1,
              minlength = 1,
              outputtype="numeric",
              aggfunction = sum,
              timestamp=F,
              reporttype="event")


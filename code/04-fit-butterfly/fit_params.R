#### Initialize ####

# Set meta-parameters (for local use/testing)
run_on_cluster <- TRUE
this_study <- "walk"
this_model <- "sr"
this_data <- "memory"
this_many_runs <- 2
this_many_iter_per_run <- 1000
this_sub <- 1

# Keep dependencies to a minimum
library(tidyverse)
library(here)
library(tictoc)

# If running on the cluster, overwrite all the meta-parameters
if ( run_on_cluster ) {
  # What's the objective function for the model we're estimating?
  # Get arg from shell script
  this_study <- commandArgs(trailingOnly = TRUE)[1]
  this_model <- commandArgs(trailingOnly = TRUE)[2]
  this_data <- commandArgs(trailingOnly = TRUE)[3]
  
  # Use SLURM array to parallelize model-fitting across jobs
  this_sub <- Sys.getenv("SLURM_ARRAY_TASK_ID")[1]
  
  # Meta-parameters
  this_many_runs <- 25
  this_many_iter_per_run <- 5000
}

# Create all needed directories
if (run_on_cluster) {
  if (dir.exists("/gpfs/scratch/json11/butterfly-param-fits") == FALSE) {
    dir.create("/gpfs/scratch/json11/butterfly-param-fits")
  }
  
  if (!dir.exists(here("data", "butterfly-param-fits"))) {
    dir.create(here("data", "butterfly-param-fits"))
  }
}

# Load in utils
source(here("code", "utils", "modeling_utils.R"))
source(here("code", "utils", "network_utils.R"))
source(here("code", "utils", "representation_utils.R"))


#### Load/tidy data ####

learn <- str_c(
  here("data", "butterfly-behavior", ""),
  this_study,
  "_learn.csv"
) %>%
  read_csv(show_col_types = FALSE) %>%
  filter(sub_id == this_sub) %>%
  select(trial_index, from, to)

if (this_data == "memory") {
  choice <- str_c(
    here("data", "butterfly-behavior", ""),
    this_study,
    "_mem.csv"
  ) %>%
    read_csv(show_col_types = FALSE) %>%
    filter(sub_id == this_sub) %>%
    select(from, to, choice)
} else if (this_data == "trust") {
  choice <- str_c(
    here("data", "butterfly-behavior", ""),
    this_study,
    "_trust.csv"
  ) %>%
    read_csv(show_col_types = FALSE) %>%
    filter(sub_id == this_sub) %>%
    select(from, to, choice = response)
}

mem_rep <- here(
  "data", "butterfly-fixed-predictions", "network_experts.csv"
) %>%
  read_csv(show_col_types = FALSE) %>%
  select(from, to, memorize_value)

race_rep <- str_c(
  here("data", "butterfly-fixed-predictions", "feature_experts_"),
  this_study,
  ".csv"
) %>%
  read_csv(show_col_types = FALSE) %>%
  filter(sub_id == this_sub) %>%
  select(from, to, feat_race_value)

#### Fit parameters ####

# Define params for objective function
if (this_model == "memorization") {
  these_params <- c("w_naught", "w_mem")
} else if (this_model == "sr") {
  these_params <- c("w_naught", "w_sr", "sr_gamma")
} else if (this_model == "memorization-sr") {
  these_params <- c("w_naught", "w_mem", "w_sr", "sr_gamma")
} else {
  stop("Invalid value of `this_model`")
}

# Load objective function
source(here("code", "04-fit-butterfly", "obj_fun.R"))

tictoc::tic("Total model-fitting time")

for (j in 1:this_many_runs) {
  print(str_c("Starting run ", j))
  tictoc::tic(str_c("Run ", j, " finished"))
  
  tryCatch(
    expr = {
      this_run <- run_optim(
        max_iter_per_run = this_many_iter_per_run,
        objective_function = this_obj_fun,
        param_guesses = runif(length(these_params), -1, 1),
        param_names = these_params,
        # Supply arguments to objective function
        learning_data = learn,
        choice_data = choice,
        mem_rep = mem_rep,
        fit_data = this_data
      ) %>% mutate(optimizer_run = j)
      
      if (j == 1) {
        out <- this_run
      } else {
        out <- bind_rows(out, this_run)
      }
    },
    error = function(e) {
      warning(str_c("tryCatch error caught on run "), j)
    }
  )
  
  tictoc::toc()
  
  # Save to scratch instead of constantly writing to data
  if (run_on_cluster) {
    out %>%
      write_csv(
        str_c(
          "/gpfs/scratch/json11/butterfly-param-fits/",
          this_data, "-",
          this_study, "-",
          this_model,
          "-sub_", this_sub, ".csv"
        )
      )
  }
}

if (run_on_cluster) {
  # Save final model fits to GPFS data
  out %>%
    write_csv(
      str_c(
        here("data", "butterfly-param-fits", ""),
        this_data, "-",
        this_study, "-",
        this_model,
        "-sub_", this_sub, ".csv"
      )
    )
}

tictoc::toc()

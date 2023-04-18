#! /bin/bash

workflow_name="02-simulate-butterfly"

parent_dir=$(Rscript -e "cat(here::here())")
output_dir=${parent_dir}/outputs
save_dir=${output_dir}/${workflow_name}

mkdir -m 775 ${output_dir} || echo "Output directory already exists"
mkdir -m 775 ${save_dir} || echo "Save directory already exists"

cd ${parent_dir}/code/${workflow_name}

R -e "rmarkdown::render('simulate_choice.Rmd', output_dir='${save_dir}')"
R -e "rmarkdown::render('simulate_experts.Rmd', output_dir='${save_dir}')"
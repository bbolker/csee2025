## should I be using renv???
## install kaleido etc
## https://search.r-project.org/CRAN/refmans/plotly/html/save_image.html
reticulate::install_miniconda(force = TRUE)
reticulate::conda_install('r-reticulate', 'python-kaleido')
reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
reticulate::use_miniconda('r-reticulate')

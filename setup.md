* Need 'kaleido' module for numpy
* webgl issues with plotly ?
    * https://search.r-project.org/CRAN/refmans/plotly/html/save_image.html
```r
reticulate::install_miniconda(force = TRUE)
reticulate::conda_install('r-reticulate', 'python-kaleido')
reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
reticulate::use_miniconda('r-reticulate')
```

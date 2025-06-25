library(plotly)
reticulate::use_miniconda('r-reticulate')
library(dplyr)


reticulate::py_available() ## why FALSE?
debug(reticulate::py_available)

datfn <- "McCoy_response_surfaces_Gamboa.csv"

## odonates only
x <- (read.csv(datfn)
    |> transform(block = factor(block))
    |> subset(cohort == "single" & predtype == "odo",
              select = -c(cohort, predtype))
    |> droplevels()
    |> transform(csize = size - mean(size), prop = killed/initial)
)

marker <- list(color = ~prop,
               colorscale = c('#FFE1A1', '#683531'), 
               showscale = TRUE)

seg_data <- function(x, zvar) {
    ## why do we need enquo here??
    zvar <- enquo(zvar)
    xx <- (x
        |> mutate(.id = seq(nrow(x)))
        |> reframe(!!zvar := c(!!zvar, 0), across(-!!zvar), .by = .id)
        |> plotly::group2NA(".id")
    )
    return(xx)
}

## https://stackoverflow.com/questions/72281954/keep-other-columns-when-doing-group-by-summarise-with-dplyr
## https://stackoverflow.com/questions/50012328/r-plotly-showlegend-false-does-not-work
odo_plotly_0 <- (plot_ly(x= ~initial, y = ~size, z = ~prop)
    |> add_markers(data = x, marker = marker, showlegend = FALSE)
    |> add_paths(data = seg_data(x, prop), , showlegend = FALSE)
    |> hide_colorbar()
    |> layout(scene = list(yaxis = list(rangemode = "tozero"),
                           xaxis = list(rangemode = "tozero"),
                           camera = list(eye = list(x = 2.5, y = 2, z = 1)),
                           showlegend=FALSE))
)

odo_plotly_0
save_image(odo_plotly_0)

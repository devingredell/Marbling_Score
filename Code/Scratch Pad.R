# Custom Functions --------------------------------------------------------

source(here::here("Code", "my_functions.R"))

# Data --------------------------------------------------------------------

source(here::here("Code", "clean_data.R"))

# Plot Options ------------------------------------------------------------

discrete_gradient <- colorRampPalette(c("white", "#002453"))

cols <- discrete_gradient(7)[-1]

options(ggplot2.discrete.colour = cols,
        ggplot2.distrete.fill = cols)

theme_set(custom_theme())


# Code --------------------------------------------------------------------



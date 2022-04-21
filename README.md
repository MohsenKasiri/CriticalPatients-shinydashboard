# Palmer Penguin Dashboard

This is a simple dashboard to illustrate RShiny's uses and capabilities. The dashboard allows for some data exploration and investigating a linear discriminant analysis of the Palmer penguin data.

Data were collected and made available by Dr. Kristen Gorman and the Palmer Station, Antarctica LTER, a member of the Long Term Ecological Research Network. The data set contains data for 344 penguins. There are 3 different species of penguins in this dataset: Adelie, Chinstrap and Gentoo. Penguin data were collected from 3 islands (Biscoe, Dream, Torgersen) in the Palmer Archipelago, Antarctica.

## Description

The dashboard is created in R using RShiny. 

### Dependencies

The following code can be run in your R console to install the required R packages:
```
install.packages(c("palmerpenguins", "ggplot2", "magrittr", "dplyr", "GGally", "plotly", "shiny", "shinydashboard", "tidyverse",
 "caret", "MASS"))
```

### Installing

The zip file can be installed right from GitHub or alternatively downloaded using the following code in your R console:

```
library(devtools)
install_github("gaudet-d/penguin-shinydashboard")
```

### Executing program


```
runApp(app.R)
```

<!-- ## Help (Known issues, etc.)

## Authors

 ## Version History 

## License

## Acknowledgements -->

## References

Horst AM, Hill AP, Gorman KB (2020). palmerpenguins: Palmer Archipelago (Antarctica) penguin
  data. R package version 0.1.0. https://allisonhorst.github.io/palmerpenguins/
  


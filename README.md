# An R implementation of a Morphological Analysis tool

<!-- [![Build Status](https://travis-ci.org/rstudio/DT.svg)](https://travis-ci.org/rstudio/DT) -->

This package provides an interactive graphical representation of a
*morphological field*, a.k.a. morphological box, or "Zwicky box", see
[[1]](#wikipedia1), [[2]](#wikipedia2), [[3]](#wikipedia-de). Morphological
Analysis (MA) is a structured method for exploring and constraining a complex
multi-dimensional, possibly non-quantifiable, problem space. It can be used to
assess mutual consistency of the problem parameters and thus reduce the
initially large number of parameter combinations. MA was developed by
astrophysicist Fritz Zwicky ([[4]](#zwi47), [[5]](#zwi48), [[6]](#zwi67),
[[7]](#zwi69)) and is since the mid-1990s advanced and applied by the Swedish
Morphological Society, in particular by Tom Ritchey, under the term "General
Morphological Analysis" ([[8]](#swemorph), [[9]](#rit11)).

The purpose of this package is to bring Morphological Analysis tools into a 
modern, mobile, cross-platform computing environment and to provide easy access.
Because it requires only a web browser to run, it can be used from almost any
platform. Perhaps this easy access will encourage more widespread usage of
Morphological Analysis techniques for structured scientific problem solving and
decision making.
<!-- Hopefully, this package will be a little contribution to the solution of complex
problems of our time. -->

Please note that Morphological Analysis as a scientific method is not trademarked
or even copyrightable. The Swedish Morphological Society write on their website:

> The term and the procedures embodied in "morphological analysis" are not
> trademarked or copyrighted. Morphological analysis is a basic scientific method,
> and is therefore no more copyrightable than "mathematical analysis" or
> "statistical analysis".
>
> -- <cite>[Swedish Morphological Society](http://www.swemorph.com/legal.html)</cite>

The code of this package is publicly available and released under the GPL-3 (GNU
GENERAL PUBLIC LICENSE version 3). It is built upon RStudio's
[DT](https://github.com/rstudio/DT) package, which is distributed under the same
license.

<a name="wikipedia1"></a>[1]: https://en.wikipedia.org/wiki/Morphological_analysis_%28problem-solving%29  
<a name="wikipedia2"></a>[2]: https://en.wikipedia.org/wiki/Morphological_box  
<a name="wikipedia-de"></a>[3]: https://de.wikipedia.org/wiki/Morphologische_Analyse_(Kreativit%C3%A4tstechnik)  
<a name="zwi47"></a>[4]: Zwicky, F. (1947). Morphology and nomenclature of jet engines. *Aeronautical Engineering Review*, Vol. 6, No. 6, pp. 49--50.  
<a name="zwi48"></a>[5]: Zwicky, F. (1948). Morphological astronomy. *Observatory*, Vol. 68, No 845, pp. 121--143.  
<a name="zwi67"></a>[6]: Zwicky, F. & Wilson, A. (Eds.) (1967). *New methods of thought an procedure: Contributions to the symposium on methodologies*. Berlin: Springer.  
<a name="zwi69"></a>[7]: Zwicky, F. (1969). *Discovery, invention, research -- through the morphological approach*. Toronto: The Macmillan Company.  
<a name="swemorph"></a>[8]: http://www.swemorph.com/ma.html, *General Morphological Analysis - A general method for non-quantified modeling*, Swedish Morphological Society, 2002 (Revised 2013), Licensed under a [Creative Commons Attribution](https://creativecommons.org/licenses/by-nd/3.0/)  
<a name="rit11"></a>[9]: Ritchey, T. (2011). [*Wicked Problems -- Social Messes, Decision Support Modelling with Morphological Analaysis*](https://link.springer.com/book/10.1007/978-3-642-19653-9), Volume 17 of the series Risk, Governane and Society, Springer-Verlag Berlin Heidelberg.


## Installation

Install the package directly from GitHub using the `devtools` package:

```r
# Install devtools
install.packages("devtools")

# Install from GitHub
devtools::install_github("sgrubsmyon/morphr")
```

<!--See the full documentation at <http://rstudio.github.io/DT>. -->
<!-- Please use [Github issues](https://github.com/sgrubsmyon/morphr/issues) if you
want to file bug reports or feature requests. -->


## Usage

This package provides a function `installMorphField()` to display R data, which
represents a morphological field, via the [DataTables](http://datatables.net/) 
library (N.B. not to be confused with the **data.table** package). The 
morphological field widget is installed into the `output` object of a 
[Shiny](https://shiny.rstudio.org) application and can be displayed by putting a
`morphFieldOutput()` call into the applications `ui`. This package is built upon
RStudio's excellent package [DT](https://github.com/rstudio/DT).


## Simple example

```r
library(shiny)

ui <- fluidPage(
  fluidRow(
    morphr::morphFieldOutput("morphfield")
  )
)

server <- function(input, output) {
  param_values <- list(
    "Parameter A" = c("A1", "A2"),
    "Parameter B" = c("B1", "B2"),
    "Parameter C" = c("C1", "C2")
  )

  specific_configurations <- list(
    "Parameter A" = list(
      "A1" = list(
        "Parameter B" = "B2",
        "Parameter C" = "C1"
      ),
      "A2" = list(
        "Parameter B" = "B1",
        "Parameter C" = "C1"
      )
    )
  )

  morphr::installMorphField(input, output, session, id = "morphfield",
                            param_values = param_values,
                            specific_configurations = specific_configurations)
}

shinyApp(ui = ui, server = server)
```

Or execute `shiny::runApp(system.file("examples", "morphr-simple", package="morphr"))`
in RStudio.

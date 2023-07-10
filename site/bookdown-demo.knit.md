--- 
title: "R for Economists"
author: "César Landín"
date: "2023-07-10"
site: bookdown::bookdown_site
output: bookdown::gitbook
documentclass: krantz
bibliography: [book.bib, packages.bib]
biblio-style: apalike
link-citations: yes
# github-repo: rstudio/bookdown-demo
description: "A short programming guide for economics research."
---

# Introduction

## Contents
* [1. Introduction](https://clandinq.github.io/recon/index.html) introduces the guide and how to get started using it.
* [2. Figures](https://clandinq.github.io/recon/figures.html) describes tips for using ggplot to generate publication-quality graphs.
* [3. Tables](https://clandinq.github.io/recon/tables.html) introduces best practices for the presentation of tables in economics papers and explains the usage of functions that can generate formatted tables from R model objects.
* [4. Statistical Tests](https://clandinq.github.io/recon/statistical-tests.html) explains how to perform hypothesis tests on model coefficients.
* [5. Geographic Place Matching](https://clandinq.github.io/recon/geographic-place-matching.html) explains how to match addresses to coordinates using Google Place API searches.



<!-- # Prerequisites -->

<!-- This is a _sample_ book written in **Markdown**. You can use anything that Pandoc's Markdown supports, e.g., a math equation $a^2 + b^2 = c^2$. -->

<!-- The **bookdown** package can be installed from CRAN or Github: -->

<!-- ```{r eval=FALSE} -->
<!-- install.packages("bookdown") -->
<!-- # or the development version -->
<!-- # devtools::install_github("rstudio/bookdown") -->
<!-- ``` -->

<!-- Remember each Rmd file contains one and only one chapter, and a chapter is defined by the first-level heading `#`. -->

<!-- To compile this example to PDF, you need XeLaTeX. You are recommended to install TinyTeX (which includes XeLaTeX): <https://yihui.name/tinytex/>. -->

<!-- ```{r include=FALSE} -->
<!-- # automatically create a bib database for R packages -->
<!-- knitr::write_bib(c( -->
<!--   .packages(), "bookdown", "knitr", "rmarkdown" -->
<!-- ), "packages.bib") -->
<!-- ``` -->

<!--chapter:end:index.Rmd-->

# Figures
Tips for using `ggplot` to generate publication-quality graphs {#ggplot}

<!-- Figures -->
<!-- - Sans-serif fonts such as Arial and Helvetica should be used whenever possible. Package `ggplot2` uses Arial as the default font. -->
<!-- - Multi-panel figures should include panel subtitles (e.g. Panel A: Intent to Treat). -->
<!-- - Figures should always include legends explaining the colors and symbols used.  -->

<!-- Distinguishing color in figures -->
<!-- - Multiple shades of color -->
<!-- - Line graphs: include different keys. -->
<!-- - Grouped bar graphs: include patterned fills. -->

<!-- If images require multiple lines, choose keys and line types that are easily distinguished from each other (give example). -->

<!-- Figure files should be in EPS format. -->

<!-- You can label chapter and section titles using `{#label}` after them, e.g., we can reference Chapter \@ref(intro). If you do not manually label them, there will be automatic labels anyway, e.g., Chapter \@ref(methods). -->
<!-- Reference a figure by its code chunk label with the `fig:` prefix, e.g., see Figure \@ref(fig:nice-fig). Similarly, you can reference tables generated from `knitr::kable()`, e.g., see Table \@ref(tab:nice-tab). -->
<!-- Figures and tables with captions will be placed in `figure` and `table` environments, respectively. -->
<!-- # ```{r nice-fig, fig.cap='Here is a nice figure!', out.width='80%', fig.asp=.75, fig.align='center'} -->

<!-- https://bookdown.org/yihui/bookdown-demo/intro.html -->
<!-- https://bookdown.org/yihui/bookdown/preview-a-chapter.html -->
<!-- bookdown::render_book(here("scripts", "book")) -->
## Plot margins
### Removing white space around the plot
To remove the white space around the plot, set the plot margins equal to 0. The order is [top, right, bottom, left].

```r
figure %>% 
  theme(plot.margin = margin(0, 0, 0, 0))
```

This setting is useful when working with package `cowplot` to generate multi-panel figures. `cowplot::plot_grid` often overlays panel labels on top of the figures, so you can add space to the top of the figure:

```r
plot_grid(g1 + theme(plot.margin = margin(t = 15)),
          g2 + theme(plot.margin = margin(t = 15)),
          nrow = 2)
```

### Removing white space between axes and plot
Sometimes it's useful to reduce the distance between the plot and axis text. You can do this by reducing the top margin of the x-axis text and the right margin of the y-axis text:

```r
figure %>% 
  theme(axis.text.x = element_text(margin = margin(t = -5, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(margin = margin(t = 0, r = -5, b = 0, l = 0)))
```

## Axis labels
### Removing axis labels
When removing axis labels (for example, when dealing with dates in the x-axis), use `labs(x = NULL)` rather than `labs(x = "")`, as this eliminates the extra white space.

```r
figure %>% 
  labs(x = NULL)
```

## Legends
### Removing the legend title
Legend titles are always redundant: if the figure is included in the paper or report, then the title and axis label give enough information; if the figure is in a presentation, the slide title and description along with axis labels provide enough information to understand what is being plotted.

```r
figure %>% 
  theme(legend.title = element_blank())
```

### Removing black boxes around legend keys
Legend keys look much better without the black border surrounding them. This should be a standard for any figure.

```r
figure %>% 
  theme(legend.key = element_blank())
```

### Putting the legend inside the figure
Useful for when there is a lot of blank space in the figure.

```r
figure %>% 
  theme(legend.position = c(0.75, 0.85))
```





<!-- # Center ggplot title -->
<!-- theme(plot.title = element_text(hjust = 0.5)) -->


<!-- ### -->

<!-- # (4.7): Merge missing municipalities with shapefiles. # -->
<!-- shp_mun <- st_read(here("proc", "shapefiles", "shp_mun.shp")) -->
<!-- missing_coords_shp <- mun_coords  %>%  -->
<!--   st_as_sf(coords = c("loc_lon", "loc_lat")) %>%  -->
<!--   st_set_crs(8999) -->
<!-- intersections <- tibble(cve_total = st_intersects(missing_coords_shp, shp_mun) %>% unlist(), -->
<!--                         cve_ent = shp_mun$cve_ent[cve_total], -->
<!--                         cve_mun_2020 = shp_mun$cve_mun[cve_total], -->
<!--                         mun_id = mun_coords$mun_id) %>%  -->
<!--   select(-cve_total) %>%  -->
<!--   left_join(mun_coords, by = "mun_id") -->
<!-- missing_muns_l %<>%  -->
<!--   left_join(intersections %>% select(-loc_lat, -loc_lon),  -->
<!--             by = c("cve_ent", "mun_id")) -->
<!-- rm(intersections, mun_coords, missing_coords_shp, key) -->





## Number formatting functions
You can save these functions in a script called number_functions.R and import them in each script where they're needed, e.g.:


```r
source(here("scripts", "programs", "number_functions.R"))
```

### Calculating the mean, median and standard deviation of a variable

```r
# Mean
num_mean <- function(df, variable, dig = 1) {
  df %>% 
    pull(eval(as.name(variable))) %>% 
    mean(na.rm = TRUE) %>% 
    round(digits = dig)
}

# Median
num_median <- function(df, variable, dig = 1) {
  df %>% 
    pull(eval(as.name(variable))) %>% 
    median(na.rm = TRUE) %>% 
    round(digits = dig)
}

# Standard deviation
num_sd <- function(df, variable, dig = 1) {
  df %>% 
    pull(eval(as.name(variable))) %>% 
    sd(na.rm = TRUE) %>% 
    round(digits = dig)
}
```

You can call these functions the following way:


```r
df %>% num_mean("number_employees")
```

### Checking if a number is an integer.
This is used in the functions that print numbers to .tex files, since no decimals should be added after integers.

```r
num_int <- function(x) {
  x == round(x)
}
```

### Number formatting function
This function formats numbers with a standard number of digits and commas to present large and small numbers in a more readable format.

How does this function work? First, the function calculates the number of digits the number should have to the right of the decimal point. For numbers from 1-9, three digits are assigned, two digits are assigned for numbers 10-99, one for 100-999, and no right digits for numbers equal or larger than 1,000. This function sets the maximum number of right digits as 3. Therefore, 0.0001 will display as 0.000. Once the number of right digits is defined, the number is formatted. Numbers smaller than 1 are padded if necessary to ensure that there are 3 right digits (e.g., 0.25 is formatted as 0.250). The default number of right digits can be overridden with the option `override_right_digits`.

This function has the same name as `scales::comma_format`. However, this function has been superseded by `scales::label_comma`, so there are no issues with this user-defined function taking priority over `scales::comma_format`.


```r
comma_format <- function(x, override_right_digits = NA) {
  # Calculate number of right digits
  if (x <= 0) {num <- 1} else {num <- x}
  right_digits <- 3 - floor(log10(abs(num)))
  if (right_digits < 0) {right_digits <- 0}
  if (right_digits > 3) {right_digits <- 3}
  if (!is.na(override_right_digits)) {right_digits <- override_right_digits}
  # Calculate number of left digits
  left_digits <- 4 + floor(log10(abs(num)))
  if (left_digits <= 0) {left_digits <- 1}
  # Format number
  proc_num <- format(round(x, right_digits), nsmall = right_digits, digits =  left_digits, big.mark = ",")
  if (proc_num != "0" & as.numeric(str_replace(proc_num, fixed(","), "")) < 1) {
    proc_num <- str_pad(proc_num, right_digits + 2, "right", "0")
  }
  return(proc_num)
}
```

Here are a few examples of the output of this function:


```r
pacman::p_load(tidyverse)

lapply(c(0.098, 0.11, 3.1233, 45.968, 1949), comma_format) %>% unlist()
```

```
## [1] "0.098" "0.110" "3.123" "45.97" "1,949"
```



<!-- # (1.3): Print number to file. -->
<!-- print_n <- function(x, file, note = "", dig = 1) { -->
<!--   # Round number to desired number of digits -->
<!--   current_num <- as.numeric(x) %>% -->
<!--     round(dig) -->
<!--   # Don't add 0 after decimal point if number is a whole number -->
<!--   # https://support.jmir.org/hc/en-us/articles/360019690851-Guidelines-for-Reporting-Statistics -->
<!--   current_num <- ifelse(num_int(current_num), -->
<!--                         comma_format(current_num, override_right_digits = 0), -->
<!--                         comma_format(current_num, override_right_digits = dig)) -->
<!--  # Export number -->
<!--   current_num %>% -->
<!--     print() %>% # prior to adding the % for Latex -->
<!--     str_c("%") %>% # this % is to prevent space after number -->
<!--     str_c(ifelse(note != "", str_c("\n% ", note), "")) %>% -->
<!--     write_(here("results", "numbers", file)) -->
<!-- } -->

<!-- # (1.4): Print percentage to file. -->
<!-- print_pct <- function(x, file, note = "", dig = 1) { -->
<!--   # Round number to desired number of digits -->
<!--   current_num <- as.numeric(x) %>% -->
<!--     "*"(100) %>% -->
<!--     round(dig) -->
<!--   # Don't add 0 after decimal point if percentage is a whole number -->
<!--   # https://support.jmir.org/hc/en-us/articles/360019690851-Guidelines-for-Reporting-Statistics -->
<!--   current_num <- ifelse(num_int(current_num), -->
<!--                         comma_format(current_num, override_right_digits = 0), -->
<!--                         comma_format(current_num, override_right_digits = dig)) -->
<!--   # Export number -->
<!--   current_num %>% -->
<!--     str_c("\\%%") %T>% # this % is to prevent space after number; not the \% in Latex -->
<!--     print() %>% -->
<!--     str_c(ifelse(note != "", str_c("\n% ", note), "")) %>% -->
<!--     write_(here("results", "numbers", file))  # includes the "%" in printing since it's a percent -->
<!-- } -->



<!-- ## Saving numbers as .tex files -->

<!-- When results change, you want numbers to change -->
<!-- Eliminates possibility of manual error -->


<!-- Folder structure should include "results/numbers" and we should include instructions about producing all numbers in a paper as tex files that are included with \input{} -->



<!-- This should be added to the RA Guide. Furthermore, the commands I've written to write these numbers to tex files with the correct formatting (e.g. print_n.R, print_as_percent.R) should be added to the R, Stata, and Python guides. For those guides, @César Landín add to R Guide first then @Anoushka Nalwa to Stata guide and @Jora to Python guide. -->

<!-- Note that when adding them to the guides you should be using command line git, clone the repo, create a new branch, make the changes, push your branch, then on GitHub submit a pull request to incorporate those changes into the main branch. There are some instructions on this in the RA Guide: https://github.com/skhiggins/ra_guide. Just as there are some scripts already included as part of the R, Stata, and Python guides, these scripts to print output to a tex file should also be included within the repo and linked in the guides. -->




## Multiplot figures
### Auxiliary functions to remove figure components: axes, legend

**Functions:**

```r
remove_axis_x <- function(plot) {
  plot + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank())
}
remove_axis_y <- function(plot) {
  plot + 
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank())
}
remove_legend <- function(plot) {
  plot +
    theme(legend.position = "none")
}
```

**Usage:**
Let `g1`, `g2` and `g3` be three figures with the same x-axis. We can plot the three figures in one multipanel figure, removing the legend and x-axis from all but the bottom figure.

```r
plot_grid(g1 %>% remove_legend() %>% remove_axis_x(), 
          g2 %>% remove_legend() %>% remove_axis_x(), 
          g3, 
          rel_heights = c(1, 1, 1),
          nrow = 3,
          axis = "b", 
          align = "h")
```


<!-- An alternative to the previous is to simply remove the axis from the three figures and then incorporate the axis in the bottom fig. -->

<!--chapter:end:01-figures.Rmd-->

# Tables

This section introduces best practices for the presentation of tables in economics papers and explains the usage of functions that can generate formatted tables from R model objects. First, we give guidelines for table formatting adapted from the Journal of Political Economy's [manuscript preparation guide](journals.uchicago.edu/journals/jpe/prep-table). Second, we construct a publication-quality table using custom functions that can be downloaded [here](https://github.com/clandinq/ra_tools/blob/main/scripts/table_functions.R).

## Guidelines for table formatting

### Horizontal and vertical lines

-   **Only three horizontal lines.** Tables should be free of horizontal lines unless they indicate the structure of the data. All tables should have the three following horizontal lines:
    -   One under the title, above the column headings (LaTeX `\toprule`)
    -   One between the column headings and the body of the table (LaTeX `\midrule`)
    -   One at the bottom of the table (LaTeX `\bottomrule`)
-   **No vertical lines.** No vertical lines should be included in the table.

### Column headings

-   **Brief column names.** Column headings should identify the column as briefly as possible. Headings should contain any necessary symbols (%, \$, etc.) or measurement abbreviations (cm, kg, etc.) that apply to the data in the column below.

### Symbols

-   **No symbols in the table body.** The body of the table should be free of symbols or measurement abbreviations (e.g. %, USD).

-   **Symbols should be included next to dependent variable names.** Symbols should appear in the column head when they apply to all values in the column, or in the leftmost column when they apply to all values in a row.

    -   For example, balance tables where each row is a regression will include symbols in row headers; regression tables where each column is a regression will include symbols in column headers.

## An example using the `iris` dataset

The following sections show how to make a publication-quality table using the `iris` dataset.

### Two simple models using the `iris` dataset

First, we load the `iris` dataset that contains measurements of 4 attributes for 50 flowers from 3 different species.


```r
pacman::p_load(here, tidyverse, magrittr, fixest, modelsummary, kableExtra, scales)
data(iris)
head(iris)
```

```
##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
## 1          5.1         3.5          1.4         0.2  setosa
## 2          4.9         3.0          1.4         0.2  setosa
## 3          4.7         3.2          1.3         0.2  setosa
## 4          4.6         3.1          1.5         0.2  setosa
## 5          5.0         3.6          1.4         0.2  setosa
## 6          5.4         3.9          1.7         0.4  setosa
```

Then, we define two models using package `fixest`. Both contain species fixed effects and cluster their errors at the species level.


```r
m1 <- feols(Petal.Width ~ Sepal.Length,
            fixef = "Species",
            cluster = "Species",
            data = iris)
m2 <- feols(Petal.Width ~ Sepal.Length + Sepal.Width, 
            fixef = "Species",
            cluster = "Species",
            data = iris)
```

#### `mshow`: **visualizing quickly regression outcomes**

Next, we introduce a function called **mshow** for visualizing quickly regression outcomes. This function is helpful when we want to quickly preview a model in R Studio's Viewer window instead of having to export the model to LaTeX. **mshow** defines standard significance levels and omits all statistics but the number of observations. The function takes as an input a list with the models (or a single model), and can include additional input that is passed on to the `modelsummary` function (for example, including additional rows).

<table style="NAborder-bottom: 0; width: auto !important; margin-left: auto; margin-right: auto;" class="table">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:center;"> Model 1 </th>
   <th style="text-align:center;"> Model 2 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Sepal.Length </td>
   <td style="text-align:center;"> 0.145** </td>
   <td style="text-align:center;"> 0.064 </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;"> (0.033) </td>
   <td style="text-align:center;"> (0.034) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sepal.Width </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> 0.232 </td>
  </tr>
  <tr>
   <td style="text-align:left;box-shadow: 0px 1px">  </td>
   <td style="text-align:center;box-shadow: 0px 1px">  </td>
   <td style="text-align:center;box-shadow: 0px 1px"> (0.146) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Num.Obs. </td>
   <td style="text-align:center;"> 150 </td>
   <td style="text-align:center;"> 150 </td>
  </tr>
</tbody>
<tfoot><tr><td style="padding: 0; " colspan="100%">
<sup></sup> * p &lt; 0.1, ** p &lt; 0.05, *** p &lt; 0.01</td></tr></tfoot>
</table>

#### Exporting the table to LaTeX

We can export the previous table to latex with the following code:



<center>![](https://raw.githubusercontent.com/clandinq/recon/main/results/tables/table_1.jpg){width="300"}</center>

The previous table is far from being ready to include in a paper: it does not specify what the outcome variable is, the default variable names have not been replaced by more descriptive names, and it includes content that is better left for the footnote (significance notes). The next thing that should be done to format the table is to adjust the table size and position, since both of these issues are fixed earlier on in the LaTeX code.

## Adjusting table size and position

The functions `float_here`, `adjust_box`, `adjust_height`, and `adjust_col_spacing` are used to adjust the table size and position:

-   `float_here` inserts the `[H]` option after `\begin{table}` so that LaTeX does not reposition the tables. This function requires including `\usepackage{float}` in the document preamble.
-   `adjust_box` reduces the size of the table so that it fits within the text column width. This function requires including `\usepackage{adjustbox}` in the document preamble.
-   `adjust_height` is used to adjust the *height* of a table. This is useful when dealing with long tables that may not fit within the same page without further adjustment. This limits the table to a user-defined text height percentage (e.g. 70% of the text height). `adjust_height` requires the previous application of the `adjust_box` function.
-   `str_insert` is an auxiliary function used to insert lines in tables.
-   `adjust_col_spacing` defines column spacings. This can be used to increase the spacing between columns when a table looks cramped.

**Functions**:


```r
float_here <- function(table) {
  if (!suppressWarnings(str_detect(string = table, 
                                   pattern = fixed("\\begin{table}[H]")))) {
    table %<>%
      str_replace(pattern = fixed("\\begin{table}"), 
                  replacement = "\\begin{table}[H]")
  }
  return(table)
}

adjust_box <- function(table) {
  table %>% 
    str_replace(pattern = fixed("\\begin{tabular"),
                replacement = "\\adjustbox{max width=\\textwidth}{ \n \\begin{tabular") %>% 
    str_replace(pattern = fixed("\\end{tabular}"),
                replacement = "\\end{tabular}}")
}

adjust_height <- function(table, height) {
  table %>% 
    str_replace(pattern = fixed("\\adjustbox{max width=\\textwidth}"),
                replacement = str_c("\\adjustbox{max totalheight=", height, 
                                    "\\textheight, max width=\\textwidth}"))
}

str_insert <- function(table, pattern, insert, before = FALSE) {
  rep <- ifelse(before,
                str_c(fixed(insert), "\n", fixed(pattern)),
                str_c(fixed(pattern), "\n", fixed(insert)))
  table %>% 
    str_replace(pattern = fixed(pattern),
                replacement = rep)
}

adjust_col_spacing <- function(table, spacing) {
  table %>% 
    str_insert(pattern = "\\centering",
               insert = str_c("\\setlength{\\tabcolsep}{", spacing, "pt}"))
}
```

**Usage**:


```r
current_table %>% 
  float_here() %>% 
  adjust_box() %>% 
  adjust_height(0.7) %>% 
  adjust_col_spacing(16) %>% 
  write(here("results", "tables", "table_2.tex"))
```

<center>![](https://raw.githubusercontent.com/clandinq/recon/main/results/tables/table_2.jpg){width="389"}</center>

The effects of `float_here`, `adjust_box`, and `adjust_height` can only be seen when compiling the table. The wide column spacing specified by `adjust_col_spacing` can be observed when comparing with the previous table.

## Updating variable names and adding a caption and label

### Updating independent variable names

To update independent variable names, you should define object called `coef_names` with the detailed independent variable names, and names equal to the original variable names (example beow). `coef_map = coef_names` is passed on to `modelsummary`, substituting the original variable names.


```r
coef_names <- c("Sepal length", "Sepal width") %>% 
  set_names(c("Sepal.Length", "Sepal.Width"))
current_table <- mshow(model_list, 
                       coef_map = coef_names,
                       output = "latex")
current_table %>%
  write(here("results", "tables", "table_3.tex"))
```

<center>![](https://raw.githubusercontent.com/clandinq/recon/main/results/tables/table_3.jpg){width="300"}</center>

### Adding dependent variable names and model numbers

The next step is to add dependent variable names and model numbers to the table. This can be done with function `insert_col_numbers`. The auxiliary function `get_num_cols` is defined later in this section.

`insert_col_numbers` has a binary toggle (TRUE includes column numbers, FALSE omits them) so that the function can be included inside other functions. That way, column numbers can be inserted depending on the number of columns.

**Function**:


```r
insert_col_numbers <- function(table, col_numbers) {
  if (col_numbers) {
    table %>% 
      str_insert(pattern = "\\midrule",
                 insert = str_c("& ", paste(str_c("(", 1:get_num_cols(table), ")"), collapse = " & "), "\\\\"),
                 before = TRUE) 
  } else {
    table
  }
}
```

**Usage**:

To update dependent variable names, you should set the names of the list with the models equal to the dependent variable names. Then, you can apply the `insert_col_numbers` function to include column numbers:


```r
current_table <- model_list %>% 
  set_names(c("Petal width", "Petal width")) %>% 
  mshow(coef_map = coef_names,
        output = "latex")
current_table %>%
  insert_col_numbers(col_numbers = TRUE) %>% 
  write(here("results", "tables", "table_4.tex"))
```

<center>![](https://raw.githubusercontent.com/clandinq/recon/main/results/tables/table_4.jpg){width="297"}</center>

### Adding a multicolumn title

An alternative to the previous table is to add a multicolumn title that describes the dependent variable. This can be done with function `multicol_title`, which takes as an input the multicolumn title and has an option to add a horizontal line below the multicolumn title.

The auxiliary function `remove_names` removes the stock model names to keep only the multicolumn title. Therefore, the model list should remain unnamed. This function depends on another auxiliary function, `get_num_cols`, which calculates the numbers of columns in the table.

**Functions**:


```r
multicol_title <- function(table, title, cline = FALSE) {
  proc_table <- table %>% 
    str_insert(pattern = "\\toprule",
               insert = str_c("& \\multicolumn{", get_num_cols(table), "}{c}{", title, "} \\\\\n%clh"))
  if (cline) {
    proc_table %<>% 
      str_replace(pattern = "%clh",
                  replacement = str_c(fixed("\\\\cline{2-"), get_num_cols(table) + 1, "}"))
  }
  return(proc_table)
}

remove_names <- function(table) {
  if (str_detect(table, "Model 1")) {
    table %>% 
      str_replace(pattern = fixed(str_c("& ", str_c("Model ", 1:get_num_cols(table), collapse = " & "), "\\\\\n")),
                  replacement = "")
  } else {
    table
  }
}

get_num_cols <- function(table) {
  pos_start <- str_locate(table, fixed("tabular}[t]{"))[2] + 1 # Get starting position of column alignment
  pos_end <- str_sub(table, pos_start, pos_start + 15) %>%     # Extract end position
    str_locate(fixed("}"))
  pos_end <- pos_end[1] + pos_start - 2
  col_num <- (str_sub(table, pos_start, pos_end) %>% str_length()) - 1 # Exclude leftmost column
  return(col_num)
}
```

**Usage**:

Remember not to name the model list when using a multicolumn title:


```r
current_table <- model_list %>% 
  mshow(coef_map = coef_names,
        output = "latex")
current_table %>%
  remove_names() %>% 
  multicol_title("Petal Width") %>% 
  insert_col_numbers(col_numbers = TRUE) %>% 
  write(here("results", "tables", "table_5.tex"))
```

<center>![](https://raw.githubusercontent.com/clandinq/recon/main/results/tables/table_5.jpg){width="267"}</center>

### Adding a caption and title to the table

`add_caption_label_center` is used to add a caption to the table float and define the float label. The caption should be as concise as possible, and the float should have the file name as label to facilitate locating the table in the paper and results folders. This function also centers the table in the middle of the float. `float_here` is required before `add_caption_label_center`.

**Functions**:


```r
add_caption_label_center <- function(table, caption, label) {
  center <- ifelse(str_detect(table, "centering"), "", "\\centering")
  table %>% 
    str_insert(pattern = "\\begin{table}[H]",
               insert = str_c("\\caption{", caption, "} \n",
                              "\\label{tab:", label, "}", center))
}
```

**Usage**:


```r
current_table %>% 
  float_here() %>% 
  remove_names() %>% 
  multicol_title("Petal Width") %>% 
  insert_col_numbers(col_numbers = TRUE) %>% 
  add_caption_label_center(caption = "Correlation Between Flower Petal Width and Sepal Length and Width",
                           label = "flower_table") %>% 
  write(here("results", "tables", "table_6.tex"))
```

<center>![](https://raw.githubusercontent.com/clandinq/recon/main/results/tables/table_6.jpg){width="525"}</center>

## Final table formatting

### Updating the number of observations

Two functions can help to update the number of observations:

-   `comma_numobs` inserts a comma every three digits in the number of observations. The auxiliary function `comma_format` is used by this function.

-   `replace_numobs` replaces "Num.Obs" with a custom text (e.g. "Number of observations" or "Number of firms").

**Functions**:


```r
comma_format <- function(x, override_right_digits = NA) {
  # Calculate number of right digits
  if (x <= 0) {num <- 1} else {num <- x}
  right_digits <- 3 - floor(log10(abs(num)))
  if (right_digits < 0) {right_digits <- 0}
  if (right_digits > 3) {right_digits <- 3}
  if (!is.na(override_right_digits)) {right_digits <- override_right_digits}
  # Calculate number of left digits
  left_digits <- 4 + floor(log10(abs(num)))
  if (left_digits <= 0) {left_digits <- 1}
  # Format number
  proc_num <- format(round(x, right_digits), nsmall = right_digits, digits =  left_digits, big.mark = ",")
  if (proc_num != "0" & as.numeric(str_replace(proc_num, fixed(","), "")) < 1) {
    proc_num <- str_pad(proc_num, right_digits + 2, "right", "0")
  }
  return(proc_num)
}

comma_numobs <- function(models) {
  model_proc <- models
  for (i in 1:length(model_proc)) {
    model_proc[[i]]$nobs <- comma(model_proc[[i]]$nobs)
  }
  return(model_proc)
}

replace_numobs <- function(table, text) {
  table %>% 
    str_replace(pattern = "Num.Obs.",
                replacement = text) %>% 
    str_replace(pattern = "Num. Obs.",
                replacement = text)
}
```

**Usage**:


```r
current_table <- model_list %>% 
  comma_numobs() %>% 
  mshow(coef_map = coef_names,
        output = "latex")
current_table %>% 
  float_here() %>% 
  remove_names() %>% 
  multicol_title("Petal Width") %>% 
  insert_col_numbers(col_numbers = TRUE) %>% 
  add_caption_label_center(caption = "Correlation Between Flower Petal Width and Sepal Length and Width",
                           label = "flower_table") %>% 
  replace_numobs("Number of observations") %>% 
  write(here("results", "tables", "table_7.tex"))
```

<center>![](https://raw.githubusercontent.com/clandinq/recon/main/results/tables/table_7.jpg){width="525"}</center>

### Inserting additional data rows

Often it's useful to insert additional rows with statistics, for example means and medians. You can feed `modelsummary` a dataframe with the desired content and add these rows below the number of observations.

**Example:**


```r
# Define additional rows
petal_mean <- mean(iris$Petal.Width) %>% comma_format()
sepal_length_mean <- mean(iris$Sepal.Length)  %>% comma_format()
sepal_width_mean <- mean(iris$Sepal.Width) %>% comma_format()
add_row <- data.frame(rbind(c("Mean petal width", rep(petal_mean, 2)),
                            c("Mean sepal length", rep(sepal_length_mean, 2)),
                            c("Mean sepal width", rep(sepal_width_mean, 2))))
# Add rows to previous table
current_table <- model_list %>% 
  comma_numobs() %>% 
  mshow(coef_map = coef_names,
        add_rows = add_row,
        output = "latex")
current_table %>% 
  float_here() %>% 
  remove_names() %>% 
  multicol_title("Petal Width") %>% 
  insert_col_numbers(col_numbers = TRUE) %>% 
  add_caption_label_center(caption = "Correlation Between Flower Petal Width and Sepal Length and Width",
                           label = "flower_table") %>% 
  replace_numobs("Number of observations") %>% 
  write(here("results", "tables", "table_8.tex"))
```

<center>![](https://raw.githubusercontent.com/clandinq/recon/main/results/tables/table_8.jpg){width="525"}</center>

### Distributing coefficient names over two rows

Interactions of variables frequently generate excessively long variable names. One solution for this is to split the variable name in two and put the second part of the variable name in the next row. Function `coef_two_rows` can do this automatically; all this needs is for the interaction variable to include "times" between the two variables.

**Functions**:


```r
coef_two_rows <- function(table) {
  # Check number of string locations
  string_loc <- str_locate_all(table, "times")[[1]]
  num_locations <- nrow(string_loc)
  # Generate temporary output
  output <- table
  # Tag words
  tagged_string <- "REPtimes"
  output %<>% str_replace_all("times", tagged_string)
  # Loop over string locations
  for (i in 1:nrow(string_loc)) {
    # Locate coefficient string to replace with ""
    pat_loc <- str_locate(output, fixed(tagged_string))[1]
    # Locate lower row to replace with coefficient string
    upper_row <- str_sub(output, pat_loc, str_length(output))
    lower_row <- upper_row %>% 
      str_sub(str_locate(upper_row, fixed("\\\\\n "))[2] + 1, str_length(upper_row))
    # Define pattern to replace in upper row
    pat_rep_upper <- str_split(upper_row, "&", n = 2)[[1]][1]
    # Define pattern to replace in lower row
    pat_rep_lower <- lower_row %>% 
      str_sub(1, str_locate(lower_row, fixed("\\\\\n"))[2])
    # Replace strings in table.
    output %<>%
      str_replace(pattern = pat_rep_upper,
                  replacement = "") %>% 
      str_replace(pattern = fixed(pat_rep_lower),
                  replacement = str_c("\\quad $\\times$", 
                                      str_replace(pat_rep_upper, tagged_string, ""),
                                      pat_rep_lower))
  }  
  return(output)
}
```

**Usage**:


```r
# Add new variable to dataset.
set.seed(123)
current_data <- iris %>%
  mutate(random = sample(0:1, nrow(.), replace = TRUE))

# Define model
m3 <- feols(Petal.Width ~ Sepal.Length * random,
            fixef = "Species",
            cluster = "Species",
            data = current_data)

# Define coefficient names
coef_names <- c("Sepal length", "Random variable", "Sepal length times Random variable") %>%
  set_names(c("Sepal.Length", "random", "Sepal.Length:random"))

# Export table
current_table <- model_list %>% 
  append(list(m3)) %>% 
  comma_numobs() %>% 
  mshow(coef_map = coef_names,
        output = "latex")
current_table %>% 
  float_here() %>% 
  remove_names() %>% 
  multicol_title("Petal Width") %>% 
  insert_col_numbers(col_numbers = TRUE) %>% 
  add_caption_label_center(caption = "Correlation Between Flower Petal Width and Sepal Length and Width",
                           label = "flower_table") %>% 
  replace_numobs("Number of observations") %>% 
  coef_two_rows() %>%
  write(here("results", "tables", "table_9.tex"))
```

<center>![](https://raw.githubusercontent.com/clandinq/recon/main/results/tables/table_9.jpg){width="525"}</center>

### Inserting space between variables

In tables with two-line variable names, it can be easier to read the table when there is sufficient spacing between each set of variable coefficient and standard errors. The function `add_line_space` inserts vertical space between each set of variable numbers and facilitates reading the table.

**Function**:


```r
add_line_space <- function(table) {
  # Keep split rows after first parentheses.
  keep_after_firstpar <- (table %>% 
                            str_sub(str_locate(table, fixed("("))[1], str_length(table)) %>% 
                            str_split("\\\n"))[[1]]
  # Keep rows with parentheses.
  keep_after_firstpar <- keep_after_firstpar[str_detect(keep_after_firstpar, fixed("("))]
  # Drop first one (column numbers)
  keep_after_firstpar <- keep_after_firstpar[2:length(keep_after_firstpar)]
  # Replace with spacing.
  proc_table <- table
  for (i in 1:length(keep_after_firstpar)) {
    proc_table %<>%
      str_replace(pattern = fixed(keep_after_firstpar[i]),
                  replacement = str_c(keep_after_firstpar[i], "[0.3cm]"))
  }
  return(proc_table)
}
```

**Usage**:


```r
current_table %>% 
  float_here() %>% 
  remove_names() %>% 
  multicol_title("Petal Width") %>% 
  insert_col_numbers(col_numbers = TRUE) %>% 
  add_caption_label_center(caption = "Correlation Between Flower Petal Width and Sepal Length and Width",
                           label = "flower_table") %>% 
  replace_numobs("Number of observations") %>% 
  coef_two_rows() %>%
  add_line_space() %>% 
  write(here("results", "tables", "table_10.tex"))
```

<center>![](https://raw.githubusercontent.com/clandinq/recon/main/results/tables/table_10.jpg){width="451"}</center>

## Putting it all together

### Combining everything into one simple function

Most of the previous tables can be combined into one simple function: `gen_base_table`. The only non-auxiliary functions that are not included by default in the options are `adjust_height`, `adjust_col_spacing`, `coef_two_rows` and `add_line_space`.

**Function**:


```r
gen_base_table <- function(models, caption, label, title = NULL, cline = FALSE, coef_names, numobs = "Number of observations", ...) {
  # Get length of model. If large, it is a regression that needs to be put in a list
  if (length(models) > 10) {models <- list(models)}
  # Generate table
  output <- models %>% 
    # Fix number of observation number format
    comma_numobs() %>% 
    # Generate table with modelsummary
    modelsummary(coef_map = coef_names,
                 stars = c("*" = .1, "**" = .05, "***" = 0.01),
                 gof_omit = "AIC|BIC|R2|R2 Adj.|R2 Pseudo|R2 Within|Log.Lik.|Std.Errors|FE|RMSE",
                 escape = FALSE,
                 output = "latex",
                 ...) %>%
    # Insert float here option
    float_here() %>% 
    # Insert adjustbox
    adjust_box() %>% 
    # Add caption and label
    add_caption_label_center(caption = caption,
                             label = label) %>% 
    # Insert column numbers
    insert_col_numbers(ifelse(length(models) == 1, FALSE, TRUE)) %>% 
    # Remove model names if no unique column names given
    remove_names() %>% 
    # Replace number of observations (Num.Obs. with Num. Obs.)
    replace_numobs(numobs)
  if (!is.null(title)) {
    output %<>% multicol_title(title, cline = cline)
  }
  return(output)
}
```

**Usage**:

You can replicate the previous table with this simple command:


```r
model_list %>% 
  append(list(m3)) %>% 
  gen_base_table(caption = "Correlation Between Flower Petal Width and Sepal Length and Width",
                 label = "flower_table",
                 title = "Petal Width",
                 coef_names = coef_names) %>% 
  coef_two_rows() %>% 
  add_line_space() %>% 
  write(here("results", "tables", "table_11.tex"))
```

<center>![](https://raw.githubusercontent.com/clandinq/recon/main/results/tables/table_10.jpg){width="451"}</center>

### Adding footnotes

The final step is to remove the notes from the bottom of the table and include our own footnote. You can do this with function `format_save_footnote` and including `options("modelsummary_stars_note" = FALSE)` in your code. This function saves a footnote to a file with the same name as the table in the folder /results/notes. This makes it easier to locate notes and include them in the LaTeX document the following way:


```r
\input{../results/tables/current_table.tex}
\input{../results/notes/current_table.tex}
```

**Usage**

**Function**:


```r
format_save_footnote <- function(text = "", filename, stars = FALSE) {
  sig <- ifelse(stars, " * $p < 0.1$, ** $p < 0.05$, *** $p < 0.01$.", "")
  footnote <- str_c("\\footnotesize \n", 
                    "\\begin{justify} \n",
                    "\\emph{Note}: ", text, sig, "\n",
                    "\\end{justify} \n",
                    "\\normalsize \n")
  clean_filename <- str_split(filename, fixed("."))[[1]][1]
  write(footnote, here("results", "notes", str_c(clean_filename, ".tex")))
}
```

**Usage**:


```r
options("modelsummary_stars_note" = FALSE)
# (generate previous table after running this line)

paste("This table reports the correlation between petal width and sepal lenght and width.",
      "The unit of observation is at the plant level.",
      "The coefficients come from the regression of petal width on sepal length (column (1)), the regression of petal width on sepal length and sepal width (column (2)), and the regression of petal width on a random dummy variable, sepal length and the interaction between these two variables (column (3)).",
      "Data comes from the iris dataset.",
      "All estimations include species fixed effects, and standard errors clustered at the species level are included in parentheses.") %>% 
  format_save_footnote("current_table.tex", stars = TRUE)
```

<center>![](https://raw.githubusercontent.com/clandinq/recon/main/results/tables/table_12.jpg){width="451"}</center>

## Preparing tables for presentations

Once you have generated a final version of the table, there are a few functions that can easily generate a presentation version of the table, remove unwanted rows and add short notes at the bottom of the table.

### Generating presentation versions of tables

Presentation versions of tables only include the content between `\begin{tabulate}` and `\end{tabulate}`. `convert_save_pres_table` can do this conversion rapidly. By default, this table looks for the input table in /results/tables, and saves the output to the same folder with the *pres\_* prefix.

**Function**:


```r
convert_save_pres_table <- function(table_name, table_path = here("results", "tables"), output_path = here("results", "tables")) {
  table <- read_file(here(table_path, table_name))
  table <- str_sub(table,
                   str_locate(table, fixed("\\begin{tabular}"))[1],
                   str_locate(table, fixed("\\end{tabular}"))[2])
  pres_table_name <- str_c("pres_", table_name)
  write(table, here(output_path, pres_table_name))
  print(str_c("Presentation table saved to",
              str_remove(output_path, here()), "/", pres_table_name))
}
```

**Usage**:


```r
convert_save_pres_table("table_11.tex")
```

### Removing standard errors and column numbers

Removing standard errors and column numbers can make it easier to fit tables in presentations without having to reduce the font size too much. `remove_std_errors_col_nums` is another simple function with no options that can quickly perform this modification.

**Function**:


```r
remove_std_errors_col_nums <- function(table) {
  table %>%   
    str_split("\n") %>% 
    .[[1]] %>% 
    .[!(str_detect(., fixed("(")))] %>% 
    str_c(collapse = "\n")
}
```

**Usage**:

This would require saving a version of the original table without standard errors and column numbers, and then converting to a presentation version. The previous table would look like this:


```r
read_file(here("results", "tables", "table_12.tex")) %>% 
  remove_std_errors_col_nums() %>% 
  write(here("results", "tables", "table_13.tex"))
convert_save_pres_table("table_13.tex")
```

<center>![](https://raw.githubusercontent.com/clandinq/recon/main/results/tables/pres_table_13.jpg){width="462"}</center>

### Adding notes

Finally, although it's best practice not to add notes directly to a table, it is the only way you can add a note to a presentation table (since footnotes can't be included). `add_note` can be used for adding notes to presentation tables.

**Function**:


```r
add_note <- function(table, note) {
  table %>% 
    str_insert(pattern = "\\bottomrule",
               insert = str_c("\\multicolumn{", get_num_cols(table) + 1, "}{c}{", note, "}"))
}
```

**Usage**:


```r
read_file(here("results", "tables", "pres_table_13.tex")) %>% 
  add_note("Quick note at the bottom of the presentation table.") %>% 
  write(here("results", "tables", "table_14.tex"))
```

<center>![](https://raw.githubusercontent.com/clandinq/recon/main/results/tables/table_14.jpg){width="462"}</center>

<!--chapter:end:02-tables.Rmd-->

---
editor_options: 
  markdown: 
    wrap: sentence
---

# Statistical tests

In this section, we introduce functions for performing hypothesis tests on model coefficients using the `multcomp` package.
We introduce a function called `extract_test_coef` that tests a hypothesis on model parameters and extracts the resulting p-values of the test.
Then, we show the usage of the function with an example using the `iris` dataset.

## Easy two-sided hypothesis tests in R

First, load the required packages and the `iris` dataset.
The packages needed are `tidyverse` for data manipulation, `fixest` for estimating models, `modelsummary` for displaying model results, `multcomp` for estimating hypothesis tests and `conflicted` to deal with conflicts in function names.
With package `conflicted`, we can specify which package to use for a given function.
Since `multcomp` and `dplyr` conflict in multiple functions, we can specify which package to use for each function and give preference to `dplyr`.
The `iris` dataset contains measurements of 4 attributes for 50 flowers from 3 different species.


```r
# Load packages
pacman::p_load(tidyverse, fixest, modelsummary, multcomp, 
               conflicted, fastDummies)

# Resolve conflicts
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("last", "dplyr")

# Load iris data
data(iris)
head(iris)
```

```
##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
## 1          5.1         3.5          1.4         0.2  setosa
## 2          4.9         3.0          1.4         0.2  setosa
## 3          4.7         3.2          1.3         0.2  setosa
## 4          4.6         3.1          1.5         0.2  setosa
## 5          5.0         3.6          1.4         0.2  setosa
## 6          5.4         3.9          1.7         0.4  setosa
```



Next, we define terciles of sepal width and a simple model using package `fixest`.
We show the model results using the `mshow` function defined in the previous chapter.


```r
# Define terciles of sepal width
proc_data <- iris %>%
  mutate(sepal_width_tercile = ntile(Sepal.Width, 3)) %>% 
  dummy_cols("sepal_width_tercile")

# Define coefficient names
coef_names <- c("Sepal length", "First tercile sepal width", "Second tercile sepal width", "Third tercile sepal width") %>% 
  set_names(c("Sepal.Length", str_c("sepal_width_tercile_", 1:3)))

# Define model 
m1 <- feols(Petal.Width ~ 0 + Sepal.Length + sepal_width_tercile_1 + sepal_width_tercile_2 + sepal_width_tercile_3,
            se = "hetero",
            data = proc_data)
mshow(list("Petal width" = m1), coef_map = coef_names)
```

<table style="NAborder-bottom: 0; width: auto !important; margin-left: auto; margin-right: auto;" class="table">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:center;"> Petal width </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Sepal length </td>
   <td style="text-align:center;"> 0.732*** </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;"> (0.041) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> First tercile sepal width </td>
   <td style="text-align:center;"> −2.882*** </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;"> (0.242) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Second tercile sepal width </td>
   <td style="text-align:center;"> −3.083*** </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;"> (0.237) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Third tercile sepal width </td>
   <td style="text-align:center;"> −3.261*** </td>
  </tr>
  <tr>
   <td style="text-align:left;box-shadow: 0px 1px">  </td>
   <td style="text-align:center;box-shadow: 0px 1px"> (0.215) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Num.Obs. </td>
   <td style="text-align:center;"> 150 </td>
  </tr>
</tbody>
<tfoot><tr><td style="padding: 0; " colspan="100%">
<sup></sup> * p &lt; 0.1, ** p &lt; 0.05, *** p &lt; 0.01</td></tr></tfoot>
</table>

The next step is to use the function `extract_test_coef` to test two hypothesis:

1.  The effect of the first tercile of sepal width on petal width is equal to the effect of the second tercile of sepal width on petal width
2.  The effect of the first tercile of sepal width on petal width is equal to the effect of the third tercile of sepal width on petal width The function returns p-values for each test.

The expression included in the function must be in the form **`coef_a == coef_b`** and will perform a two-sided hypothesis test where the null hypothesis is $H_0: coef_a = coef_b$.


```r
# Define function
extract_test_coef <- function(model, comp) {
  expr <- comp %>% 
    str_replace(pattern = fixed("=="),
                replacement = "-")
  res <- model %>% 
    glht(linfct = str_c(expr, "= 0")) %>% 
    confint() %>% 
    summary()
  return(as.numeric(res$test$pvalues))
}

# Test hypotheses
pval_1_2 <- extract_test_coef(m1, "sepal_width_tercile_1 == sepal_width_tercile_2")
pval_1_3 <- extract_test_coef(m1, "sepal_width_tercile_1 == sepal_width_tercile_3")
```

Finally, we add the p-values to the model results using the `add_row` argument of the `mshow` function.


```r
# Add p-values to model results
add_row <- tribble(~name, ~value,
                   "P-value (Sepal width 1st tercile = Sepal width 2nd tercile)", comma_format(pval_1_2),
                   "P-value (Sepal width 1st tercile = Sepal width 3rd tercile)", comma_format(pval_1_3))
mshow(list("Petal width" = m1), 
      coef_map = coef_names, 
      add_row = add_row)
```

<table style="NAborder-bottom: 0; width: auto !important; margin-left: auto; margin-right: auto;" class="table">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:center;"> Petal width </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Sepal length </td>
   <td style="text-align:center;"> 0.732*** </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;"> (0.041) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> First tercile sepal width </td>
   <td style="text-align:center;"> −2.882*** </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;"> (0.242) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Second tercile sepal width </td>
   <td style="text-align:center;"> −3.083*** </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;"> (0.237) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Third tercile sepal width </td>
   <td style="text-align:center;"> −3.261*** </td>
  </tr>
  <tr>
   <td style="text-align:left;box-shadow: 0px 1px">  </td>
   <td style="text-align:center;box-shadow: 0px 1px"> (0.215) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Num.Obs. </td>
   <td style="text-align:center;"> 150 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P-value (Sepal width 1st tercile = Sepal width 2nd tercile) </td>
   <td style="text-align:center;"> 0.008 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P-value (Sepal width 1st tercile = Sepal width 3rd tercile) </td>
   <td style="text-align:center;"> 0.000 </td>
  </tr>
</tbody>
<tfoot><tr><td style="padding: 0; " colspan="100%">
<sup></sup> * p &lt; 0.1, ** p &lt; 0.05, *** p &lt; 0.01</td></tr></tfoot>
</table>

From the results of these hypotheses tests, we can reject the hypothesis that the effect of the first tercile of sepal width on petal width is equal to the effect of the second tercile of sepal width on petal width, and that the effect of the first tercile of sepal width on petal width is equal to the effect of the third tercile of sepal width on petal width.

<!--chapter:end:03-stat_tests.Rmd-->

# Geographic place matching
In applied work, we often have to deal with observations that have an associated address or coordinates but no geographic codes. For cases in which no coordinates are available, one option is to match addresses to coordinates using Google Place API searches, and then merge the resulting coordinates with shapefiles to obtain geographic codes.

## Google Places API searches
R package `googleway` makes it easy to perform Google Place API searches. `googleway::google_find_place` generates a [Find Place request](https://developers.google.com/maps/documentation/places/web-service/search-find-place), taking a text input and returning an array of place candidates, along with their corresponding search status. From this result, we can extract the address and coordinates. 

Currently, you get \$200 of Google Maps Platform usage every month for free. [Each request costs $0.017](https://mapsplatform.google.com/pricing/). While this may seem like little, generating 12,000 requests will already exceed the monthly free usage quota ($200 = 11,764.7 requests). It's easy to exceed this number of requests when you're running loops for large query vectors repeatedly. Therefore, the best practice is to start out with a small sample, ensure that searches are returning valid results, and then extend the method to the full sample. You can [set a maximum quota](https://developers.google.com/maps/documentation/places/web-service/report-monitor#quotas) of 375 requests per day (375 x 31 = 11,625) to ensure you don't exceed the monthly free usage limit.

To start using `googleway` to conduct Google Place API searches, you first need to [create a Google Cloud project](https://developers.google.com/maps/documentation/places/web-service/cloud-setup) and [set up an API key](https://developers.google.com/maps/documentation/places/web-service/get-api-key). Once you have set this up, you can load `googleway`, define the API key and start conducting searches. 
Here is a simple example of an individual query.


```r
pacman::p_load(here, tidyverse, googleway)

# (1.1): Set Google Place search API key.
key <- "KJzaLyCLI-nXPsHqVwz-jna1HYg2jKpBueSsTWs" # insert API key here
set_key(key)

# (1.2): Define tibble with addresses to look up.
missing_locs <- tribble(~id, ~address,
                        1, "Av. Álvaro Obregón 225, Roma Norte, Cuauhtémoc, CDMX, Mexico",
                        2, "Río Hondo #1, Col. Progreso Tizapán, Álvaro Obregón, CDMX, México")

# (1.3): Loop over missing addresses.
loc_coords <- tibble()
for (i in 1:nrow(missing_locs)) {     
  # Get results from Google Place search
  results <- google_find_place(missing_locs$address[i], inputtype = "textquery", language = "es")
  
  # Print results
  search_status <- ifelse(results[["status"]] == "OK", 
                          "search successful", 
                          "search returned no results")
  print(str_c("Working on address ", i, " out of ", nrow(missing_locs), ", ", search_status))
  
  # Extract formatted address and coordinates results
  clean_results <- tibble(id = missing_locs$id[i],
                          address_clean = results[["candidates"]][["formatted_address"]],
                          loc_lat = results[["candidates"]][["geometry"]][["location"]][["lat"]],
                          loc_lon = results[["candidates"]][["geometry"]][["location"]][["lng"]])
  
  # Append to full results dataframe.
  loc_coords %<>% bind_rows(clean_results)
}
rm(results, clean_results, i, search_status)

# (1.4): Keep first result for each address.
clean_results %<>%
  group_by(id) %>% 
  slice(1)
```

Once you finish your set of Google Place API requests, you should save the results to a CSV file for later use. The search process is not perfectly replicable as identical searches can produce different results over time, so you should only run your full search loop once.


<!--chapter:end:04-geo_data.Rmd-->


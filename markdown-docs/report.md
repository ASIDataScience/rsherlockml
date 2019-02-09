# `report`: Rmarkdown format for a Faculty report

## Description

Knits a file to HTML and makes that HTML available as a Faculty report

## Usage

```r
report(..., quiet = TRUE, mathjax = NULL)
```

## Arguments

Argument      |Description
------------- |----------------
```...```     |     all parameters that can be passed to `rmarkdown::html_document`

## Details

Simply add `output: rfaculty::report` to your Rmd yaml.

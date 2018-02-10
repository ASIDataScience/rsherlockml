# rsherlockml

This package could have a bunch of sherlock utilities in it. Right now, it has the report `Rmd` template and output format.

### Usage

Create a new R-markdown report by going to:

New file -> R Markdown -> from Template -> SherlockML report

Or simply put `output: rsherlockml::report` in the yaml front matter of your Rmd file.

### Command line usage

You can also use it from the command line, simply call:

`R -e "rmarkdown::render('path/to/file.Rmd')"`

**However**, this requires either an up-to-date version of pandoc to be available, or for the Rstudio-shipped version
of pandoc to be available as an environment variable (RSTUDIO_PANDOC, see here: https://github.com/rstudio/rmarkdown/issues/289)

Maybe we can add this to the right click -> publish options.

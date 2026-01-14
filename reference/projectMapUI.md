# Shiny Map UI for Projects

Shiny module creating an interactive project map

## Usage

``` r
projectMapUI(
  id,
  label = "projectMap",
  custom_css_path = system.file("app/styles.css", package = "weda"),
  custom_js_path = system.file("app/gomap.js", package = "weda"),
  colour_vars
)

projectMapServer(id, project_locations, con)
```

## Arguments

- id:

  module id

- label:

  module label

- custom_css_path:

  custom css path for map

- custom_js_path:

  custom javascript path for module

- colour_vars:

  variables to colour by

- project_locations:

  data.frame of survey locations

- con:

  database connection

## Value

shiny module

shiny module

## Functions

- `projectMapServer()`:

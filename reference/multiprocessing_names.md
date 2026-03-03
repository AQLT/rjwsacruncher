# Get the names of the multiprocessings of a workspace

Function to get the name of the multiprocessings that appears on
'JDemetra+' and the name of the corresponding XML file.

## Usage

``` r
multiprocessing_names(workspace)
```

## Arguments

- workspace:

  Path to the workspace. By default a dialog box opens to choose the
  workspace.

## Value

A `data.frame` containing the name of the multiprocessings that appears
on 'JDemetra+' (column `name`) and the name of the associated XML files
(column `file`).

## See also

[`cruncher_and_param()`](cruncher_and_param.md).

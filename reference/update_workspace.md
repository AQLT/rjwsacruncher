# Update a workspace

Function to update a workspace without exporting the results.

## Usage

``` r
update_workspace(
  workspace = NULL,
  policy = "parameters",
  cruncher_bin_directory = getOption("cruncher_bin_directory"),
  log_file = NULL
)
```

## Arguments

- workspace:

  Path to the workspace. By default a dialog box opens to choose the
  workspace.

- policy:

  Refreshing policy of the processing. By default
  `policy = "parameters"` (re-estimation of the coefficients of the
  reg-ARIMA model, see details).

- cruncher_bin_directory:

  Path to the "bin" directory of the 'JWSACruncher'. By default the
  value of the option `"cruncher_bin_directory"` is used.

- log_file:

  Name of the log file of 'JWSACruncher'. By default the log isn't
  exported.

## Value

Path to the workspace.

## See also

[`cruncher()`](cruncher.md),
[`cruncher_and_param()`](cruncher_and_param.md).

# Create parameter file for the 'JWSACruncher'

To run the 'JWSACruncher' needs a parameter file and `create_param_file`
allows to create it.

## Usage

``` r
create_param_file(
  dir_file_param,
  bundle = 10000,
  csv_layout = "list",
  csv_separator = ";",
  ndecs = 6,
  full_series_name = TRUE,
  short_column_headers = TRUE,
  rslt_name_level = 2,
  policy = "parameters",
  refreshall = TRUE,
  output = NULL,
  matrix_item = getOption("default_matrix_item"),
  tsmatrix_series = getOption("default_tsmatrix_series"),
  paths_path = NULL,
  v3 = getOption("is_cruncher_v3"),
  file_param = file.path(dir_file_param, "parameters.param")
)
```

## Arguments

- dir_file_param:

  Path to the directory that will contains the parameter file
  `"parameters.param"` (if `file_param` not supplied).

- bundle:

  Maximum size for a group of series (in output). By default
  `bundle = 10000`.

- csv_layout:

  Layout of the CSV files (series only). By default
  `csv_layout = "list"`. Other options: `csv_layout = "vtable"`
  (vertical table) or `csv_layout = "htable"` (horizontal table).

- csv_separator:

  The field separator string used in the CSV file. By default
  `csv_separator = ";"`.

- ndecs:

  Number of decimals used in the output. By default `ndec = 6`.

- full_series_name:

  Boolean indicating if the fully qualified name of the series will be
  used (the default `full_series_name = TRUE`) or if only the name of
  the series should be displayed. Only used when `v3 = TRUE`.

- short_column_headers:

  Boolean indicating if the full column names should be printed (the
  default `short_column_headers = TRUE`) instead of always outputting
  shortened ones. Only used when `v3 = TRUE` (since v.3.4.0).

- rslt_name_level:

  Only used when `v3 = TRUE`.

- policy:

  Refreshing policy of the processing. By default
  `policy = "parameters"` (re-estimation of the coefficients of the
  reg-ARIMA model, see details).

- refreshall:

  Boolean indicating if the data is refreshed (by default
  `refreshall = TRUE`).

- output:

  Full path of the output folder. By default (`output = NULL`) a folder
  is create in the path to the workspace (\[workspace\]/Output).

- matrix_item:

  Character containing the items of the matrix output (see the
  'JDemetra+' manual for more information). By default, the items
  defined in the option `getOption("default_matrix_item")` are used
  (option initialized by the default output of the 'JWSACruncher'
  2.2.2).

- tsmatrix_series:

  Character containing the names of the times series to export (see the
  'JDemetra+' manual for more information). By default, the items
  defined in the option `getOption("default_tsmatrix_series")` are used
  (option initialized by the default output of the 'JWSACruncher'
  2.2.2).

- paths_path:

  The paths used for relative addresses (see the "Demetra Paths" of the
  graphical interface of 'JDemetra+').

- v3:

  Boolean indicating if the parameter file should be compatible with a
  version 3.0.0 and higher of 'JWSACRuncher' (`v3 = TRUE`) or a lower
  version (`v3 = FALSE`). By default the value of the option
  `"is_cruncher_v3"` is used.

- file_param:

  Path to the parameters file. By default the file is named
  `parameters.param` and it is created at the `fir_file_param`
  directory.

## Value

Path to the parameter file.

## Details

When the 'JWSACruncher' is launched, the data is refreshed with a
specific policy that is defined by the parameter `policy`. The available
options are:

- `policy = "current"`: all the estimations are fixed and AO added for
  new data (since v.2.2.3), short name `policy = "n"`;

- `policy = "fixed"`: all the estimations are fixed (since v.2.2.3),
  short name `policy = "f"`;

- `policy = "fixedparameters"`: re-estimation of the coefficients of the
  regression variables (but not the ARIMA coefficients), short name
  `policy = "fp"`;

- `policy = "fixedarparameters"`: re-estimation of the coefficients of
  the regression variables and of the MA coefficients of the ARIMA model
  (but not the AR coefficients), short name `policy = "farp"` (since
  v.3.4.0);

- `policy = "parameters"` (the default): `policy = "fixedparameters"` +
  re-estimation of ARIMA coefficients, short name `policy = "p"`;

- `policy = "lastoutliers"`: `policy = "parameters"` + re-identification
  of last outliers (on the last year), short name `policy = "l"`;

- `policy = "outliers"`: `policy = "lastoutliers"` + re-identification
  of all outliers, short name `policy = "o"`;

- `policy = "stochastic"`: `policy = "outliers"` + re-identification of
  ARIMA orders, short name `policy = "s"`;

- `policy = "complete"` or `policy = "concurrent"`: the model is
  completely re-identified and re-estimated, short name `policy = "c"`.

## See also

[`read_param_file()`](read_param_file.md),
[`list2param_file()`](list2param_file.md),
[default_param_file](default_param_file.md)
[`cruncher_and_param()`](cruncher_and_param.md).

## Examples

``` r
if (FALSE) { # \dontrun{
dir = tempdir()
# Here a file parameters.param is created in the directory dir
# with default parameters of the different options
create_param_file(dir)
# to only export the raw and the seasonally adjusted series
 create_param_file(dir, 
                   tsmatrix_series = c("y", "sa"))
} # }
```

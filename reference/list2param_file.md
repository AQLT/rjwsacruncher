# Create parameter file from list

Create parameter file from list

## Usage

``` r
list2param_file(
  dir_file_param,
  x,
  file_param = file.path(dir_file_param, "parameters.param")
)
```

## Arguments

- dir_file_param:

  Path to the directory that will contains the parameter file
  `"parameters.param"` (if `file_param` not supplied).

- x:

  A list, for example create by
  [`read_param_file()`](read_param_file.md) or
  [`default_param_file()`](default_param_file.md).

- file_param:

  Path to the parameters file. By default the file is named
  `parameters.param` and it is created at the `fir_file_param`
  directory.

## See also

[`create_param_file()`](create_param_file.md),
[`read_param_file()`](read_param_file.md),
[`default_param_file()`](default_param_file.md)
[`cruncher_and_param()`](cruncher_and_param.md).

## Examples

``` r
if (FALSE) { # \dontrun{
dir = tempdir()
# Here a file parameters.param is created in the directory dir
# with default parameters of 'JWSACruncher' v2
list_param <- default_param_file(v3 = FALSE)
list2param_file(dir, list_param)
# to only export the raw and the seasonally adjusted series
list_param$tsmatrix_series <- c("y", "sa")
list2param_file(dir, list_param)
} # }
```

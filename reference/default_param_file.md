# Get default parameters of the 'JWSACruncher'

Get default parameters of the 'JWSACruncher'

## Usage

``` r
default_param_file(
  v3 = getOption("is_cruncher_v3"),
  cruncher_bin_directory = NULL
)
```

## Arguments

- v3:

  Boolean indicating if the parameters are the from a version 3.0.0 and
  higher of 'JWSACRuncher' (`v3 = TRUE`) or a lower version
  (`v3 = FALSE`). By default the value of the option `"is_cruncher_v3"`
  is used.

- cruncher_bin_directory:

  Path to the directory that contains the 'JWSACruncher' binary. If
  defined, the parameter `v3` is ignored and the 'JWSACruncher' is run
  without parameter to generate the default parameters file.

## See also

[`create_param_file()`](create_param_file.md),
[`read_param_file()`](read_param_file.md),
[`list2param_file()`](list2param_file.md),
[`cruncher_and_param()`](cruncher_and_param.md).

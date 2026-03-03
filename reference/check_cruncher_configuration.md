# Check Cruncher configuration This function checks if the Cruncher configuration is valid by verifying the presence of the `jwsacruncher` executable in the specified `cruncher_bin_directory`.

Check Cruncher configuration This function checks if the Cruncher
configuration is valid by verifying the presence of the `jwsacruncher`
executable in the specified `cruncher_bin_directory`.

## Usage

``` r
check_cruncher_configuration(
  cruncher_bin_directory = getOption("cruncher_bin_directory"),
  print_message = TRUE
)
```

## Arguments

- cruncher_bin_directory:

  Path to the "bin" directory of the 'JWSACruncher'. By default the
  value of the option `"cruncher_bin_directory"` is used.

- print_message:

  Logical. If `TRUE`, a message will be printed indicating whether the
  configuration is valid or not.

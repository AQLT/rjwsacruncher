# Read parameter file of the 'JWSACruncher'

Read parameter file of the 'JWSACruncher'

## Usage

``` r
read_param_file(file)
```

## Arguments

- file:

  Path to the parameter file.

## See also

[`create_param_file()`](create_param_file.md),
[`list2param_file()`](list2param_file.md),
[`default_param_file()`](default_param_file.md)
[`cruncher_and_param()`](cruncher_and_param.md).

## Examples

``` r
dir = tempdir()
list_param <- default_param_file(v3 = FALSE)
list2param_file(dir, list_param)
list_param_2 <- read_param_file(file.path(dir, "parameters.param"))
all.equal(list_param, list_param_2)
#> [1] TRUE
```

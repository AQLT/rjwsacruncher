# Run the 'JWSACruncher'

Function to run the 'JWSACruncher' on a workspace from a parameter file.

## Usage

``` r
cruncher(
  workspace,
  cruncher_bin_directory = getOption("cruncher_bin_directory"),
  param_file_path,
  log_file = NULL,
  rename_multi_documents = FALSE,
  delete_existing_file = FALSE
)
```

## Arguments

- workspace:

  Path to the workspace. By default a dialog box opens to choose the
  workspace.

- cruncher_bin_directory:

  Path to the "bin" directory of the 'JWSACruncher'. By default the
  value of the option `"cruncher_bin_directory"` is used.

- param_file_path:

  Path to the parameter file of the 'JWSACruncher'. By default a .params
  file is search in the save directory of the workspace.

- log_file:

  Name of the log file of 'JWSACruncher'. By default the log isn't
  exported.

- rename_multi_documents:

  Boolean indicating whether to rename the folders containing the
  outputs according to the names of the multi-documents of the
  workspace. By default `rename_multi_documents = FALSE`: the names of
  the XML files of the multi-documents are used.

- delete_existing_file:

  Only used if `rename_multi_documents = TRUE`. Boolean indicating
  whether to delete existing folders when renaming them. By default
  (`delete_existing_file = FALSE`) they are not deleted.

## Value

The path to the workspace.

## See also

Around the 'JWSACruncher':
[`cruncher_and_param()`](cruncher_and_param.md),
[`update_workspace()`](update_workspace.md). To create the parameter
file: [`create_param_file()`](create_param_file.md) and
[`list2param_file()`](list2param_file.md).

## Examples

``` r
if (FALSE) { # \dontrun{
dir = tempdir()
# First create a parameter file
# here a file parameters.param is created in the directory dir
# with default parameters of 'JWSACruncher' v2
list2param_file(dir, default_param_file(v3 = FALSE))
# If the option "cruncher_bin_directory" is correctly defined:
cruncher("workspace.xml",
          param_file_path = file.path(dir, "parameters.param"))
} # }
```

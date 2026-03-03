# Run quickly the 'JWSACruncher'

Function to run the 'JWSACruncher' on a workspace while creating the
parameter file.

## Usage

``` r
cruncher_and_param(
  workspace = NULL,
  output = NULL,
  rename_multi_documents = FALSE,
  delete_existing_file = FALSE,
  log_file = NULL,
  cruncher_bin_directory = getOption("cruncher_bin_directory"),
  ...
)
```

## Arguments

- workspace:

  Path to the workspace. By default a dialog box opens to choose the
  workspace.

- output:

  Full path of the output folder. By default (`output = NULL`) a folder
  is create in the path to the workspace (\[workspace\]/Output).

- rename_multi_documents:

  Boolean indicating whether to rename the folders containing the
  outputs according to the names of the multi-documents of the
  workspace. By default `rename_multi_documents = FALSE`: the names of
  the XML files of the multi-documents are used.

- delete_existing_file:

  Only used if `rename_multi_documents = TRUE`. Boolean indicating
  whether to delete existing folders when renaming them. By default
  (`delete_existing_file = FALSE`) they are not deleted.

- log_file:

  Name of the log file of 'JWSACruncher'. By default the log isn't
  exported.

- cruncher_bin_directory:

  Path to the "bin" directory of the 'JWSACruncher'. By default the
  value of the option `"cruncher_bin_directory"` is used.

- ...:

  Other parameters of [`create_param_file()`](create_param_file.md).

## Value

Path to the workspace.

## See also

[`cruncher()`](cruncher.md),
[`update_workspace()`](update_workspace.md),
[`create_param_file()`](create_param_file.md),
[`multiprocessing_names()`](multiprocessing_names.md).

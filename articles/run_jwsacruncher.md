# Run the 'JWSACruncher' with rjwsacruncher

## Launch the ’JWSACruncher”

To use `rjwsacruncher` you need to download the JWACruncher. It is
available on <https://github.com/jdemetra/jdemetra-app/releases> (for
JDemetra+ 2.x.y versions) or
<https://github.com/jdemetra/jdplus-main/releases> (for JDemetra+ 3.x.y
versions). It can also be downloaded with the function
[`download_cruncher()`](../reference/download_cruncher.md):

``` r
library(rjwsacruncher)
# Directory where to save the JWSACruncher:
directory <- tempdir()
download_cruncher(directory)
# for JDemetra+ 3.x.y versions :
download_cruncher(directory, v3 = TRUE)
```

To configure the JWSACruncher with a portable version of Java you can
use the function
[`configure_jwsacruncher()`](../reference/configure_jwsacruncher.md).
See [`?configure_jwsacruncher`](../reference/configure_jwsacruncher.md)
for more informations.

To run the JWSACruncher you need three files:

- a “.param” file containing the refresh policy and the items of the
  seasonal adjustment to export;  
- a valid JDemetra+ workspace;
- the path to the JWSACruncher.

In rjwsacruncher, there are four functions associated to run the
JWSACruncher:

- [`create_param_file()`](../reference/create_param_file.md) or
  [`list2param_file()`](../reference/list2param_file.md) to create the
  “.param” parameter file;  
- [`cruncher()`](../reference/cruncher.md) to run the JWSACruncher on a
  workspace from a parameter file;  
- [`cruncher_and_param()`](../reference/cruncher_and_param.md) to run
  the ‘JWSACruncher’ on a workspace while creating the parameter file;
- [`update_workspace()`](../reference/update_workspace.md) to update a
  workspace without exporting the results.

### Create the parameter file with `create_param_file()`

The parameters of the function
[`create_param_file()`](../reference/create_param_file.md) are those
described in the wiki of the JWSACruncher:
<https://github.com/jdemetra/jwsacruncher/wiki>. The three most
important parameters of
[`create_param_file()`](../reference/create_param_file.md) are:

1.  `policy` the refresh policy (see table below).

| Option on JDemetra+                                                                                   | Option for the JWSACruncher | Short name | Description                                                                                                                                                                                                                                                                                                 |
|:------------------------------------------------------------------------------------------------------|:----------------------------|:-----------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Current\[AO\]: fixed model + AO for new data                                                          | current                     | n          | The ARIMA model, outliers and other regression parameters are not re-identified and the values of all parameters are fixed. The transformation type remains unchanged. An additive outlier (AO) is added for new data. (since v.2.2.3)                                                                      |
| Partial concurrent adjustment -\> Fixed model                                                         | fixed                       | f          | The ARIMA model, outliers and other regression parameters are not re-identified and the values of all parameters are fixed. The transformation type remains unchanged. (since v.2.2.3)                                                                                                                      |
| Partial concurrent adjustment -\> Estimate regression coefficients                                    | fixedparameters             | fp         | The ARIMA model, outliers and other regression parameters are not re-identified. The coefficients of the ARIMA model are fixed, other coefficients are re-estimated. The transformation type remains unchanged.                                                                                             |
| Partial concurrent adjustment -\> Estimate regression coefficients + MA parameters of the ARIMA model | fixedarparameters           | farp       | The ARIMA model, outliers and other regression parameters are not re-identified. The AR coefficients of the ARIMA model are fixed, other coefficients are re-estimated (MA coefficients + regressors). The transformation type remains unchanged. (since v.3.4.0)                                           |
| Partial concurrent adjustment -\> Estimate regression coefficients + Arima parameters                 | parameters (by default)     | p          | The ARIMA model, outliers and other regression parameters are not re-identified. All parameters of the RegARIMA model are re-estimated. The transformation type remains unchanged.                                                                                                                          |
| Partial concurrent adjustment -\> Estimate regression coefficients + Last outliers                    | lastoutliers                | l          | The ARIMA model, outliers (except from the outliers in the last year of the sample) and other regression parameters are not re-identified. All parameters of the RegARIMA model are re-estimated. The outliers in the last year of the sample are re-identified. The transformation type remains unchanged. |
| Partial concurrent adjustment -\> Estimate regression coefficients + all outliers                     | outliers                    | o          | The ARIMA model and regression parameters, except from outliers) are not re-identified. All parameters of the RegARIMA model are re-estimated. All outliers are re-identified. The transformation type remains unchanged.                                                                                   |
| Partial concurrent adjustment -\> Estimate regression coefficients + Arima model                      | stochastic                  | s          | Re-identification of the ARIMA model, outliers and regression variables, except from the calendar variables. The transformation type remains unchanged.                                                                                                                                                     |
| Concurrent                                                                                            | complete (or concurrent)    | c          | Re-identification of the whole RegARIMA model.                                                                                                                                                                                                                                                              |

Refresh policies

2.  `matrix_item` character containing the items of the matrix output.
    By default, the items defined in the option `default_matrix_item`
    are exported. To change it we can change the option
    `default_matrix_item` or the parameter `matrix_item`:

``` r
# To get the default parameters
getOption("default_matrix_item")
# To change the default parameters to, for example, only export
# the information criteria:
options(default_matrix_item = c("likelihood.aic",
                                "likelihood.aicc",
                                "likelihood.bic",
                                "likelihood.bicc"))
```

3.  `tsmatrix_series` character containing the names of the times series
    to export. By default, the items defined in the option
    `default_tsmatrix_series` are exported. To change it we can change
    the option `default_tsmatrix_series` or the parameter
    `tsmatrix_series`:

``` r
# To get the default parameters
getOption("default_tsmatrix_series")
# To change the default parameters to, for example, only export
# the seasonally adjusted series and its forecasts:
options(default_tsmatrix_series = c("sa", "sa_f"))
```

For more informations, see the help of the function:
[`?create_param_file`](../reference/create_param_file.md). Below some
examples to create the parameter file:

``` r
export_dir <- tempdir()
# To create the file parameters.params in the directory export_dir with
# the refresh policy "lastoutliers" and the others default parameters:
create_param_file(dir_file_param = export_dir,
                  policy = "lastoutliers")

# If the options "default_matrix_item" and "default_tsmatrix_series" were
# changed to only export the information criteria, the seasonally adjusted series and its forecasts, the previous code is equivalent to:
create_param_file(dir_file_param = export_dir,
                  policy = "lastoutliers",
                  matrix_item = c("likelihood.aic", "likelihood.aicc",
                                  "likelihood.bic", "likelihood.bicc"),
                  tsmatrix_series = c("sa", "sa_f"))
```

Parameter files can be read with
[`read_param_file()`](../reference/read_param_file.md) which returns a
list than can also be modified and exported with
[`list2param_file()`](../reference/list2param_file.md):

``` r
param_f <- read_param_file(file.path(export_dir, "parameters.param"))
str(param_f)
```

    ## List of 7
    ##  $ config         :List of 4
    ##   ..$ bundle       : chr "10000"
    ##   ..$ csv_layout   : chr "list"
    ##   ..$ csv_separator: chr ";"
    ##   ..$ ndecs        : chr "6"
    ##  $ policy         : chr "lastoutliers"
    ##  $ refreshall     : logi TRUE
    ##  $ output         : NULL
    ##  $ matrix_item    : chr [1:4] "likelihood.aic" "likelihood.aicc" "likelihood.bic" "likelihood.bicc"
    ##  $ tsmatrix_series: chr [1:2] "sa" "sa_f"
    ##  $ paths_path     : NULL

The default parameters files of JDemetra+ 2.x.y and JDemetra+ 3.x.y can
be retrieved with
[`default_param_file()`](../reference/default_param_file.md).

### To run the JWSACruncher

To run the JWSACruncher with [`cruncher()`](../reference/cruncher.md) or
[`cruncher_and_param()`](../reference/cruncher_and_param.md), you have
to specify the path to the JWSACruncher (`cruncher_bin_directory`
parameter) and the path to the workspace (`workspace` parameter).

By default, the path to the JWSACruncher is the value of the option
`cruncher_bin_directory`: therefore you only have to change this option
once so that it applies to all the future running. The path to specify
is the folder containing the *jwsacruncher.bat* file which is under the
“Bin” folder of the JWSACruncher. Thus, if it is installed in
`D:\jdemetra-cli-2.2.2`, the file *jwsacruncher.bat* will be under
`D:\jdemetra-cli-2.2.2\bin` and you have to change the
`cruncher_bin_directory` option as follows:

``` r
options(cruncher_bin_directory = "D:/jdemetra-cli-2.2.2/bin/")
```

If no workspace is specified, a dialog box opens to select it.

The [`cruncher_and_param()`](../reference/cruncher_and_param.md)
function allows to create a temporary parameter file with
[`create_param_file()`](../reference/create_param_file.md) and then run
the JWSACruncher with [`cruncher()`](../reference/cruncher.md). In
addition to the parameters available in these two functions,
[`cruncher_and_param()`](../reference/cruncher_and_param.md) allows to
rename the output folder containing the workspace results so that they
are equal to the names of the multi-documents displayed in the JDemetra+
software with the parameter `rename_multi_documents` (equals to `FALSE`
by default). Below are some examples:

``` r
# The following code updates the workspace "workspace", that is under the folder D:/, 
# with the refresh policy "lastoutliers". Others parameters are the default ones of create_param_file().
# In particular, the exported parameters are those of the options
# "default_matrix_item" options and "default_tsmatrix_series" and the results 
# will be under D:/workspace/Output/.
cruncher_and_param(workspace = "D:/workspace.xml",
                   policy = "lastoutliers")

# Use the parameter "outpout" to change the folder that will contains the results
cruncher_and_param(workspace = "D:/workspace.xml",
                   output = "D:/Results/",
                   policy = "lastoutliers")

# To change the names of the folders containing the outputs so that they are equal
# to the names of the multi-documents displayed in JDemetra+, use the parameter
# "rename_multi_documents = TRUE". The parameter "delete_existing_file = TRUE"
# allows to delete any existing folders with the same name as the multi-documents.
cruncher_and_param(workspace = "D:/workspace.xml",
                   rename_multi_documents = TRUE,
                   delete_existing_file = TRUE,
                   policy = "lastoutliers")

# To see the other parameters:
?cruncher_and_param
```

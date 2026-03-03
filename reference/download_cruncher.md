# Dowload the 'JWSACruncher'

Function to download the ZIP file of the 'JWSACruncher'
(`download_cruncher()`) or 'JDemetra+' (`download_jdemetra()`).

## Usage

``` r
download_cruncher(
  directory,
  cruncher_version = NULL,
  v3 = getOption("is_cruncher_v3"),
  standalone = FALSE
)

download_jdemetra(
  directory,
  cruncher_version = NULL,
  v3 = getOption("is_cruncher_v3"),
  standalone = FALSE
)
```

## Arguments

- directory:

  Directory where to save the file. In Windows, a dialog box opens by
  default to select the directory.

- cruncher_version:

  Character of the version to download ("X.Y.Z" format). By default the
  last version is downloaded.

- v3:

  Boolean indicating, when parameter `cruncher_version` is missing, if
  the last version of the 'JWSACruncher' should be a 3.x.y version or a
  2.x.y. By default the value of the option `"is_cruncher_v3"` is used.

- standalone:

  Boolean indicating if the standalone version should be downloaded
  (only available when `v3 = TRUE`).

## Details

The 'JWSACruncher' is downloaded from
<https://github.com/jdemetra/jwsacruncher/releases> for versions lower
than 3.0.0 and from <https://github.com/jdemetra/jdplus-main/releases>
for the other versions. To use it, it has to be unzip.

## See also

[`configure_jwsacruncher()`](configure_jwsacruncher.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# On Windows opens a dialog box to choose the directory where to
# download the last version of the 'JWSACruncher'
download_cruncher()

dir <- tempdir()
# To download the last release:
download_cruncher(dir)

# To download the release of the version 2.2.2:
download_cruncher(dir, "2.2.2")
} # }
```

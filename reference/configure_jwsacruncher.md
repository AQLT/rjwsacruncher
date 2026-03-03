# Configure the 'JWSACruncher' with a portable version of 'Java'

Function configure the 'JWSACruncher' with a portable version of 'Java'.

## Usage

``` r
configure_jwsacruncher(jwsacruncher_path, java_path)
```

## Arguments

- jwsacruncher_path:

  Path to the file `jwsacruncher.bat` of the 'JWSACruncher' (see
  details).

- java_path:

  Path to the file `java.exe` of the portable version of 'Java' (see
  details).

## Details

Since the version 2.2.0, the 'JWSACruncher' needs 'Java' 8 or higher to
run. For versions 3.0.0 and higher, 'JWSACruncher' needs 'Java' 17 or
higher. In 'Windows' versions 3.0.0 and higher of 'JWSACruncher'
includes a portable version of 'Java'. For lower version of
'JWSACruncher', if you cannot install 'Java' (for example for security
reasons) you can install a portable version of 'Java' (that does not
require administrator rights) and configure the 'JWSACruncher' to use
this portable version. To do it you have to:

1.  Unzip the downloaded file of the 'JWSACruncher';

2.  Open, with a Text Editor, the file `jwsacruncher.bat` that is in the
    sub-folder `/bin/` of the 'JWSACruncher';

3.  Edit the line 71 that contains `if "%JAVACMD%"=="" set JAVACMD=java`
    and replace `java` by the path to the file `java.exe` of the
    portable version. For example, if the portable version of 'Java' is
    installed under `D:/Software/Java`, the path to `java.exe` should be
    at `D:/Software/Java/bin/java.exe` and the new line 71 would be
    `if "%JAVACMD%"=="" set JAVACMD="D:\Software\Java\bin\java.exe"`.

The function `configure_jwsacruncher()` does the steps 2 and 3.

## See also

[`download_cruncher()`](download_cruncher.md).

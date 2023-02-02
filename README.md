# deposits

<!-- badges: start -->

[![R build
status](https://github.com/ropenscilabs/deposits/workflows/R-CMD-check/badge.svg)](https://github.com/ropenscilabs/deposits/actions?query=workflow%3AR-CMD-check)
[![codecov](https://codecov.io/gh/ropenscilabs/deposits/branch/main/graph/badge.svg)](https://codecov.io/gh/ropenscilabs/deposits)
[![Project Status:
Concept](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

The `deposits` R package is a universal client for depositing and
accessing research data anywhere. Currently supported services are
[zenodo](https://zenodo.org) and [figshare](https://figshare.com). The
package works seamlessly with [the “frictionless” data
workflow](https://frictionlessdata.io/). Instructions for installing and
setting up the package are in [a separate
vignette](https://docs.ropensci.org/deposits/articles/install-setup.html).
This README gives a brief overview of package usage, with more detail in
[the introductory
vignette](https://docs.ropensci.org/deposits/articles/deposits.html).

## Data Repositories

The list of data repositories currently supported is accessible by the
`deposits_services()` function:

``` r
library (deposits)
deposits_services ()
```

    ##             name                           docs                    api_base_url
    ## 1         zenodo https://developers.zenodo.org/         https://zenodo.org/api/
    ## 2 zenodo-sandbox https://developers.zenodo.org/ https://sandbox.zenodo.org/api/
    ## 3       figshare     https://docs.figshare.com/    https://api.figshare.com/v2/

[`zenodo`](https://zenodo.org) offers a “sandbox” environment, which
offers an ideal environment for testing the functionality of this
package.

## The “frictionless” data workflow

Prior to describing the `deposits` workflow, it is useful to understand
the [“frictionless” data workflow](https://frictionlessdata.io/), which
provides a standardised way to document the contents and structure of
data files through a separate metadata file. The `deposits` package
links directly with [rOpenSci’s `frictionless`
package](https://docs.ropensci.org/frictionless) for writing, editing,
and reading frictionless metadata files.

The `deposits` package nevertheless aims to make this integration as
seamless and painless as possible. No knowledge of the frictionless
workflow is necessary, and the `deposits` workflow should “just work” in
many cases, appropriately constructing frictionless metadata files and
uploading them to deposits services. It is of course also possible to
use the `deposits` package without frictionless metadata. All of these
possibilities are illustrated in the following sections.

## The deposits workflow

The `deposits` package uses [the `R6`
package](https://github.com/r-lib/R6) to create [a
`depositsClient`](https://docs.ropensci.org/deposits/reference/depositsClient.html)
used to perform all functions. All `deposits` operations start with a
client constructed with the `new()` function:

``` r
cli <- depositsClient$new (service = "zenodo", sandbox = TRUE)
print (cli)
#> <deposits client>
#>  deposits service : zenodo
#>            sandbox: TRUE
#>          url_base : https://sandbox.zenodo.org/api/
#>  Current deposits : <none>
#>
#>    hostdata : <none>
#>    metadata : <none>
```

The upper section of information shown by printing the client describes
information on the client in general, and aspects of the nominated
service associated with the user (identified through the token, as
described in [the “Installation and Setup”
vignette](https://docs.ropensci.org/deposits/articles/install-setup.html#setup-api-tokens)).
The lower section contains information on the current deposit held
within the client. A new client initially contains no information on a
deposit, and so these lower sections are empty.

Having constructed a new client, the deposits workflow then involves the
following main steps:

1.  Fill the client with metadata;
2.  Create a new deposit on the nominated service; and
3.  Upload files for the deposit.

### Fill client with metadata

The `deposits` package ultimately aims to simplify the management of
data on external repositories. All of these platforms require additional
metadata describing the data to be deposited, with the `deposits`
package working with two main types of metadata:

1.  Metadata which describe a deposit and associated properties, such as
    author names and affiliations, deposit titles and descriptions,
    dates, keywords, links to other deposits or publications, and many
    other terms.
2.  Metadata which describe the actual contents of the data to be
    deposited. These kinds of metadata are (optionally) generated and
    (always) handled here by [the `frictionless`
    package](https://docs.ropensci.org/frictionless).

As explained at the outset, the `deposits` package can be used without
knowing or understanding anything about [the `frictionless`
package](https://docs.ropensci.org/frictionless) or [frictionless
workflows](https://frictionlessdata.io). Most of this present section
therefore concerns the first of these two kinds of metadata, with brief
demonstrations of the second kind used to illustrate how the
frictionless workflow integrates with the general `deposits` workflow.
The term “metadata” throughout the following should be understood to
refer to the first of the above kinds of metadata; the second kind is
always referred to here as “frictionless metadata.”

Metadata can be included in a deposits client in two main ways, either
on initial construction, or through attaching metadata to an existing
client. In both cases, metadata may be specified in a variety of ways,
one of the easiest of which is to specify metadata is as a list of named
terms like the following example:

``` r
metadata <- list (
    title = "New Title",
    abstract = "This is the abstract",
    creator = list ("A. Person", "B. Person")
)
```

These data can be used in construction of a new client by passing a
`metadata` argument:

``` r
cli <- depositsClient$new (
    service = "zenodo",
    sandbox = TRUE,
    metadata = metadata
)
print (cli)
#> <deposits client>
#>  deposits service : zenodo
#>            sandbox: TRUE
#>          url_base : https://sandbox.zenodo.org/api/
#>  Current deposits : <none>
#>
#>    hostdata : <none>
#>    metadata : 4 terms (see 'metadata' element for details)
```

The “metadata” component of the client now holds 4 terms (including an
additional “created” datestamp). Equivalently, metadata can be added to
an existing client with [the `deposit_fill_metadata()`
function](https://docs.ropensci.org/deposits/reference/depositsClient.html#method-depositsClient-deposit_fill_metadata):

``` r
cli <- depositsClient$new (service = "zenodo", sandbox = TRUE)
cli$deposit_fill_metadata (metadata)
```

Other ways of specifying and entering metadata are described in [the
introductory
vignette](https://docs.ropensci.org/deposits/articles/deposits.html).

Note that R6 functions are called directly on the client, with the
object itself (`cli`) updated by the call. There is thus no need to
call,

``` r
cli <- cli$deposit_fill_metadata (metadata)
```

as the `cli` object is updated by the call as shown above, and does not
need to be assigned to any return value.

### Create a new deposit

The metadata filled with the above steps can then be used to initiate a
new deposit on the associated server using the `deposit_new()` function:

``` r
cli$deposit_new ()
#> ID of new deposit : 1065666
print (cil)
#> <deposits client>
#>  deposits service : zenodo
#>            sandbox: TRUE
#>          url_base : https://sandbox.zenodo.org/api/
#>  Current deposits : <none>
#>
#>  url_service : https://sandbox.zenodo.org/deposit/1065666
#>   deposit id : 1065666
#>     hostdata : list with 14  elements
#>     metadata : 4 terms (see 'metadata' element for details)
```

The client now includes several additional elements, notably a “deposit
id” (stored in `cli$id`) giving the unique identifier for the new
deposit, and a `hostdata` item with, in this case, 14 elements as
specified by the host service. The `url_service` is the URL for the
newly-created deposit. (Viewing in a web browser will require logging in
for all private and sandbox deposits).

### Uploading (and downloading) files

A deposit is really about data, not just metadata. Uploading data to a
deposit is as simple as the following function:

``` r
cli$deposit_upload_file (path = "<path>/<to>/my-data.dat")
```

Details of files associated with deposits are stored in a `data.frame`
held as `cli$hostdata$files` which after running that command will
include a row with an item whose “filename” will be “my-data.dat”.

``` r
cli$hostdata$files
#>                           checksum    filename filesize
#> 1 5955bb96a8fee3bc89549bde9ef9b470 my-data.dat      829
#>                                     id
#> 1 618ae9b9-af48-4b86-aa37-7b4e767dccb7
#>                                                 links.download
#> 1 https://sandbox.zenodo.org/api/files/<file-hash>/my-data.dat
#>                                                                links.self
#> 1 https://sandbox.zenodo.org/api/deposit/depositions/1065666/files/<hash>
```

The `deposit_download_file()` function does the reverse:

``` r
path <- cli$deposit_download_file ("my-data.dat")
```

Files are by default downloaded to the current working directory, or
elsewhere specified by an additional `path` parameter. The
`deposit_download_file()` function is the only main client function
which returns a value, in this case of the full path to the downloaded
file.

### Additional functions

A deposits client offers a few functions beyond those demonstrated
above. The command `ls(cli)` lists all elements of the R6 client,
including all variables and functions, with functions prefixed with
`deposit_` or `deposits_`. Additional functions include:

1.  `deposit_delete(deposit_id)` to delete a nominated deposit;
2.  `deposit_retrieve(deposit_id)` to populate a local deposits client
    with information on a specified deposit;
3.  `deposit_update(deposit_id)` to update deposit on server with local
    metadata;
4.  `deposits_list()` to update lists of current deposits within client.
5.  `deposits_search()` to search deposits in an online service.

See [the introductory
vignette](https://docs.ropensci.org/deposits/articles/deposits.html) for
more details.

## Code of Conduct

Please note that this package is released with a [Contributor Code of
Conduct](https://ropensci.org/code-of-conduct/). By contributing to this
project, you agree to abide by its terms.

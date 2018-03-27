
Table of contents
=================

-   [Basic functionality](#basics)
-   Other examples
    -   Complete listing (validated version): [everyfunction.md](https://ghe.metrumrg.com/software/pmplots/blob/426c6f570e7968a996525e1050162c8f494d85bb/inst/examples/everyfunction.md)
    -   Other examples: [example1.md](inst/examples/example1.md)

<a name="basics"></a>

Basics
======

Simple function calls
---------------------

``` r
dv_pred(df, yname="NoDoze conc (ng/mL)")
```

![](inst/img/README-unnamed-chunk-2-1.png)

Add a title
-----------

``` r
dv_time(df, title="Lookie here!")
```

![](inst/img/README-unnamed-chunk-3-1.png)

Modify the axes
---------------

``` r
dv_time(df, xs=list(br=seq(0,240,24)))
```

![](inst/img/README-unnamed-chunk-4-1.png)

``` r
dv_time(df, log=TRUE)
```

![](inst/img/README-unnamed-chunk-4-2.png)

``` r
dv_time(df, yname="NoDoze (mg/L)",
        ys = list(trans="log", breaks=logbr()),
        xs = list(br=seq(0,240,72)))
```

![](inst/img/README-unnamed-chunk-4-3.png)

Faceting
--------

``` r
dv_pred(df) + facet_wrap(~STUDY, ncol=2)
```

![](inst/img/README-unnamed-chunk-5-1.png)

Add theme
---------

``` r
dv_pred(df) + theme_plain()
```

![](inst/img/README-unnamed-chunk-6-1.png)

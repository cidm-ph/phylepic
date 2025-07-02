## R CMD check results

0 errors | 2 warnings | 0 notes

This warnings are expected:

❯ checking S3 generic/method consistency ... WARNING
  ggplot_gtable:
    function(data)
  ggplot_gtable.phylepic_ggplot_build:
    function(plot)
  See section ‘Generic functions and methods’ in the ‘Writing R
  Extensions’ manual.

This is because the generic signatures in this release are consistent with the version of the generics in the upcoming ggplot2 4.0.0 release. This change was coordinated with ggplot2 maintainers.
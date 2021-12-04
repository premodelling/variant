<br>

## Variant


### Q & A

Q: Graphs of Variant for Travel/non-Travel
> "You are trying to find five different variants. The first one will be the one which has the largest proportion 
value over the whole data set. The second one will be the one with the next largest proportion value over 
the whole data set (that isn't the first variant) ... imagine plotting the time series for each variant - it's 
the five variants that have the highest points on those graphs."

<br>

Q: Is 4.2 numerically possible?  Or, can you explain the question to us?

> 4.2 Stacked Bars
Plot two stacked bar charts – one for travel-related and one for non-travel related cases – of these five variants,
including a bar for “Other” such that the weekly proportions sum to one again.
>
> Other = 'Other' + 'The variants that are not amongst the top five'

<br>
<br>

### Notes

The first occurrence of a non-zero fraction w.r.t. the data available; the data frame must be in date order.

```r
  # The list/vector of variant types
  types <- names(x = variants)
  types <- types[ !(types %in% c('week', 'travel')) ]

  appears <- function (x) {
    starting <- head(variants[which(variants[x] > 0), 'week'], 1)
    return(c('variant' = x, '1st fraction > 0' = as.character(starting)))
  }
  datavariants <- dplyr::bind_rows(lapply(X = types, FUN = appears))
```

<br>
<br>

### References

* [dplyr::select](https://dplyr.tidyverse.org/reference/select.html)

* [a philosophy of clean code](https://www.tinyverse.org/)

<br>
<br>

### Development Environment

* Edit the help file skeletons in 'man', possibly combining help files
  for multiple functions.
* Edit the exports in 'NAMESPACE', and add necessary imports.
* Put any C/C++/Fortran code in 'src'.
* If you have compiled code, add a useDynLib() directive to
  'NAMESPACE'.
* Run R CMD build to build the package tarball.
* Run R CMD check to check the package tarball.

Read "Writing R Extensions" for more information.


<br>
<br>
<br>
<br>

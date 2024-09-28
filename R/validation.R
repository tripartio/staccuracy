# validation.R
# Data validation code shared across some functions.


# Custom version of asserthat::assert_that. This way, I skip that dependency
# and my simplified version is lighter with only base R functions and cli.
# Note: license of asserthat is GPL-3.
# I guess that my customizations are sufficient to change license.
validate <- function(..., msg = NULL)
{
  # extract assertions from ...
  asserts <- eval(substitute(alist(...)))

  # Iterate through all assertions until one is FALSE (break in the for loop).
  for (assertion in asserts) {
    # Create and overwrite result {res} of each assertion.
    # If all are TRUE, then the final value of res will also be TRUE.
    # break out of the for loop on the first FALSE value, so the final value
    # of res would be FALSE.
    res <- eval(assertion, parent.frame())

    # Validate the assertion itself--this is purely internal validation
    if (length(res) != 1) {
      cli_abort('ale:::validate: length of assertion is not 1')
    }
    if (!is.logical(res)) {
      cli_abort('ale:::validate: assertion must return a logical value')
    }
    if (any(is.na(res))) {
      cli_abort('ale:::validate: missing values present in assertion')
    }

    # On the first FALSE res, break out of the for loop
    if (!res) {
      if (is.null(msg)) {
        # With no default msg, generic msg is 'assertion is FALSE'
        msg <- paste0(deparse(assertion), ' is FALSE')
      }

      res <- structure(FALSE, msg = msg)
      break
    }
  }

  # At this point, if all assertions were TRUE, res is TRUE.
  # Otherwise, res is FALSE with its msg corresponding to the first FALSE assertion.

  if (res) {
    return(TRUE)
  }
  else {
    cli_abort(c('x' = attr(res, 'msg')))
  }
}


# TRUE if x is length 1 and is either a double or an integer
is_scalar_number <- function(x) {
  rlang::is_scalar_double(x) || rlang::is_scalar_integer(x)
}

# TRUE if x is a scalar natural number (positive integer, zero excluded)
is_scalar_natural <- function(x) {
  rlang::is_scalar_integer(x) || x > 0
}

# TRUE if x is a scalar whole number (non-negative integer, zero included)
is_scalar_whole <- function(x) {
  rlang::is_scalar_integer(x) || x >= 0
}

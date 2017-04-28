#' Returns cols with NA as string or vector
#'
#' @param df A dataframe.
#' @param show A boolean.
#' @return If \code(show), returns a string. Else, it returns a vector
#' @examples
#' na_cols(df)
#' na_cols(df, show = TRUE)
#'
#'

na_cols <- function(df, show=FALSE)
{
  if (show)
  {
    missing_cols <- colSums(is.na(df))
    return(missing_cols)
  }
  else
  {
    num_missing <- colSums(is.na(df)) %>% sum()
    missing_cols <- paste("Num cols w/ NA:", num_missing)
    print(missing_cols)
  }
}

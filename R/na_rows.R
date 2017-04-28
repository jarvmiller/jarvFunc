#' Returns rows with NA as string or dataframe
#'
#' @param df A dataframe.
#' @param show A boolean.
#' @return If \code(show), returns a string. Else, it returns a dataframe
#' @examples
#' na_rows(df)
#' na_rows(df, show = TRUE)
#'
#'
na_rows <- function(df, show=FALSE)
{
  library(dplyr)
  if (show)
  {
    missing_rows <- df[rowSums(is.na(df)) > 0, ]
    return(missing_rows)
  }
  else
  {
    num_missing <- (rowSums(is.na(df)) > 0) %>% sum()
    missing_rows <- paste("Num rows w/ NA:", num_missing)
    print(missing_rows)
  }
}




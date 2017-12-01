#' Data Frame Production (From Variable Functions)
#'
#' Produce a \code{\link[dplyr]{tbl_df}} data frame that allows the user to
#' lazily pass unnamed \pkg{wakefield} variable functions (optionally, without
#' call parenthesis).
#'
#' @inheritParams r_list
#' @param geo A logical which on TRUE, denoted that geographies need to be fetched from HITL entities
#' @param \ldots A set of optionally named arguments.  Using \pkg{wakefield}
#' variable functions require no name or call parenthesis.
#' @param rep.sep A separator to use for repeated variable names.  For example
#' if the \code{\link[wakefield]{age}} is used three times
#' @param entity_name A string which defines the entity name to be fetched from HITL, when geo = TRUE
#' (\code{r_data_frame(age, age, age)}), the name "Age" will be assigned to all
#' three columns.  The results in column names \code{c("Age_1", "Age_2", "Age_3")}.
#' To turn of this behavior use  \code{rep.sep = NULL}.  This results in
#' \code{c("Age", "Age.1", "Age.2")} column names in the
#' \code{\link[base]{data.frame}}.
#' @return Returns a \code{\link[dplyr]{tbl_df}}.
#' @author Josh O'Brien and Tyler Rinker <tyler.rinker@@gmail.com>.
#' @references \url{http://stackoverflow.com/a/29617983/1000343}
#' @export
#' @seealso \code{\link[wakefield]{r_list}},
#' \code{\link[wakefield]{r_series}}
#' \code{\link[wakefield]{r_dummy}}
#' @examples
#' r_data_frame(n = 30,
#'     id,
#'     race,
#'     age,
#'     sex,
#'     hour,
#'     iq,
#'     height,
#'     died,
#'     Scoring = rnorm,
#'     Smoker = valid
#' )
#'
#' r_data_frame(n = 30,
#'     id,
#'     race,
#'     age(x = 8:14),
#'     Gender = sex,
#'     Time = hour,
#'     iq,
#'     grade, grade, grade,  #repeated measures
#'     height(mean=50, sd = 10),
#'     died,
#'     Scoring = rnorm,
#'     Smoker = valid
#' )
#'
#' r_data_frame(n = 500,
#'     id,
#'     age, age, age,
#'     grade, grade, grade
#' )
#'
#' ## Repeated Measures/Time Series
#' r_data_frame(n=100,
#'     id,
#'     age,
#'     sex,
#'     r_series(likert, 3),
#'     r_series(likert, 4, name = "Item", integer = TRUE)
#' )
#'
#' ## Expanded Dummy Coded Variables
#' r_data_frame(n=100,
#'     id,
#'     age,
#'     r_dummy(sex, prefix=TRUE),
#'     r_dummy(political)
#' )
#'
#' ## `peek` to view al columns
#' ## `plot` (`table_heat`) for a graphic representation
#' library(dplyr)
#' r_data_frame(n=100,
#'     id,
#'     dob,
#'     animal,
#'     grade, grade,
#'     death,
#'     dummy,
#'     grade_letter,
#'     gender,
#'     paragraph,
#'     sentence
#' ) %>%
#'    r_na() %>%
#'    peek %>%
#'    plot(palette = "Set1")

library(httr)
library(jsonlite)

r_data_frame <-
function (n, geo = F,..., rep.sep = "_",entity_name = NA) {
  
  if(geo ==  F){
    print("1")
    ll <- as.list(substitute(list(...)))[-1]
    print(ll)
    out <- r_list(n = n, ..., rep.sep = rep.sep)
    nms <- get_names(out, rep.sep)
    out <- stats::setNames(data.frame(out, stringsAsFactors = FALSE,
        check.names = FALSE), nms)
    return(dplyr::tbl_df(out))
    }
  
  if(geo == T){
    if (missing(entity_name)) stop("Provide Entity Name")
    
    possibleError <- tryCatch(
      httr::set_config(httr::config(ssl_verifypeer = 0L)),
      error=function(e) e
    )
    if(!inherits(possibleError, "error")){
      httr::set_config(httr::config(ssl_verifypeer = 0L))
    }
    result <- (GET(paste0("http://13.126.115.213/entity/get_entity_list?name=",entity_name,"& *contextid=true"), authenticate("admin", "clean4india")))
    if(grepl("^5",result$status_code)){
      stop(print("Error 500: Server Side Error"))
    } else if(grepl("^4",result$status_code)){
      stop(print("Error 500: Client Side Error"))
    } else{
      print("Data from server stored")
    }
  
    column_names <- (content(result))$colnames
    entity_df = as.data.frame(fromJSON(rawToChar(result$content))$result)
    colnames(entity_df) <- column_names
    
    num_rows <- nrow(entity_df)
    
    out <- r_list(n = num_rows, ..., rep.sep = rep.sep)
    nms <- get_names(out, rep.sep)
    out <- stats::setNames(data.frame(out, stringsAsFactors = FALSE,
                                      check.names = FALSE), nms)
    
    out <- dplyr::tbl_df(out)
    out <- cbind(entity_df,out)
    return(out)
  }
}



get_names <- function(x, rep.sep){

    listnames <- lapply(x, function(y) {
        if (is.list(y)) return(names(y))
        NA
    })

    nms <- as.list(names(x))
    nms[sapply(x, is.list)] <- NA
    cnames <- sapply(x, function(y) attributes(y)[["cname"]])
    nms[!sapply(cnames, is.null)] <- cnames[!sapply(cnames, is.null)]
    nms[is.na(nms)] <- listnames[is.na(nms)]
    nms <- unlist(nms)

    ## If duplicate names exist fix their suffix
    if (!is.null(rep.sep)){
        nms <- stats::ave(unlist(nms), unlist(nms), FUN = function(x) {
            if (length(x) == 1) {x} else {paste(x, seq_along(x), sep = rep.sep)}
        })
    }
    nms
}










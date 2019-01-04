##' mock a value for a given reference
##'
##' the function creates a random value for a given age and sex value and a given reference
##' @title Mock Value
##' @param ref a valid RefGroup object
##' @param item a valid item present in ref
##' @param sex character male or female
##' @param age numeric age value
##' @return a random value from the conditional distribution (conditionally on age and sex)
##' @author mandy
mock_value <- function(ref, item, sex = c("male","female"), age){
    refs <- ref@refs[[item]]@params[[sex]]
    dist <- ref@refs[[item]]@dist[[sex]]
    par.appr <- purrr::map(refs, function(param) stats::approx(refs$age, 
                                                               param,
                                                               xout = age, rule = 2)$y)
    eval(parse(text = paste0("gamlss.dist::q", 
                dist, "(", stats::pnorm(stats::rnorm(1)), ",", paste(paste(names(par.appr)[-1], 
                  "=", par.appr[-1]), collapse = ","), 
                ")")))
}

##' mock values for a given reference, given age and given sex
##'
##' the function creates random values for given age and sex values and a given reference
##' @title Mock Values
##' @param df data frame containing the age and sex
##' @param sex name of the sex variable
##' @param age name of the age variable
##' @param ref a valid RefGroup object
##' @param item a valid imte present in ref
##' @return data frame containing the additional column with random numbers
##' @author mandy
##' @export
mock_values <- function(df, sex, age, ref, item ){
    purrrlyr::by_row(df, function(row){
        mock_value(ref = ref, item = item, sex = row[[sex]], age = row[[age]])
    }, .to = item, .collate = "rows")
}

##' mock values for a given reference
##'
##' mock values for a given reference
##' @title Mock a data frame
##' @param ref a valid RefGroup object
##' @param item a valid imte present in ref
##' @param n how many values should be created
##' @return data frame containing a age, sex, and value column
##' @author mandy
##' @export
mock_df <- function(ref, item, n = 1000){
    age <- purrr::reduce(purrr::map(ref@refs[[item]]@params, function(x){
        stats::runif(n/2, min(x$age), max(x$age))
    }), c)
    sex <- names(ref@refs[[item]]@params)
    df <- data.frame(age = age, sex = sex)
    mock_values(df = df, age = "age", sex = "sex", ref = ref, item = item )
}



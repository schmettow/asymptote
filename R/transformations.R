#' Convert a SCOR model to ARY
#'
#' computes the asymptote parameter and adds it to the posterior
#'
#' @param posterior tbl_post object (bayr)
#' @return tbl_post
#'
#' The SCOR model often converges better, when estimated with brms.
#' Unfortunately, its offset parameter has rather little value. It is
#' converted to an asymptote parameter by offset * scale.
#'
#'
#' @author Martin Schmettow
#' @export

scor_to_ary <- function(posterior) {
  posterior_offset <-
    posterior %>%
    dplyr::filter(nonlin %in% c("offset"),
           type %in% c("ranef", "fixef")) %>%
    dplyr::rename(offset = value)

  posterior_scale <-
    posterior %>%
    dplyr::filter(nonlin %in% c("scale"),
           type %in% c("ranef", "fixef")) %>%
    dplyr::rename(scale = value)

  posterior_asym <-
    mascutils::left_union(posterior_offset, posterior_scale) %>%
    dplyr::mutate(value = scale * offset,
                  nonlin = "asym",
                  parameter = str_replace(parameter, "offset", "asym"),
                  order = order + 100) %>%
    dplyr::select(-scale, -offset)

  dplyr::bind_rows(posterior, posterior_asym) %>%
    bayr::posterior()

}



#' Set response variable
#'
#' sets the response variable of a formula
#'
#' @param formula learning curve formula
#' @param response response variable
#' @return formula
#'
#'
#'
#' @author Martin Schmettow
#' @export

set_response <-
  function(formula, response) {
    lazyeval::f_lhs(formula) <- response
    formula
  }


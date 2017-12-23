#' @importFrom xml2 xml_children xml_find_all xml_attr xml_parents
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
as.data.frame.clarity_xml <- function(x){

    co <- xml2::xml_find_all(x, "Contest")

    fd <- purrr::map_dfr(co, function(y){
        ch <- xml2::xml_find_all(y, "Choice")
        cd <- purrr::map_dfr(ch, function(z){
            vt <- xml2::xml_find_all(z, "VoteType")
            vd <- purrr::map_dfr(vt, function(a){
                pt <- xml2::xml_find_all(a, "Precinct")
                tibble::tibble(
                    precinct = xml2::xml_attr(pt, "name"),
                    votes = xml2::xml_attr(pt, "votes"),
                    votetype = xml2::xml_attr(a, "name")
                )
            })
            dplyr::mutate(vd, candidate = xml2::xml_attr(z, "text"))
        })
        dplyr::mutate(cd, race = xml2::xml_attr(y, "text"))
    })

    fd
}

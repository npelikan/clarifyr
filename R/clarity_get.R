
#' @importFrom stringr str_detect str_split
#' @importFrom httr GET content
clarity_get <- function(url){
    ## tests if url is a valid clarity site
    if(!stringr::str_detect(url, "results\\.enr\\.clarityelections\\.com")) stop("`url` is not a valid clarity elections site", call. = FALSE)

    ## parses URL to extract state/jurisdiction/electionid
    url_comp <- stringr::str_split(url, "/")[[1]]

    st <- url_comp[4]
    juris <- url_comp[5]
    electionid <- url_comp[6]

    ## performs current version query
    versionid <- httr::content(httr::GET(paste("http://results.enr.clarityelections.com", st, juris, electionid, "current_ver.txt", sep = "/")))


}

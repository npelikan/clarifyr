# parses url into chunks for clarity loader
#' @importFrom stringr str_split
url_split <- function(x) {
    # when state functionality is added, URL can be split and attribute passed
    # back here
    ot <- stringr::str_split(x, "/")[[1]]

    list(state = ot[4],
         jurisdiction = ot[5],
         electionid = ot[6])
}


#' Retrieve Clarity XML files
#'
#' @param url The landing page URL of a clarity elections site.
#'
#' @return an object of class `clarity_xml` for manipulation/extraction
#'
#' @importFrom stringr str_detect str_split
#' @importFrom httr GET content add_headers
#' @importFrom curl curl_fetch_disk new_handle handle_setheaders
#' @importFrom xml2 read_xml xml_add_child
#' @export
clarity_get <- function(url) {
    # tests if url is a valid clarity site
    if (!stringr::str_detect(url, "results\\.enr\\.clarityelections\\.com")) {
        stop("`url` is not a valid clarity elections site", call. = FALSE)
    }

    # parses URL to extract state/jurisdiction/electionid
    url_comp <- url_split(url)

    st <- url_comp$state
    juris <- url_comp$jurisdiction
    electionid <- url_comp$electionid

    # performs current version query
    versionid <- httr::content(
        httr::GET(
            paste("http://results.enr.clarityelections.com",
                  st, juris, electionid, "current_ver.txt", sep = "/"),
            httr::add_headers(
                "User-Agent" = "Mozilla/5.0 (X11; Linux x86_64; rv:106.0) Gecko/20100101 Firefox/106.0"
            )
        )
    )

    xml_loc <- paste("http://results.enr.clarityelections.com",
                     st, juris, electionid, versionid, "reports",
                     "detailxml.zip", sep = "/")

    print(xml_loc)

    h <- curl::new_handle()

    curl::handle_setheaders(
        h,
        "User-Agent" = "Mozilla/5.0 (X11; Linux x86_64; rv:106.0) Gecko/20100101 Firefox/106.0"
    )

    # put checks for invalid transfer below
    zd <- curl::curl_fetch_disk(xml_loc, tempfile(fileext = ".zip"), handle = h)$content

    xf <- xml2::read_xml(zd)
    # adds in a report download time object for later use
    xml2::xml_add_child(xf, "DownloadTime", as.character(Sys.time()))

    class(xf) <- c("clarity_xml", class(xf))
    xf
}

cat_line <- function(...) {
    cat(..., "\n", sep = "")
}


#' @importFrom xml2 xml_find_all xml_contents xml_attr
#' @export
summary.clarity_xml <- function(x){
    time <- as.character(xml2::xml_contents((xml2::xml_find_all(x = x, xpath = "Timestamp"))))
    voters <- as.numeric(xml2::xml_attr(xml2::xml_find_all(x = x, xpath = "VoterTurnout"), attr = "totalVoters"))
    ballots <- as.numeric(xml2::xml_attr(xml2::xml_find_all(x = x, xpath = "VoterTurnout"), attr = "ballotsCast"))
    contests <- xml2::xml_attr(xml2::xml_find_all(x = x, xpath = "Contest"), attr = "text")

    list(time = time,
         voters = voters,
         ballots = ballots,
         contests = contests)
}

#' @importFrom xml2 xml_find_all xml_content
get_xml_contents <- function(x, xpath) as.character(xml2::xml_contents(xml2::xml_find_all(x = x, xpath = xpath)))

#' @importFrom xml2 xml_find_all xml_contents xml_attr
#' @export
print.clarity_xml <- function(x){
    name <- get_xml_contents(x = x, xpath = "ElectionName")
    date <- get_xml_contents(x = x, xpath = "ElectionDate")
    region <- get_xml_contents(x = x, xpath = "Region")
    time <- get_xml_contents(x = x, xpath = "Timestamp")
    dltime <- get_xml_contents(x = x, xpath = "DownloadTime")
    voters <- as.numeric(xml2::xml_attr(xml2::xml_find_all(x = x, xpath = "VoterTurnout"), attr = "totalVoters"))
    ballots <- as.numeric(xml2::xml_attr(xml2::xml_find_all(x = x, xpath = "VoterTurnout"), attr = "ballotsCast"))
    contests <- xml2::xml_attr(xml2::xml_find_all(x = x, xpath = "Contest"), attr = "text")
    ncontests <- length(contests)

    cat_line("<Clarity XML Report downloaded ", dltime, ">")
    cat_line(name, ", ", date, ", ", region)
    cat_line("Report Time: ", time)
    cat_line("Voters: ", voters)
    cat_line("Ballots: ", ballots)
    cat_line("Number of Contests: ", ncontests)
}



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

#' @importFrom xml2 xml_find_all xml_contents xml_attr
#' @export
print.clarity_xml <- function(x){
    time <- as.character(xml2::xml_contents((xml2::xml_find_all(x = x, xpath = "Timestamp"))))
    voters <- as.numeric(xml2::xml_attr(xml2::xml_find_all(x = x, xpath = "VoterTurnout"), attr = "totalVoters"))
    ballots <- as.numeric(xml2::xml_attr(xml2::xml_find_all(x = x, xpath = "VoterTurnout"), attr = "ballotsCast"))
    contests <- xml2::xml_attr(xml2::xml_find_all(x = x, xpath = "Contest"), attr = "text")
    ncontests <- length(contests)

    cat_line("Time: ", time)
    cat_line()
    cat_line("Voters: ", voters)
    cat_line()
    cat_line("Ballots: ", ballots)
    cat_line()
    cat_line("Number of Contests: ", ncontests)
}



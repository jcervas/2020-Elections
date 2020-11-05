

getHouse2020 <- function() {
	house2020 <- jsonlite::fromJSON("https://static01.nyt.com/elections-assets/2020/data/api/2020-11-03/national-map-page/national/house.json")
	# str(house2020)

	# vote.order <- order(house2020$data$races$votes, decreasing=T)
	# house2020.totalvotes <- data.frame(rank=1:435, totalvotes=house2020$data$races$votes[vote.order])
	# race.name <- paste(house2020$data$races$state_name[vote.order], house2020$data$races$seat[vote.order])
	# rownames(house2020.totalvotes) <- race.name
	# house2020.totalvotes

	rep.votes <- rep(0,435)
	dem.votes <- rep(0,435)
	other.votes <- rep(0,435)
	for (j in 1:length(house2020$data$races$candidates)) {
		race.votes.order <- order(house2020$data$races$candidates[[j]]$votes, decreasing=T)
		othervotes.tmp <- 0
			for (i in 1:length(house2020$data$races$candidates[[j]]$party_id)){
				party.tmp <- house2020$data$races$candidates[[j]]$party_id[race.votes.order[i]]
			
			# if (length(house2020$data$races$candidates[[j]]$party_id %in% "republican") > 1) stop
			if (party.tmp %in% "republican") rep.votes[j] <- house2020$data$races$candidates[[j]]$votes[race.votes.order[i]]
			if (party.tmp %in% "democrat") dem.votes[j] <- house2020$data$races$candidates[[j]]$votes[race.votes.order[i]]
			if (!party.tmp %in% c("democrat","republican")) {
						othervotes.tmp <- othervotes.tmp + house2020$data$races$candidates[[j]]$votes[race.votes.order[i]]
						other.votes[j] <- othervotes.tmp
			}
		}
	}
	return(data.frame(st=house2020$data$races$state_name, dist=house2020$data$races$seat, dem=unlist(dem.votes), rep=unlist(rep.votes), other=unlist(other.votes), totalvotes=house2020$data$races$votes))
}

GetPresMargin <- function(STATE = NA) { #Contribution from Nathan Cisneros
	if (is.na(STATE)) return("Please list a state")
  STATE <- gsub(" ", "-", STATE)
  DC <- jsonlite::fromJSON(paste0("https://static01.nyt.com/elections-assets/2020/data/api/2020-11-03/race-page/", tolower(STATE), "/president.json"), simplifyDataFrame = F)
  DC$data$races[[1]]$votes
  Remaining <- DC$data$races[[1]]$tot_exp_vote - DC$data$races[[1]]$votes
  C1 <- DC$data$races[[1]]$candidates[[1]]$votes
  C2 <- DC$data$races[[1]]$candidates[[2]]$votes
  CurrentLead <- C1-C2
  Margin <- (Remaining - CurrentLead)/2/Remaining
  print(paste0(DC$data$races[[1]]$candidates[[1]]$last_name, " leads with ", CurrentLead, " votes."))
  print(paste0(DC$data$races[[1]]$candidates[[1]]$last_name, " needs ", round(Margin, 3)*100, " percent of the remaining ballots to hold onto the lead."))
  print(paste0("There are ", Remaining, " estimated ballots outstanding, which is ", round(Remaining/DC$data$races[[1]]$tot_exp_vote*100, 2), " percent of the total ballots."))
}


getPresidential2020 <- function() {
		pres2020 <- jsonlite::fromJSON("https://static01.nyt.com/elections-assets/2020/data/api/2020-11-03/national-map-page/national/president.json")
		# str(pres2020)
	rep.votes <- rep(0,51)
	dem.votes <- rep(0,51)
	other.votes <- rep(0,51)
	for (j in 1:length(pres2020$data$races$candidates)) {
		race.votes.order <- order(pres2020$data$races$candidates[[j]]$votes, decreasing=T)
		othervotes.tmp <- 0
			for (i in 1:length(pres2020$data$races$candidates[[j]]$party_id)){
				party.tmp <- pres2020$data$races$candidates[[j]]$party_id[race.votes.order[i]]
			
			if (party.tmp %in% "republican") rep.votes[j] <- pres2020$data$races$candidates[[j]]$votes[race.votes.order[i]]
			if (party.tmp %in% "democrat") dem.votes[j] <- pres2020$data$races$candidates[[j]]$votes[race.votes.order[i]]
			if (!party.tmp %in% c("democrat","republican")) {
						othervotes.tmp <- othervotes.tmp + pres2020$data$races$candidates[[j]]$votes[race.votes.order[i]]
						other.votes[j] <- othervotes.tmp
			}
		}
	}
	return(data.frame(st=pres2020$data$races$state_name, ecvotes=pres2020$data$races$electoral_votes, dem=unlist(dem.votes), rep=unlist(rep.votes), other=unlist(other.votes), totalvotes=pres2020$data$races$votes))

}

GetPresMargin("Nevada")


write.csv(getPresidential2020(), "/Users/user/Google Drive/GitHub/2020-Elections/pres2020.csv", row.names=F)
write.csv(getHouse2020(), "/Users/user/Google Drive/GitHub/2020-Elections/house2020.csv", row.names=F)
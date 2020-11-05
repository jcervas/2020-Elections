

getHouse2020 <- function() {
	house2020 <- jsonlite::fromJSON("https://static01.nyt.com/elections-assets/2020/data/api/2020-11-03/national-map-page/national/house.json")

	# str(house2020)

	vote.order <- order(house2020$data$races$votes, decreasing=T)


	house2020.totalvotes <- data.frame(rank=1:435, totalvotes=house2020$data$races$votes[vote.order])
	race.name <- paste(house2020$data$races$state_name[vote.order], house2020$data$races$seat[vote.order])
	rownames(house2020.totalvotes) <- race.name
	house2020.totalvotes


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

	house.2020 <- data.frame(st=house2020$data$races$state_name, dist=house2020$data$races$seat, dem=unlist(dem.votes), rep=unlist(rep.votes), other=unlist(other.votes), totalvotes=house2020$data$races$votes)

	return(house.2020)
}


write.csv(getHouse2020(), "/Users/user/Google Drive/GitHub/2020-Elections/house2020.csv", row.names=F)
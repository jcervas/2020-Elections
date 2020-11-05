house2020 <- jsonlite::fromJSON("https://static01.nyt.com/elections-assets/2020/data/api/2020-11-03/national-map-page/national/house.json")

# str(house2020)

vote.order <- order(house2020$data$races$votes, decreasing=T)


house2020.totalvotes <- data.frame(rank=1:435, totalvotes=house2020$data$races$votes[vote.order])
rownames(house2020.totalvotes) <- paste(house2020$data$races$state_name[vote.order], house2020$data$races$seat[vote.order])
house2020.totalvotes


rep.votes <- list()
dem.votes <- list()
for (j in 1:length(house2020$data$races$candidates)) {
	for (i in 1:length(house2020$data$races$candidates[[j]]$party_id))
		if (house2020$data$races$candidates[[j]]$party_id %in% "republican") rep.votes[[j]] <- house2020$data$races$candidates[[j]]$votes
		if (house2020$data$races$candidates[[j]]$party_id %in% "democrat") dem.votes[[j]] <- house2020$data$races$candidates[[j]]$votes
}



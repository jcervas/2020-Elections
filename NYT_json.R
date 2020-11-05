house2020 <- jsonlite::fromJSON("https://static01.nyt.com/elections-assets/2020/data/api/2020-11-03/national-map-page/national/house.json")

# str(house2020)

vote.order <- order(house2020$data$races$votes, decreasing=T)


house2020.totalvotes <- data.frame(rank=1:435, totalvotes=house2020$data$races$votes[vote.order])
rownames(house2020.totalvotes) <- paste(house2020$data$races$state_name[vote.order], house2020$data$races$seat[vote.order])
house2020.totalvotes



house2020$data$races$candidates

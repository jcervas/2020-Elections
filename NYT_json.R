

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
	return(data.frame(state=house2020$data$races$state_name, district=house2020$data$races$seat, dem=unlist(dem.votes), rep=unlist(rep.votes), other=unlist(other.votes), totalvotes=house2020$data$races$votes, margin=unlist(dem.votes)-unlist(rep.votes), remainingvote=paste0(100 - house2020$data$races$eevp,"%")))
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
	libertarian.votes <- rep(0,51)
	green.votes <- rep(0,51)
	other.votes <- rep(0,51)
	for (j in 1:length(pres2020$data$races$candidates)) {
		race.votes.order <- order(pres2020$data$races$candidates[[j]]$votes, decreasing=T)
		othervotes.tmp <- 0
			for (i in 1:length(pres2020$data$races$candidates[[j]]$party_id)){
				party.tmp <- pres2020$data$races$candidates[[j]]$party_id[race.votes.order[i]]
			
			if (party.tmp %in% "republican") rep.votes[j] <- pres2020$data$races$candidates[[j]]$votes[race.votes.order[i]]
			if (party.tmp %in% "democrat") dem.votes[j] <- pres2020$data$races$candidates[[j]]$votes[race.votes.order[i]]
			if (party.tmp %in% "libertarian") libertarian.votes[j] <- pres2020$data$races$candidates[[j]]$votes[race.votes.order[i]]
			if (party.tmp %in% "green") green.votes[j] <- pres2020$data$races$candidates[[j]]$votes[race.votes.order[i]]
			
			if (!party.tmp %in% c("democrat","republican","libertarian", "green")) {
						othervotes.tmp <- othervotes.tmp + pres2020$data$races$candidates[[j]]$votes[race.votes.order[i]]
						other.votes[j] <- othervotes.tmp
			}
		}
	}
	return(data.frame(state=pres2020$data$races$state_name, ecvotes=pres2020$data$races$electoral_votes, dem=unlist(dem.votes), rep=unlist(rep.votes), lib=unlist(libertarian.votes), green=unlist(green.votes), other=unlist(other.votes), totalvotes=pres2020$data$races$votes, margin=unlist(dem.votes)-unlist(rep.votes), remainingvote=paste0(100 - pres2020$data$races$eevp,"%")))

}

getCounties2020 <- function() {
		pres2020 <- jsonlite::fromJSON("https://static01.nyt.com/elections-assets/2020/data/api/2020-11-03/national-map-page/national/president.json")
		counties.tmp <- list()
		for (j in 1:length(pres2020$data$races$counties)) {
			county.tmp <- pres2020$data$races$counties[[j]]
			othervotes.tmp <- rep(0, nrow(county.tmp$results))
			for (i in 1:ncol(county.tmp$results)) {
				column.i <- names(county.tmp$results[i])
				if (column.i %in% c("trumpd", "bidenj")) next
					othervotes.tmp <- othervotes.tmp + unname(unlist(county.tmp$results[i]))
			}
				counties.tmp[[j]] <- data.frame(state= pres2020$data$races$state_name[j], name= county.tmp$name, dem=county.tmp$results$bidenj, rep=county.tmp$results$trumpd, other=othervotes.tmp, totalvotes=county.tmp$votes, margin=county.tmp$results$bidenj-county.tmp$results$trumpd, remainingvote=paste0(100 - county.tmp$eevp,"%"))
		}
		return(do.call(rbind, counties.tmp))
}


fips <- read.csv("https://raw.githubusercontent.com/jcervas/Data/master/fips.csv")
fips$fips <- as.character(sprintf("%02d", fips$fips))



getMITpresdata <- function(x) {
	historic <- read.csv(x)
	historic <- historic[historic$writein %in% "FALSE",]
		hist.dem.tmp <- historic[historic$party %in% "democrat",]
			hist.dem <- hist.dem.tmp[,c("year", "state", "candidatevotes")]
			colnames(hist.dem) <- c("year","state","dem")
		hist.rep.tmp <- historic[historic$party %in% "republican",]
			hist.rep <- hist.rep.tmp[,c("year", "state", "candidatevotes")]
			colnames(hist.rep) <- c("year","state","rep")
		hist.lib.tmp <- historic[historic$party %in% "libertarian",]
			hist.lib <- hist.lib.tmp[,c("year", "state", "candidatevotes")]
			colnames(hist.lib) <- c("year","state","lib")
		hist.green.tmp <- historic[historic$party %in% "green",]
			hist.green <- hist.green.tmp[,c("year", "state", "candidatevotes")]
			colnames(hist.green) <- c("year","state","green")
		hist.other <- historic[!historic$party %in% c("democrat","republican", "libertarian","green"),]
			hist.other <- aggregate(hist.other$candidatevotes, by=list(hist.other$year, hist.other$state), FUN=sum)
			colnames(hist.other) <- c("year","state","other")
	hist.pres <- merge(hist.dem, hist.rep, by=c("year","state"), all=T)
	hist.pres <- merge(hist.pres, hist.lib, by=c("year","state"), all=T)
	hist.pres <- merge(hist.pres, hist.green, by=c("year","state"), all=T)
	hist.pres <- merge(hist.pres, hist.other, by=c("year","state"), all=T)
	hist.pres$totalvotes <- replaceNA(hist.pres$dem) + replaceNA(hist.pres$rep) + replaceNA(hist.pres$lib) + replaceNA(hist.pres$green) + replaceNA(hist.pres$other)
	hist.pres[order(hist.pres$totalvotes),]
			return(hist.pres)
}

pres.hist <- getMITpresdata("https://raw.githubusercontent.com/jcervas/Data/master/Elections/Presidential/1976-2016-president.csv")
write.csv(pres.hist, "/Users/user/Google Drive/Data/Elections/Presidential/WIDE_1976-2016-president.csv")


house.historic <- read.csv("https://raw.githubusercontent.com/jcervas/2020-Elections/main/1976-2018-house2.csv")
	hist.dem.tmp <- house.historic[house.historic$party %in% "democrat",]
		hist.dem <- hist.dem.tmp[,c("year", "state", "district", "candidatevotes")]
		colnames(hist.dem) <- c("year","state","district","dem")
	hist.rep.tmp <- house.historic[house.historic$party %in% "republican",]
		hist.rep <- hist.rep.tmp[,c("year", "state", "district", "candidatevotes")]
		colnames(hist.rep) <- c("year","state","district","rep")
	hist.other <- house.historic[!house.historic$party %in% c("democrat","republican"),]
		hist.other <- aggregate(hist.other$candidatevotes, by=list(hist.other$year, hist.other$state, hist.other$district), FUN=sum)
		colnames(hist.other) <- c("year","state","district","other")

hist.house <- merge(hist.dem, hist.rep, by=c("year","state","district"), all=T)
hist.house <- merge(hist.house, hist.other, by=c("year","state","district"), all=T)
hist.house$totalvotes <- replaceNA(hist.house$dem) + replaceNA(hist.house$rep) + replaceNA(hist.house$other)
hist.house[order(hist.house$totalvotes),]

pres <- getPresidential2020()
	two_party(sum(pres$dem),sum(pres$rep))
sum(pres$lib)
sum(pres$dem)-sum(pres$rep)

pres.margin <- pres[order(abs(pres$margin/pres$totalvotes)),]

data.frame(
	margin=pres.margin$margin/pres.margin$totalvotes,
	dem=percent(pres.margin$dem/pres.margin$totalvotes),
	rep=pres.margin$rep/pres.margin$totalvotes,
	lib=pres.margin$lib/pres.margin$totalvotes,
	other=(pres.margin$other+pres.margin$green)/pres.margin$totalvotes)
pres.margin[abs(pres.margin$margin)<pres.margin$lib,]
pres.margin[abs(pres.margin$margin)<(pres.margin$lib+pres.margin$green+pres.margin$other),]

GetPresMargin("arizona")
GetPresMargin("pennsylvania")
GetPresMargin("georgia")


house <- getHouse2020()
head(house)
sum(house$dem)/(sum(house$dem)+sum(house$rep))
sum(find.winner(two_party(house$dem,house$rep)), na.rm=T)/435
house

house.pop.tmp <- jsonlite::fromJSON("https://api.census.gov/data/2019/acs/acs1?get=NAME,B01001_001E&for=congressional%20district:*&key=7865f31139b09e17c5865a59c240bdf07f9f44fd")
colnames(house.pop.tmp) <- house.pop.tmp[1,]
house.pop.tmp <- house.pop.tmp[-1,]
house.pop <- merge(house.pop.tmp, fips, by.x="state", by.y="fips")
colnames(house.pop) <- c("fips","NAME","pop","district","state","abv","geo")
house.pop$district[house.pop$district %in% "00"] <- "01"
head(house.pop)
head(house)
house$district <- as.character(sprintf("%02d", house$district))
house <- merge(house,house.pop, by=c("state","district"))
house$turnout <- house$totalvotes/as.numeric(house$pop)

house[order(house$turnout),]

write.csv(getPresidential2020(), "/Users/user/Google Drive/GitHub/2020-Elections/pres2020.csv", row.names=F)
write.csv(getHouse2020(), "/Users/user/Google Drive/GitHub/2020-Elections/house2020.csv", row.names=F)
write.csv(getCounties2020(), "/Users/user/Google Drive/GitHub/2020-Elections/counties2020.csv", row.names=F)

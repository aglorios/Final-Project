#Fiction and Memoirs from Wars From Beginning of The Respective War Up to 10 Years After (WWII to Afghanistan)
#Goals: Compare How Quickly People are writing about war after each war
	#Create line graphs comparing war writings. One for fiction and one for memoirs.

#Steps:
#Clean the data. Need to remove duplicate publications
#1. Sort by Author. Start with Fiction WWII.
View(FictionWWII)
names(FictionWWII)
sortFWWII<- FictionWWII[order(FictionWWII[,"author"]),]
View(sortFWWII)

#2. Identify Duplicate Titles.
#Because these a soft comparisons, e.g.	Bugle blast, an anthology from the services, and Bugle blast : an anthology from the services /, I will need a soft comparison tool.
# I could use agrep but my data sets are large and so I'm going to go for a more robust tool.
# The Internet is recommending RecordLinkage (http://cran.r-project.org/web/packages/RecordLinkage/RecordLinkage.pdf) and so I'm going to try that.

install.packages("RecordLinkage")
library("RecordLinkage")
				
				??compare.dedup
			
pairsFWWII<- compare.dedup(sortFWWII)
possiblesFWWII<- getPairs(pairsFWWII, show="possible")

#Screw this. Mark, this package is hard to use. There are weights and subsets and yeah, I don't think I have time for this.
#Instead, I am going the function duplicated()

#Show duplicated lines for sortFWWII
duplicated(sortFWWII)

#Show which row numbers were duplicated
which(duplicated(sortFWWII))

#Spot check lines 23 and 28. Are the duplicates?
#Yes. The problem is, this only removes hard duplicates. Later, I am going to have to figure out how to properly use Record Linkage to remove soft duplicates.

#3. Remove Duplicate Titles.
ndsFWWII<- sortFWWII[!duplicated(sortFWWII), ]

dim(sortFWWII) - dim(NDsortFWWII)
	#I removed 133 duplicates.

##Repeat for all other data sets
#. Memoir for WWII

sortMWWII<- MemoirWWII[order(MemoirWWII[,"author"]),]
View(sortMWWII)
duplicated(sortMWWII)
which(duplicated(sortMWWII))
ndsMWWII<- sortMWWII[!duplicated(sortMWWII),]
dim(sortMWWII) - dim(ndsMWWII)
	# I removed 376 hard duplicates.

#. Fiction Vietnam

sortFViet<- FictionVietnam[order(FictionVietnam[,"author"]),]
View(sortFViet)
duplicated(sortFViet)
which(duplicated(sortFViet))
ndsFV<- sortFViet[!duplicated(sortFViet),]
View(ndsFV)
dim(sortFViet) - dim(ndsFV)
	# I removed 81 hard duplications

# Memoir Vietnam

sortMViet<- MemoirVietnam[order(MemoirVietnam[,"author"]),]
ndsMV<- sortMViet[!duplicated(sortMViet),]
dim(sortMViet) - dim(ndsMV)
	# removed 73 hard duplicates

# Iraq Fiction

sortFIraq<- FictionIraq[order(FictionIraq[,"author"]),]
ndsFI<- sortFIraq[!duplicated(sortFIraq),]
dim(sortFIraq) - dim(ndsFI)
	# removed 52 hard duplicates

# Iraq Memoir

sortMIraq<- MemoirIraq[order(MemoirIraq[,"author"]),]
ndsMI<- sortMIraq[!duplicated(sortMIraq),]
dim(sortMIraq) - dim(ndsMI)
	# removed 84 hard duplicates

# Afghanistan Fiction

sortFAfghan<- FictionAfghanistan[order(FictionAfghanistan[,"author"]),]
ndsFA<- sortFAfghan[!duplicated(sortFAfghan),]
dim(sortFAfghan) - dim(ndsFA)
	# removed 22 duplicates

# Afghanistan Memoir

sortMAfghan<- MemoirAfghanistan[order(MemoirAfghanistan[,"author"]),]
ndsMA<- sortMAfghan[!duplicated(sortMAfghan),]
dim(sortMAfghan) - dim(ndsMA)
	# removed 28 duplicates

#4. Make new date column/Make Basic Histograms
# Need to extract the year from pub and turn it into a date

library(stringr)
ndsFWWII$date<- (str_match(ndsFWWII$pub, "\\d\\d\\d\\d"))
View(ndsFWWII)

# Turn this date into a proper 
# I can't figure out how to turn just a year into a proper date and so I turned it into an integer.

class(ndsFWWII$date)
	# "matrix" -- WTF?

# Turning my "matrix" into a character first

ndsFWWII$cdate<- strptime(as.character(ndsFWWII$date), format= "%Y")
	# Adds "12-18" on the end of everything. 
	# I think this is because the default ISO 8601 format demands things have a month and day. Not sure.
	# ISO 8601

class(ndsFWWII$cdate)
	# [1] "POSIXlt" "POSIXt" 

# So, like I said above, I turn everything into an integer.

ndsFWWII$cdate2<-year(as.character(ndsFWWII$cdate))
	#dates look good

class(ndsFWWII$cdate2)
	# [1] "integer
	# I think this should work?

hist(ndsFWWII$cdate2)

## Repeat for other data sets. 

# WWII Memoir

ndsMWWII$date<- (str_match(ndsMWWII$pub, "\\d\\d\\d\\d"))
ndsMWWII<- ndsMWWII[order(ndsMWWII[,"date"], decreasing=TRUE),]
View(ndsMWWII)
# The first three are wrong so I am going to manually substitute the correct dates

ndsMWWII$date[1] <- "1940"
ndsMWWII$date[2] <- "1947"
ndsMWWII$date[3] <- "1953"
View(ndsMWWII)
table(ndsMWWII$date)
class(ndsMWWII$date)

#remove NAs

NAndsMWWII<- ndsMWWII[!(is.na(ndsMWWII$date)),]
dim(ndsMWWII)
dim(NAndsMWWII)

NAndsMWWII$cdate<- strptime(as.character(NAndsMWWII$date), format= "%Y")
View(NAndsMWWII)
	#returns the "12-18" at the end
NAndsMWWII$cdate<- (str_match(NAndsMWWII$date, "\\d\\d\\d\\d"))
View(NAndsMWWII)
class(ndsMWWII$cdate)
NAndsMWWII$cdate2<-year(as.character(NAndsMWWII$cdate))
# Gah. Year function isn't working now, 4 hours later... Going to just make this a regular integer.

NAndsMWWII$cdate2<- as.integer(as.character(NAndsMWWII$cdate))
class(NAndsMWWII$cdate2)
table(NAndsMWWII$cdate2)
View(NAndsMWWII)
NAndsMWWII<- NAndsMWWII[order(NAndsMWWII[,"cdate2"], decreasing=TRUE),]
View(NAndsMWWII)
hist(NAndsMWWII$cdate2)

# Vietnam Fiction

ndsFV$date<- (str_match(ndsFV$pub, "\\d\\d\\d\\d"))
table(ndsFV$date)
ndsFV<- ndsFV[order(ndsFV[,"date"], decreasing=TRUE),]
View(ndsFV)
class(ndsFV$date)
	#matrix

ndsFV$cdate<- strptime(as.character(ndsFV$date), format= "%Y")
	#returns the "12-18" at the end
ndsFV$cdate<- (str_match(ndsFV$date, "\\d\\d\\d\\d"))
View(ndsFV)
class(ndsFV$cdate)
ndsFV$cdate2<-as.integer(as.character(ndsFV$cdate))
table(ndsFV$cdate2)
class(ndsFV$cdate2)
hist(ndsFV$cdate2)

# Memoir Vietnam

ndsMV$date<- (str_match(ndsMV$pub, "\\d\\d\\d\\d"))
table(ndsMV$date)
	#We have some manual corrections to do.

ndsMV<- ndsMV[order(ndsMV[,"date"], decreasing=TRUE),]
View(ndsMV)

#It looks like the corrections are mostly these army briefing reports, which aren't memoirs, so I am going to remove them.
NAndsMV<- ndsMV[-(1:146),]
View(NAndsMV)

#Now I'm going to manually fix the rest
table(NAndsMV$date)
NAndsMV<- NAndsMV[order(NAndsMV[,"date"], decreasing=TRUE),]
View(NAndsMV)
NAndsMV$date[1]<- "1981"
View(NAndsMV)
NAndsMV<- NAndsMV[order(NAndsMV[,"date"]),]
View(NAndsMV)
NAndsMV$date[1]<- "1981"

class(NAndsMV$date)
	#matrix
NAndsMV$cdate<- strptime(as.character(NAndsMV$date), format= "%Y")
class(NAndsMV$cdate)
View(NAndsMV)
NAndsMV$cdate<- (str_match(NAndsMV$cdate, "\\d\\d\\d\\d"))
NAndsMV$cdate2<-year(as.character(NAndsMV$cdate))
class(NAndsMV$cdate2)
View(NAndsMV$cdate2)

# Fiction Iraq

ndsFI$date<- (str_match(ndsFI$pub, "\\d\\d\\d\\d"))
View(ndsFI)
class(ndsFI$date)
ndsFI$cdate<- strptime(as.character(ndsFI$date), format= "%Y")
class(ndsFI$cdate)
ndsFI$cdate2<-year(as.character(ndsFI$cdate))
class(ndsFI$cdate2)

# Memoir Iraq

ndsMI$date<- (str_match(ndsMI$pub, "\\d\\d\\d\\d"))
View(ndsMI)
class(ndsMI$date)
ndsMI$cdate<- strptime(as.character(ndsMI$date), format= "%Y")
class(ndsMI$cdate)
ndsMI$cdate2<-year(as.character(ndsMI$cdate))
class(ndsMI$cdate2)

# Fiction Afghanistan

ndsFA$date<- (str_match(ndsFA$pub, "\\d\\d\\d\\d"))
View(ndsFA)
class(ndsFA$date)
ndsFA$cdate<- strptime(as.character(ndsFA$date), format= "%Y")
class(ndsFA$cdate)
ndsFA$cdate2<-year(as.character(ndsFA$cdate))
class(ndsFA$cdate2)

# Memoir Afghanistan

ndsMA$date<- (str_match(ndsMA$pub, "\\d\\d\\d\\d"))
View(ndsMA)
class(ndsMA$date)
ndsMA$cdate<- strptime(as.character(ndsMA$date), format= "%Y")
class(ndsMA$cdate)
ndsMA$cdate2<-year(as.character(ndsMA$cdate))
class(ndsMA$cdate2)

# 5. Plot this B.

# I need to count number of books in a given year. So, let's make some basic histograms of dates.
# First, quick run down of the ranges:

range(ndsFWWII$cdate2)
	# 1939 1955
range(ndsMWWII$cdate2)
	# NA NA --- WEIRD
	class(ndsMWWII$cdate2)
		#integer -- 	WTF??
	max(ndsMWWII$cdate2)
table(ndsMWWII$cdate2)
	#this works?? 
	# But it looks like I have a pub in 1998 and 2000 and so I need to remove those
ndsMWWII[, ndsMWWII$cdate2== "1998"]
ndsrmMWWII<- ndsMWWII[order(ndsMWWII[,"cdate2"], decreasing=TRUE),]
View(ndsrmMWWII)
ndsrmMWWII<- ndsrmMWWII[-(1:3),]

#Try range again for Memoir WWII
range(ndsrmMWWII$cdate2)
	# [1] NA NA
	# I give up

range(ndsFV$cdate2)
	# [1] 1961 1985

range(ndsMV$cdate2)
	#[1] 1300 9660
	# Vietnam memoirs are messed up
table(ndsMV$cdate2)
ndsrmMV<- ndsMV[order(ndsMV[,"cdate2"], decreasing=TRUE),]
View(ndsrmMV)

hist(ndsFWWII$cdate2)
hist(ndsrmMWWII$cdate2)

class(ndsMWWII$cdate2)







# Should I turn title into a factor?

class(ndsFWWII$title)
	# It IS a factor

# So, I think I need to cut the titles by date

range(ndsFWWII$cdate2)
	
max(ndsFWWII$cdate2) - min(ndsFWWII$cdate2)
	#16

table(cut(ndsFWWII$title, b=16))

??count

?cut

#Plot Fiction

install.packages(ggplot2)

range(ndsFWWII$cdate2)
hist(ndsFWWII)
plot(ndsFWWII$title, type="o", col="blue", ylim=c(1939,1955))
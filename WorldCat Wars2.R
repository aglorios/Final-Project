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
#possiblesFWWII<- getPairs(pairsFWWII, show="possible")

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
NAndsMV$date[1]<- "1982"
NAndsMV$date[2]<- "1981"
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
	#Error. I swear this thing is finicky.
NAndsMV$cdate2<-year(as.character(NAndsMV$cdate))
NAndsMV$cdate2<- as.integer(as.character(NAndsMV$date))
class(NAndsMV$cdate2)
View(NAndsMV)
hist(NAndsMV$cdate2)

# Fiction Iraq

ndsFI$date<- (str_match(ndsFI$pub, "\\d\\d\\d\\d"))
View(ndsFI)
table(ndsFI$date)
class(ndsFI$date)
ndsFI$cdate<- strptime(as.character(ndsFI$date), format= "%Y")
View(ndsFI)
ndsFI$cdate<- (str_match(ndsFI$cdate, "\\d\\d\\d\\d"))
	#error
class(ndsFI$cdate)
	#[1] "POSIXlt" "POSIXt" 
ndsFI$cdate2<-year(as.character(ndsFI$cdate))
ndsFI$cdate2<- as.integer(as.character(ndsFI$date))
class(ndsFI$cdate2)
hist(ndsFI$cdate2)

# Memoir Iraq

ndsMI$date<- (str_match(ndsMI$pub, "\\d\\d\\d\\d"))
View(ndsMI)
table(ndsMI$date)
# Manual Fixes

NAndsMI<- ndsMI[order(ndsMI[,"date"], decreasing=TRUE),]
View(NAndsMI)
NAndsMI$date[1]<-"2003"
View(NAndsMI)
NAndsMI<- ndsMI[order(ndsMI[,"date"]),]
View(NAndsMI)
NAndsMI$date[1]<-"2008"
View(NAndsMI)
table(NAndsMI$date)
NAndsMI<- NAndsMI[order(NAndsMI[,"date"], decreasing=TRUE),]
View(NAndsMI)
NAndsMI$date[1]<- "2003"
table(NAndsMI$date)

class(NAndsMI$date)
NAndsMI$cdate<- strptime(as.character(NAndsMI$date), format= "%Y")
class(NAndsMI$cdate)
NAndsMI$cdate2<- as.integer(as.character(NAndsMI$date))
class(NAndsMI$cdate2)
hist(NAndsMI$cdate2)

# Fiction Afghanistan

ndsFA$date<- (str_match(ndsFA$pub, "\\d\\d\\d\\d"))
table(ndsFA$date)
View(ndsFA)
class(ndsFA$date)
ndsFA$cdate2<- as.integer(as.character(ndsFA$date))
class(ndsFA$cdate2)
hist(ndsFA$cdate2)

# Memoir Afghanistan

ndsMA$date<- (str_match(ndsMA$pub, "\\d\\d\\d\\d"))
View(ndsMA)
table(ndsMA$date)
class(ndsMA$date)
ndsMA$cdate2<- as.integer(as.character(ndsMA$date))
class(ndsMA$cdate2)
hist(ndsMA$cdate2)

# 5. Plot this B.

#Basic Histograms:

hist(ndsFWWII$cdate2, main = paste("WWII Fiction: 1939-1955"), xlab= "Publication Year", breaks=1000)
hist(NAndsMWWII$cdate2, main = paste("WWII Memoirs: 1939-1955"), xlab="Publication Year", breaks=1000)
hist(ndsFV$cdate2, main = paste("Vietnam Fiction: 1961-1985"), xlab="Publication Year", breaks=1000)
hist(NAndsMV$cdate2, main = paste("Vietnam Memoirs: 1961-1985"), xlab="Publication Year", breaks=1000)
hist(ndsFI$cdate2, main = paste("Iraq War Fiction: 2003-2013"), xlab = "Publication Year", breaks=1000)
hist(NAndsMI$cdate2, main = paste("Iraq War Memoirs: 2003-2013"), xlab = "Publication Year", breaks=1000)
hist(ndsFA$cdate2, main = paste("Afghan War Fiction: 2001-2013"), xlab = "Publication Year", breaks=1000)
hist(ndsMA$cdate2, main = paste("Afghan War Memoirs: 2001-2013"), xlab = "Publication Year", breaks=1000)
?hist

# What I really need to compare is when the fiction and memoir spikes were for Iraq and Afghanistan and to apply this retroactively to the other wars.
# Fiction spike for Iraq was in 2008, three years before the war ended.
# Memoir spike for Iraq was in 2008. three years before the war ended.
#Fiction spike for Afghanistan was in 2012. The war hasn't ended but if we pretend the war will end in 2014, this will mean the spike is two years before end date.
# Memoir spike for Afghanistan was in 2012. Four years before "end date."
	# What is the clear way to show this stuff?

# Is that even what I should be looking at? 
# Because people write after they've had time to process, perhaps I should at least also be looking at length of war.
# US Involvement in WWII: 1941 - 1945; 3 years.
	# Fiction peak in 1942.
	# Memoir peak in 1943
# US Involvement in Vietnamn: 1961 - 1975; 14 years.
	# Fiction peak in 1985
	# Memoir peak in 1985
		# Jumps in 1985 for both
# Iraq War: 2003 - 2011; 8 years.
	# Fiction peak in 2008
	# Memoir peak in 2008
# Afghanistan War: 2001 - ; 12 years so far.
	# Fiction peak in 2012
	# Memoir peak in 2010
## I need a 15 year spread and to plot these again over that spread

# I am going to create this range by adding a new column to each data frame that subtracts the given date from the max date.
ndsFWWII$range<- max(ndsFWWII$cdate2) - ndsFWWII$cdate2
View(ndsFWWII)
	max(ndsFWWII$cdate2)
sum(t)
#It's not the title I care about, it's the year. 
#I need to take year/sum(year)

NAndsMWWII$range<- max(NAndsMWWII$cdate2) - NAndsMWWII$cdate2
max(NAndsMWWII$cdate2)
View(NAndsMWWII)

ndsFV$range<- max(ndsFV$cdate2) - ndsFV$cdate2
max(ndsFV$range)
View(ndsFV)

NAndsMV$range<- max(NAndsMV$cdate2) - NAndsMV$cdate2
max(NAndsMV$range)
View(NAndsMV)

ndsFI$range<- max(ndsFI$cdate2) - ndsFI$cdate2
max(ndsFI$cdate2)
	#coming up as NA. I don't know why. Sigh.
ndsFI$range<- 2013 - ndsFI$cdate2
View(ndsFI)

NAndsMI$range<- max(NAndsMI$cdate2) - NAndsMI$cdate2
max(NAndsMI$cdate2)
View(NAndsMI)

ndsFA$range<- max(ndsFA$cdate2) - ndsFA$cdate2
max(ndsFA$cdate2)
View(ndsFA)

ndsMA$range<- max(ndsMA$cdate2) - ndsMA$cdate2
max(ndsMA$cdate2)
View(ndsMA)

#I think I should make a mosaic plot. But I don't know how with all my different data frames.
install.packages("vcd")
library(vcd)
install.packages("ggplot2")
library(ggplot2)

#So I am going to try and do a basic multiple line plot.
plot(ndsFWWII$range, main = paste("Fiction Publication Range from Start of War to Ten Years After War"), xlab="Publication Frequency", ylab= "War Range")
points(ndsFV$range, col='red')
points(ndsFI$range, col='blue')
points(ndsFA$range, col='green')
legend("bottomright", title= "War Colors", title.col="black", c("Afghan", "Iraq", "Vietnam", "WWII"), box.col=" dark gray", text.col=c("green", "blue", "red", "black"))

plot(NAndsMWWII$range, main = paste("Memoir Publication Range from Start of War to Ten Years After War"), xlab="Publication Frequency", ylab= "War Range")
points(NAndsMV$range, col='red')
points(NAndsMI$range, col='blue')
points(ndsMA$range, col='green')
legend("bottomright", title= "War Colors", title.col="black", c("Afghan", "Iraq", "Vietnam", "WWII"), box.col=" dark gray", text.col=c("green", "blue", "red", "black"))
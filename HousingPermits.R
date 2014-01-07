#DOB_Permit_Issuance Data. I need to figure out who owns what buildings.

#Sort by Business Name:

sortBusiness<- DOB_Permit_Issuance[order(DOB_Permit_Issuance[,"OwnerBusinessName"], decreasing=TRUE),]

## I want to find out how many global investors are in each borough
# Test on Brooklyn
# Merge Global Investors with BK

mBK<- merge(BK, GlobalInvestors, by.x="OwnerName", by.y = "Name")
mMN<- merge(MN, Essex.idx, by.x="OwnerName", by.y = "name")
mSI<- merge(SI, GlobalInvestors, by.x="OwnerName", by.y = "Name")
mQN<- merge(QN, GlobalInvestors, by.x="OwnerName", by.y = "Name")
mBX<- merge(BX, GlobalInvestors, by.x="OwnerName", by.y = "Name")

#Make Indeces for other possible names of investment groups
Essex.idx<- data.frame(Name=c('ESSEX DEVELOPMENT LLC', '23 ESSEX REALTY CORP.', '13-15 ESSEX STREET, L', '11 ESSEX ST. CORP.', '3-5 ESSEX PARTNERS, L', '45 ESSEX STREET LLC', '37 ESSEX STREET CORP.', '31 ESSEX STREET CORP.', '29 ESSEX STREET LLC', '85 ESSEX RLTY CO', '123 ESSEX LLC', 'ESSEX UNION LLC', '115 ESSEX ST. LLC', '103 ESSEX STREET REAL', 'LUDLOW ESSEX PARTNERS', '147 ESSEX STREET REAL', 'ESSEX FUNDING LLC', 'HOUSTON ESSEX RE CORP', '179-181 ESSEX LLC', 'ESSEX OWNERS CORP'), idx=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20), GlobalName= c('ESSEX PROPERTY TRUST'))

mEssex<- merge(Essex.idx, GlobalInvestors, by.x = "GlobalName", by.y = "Name")

JPM.idx<- data.frame(Name=c('JPMORGAN CHASE BANK', 'JP HOUSE, LLC', 'JP EQUITIES LLC', 'JPS 050 REALTY LLC.', 'JPS 020 REALTY LLC.', 'JP & ASSOCIATES PROPE', 'EJP REALTY ASSOCIATES', 'JOSEPH ROSEN TRUST,JP', 'JPIG CORP'), idx = c(1, 2, 3, 4, 5, 6, 7, 8, 9), GlobalName = c('JP MORGAN'))

mJPM<- merge(JPM.idx, GlobalInvestors, by.x = "GlobalName", by.y = "Name")

mMNJPM<- merge(MN, mJPM, by.x="OwnerName", by.y = "Name")

View(mMNJPM)

rmduplicates<- unique(mMNJPM[,-(84:88)])
View(rmduplicates)

uniqueMNJPM<- rmduplicates
library(maps)
library(mapdata)

manhattan.mp<- readShapeSpatial("~/Desktop/Data2/Thesis/Manhattan/MNMapPLUTO.shp")
plot(manhattan.mp)
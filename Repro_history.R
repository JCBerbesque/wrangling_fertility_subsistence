#Create Reproductive history file 
#Guidelines for consistent data
# 1. respondent ID (var = respondentid) - the individual giving the reproductive history
# 2. Child ID (var = childid)
# 3. respondents age at birth (Var = ageatbirth)--we don't have this
# 4. Childsex(var=sex,0=m,1=f)
# 5. Child date of birth (var = dob) - as exact as possible
# 6. Mother ID (var = mothid)
# 7. Father ID (var = fathid)
# 8. Child alive (var = alive: y/n)
# 9. Child date of death (var = dod) - As exact as possible - N.B if dates not known it is OK to include age at death



#load data
library(readxl)
repro_hist <- read_excel("/Users/jcberbesque/Desktop/Data Science/demographic_interviews_trial.xls")


#convert wide to long for children's IDs
library(tidyr)
repro_hist <- repro_hist %>% gather(Childid, kidID, Idk_1, Idk_2, Idk_3, Idk_4, Idk_5, Idk_6, Idk_7, Idk_8, Idk_9, Idk_10, Idk_11, na.rm = FALSE, convert = FALSE)
head(repro_hist, 24) 

#rearrange by adult ID
order(repro_hist$ID,na.last = TRUE)

#move columns Childid and kidID to easier position to see
library(dplyr)
repro_hist<-relocate(repro_hist, 40, 41, .after = 6)

#duplicate kidID
repro_hist$kidID2 <- repro_hist$kidID
View(repro_hist)

#convert to long sex variable
library(tidyr)
repro_hist <- repro_hist %>% gather(kidID, sex, sexkid_1, sexkid_2, sexkid_3, sexkid_4, sexkid_5, sexkid_6, sexkid_7, sexkid_8, sexkid_9, sexkid_10, sexkid_11, na.rm = FALSE, convert = FALSE)
head(repro_hist, 24) 

#convert to long DOB variable
library(tidyr)
repro_hist <- repro_hist %>% gather(kidID, DOBk, DOBK_1, DOBK_2, DOBK_3, DOBK_4, DOBK_5, DOBK_6, DOBK_8...34, DOBK_8...38, DOBK_9, DOBK_10, DOBK_11, na.rm = FALSE, convert = FALSE)
head(repro_hist, 24) 

#convert live variable to long
library(tidyr)
repro_hist <- repro_hist %>% gather(kidID, live, livek_1, livek_2, livek_3, livek_4, livek_5, livek_6, livek_7, livek_8, livek_9, livek_10, livek_11, na.rm = FALSE, convert = FALSE)
head(repro_hist, 24)

#save progress
reprohist2<-repro_hist

#get rid of columns not needed 
library(dplyr)
reprohist2 <-  select(repro_hist, 1:6, 8:10, 12)

#load data masterfile
library(readxl)
repro_hist <- read_excel("/Users/jcberbesque/Desktop/colette/Hadza data files/Master_File/new hadza master_CB mod.xlsx")

#same, trim master file to needed variables for merge later
library(dplyr)
master <-  select(new_hadza_master_CB_mod, 12,18,22)


#to undo stuff if things got messed up
reprohist2<-repro_hist

#save progress
reprohist3 <- reprohist2

#drop duplicate rows of each child's ID
library(dplyr)
reprohist3<-distinct(reprohist2, reprohist2$kidID2, .keep_all = TRUE)

#rename variables according to standardised project guide
library(dplyr)
repro <- reprohist3 %>% 
  rename(
    respondentid = ID, childid = kidID2, Childsex = sex, dob = DOBk, alive = live
  )

#rename key in masterfile
library(dplyr)
master1 <- master %>% 
  rename(
    childid = UNIQUEID
    )


# bring in mother/father ID (through SQL?)
#read in masterfile for left join ID with kidID and pull momID and dadID
#using data.table?
library(data.table)
merge <- merge(repro, master1)

#inspect in csv
write.csv(merge,"~/Desktop/Data Science/\\long_repo_DF_27_may.csv", row.names = FALSE)

#fix names according to style guide
merge <- merge %>% 
rename(
  resp_sex = SEX, resp_age = AGE, mothid = momID, fathid = daduniqid, resp_DOB = DOB
)

#recode sex again
library(dplyr)
merge <- merge %>% mutate(resp_sex=recode(resp_sex, 
                                 `1`="M",
                                 `2`="F"))

#recode sex child again
library(dplyr)
merge <- merge %>% mutate(Childsex=recode(Childsex, 
                                 `1`="M",
                                 `2`="F"))
merge <- merge %>% mutate(alive=recode(alive, 
                                      `1`="Y",
                                      `0`="N"))

#remove unnecessary column 
library(dplyr)
merge <-  select(merge, -11)

#reorder variables
library(dplyr)
merge <-relocate(merge, 2)


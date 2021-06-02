#Create Reproductive history file 
#Guidelines for consistent data
# 1. respondent ID (var = respondentid) => the individual giving the reproductive history
# 2. Child ID (var = childid) => childid
# 3. respondents age at birth (Var = ageatbirth) => we don't have this always, but (mom dob - kid dob) where possible
# 4. Childsex(var=sex,0=m,1=f) => childsex
# 5. Child date of birth (var = dob) => dob
# 6. Mother ID (var = mothid) => mothid
# 7. Father ID (var = fathid) => fathid
# 8. Child alive (var = alive: y/n) => aive
# 9. Child date of death (var = dod) => we don't have this
#Other variable left in
#Year--this may help you calculate oldest possible age still alive if needed

#Note: respondent's age at birth is dodgy sometimes (says 9 years old in places).  
#Hadza don't keep track of time, so estimates can be far off occasionally.  I've left this for Abbey to decide how to handle.


#load libraries
library(data.table)
library(readxl)
library(tidyr)
library(dplyr)

#load data
repro_hist <- read_excel("/Users/jcberbesque/Desktop/Data Science/demographic_interviews_trial.xls")


#convert wide to long for children's IDs
repro_hist <- repro_hist %>% gather(Childid, kidID, Idk_1, Idk_2, Idk_3, Idk_4, Idk_5, Idk_6, Idk_7, Idk_8, Idk_9, Idk_10, Idk_11, na.rm = FALSE, convert = FALSE)
head(repro_hist, 24) 

#rearrange by adult ID
order(repro_hist$ID,na.last = TRUE)

#move columns Childid and kidID to easier position to see
repro_hist <- relocate(repro_hist, 40, 41, .after = 6)

#duplicate kidID
repro_hist$kidID2 <- repro_hist$kidID
View(repro_hist)

#convert to long sex variable
repro_hist <- repro_hist %>% gather(kidID, sex, sexkid_1, sexkid_2, sexkid_3, sexkid_4, sexkid_5, sexkid_6, sexkid_7, sexkid_8, sexkid_9, sexkid_10, sexkid_11, na.rm = FALSE, convert = FALSE)
head(repro_hist, 24) 

#convert to long DOB variable
repro_hist <- repro_hist %>% gather(kidID, DOBk, DOBK_1, DOBK_2, DOBK_3, DOBK_4, DOBK_5, DOBK_6, DOBK_8...34, DOBK_8...38, DOBK_9, DOBK_10, DOBK_11, na.rm = FALSE, convert = FALSE)
head(repro_hist, 24) 

#convert live variable to long
repro_hist <- repro_hist %>% gather(kidID, live, livek_1, livek_2, livek_3, livek_4, livek_5, livek_6, livek_7, livek_8, livek_9, livek_10, livek_11, na.rm = FALSE, convert = FALSE)
head(repro_hist, 24)

#save progress
reprohist2 <- repro_hist

#get rid of columns not needed 
reprohist2 <-  select(repro_hist, 1:6, 8:10, 12)

#load data masterfile
repro_hist <- read_excel("/Users/jcberbesque/Desktop/colette/Hadza data files/Master_File/new hadza master_CB mod.xlsx")

#same, trim master file to needed variables for merge later
master <-  select(new_hadza_master_CB_mod, 12,18,22)

#save progress
reprohist3 <- reprohist

#drop duplicate rows of each child's ID
reprohist3 <- distinct(reprohist2, reprohist2$kidID2, .keep_all = TRUE)

#rename variables according to standardised project guide
repro <- reprohist3 %>% 
  rename(
    respondentid = ID, childid = kidID2, Childsex = sex, dob = DOBk, alive = live
  )

#rename key in masterfile
master1 <- master %>% 
  rename(
    childid = UNIQUEID
    )

#read in masterfile for left join ID with kidID and pull momID and dadID
merge <- merge(repro, master1)

#inspect in csv
write.csv(merge,"~/Desktop/Data Science/\\long_repo_DF_27_may.csv", row.names = FALSE)

#fix names according to style guide
merge <- merge %>% 
rename(
  resp_sex = SEX, resp_age = AGE, mothid = momID, fathid = daduniqid, resp_DOB = DOB
)

#recode sex again
merge <- merge %>% mutate(resp_sex = recode(resp_sex, 
                                 `1` = "M",
                                 `2` = "F"))

#recode sex of child and whether alive
merge <- merge %>% mutate(Childsex = recode(Childsex, 
                                 `1` = "M",
                                 `2` = "F"))

merge <- merge %>% mutate(alive=recode(alive, 
                                      `1` = "Y",
                                      `0` = "N"))

#remove unnecessary column 
merge <-  select(merge, -11)

#reorder variables
merge <- relocate(merge, 2)

#read saved data after break in project
repro_hist_file <- read_csv("/Users/jcberbesque/Desktop/colette/Fertility_subsistenvce_project/fertility_files/repro_history.csv")

#if responder ID == mom ID, subtract child dob from mom dob
repro_hist_file <- repro_hist_file %>% 
  dplyr::mutate(ageatbirth = ifelse(repro_hist_file$respondentid == repro_hist_file$mothid, (repro_hist_file$dob - repro_hist_file$resp_DOB), NA))

#remove all other data columns (previously kept to calculate or verify data integrity)
repro_hist_file <- select(repro_hist_file, -4, -6)

#inspect in csv
write.csv(repro_hist_file,"/Users/jcberbesque/Desktop/colette/Fertility_subsistenvce_project/fertility_files/final_repro.csv", row.names = FALSE)

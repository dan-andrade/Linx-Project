library(dplyr)
library(mice)
library(tree)
library(data.table)
library(stringr)
library(stringdist)
library(reshape2)
library(car)
library(doBy)
library(sqldf)

###loading R files

load('C:/Users/Altran/Desktop/BD/29-08/R files/resources.RData')
load('C:/Users/Altran/Desktop/BD/29-08/R files/CF.RData')
load('C:/Users/Altran/Desktop/BD/29-08/R files/diplomas.RData')
load('C:/Users/Altran/Desktop/BD/29-08/R files/lang.RData')
load('C:/Users/Altran/Desktop/BD/29-08/R files/certif.RData')
load('C:/Users/Altran/Desktop/BD/29-08/R files/skills.RData')
load('C:/Users/Altran/Desktop/BD/29-08/R files/keywords.RData')
load('C:/Users/Altran/Desktop/BD/29-08/R files/experiences.RData')
load('C:/Users/Altran/Desktop/BD/29-08/R files/exp.skills.RData')
load('C:/Users/Altran/Desktop/BD/29-08/R files/exp.keywords.RData')

### read original xlsx files

resources <- read.csv2("C:/Users/Altran/Desktop/BD/29-08/Staffing/csv_GridMyResourcesExport Portugal.csv", colClasses = "factor")
CF <- read.csv2("C:/Users/Altran/Desktop/BD/29-08/MyCF/csv_CF_CMAPS_20160814_Portugal.csv", colClasses = "factor")
diplomas <- read.csv2("C:/Users/Altran/Desktop/BD/29-08/MyCF/csv_diplomas_CMAPS_20160814_Portugal.csv", colClasses = "factor")
lang <- read.csv2("C:/Users/Altran/Desktop/BD/29-08/MyCF/cvs_lang_CMAPS_20160814_Portugal.csv", colClasses = "factor")
certif <- read.csv2("C:/Users/Altran/Desktop/BD/29-08/MyCF/csv_certif_CMAPS_20160814_Portugal.csv", colClasses = "factor")
skills <- read.csv2("C:/Users/Altran/Desktop/BD/29-08/MyCF/csv_skills_CMAPS_20160814_Portugal.csv", colClasses = "factor")
keywords <- read.csv2("C:/Users/Altran/Desktop/BD/29-08/MyCF/csv_keywords_CMAPS_20160814_Portugal.csv", colClasses = "factor")
experiences <- read.csv2("C:/Users/Altran/Desktop/BD/29-08/MyCF/csv_experiences_CMAPS_20160814_Portugal.csv", colClasses = "factor")
exp.skills <- read.csv2("C:/Users/Altran/Desktop/BD/29-08/MyCF/csv_exp_skills_CMAPS_20160814_Portugal.csv", colClasses = "factor")
exp.keywords <- read.csv2("C:/Users/Altran/Desktop/BD/29-08/MyCF/csv_exp_keywords_CMAPS_20160814_Portugal.csv", colClasses = "factor")


### remove duplicates

resources[duplicated(resources)==T, ]
nrow(CF[duplicated(CF)==T,])
nrow(diplomas[duplicated(diplomas)==T, ])
#if duplicates, then:
diplomas <- distinct(diplomas)
nrow(lang[duplicated(lang)==T, ])
lang <- distinct(lang)
nrow(certif[duplicated(certif)==T, ])
certif <- distinct(certif)
nrow(skills[duplicated(skills)==T, ])
nrow(keywords[duplicated(keywords)==T, ])
keywords <- distinct(keywords)
nrow(experiences[duplicated(experiences)==T, ])
experiences <- distinct(experiences)
nrow(exp.skills[duplicated(exp.skills)==T, ])
exp.skills <- distinct(exp.skills)
nrow(exp.keywords[duplicated(exp.keywords)==T, ])  
exp.keywords <- distinct(exp.keywords)


### dropping variables with too many NAs & dealing with NAs
#' investigate later if
#' records with NAs will be dropped

resources[resources==''] <- NA #setting all blanks as NA
sum(is.na(resources))
md.pattern(resources) #verifiy which variables have NA
#or (if one wants percentages):
colMeans(is.na(resources)>0)
resources <- resources %>% select(-EC, -STAFFING.PLAN) # had 100% NAs
resources <- na.tree.replace(resources) #will create a new level for NA named 'NA'

CF[CF==''] <- NA
sum(is.na(CF))
md.pattern(CF)
colMeans(is.na(CF)>0)
CF <- CF %>% select(-Nationality2, -ProfileTitle02, -ProfileTitle03, -ProfileTitle04, -ProfileTitle05)
CF <- na.tree.replace(CF)

diplomas[diplomas==''] <- NA
sum(is.na(diplomas))
md.pattern(diplomas)
colMeans(is.na(diplomas)>0)
diplomas <- diplomas %>% select(-SchoolCode, -GraduationCode, -SpecializationCode)
diplomas <- na.tree.replace(diplomas)

lang[lang==''] <- NA
sum(is.na(lang))
md.pattern(lang)
colMeans(is.na(lang)>0)

certif[certif==''] <- NA
sum(is.na(certif))
md.pattern(certif)
colMeans(is.na(certif)>0)
certif <- certif %>% select(-ProviderCode, -CertificationCode, -ExpiryDate, -AdditionalInformation)
certif <- na.tree.replace(certif)

skills[skills==''] <- NA
sum(is.na(skills))
md.pattern(skills)
colMeans(is.na(skills)>0)
skills <- na.tree.replace(skills)

keywords[keywords==''] <- NA
sum(is.na(keywords))
md.pattern(keywords)
colMeans(is.na(keywords)>0)
keywords <- na.tree.replace(keywords) 

experiences[experiences==''] <- NA
sum(is.na(experiences))
md.pattern(experiences)
colMeans(is.na(experiences)>0)
experiences <- experiences %>% select(-Program, -ProjectDescription, -Miscellaneous)
experiences <- na.tree.replace(experiences)

exp.skills[exp.skills==''] <- NA
sum(is.na(exp.skills))
md.pattern(exp.skills)
colMeans(is.na(exp.skills)>0)

exp.keywords[exp.keywords==''] <- NA
sum(is.na(exp.keywords))
md.pattern(exp.keywords)
colMeans(is.na(exp.keywords)>0)


### drop variables with one level only

resources <- resources %>% select(-COUNTRY, -GEOGRAPHICAL.AREA)
CF <- CF %>% select(-(1:10))
keywords <- keywords %>% select(-IsPrincipal)


### drop unnecessary variables - PHASE 1

resources <- resources %>% select(-RESMNGRID)
CF <- CF %>% select(-OBSUnit6Code, -OBSUnit11Code, -ResourceLogin, -Resource.Manager.CorporateId, -Hierarchical.Manager.CorporateId, -Hierarchical.Manager.full.name,-CfId, -CFLastUpdated, -CFName, -CFLanguage)
diplomas <- diplomas %>% select(-CFId, -CountryCode, -City, -ducationalLevelCode)
lang <- lang %>% select(-CFId, -languageid, -GlobalLevelId, -GlobalLevelName)
certif <- certif %>% select(-CFId, -Provider, -CertificationDate)
skills <- skills %>% select(-CFId, -CompetencyLine_ID, -Weight, -Seniority, -LastUsed, -DisciplinaryField_ID, -ExpertiseLevel, -Specialty_ID)
keywords <- keywords %>% select(-CFId, -KeywordCategory_ID, -Keyword_ID, -Weight, -Seniority, -ExpertiseLevel, -LastUsed)
experiences <- experiences %>% select(-CFId, -CompanyId, -CountryCode, -ClientSubIndustry_ID, -ClientSector_ID, -ProductService_ID, -BusinessDepCode, -IsCompanyConfidential, -IsProjectConfidential)
exp.skills <- exp.skills %>% select(-CFId, -CompetencyLine_ID, -DisciplinaryField_ID, -Specialty_ID)
exp.keywords <- exp.keywords %>% select(-CFId, -KeywordCategory_ID, -Keyword_ID)


### normalize variable names

#resources
setnames(resources, old=names(resources), new=c('name', 'location', 'manager_name', 
                                                'status', 'sub_division', 'department', 'contract_type', 'hierar_mgr', 'employee_id'))

#CF
setnames(CF, old=names(CF), new=c('sub_division_2', 'unit_name', 'employee_id', 'employee_type', 'is_active', 
                                  'position', 'hire_date', 'leave_date', 'career_start_date', 'location_2','nationality',
                                  'manager_name', 'linx_pub_status', 'summary', 'profile_title'))

#diplomas
setnames(diplomas, old=names(diplomas), new=c('employee_id', 'diploma_country', 'school_name', 'graduation_name',
                                              'graduation_date', 'education_type', 'dip_specialization_name', 'thesis_title', 'thesis_description', 'comment'))

#lang
setnames(lang, old=names(lang), new=c('employee_id', 'languages', 'language_level'))

#certif
setnames(certif, old=names(certif), new=c('employee_id', 'certification'))

#skills
setnames(skills, old=names(skills), new=c('employee_id', 'skill_competency_line', 'skill_field', 'skill_specialty',
                                          'is_main_skill', 'skill_level', 'skill_duration_use'))

#keywords
setnames(keywords, old=names(keywords), new=c('employee_id', 'keyword_categ', 'keyword', 
                                              'keyword_level', 'keyword_duration_use'))

#experiences
setnames(experiences, old=names(experiences), new=c('employee_id', 'experience_id', 'company_name',
                                                    'country_of_exp', 'exp_position', 'exp_start_date', 'exp_end_date', 'exp_duration',
                                                    'is_altran_exp', 'exp_title', 'client_sub_industry', 'client_sector', 'exp_prod_serv',
                                                    'client_depart_type', 'exp_objective', 'exp_approach', 'exp_result_description', 'exp_project'))

#exp.skills
setnames(exp.skills, old=names(exp.skills), new=c('employee_id', 'experience_id', 'exp_skill_competency_line',
                                                  'exp_skill_field', 'exp_skill_specialty'))

#exp.keywords
setnames(exp.keywords, old=names(exp.keywords), new=c('employee_id', 'experience_id', 'exp_keyword_categ', 'exp_keyword'))


### saving R files

save(resources, file='C:/Users/Altran/Desktop/BD/29-08/R files/resources.RData', ascii=T)
save(CF, file='C:/Users/Altran/Desktop/BD/29-08/R files/CF.RData', ascii=T)
save(diplomas, file='C:/Users/Altran/Desktop/BD/29-08/R files/diplomas.RData', ascii=T)
save(lang, file='C:/Users/Altran/Desktop/BD/29-08/R files/lang.RData', ascii=T)
save(certif, file='C:/Users/Altran/Desktop/BD/29-08/R files/certif.RData', ascii=T)
save(skills, file='C:/Users/Altran/Desktop/BD/29-08/R files/skills.RData', ascii=T)
save(keywords, file='C:/Users/Altran/Desktop/BD/29-08/R files/keywords.RData', ascii=T)
save(experiences, file='C:/Users/Altran/Desktop/BD/29-08/R files/experiences.RData', ascii=T)
save(exp.skills, file='C:/Users/Altran/Desktop/BD/29-08/R files/exp.skills.RData', ascii=T)
save(exp.keywords, file='C:/Users/Altran/Desktop/BD/29-08/R files/exp.keywords.RData', ascii=T)


### search for duplicate IDs in files


dup.id <- function(data) {
  cond <- length(unique(data$employee_id)) == nrow(data)
  myfunc <- function(dt) {
    deparse(substitute(dt))
  }
  if (cond == TRUE) {
    print(paste0('all IDs in ', myfunc(data), ' are unique'))
  } else {
    print(paste0('presence of repeated IDs in ', myfunc(data))) 
  }
}

dup.id(resources)
dup.id(CF)
dup.id(diplomas)
dup.id(lang)
dup.id(certif)
dup.id(skills)
dup.id(keywords)
dup.id(experiences)
dup.id(exp.skills)
dup.id(exp.keywords)



### which IDs are repeated

# res.dup.id <- resources %>% group_by(employee_id) %>% summarise(count=n()) %>% filter(count>1)
# CF.dup.id <- CF %>% group_by(employee_id) %>% summarise(count=n()) %>% filter(count>1)
diplomas.dup.id <- diplomas %>% group_by(employee_id) %>% summarise(count=n()) %>% filter(count>1)
orderBy(~-count, diplomas.dup.id)
lang.dup.id <- lang %>% group_by(employee_id) %>% summarise(count=n()) %>% filter(count>1)
orderBy(~-count, lang.dup.id)
certif.dup.id <- certif %>% group_by(employee_id) %>% summarise(count=n()) %>% filter(count>1)
orderBy(~-count, certif.dup.id)
skills.dup.id <- skills %>% group_by(employee_id) %>% summarise(count=n()) %>% filter(count>1)
orderBy(~-count, skills.dup.id)
keywords.dup.id <- keywords %>% group_by(employee_id) %>% summarise(count=n()) %>% filter(count>1)
orderBy(~-count, keywords.dup.id)
experiences.dup.id <- experiences %>% group_by(employee_id) %>% summarise(count=n()) %>% filter(count>1)
orderBy(~-count, experiences.dup.id)
exp.skills.dup.id <- exp.skills %>% group_by(employee_id) %>% summarise(count=n()) %>% filter(count>1)
orderBy(~-count, exp.skills.dup.id)
exp.keywords.dup.id <- exp.keywords %>% group_by(employee_id) %>% summarise(count=n()) %>% filter(count>1)
orderBy(~-count, exp.keywords.dup.id)


### keyword cleaning

#' for keywords:
#' 1st step: recode manually with sql
#' 2nd step: freqs (with >1 and 'other')
#' (code evenutual NAs as 'other')
#' 3rd step: either use freqs or use it for fuzzy matching or for kw clustering

# sql query
keywords2 <- keywords

keywords2 <- sqldf("select *,
                   case 
                   when upper(keyword) like '%.NET%' then '.NET'
                   when upper(keyword) like '%EXCEL%' then 'Excel'
                   when upper(keyword) like '%ALCATELLUCENT%' then 'alcatellucent'
                   when upper(keyword) like '%v?rgula(%' then 'other'
                   when upper(keyword) like '%semi-colon (%' then 'other'
                   when upper(keyword) like '%WORD%' then 'Word'
                   when upper(keyword) like '%BPM%' then 'BPM'
                   when upper(keyword) like '%JBOSS%' then 'JBoss'
                   when upper(keyword) like '%ABAP%' then 'ABAP'
                   when upper(keyword) like '%FIREWORKS%' then 'Fireworks'
                   when upper(keyword) like '%ILLUSTRATOR%' then 'Illustrator'
                   when upper(keyword) like '%INDESIGN%' then 'InDesign'
                   when upper(keyword) like '%LIFECYCLE%' then 'LifeCycle'
                   when upper(keyword) like '%PHOTOSHOP%' then 'Photoshop'
                   when upper(keyword) like '%PREMIERE%' then 'Premiere'
                   when upper(keyword) like '%SCOUT%' then 'Scout'
                   when upper(keyword) like '%OFFICE%' then 'Office'
                   when upper(keyword) like '%AGILE%' then 'Agile'
                   when upper(keyword) like '%AJAX%' then 'Ajax'
                   when upper(keyword) like '%ALTAMIRA%' then 'Altamira'
                   when upper(keyword) like '%ANALISTA%' then 'Analista'
                   when upper(keyword) like '%AN?LISE%' then 'Analista'
                   when upper(keyword) like '%ANALYSIS%' then 'Analista'
                   when upper(keyword) like '%ANGULAR%' then 'Angular'
                   when upper(keyword) like '%APACHE%' then 'Apache'
                   when upper(keyword) like '%ARIBA%' then 'ARIBA'
                   when upper(keyword) like '%AS400%' then 'AS400'
                   when upper(keyword) like '%AUDITORIA%' then 'auditoria'
                   when upper(keyword) like '%ASP%' then 'ASP'
                   when upper(keyword) like '%BMC%' then 'BMC'
                   when upper(keyword) like '%BOOTSARP%' then 'Bootstrap'
                   when upper(keyword) like '%BUGTRACK%' then 'Bugtracking'
                   when upper(keyword) like 'C LANG%' then 'C'
                   when upper(keyword) like 'C PROG%' then 'C'
                   when upper(keyword) like '%C#%' then 'C#'
                   when upper(keyword) like '%C++%' then 'C++'
                   when upper(keyword) like '%CICS%' then 'CICS'
                   when upper(keyword) like '%CITRIX%' then 'Citrix'
                   when upper(keyword) like '%CLIPPER%' then 'Clipper'
                   when upper(keyword) like '%COBOL%' then 'Cobol'
                   when upper(keyword) like '%CONSULT%' then 'Consultoria'
                   when upper(keyword) like '%CONTROLL%' then 'Controlling'
                   when upper(keyword) like '%CRYSTAL%' then 'Crystal'
                   when upper(keyword) like '%CSS%' then 'CSS'
                   when upper(keyword) like '%DELPHI%' then 'Delphi'
                   when upper(keyword) like '%DISASTER%' then 'Disaster Recovery'
                   when upper(keyword) like '%EDUCA??O%' then 'Educa??o'
                   when upper(keyword) like '%LINUX%' then 'Linux'
                   when upper(keyword) like '%ENTIDY%' then 'Entity Framework'
                   when upper(keyword) like '%ETL%' then 'ETL'
                   when upper(keyword) like '%JS%' then 'JS'
                   when upper(keyword) like '%JAVASCRIPT%' then 'JS'
                   when upper(keyword) like '%JAVA%' then 'JAVA'
                   when upper(keyword) like '%GEST?O%' then 'Gest?o'
                   when upper(keyword) like '%GITHUB%' then 'GITHUB'
                   when upper(keyword) like '%GIT%' then 'GIT'
                   when upper(keyword) like '%GOOGLE%' then 'Google'
                   when upper(keyword) like '%HADOOP%' then 'Hadoop'
                   when upper(keyword) like '%HP%' then 'HP'
                   when upper(keyword) like '%HTML%' then 'HTML'
                   when upper(keyword) like '%HTTP%' then 'HTTP'
                   when upper(keyword) like '%IBM%' then 'IBM'
                   when upper(keyword) like '%INFORMIX%' then 'Informix'
                   when upper(keyword) like '%ITIL%' then 'ITIL'
                   when upper(keyword) like '%JOOMLA%' then 'JOOMLA'
                   when upper(keyword) like '%JQUERY%' then 'JQUERY'
                   when upper(keyword) like '%LEAN%' then 'LEAN'
                   when upper(keyword) like '%LINQ%' then 'LINQ'
                   when upper(keyword) like '%LINUX%' then 'LINUX'
                   when upper(keyword) like '%MAINFRAME%' then 'Mainframe'
                   when upper(keyword) like '%MATEM?TICA%' then 'Matem?tica'
                   when upper(keyword) like '%MATH%' then 'Matem?tica'
                   when upper(keyword) like '%BIZTALK%' then 'Microsoft BizTalk'
                   when upper(keyword) like '%OFFICE%' then 'Office'
                   when upper(keyword) like '%SHAREPOINT%' then 'SharePoint'
                   when upper(keyword) like '%VISUAL STUD%' then 'Visual Studio'
                   when upper(keyword) like '%WINDOWS%' then 'Windows'
                   when upper(keyword) like '%ACCESS%' then 'Access'
                   when upper(keyword) like '%MVC%' then 'MVC'
                   when upper(keyword) like '%NETBACKUP%' then 'Netbackup'
                   when upper(keyword) like '%NOVELL%' then 'NOVELL'
                   when upper(keyword) like '%ORACLE%' then 'Oracle'
                   when upper(keyword) like '%OUTSYSTEMS%' then 'Outsystems'
                   when upper(keyword) like '%PACBASE%' then 'Packbase'
                   when upper(keyword) like '%PAGAMENTOS%' then 'Payments'
                   when upper(keyword) like '%PLANEAMENTO%' then 'Planeamento'
                   when upper(keyword) like '%PLANO %' then 'NET'
                   when upper(keyword) like '%POWERCENTER%' then 'Power Center'
                   when upper(keyword) like '%QUALIDADE%' then 'Qualidade'
                   when upper(keyword) like '%QUALITY%' then 'Qualidade'
                   when upper(keyword) like '%REPORT%' then 'Reporting'
                   when upper(keyword) like '%BANC%' then 'BANCA'
                   when upper(keyword) like '%BANK%' then 'BANCA'
                   when upper(keyword) like '%SAP%' then 'SAP'
                   when upper(keyword) like '%SAS%' then 'SAS'
                   when upper(keyword) like '%Serena%' then 'Serena'
                   when upper(keyword) like '%SHELLSCRIPT%' then 'Shell Script'
                   when upper(keyword) like '%SOAP%' then 'SOAP'
                   when upper(keyword) like '%SUMTOTAL%' then 'SumTotal'
                   when upper(keyword) like '%SUPPORT%' then 'Support'
                   when upper(keyword) like '%SVN%' then 'SVN'
                   when upper(keyword) like '%SYBASE%' then 'Sybase'
                   when upper(keyword) like '%SQL%' then 'SQL'
                   when upper(keyword) like '%TEAM FOUNDATION%' then 'Team Foundation'
                   when upper(keyword) like '%TELERIK%' then 'Telerik'
                   when upper(keyword) like '%TEST%' then 'Testes'
                   when upper(keyword) like '%TFS%' then 'TFS'
                   when upper(keyword) like '%TOOLBOOK%' then 'Toolbook'
                   when upper(keyword) like '%UML%' then 'UML'
                   when upper(keyword) like '%UNISYS%' then 'Unisys'
                   when upper(keyword) like '%VB%' then 'VB'
                   when upper(keyword) like '%WCF%' then 'WCF'
                   when upper(keyword) like '%WEB%' then 'WEB'
                   when upper(keyword) like '%WINDOWS%' then 'Windows'
                   when upper(keyword) like '%XML%' then 'XML'
                   when upper(keyword) like '%XSL%' then 'XSL'
                   else keyword
                   end as k1_new
                   from keywords2")

# remove punctuation
keywords2$k1_new <- gsub("[[:punct:]]", "", as.character(keywords2$k1_new))
# remove numbers
keywords2$k1_new <- gsub("[[:digit:]]", "", as.character(keywords2$k1_new))
# remove "Microsoft"
keywords2$k1_new <- gsub("microsoft ", "", as.character(tolower(keywords2$k1_new)))
# remove brackets
keywords2$k1_new <- gsub("\\(|\\)", "", as.character(tolower(keywords2$k1_new)))
# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
keywords2$k1_new <- trim(as.character(tolower(keywords2$k1_new)))
# blanks as 'others'
keywords2$k1_new <- ifelse(keywords2$k1_new=='', 'other', keywords2$k1_new)

# corrections
keywords2$k1_new <- replace(keywords2$k1_new, agrep('alcatellucent', tolower(keywords2$k1_new)), 'alcatellucent')

# combine levels of keywords
keywords2$k1_new <- combine.levels(keywords2$k1_new, minlev = 0.0005)
#corrections
keywords2$k1_new <- replace(keywords2$k1_new, agrep('OTHER', keywords2$k1_new), 'other')
keywords2$k1_new <- replace(keywords2$k1_new, agrep('android studio', keywords2$k1_new), 'android')
keywords2$k1_new <- replace(keywords2$k1_new, agrep('github', keywords2$k1_new), 'git')
keywords2$k1_new <- replace(keywords2$k1_new, agrep('ms visio', keywords2$k1_new), 'visio')
keywords2$k1_new <- replace(keywords2$k1_new, agrep('project management', keywords2$k1_new), 'project')
keywords2$k1_new <- replace(keywords2$k1_new, agrep('rest architecture', keywords2$k1_new), 'rest')
keywords2$k1_new <- replace(keywords2$k1_new, agrep('shell scripting', keywords2$k1_new), 'shell script')
keywords2$k1_new <- replace(keywords2$k1_new, agrep('shell scripting ksh sh bash…', keywords2$k1_new), 'shell script')
keywords2$k1_new <- replace(keywords2$k1_new, agrep('tems discovery', keywords2$k1_new), 'tems')
keywords2$k1_new <- replace(keywords2$k1_new, agrep('vb', keywords2$k1_new), 'visual basic')


### frequencies
freq.k <- as.data.frame(table(keywords2$k1_new))
colnames(freq.k)[1] <- 'kw'
freq.k <- orderBy(~-Freq, freq.k) # order freqs
# to eliminate keywords with freq = 1
freq.main.k <- filter(freq.k, Freq>1) # filter when freq>1

# recode original keywords from the freqs (least frequents: 'other')
kw_reduced <- ifelse(as.character(tolower(keywords2$k1_new)) %in% as.character(freq.main.k$kw),
                     tolower(keywords2$k1_new), 'other')

k2 <- keywords2

# correct keywords (ex.: 'a' as 'other')
kw_reduced <- ifelse(kw_reduced=='a', 'other', kw_reduced)

# create new column with the reduced list of keywords
k2$kw_reduced <- as.factor(kw_reduced)

###

save(keywords2, file='C:/Users/Altran/Desktop/BD/29-08/R files/keywords2.RData', ascii=T)
load('C:/Users/Altran/Desktop/BD/29-08/R files/keywords2.RData')
# with small set of kw:
save(k2, file='C:/Users/Altran/Desktop/BD/29-08/R files/k2.RData', ascii=T)
load('C:/Users/Altran/Desktop/BD/29-08/R files/k2.RData')

save(freq.k, file='C:/Users/Altran/Desktop/BD/29-08/R files/freq.k.RData', ascii=T)
load('C:/Users/Altran/Desktop/BD/29-08/R files/freq.k.RData')


### transpositions from long to wide format

# keywords

#create index for each keyword by ID
k2 <- keywords2 %>% group_by(employee_id) %>% mutate(kw_nr = row_number())
k2 <- data.frame(k2)
k2$kw_nr <- paste0('kw_', k2$kw_nr)
#from long to wide
k2.wide <- dcast(k2, employee_id ~ kw_nr, value.var = 'k1_new')
k2.wide[sapply(k2.wide, is.character)] <- lapply(k2.wide[sapply(k2.wide, is.character)],
                                                 as.factor) #convert vars again into factors to use na.tree.replace to recode NAs
#which IDs are repeated
k2.dup.id <- k2 %>% group_by(employee_id) %>% summarise(count=n()) %>% filter(count>1)
#to see number of keywords by ID (it will show the max, giving the number of new 'kw_' vars created)
orderBy(~-count, k2.dup.id)

# perc distribution of NAs

# k2.wide[k2.wide=='NA'] <- NA # if NAs are 'NA'
NA_perc.kw <- sapply(k2.wide, function(y) round(sum(length(which(is.na(y))))*100/length(y), 1))
NA_perc.kw <- data.frame(NA_perc.kw)
orderBy(~-NA_perc.kw, NA_perc.kw)

# to delete vars with more than a certain $ of NAs
k2.wide_small <- k2.wide[, colSums(is.na(k2.wide)) < nrow(k2.wide) * 0.3]

# or when 'NA'
NAfact_perc.kw <- sapply(k2.wide, function(y) round(as.numeric(length(y[y=='NA'])*100/length(y)), 1))
k2.wide_small <- na.tree.replace(k2.wide_small)


###
# original keyword transposition

#create index for each keyword by ID
keywords <- keywords %>% group_by(employee_id) %>% mutate(kw_nr = row_number())
keywords <- data.frame(keywords)
keywords$kw_nr <- paste0('kw_', keywords$kw)
#from long to wide
keywords.wide <- dcast(keywords, employee_id ~ kw_nr, value.var = 'keyword')
keywords.wide[sapply(keywords.wide, is.character)] <- lapply(keywords.wide[sapply(keywords.wide, is.character)],
                                                             as.factor) #convert vars again into factors to use na.tree.replace to recode NAs
keywords.wide <- na.tree.replace(keywords.wide)
#which IDs are repeated
keywords.dup.id <- keywords %>% group_by(employee_id) %>% summarise(count=n()) %>% filter(count>1)
#to see number of keywords by ID (it will show the max, giving the number of new 'kw_' vars created)
orderBy(~-count, keywords.dup.id)
###



# skills

#skills cleaning

# remove punctuation
skills2 <- skills
skills2$skill_specialty <- gsub("[[:punct:]]", "", as.character(skills2$skill_specialty))
# remove numbers
skills2$skill_specialty <- gsub("[[:digit:]]", "", as.character(skills2$skill_specialty))
# remove term "---"
#skills2$skill_specialty<- gsub("--- ", "", as.character(tolower(skills2$skill_specialty)))
# remove brackets
skills2$skill_specialty <- gsub("\\(|\\)", "", as.character(tolower(skills2$skill_specialty)))
# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
skills2$skill_specialty <- trim(as.character(tolower(skills2$skill_specialty)))
# blanks as 'others'
skills2$skill_specialty <- ifelse(skills2$skill_specialty=='', 'other', skills2$skill_specialty)

# corrections
skills2$skill_specialty <- replace(skills2$skill_specialty, agrep('desenvolvimento aplicacional de software', tolower(skills2$skill_specialty)), 'software application development')

# combine levels of skills
skills2$skill_specialty <- combine.levels(skills2$skill_specialty, minlev = 0.001)
#corrections
skills2$skill_specialty <- tolower(skills2$skill_specialty) #correcting 'OTHERS'
skills2$skill_specialty <- replace(skills2$skill_specialty, agrep('architecture  design', skills2$skill_specialty), 'arquitetura e desenho')
skills2$skill_specialty <- replace(skills2$skill_specialty, agrep('arquitetura e desenho de sistemas', skills2$skill_specialty), 'arquitetura e desenho')
skills2$skill_specialty <- replace(skills2$skill_specialty, agrep('banking product', skills2$skill_specialty), 'banking')
skills2$skill_specialty <- replace(skills2$skill_specialty, agrep('banking services', skills2$skill_specialty), 'banking')
skills2$skill_specialty <- replace(skills2$skill_specialty, agrep('dashboarding  reporting', skills2$skill_specialty), 'dashboarding e relatórios')
skills2$skill_specialty <- replace(skills2$skill_specialty, agrep('diagnostic maintenance  aftersales', skills2$skill_specialty), 'diagnóstico manutenção e pósvendas')
skills2$skill_specialty <- replace(skills2$skill_specialty, agrep('financial management', skills2$skill_specialty), 'finance services')
skills2$skill_specialty <- replace(skills2$skill_specialty, agrep('gestão de processos de negócio e fluxos de trabalho', skills2$skill_specialty), 'gestão de processos ti')
skills2$skill_specialty <- replace(skills2$skill_specialty, agrep('it test  validation automation', skills2$skill_specialty), 'it technical test  validation')
skills2$skill_specialty <- replace(skills2$skill_specialty, agrep('packaging  integration', skills2$skill_specialty), 'packaging e integração')
skills2$skill_specialty <- replace(skills2$skill_specialty, agrep('bancários', skills2$skill_specialty), 'banking')
skills2$skill_specialty <- replace(skills2$skill_specialty, agrep('finance', skills2$skill_specialty), 'finance services')
skills2$skill_specialty <- replace(skills2$skill_specialty, agrep('software', skills2$skill_specialty), 'software')
skills2$skill_specialty <- replace(skills2$skill_specialty, agrep('sap', skills2$skill_specialty), 'sap')
skills2$skill_specialty <- replace(skills2$skill_specialty, agrep('customer', skills2$skill_specialty), 'customer service')
skills2$skill_specialty <- replace(skills2$skill_specialty, agrep('cliente', skills2$skill_specialty), 'customer service')
skills2$skill_specialty <- replace(skills2$skill_specialty, agrep('test', skills2$skill_specialty), 'testing')
skills2$skill_specialty <- replace(skills2$skill_specialty, agrep('transmissão', skills2$skill_specialty), 'transmission')
skills2$skill_specialty <- as.factor(skills2$skill_specialty)

# frequencies
freq.skills <- as.data.frame(table(skills2$skill_specialty))
colnames(freq.skills)[1] <- 'sk'
freq.skills <- orderBy(~-Freq, freq.skills) # order freqs
# to eliminate keywords with freq = 1
#freq.main.skills <- filter(freq.skills, Freq>1) # filter when freq>1

# recode original skills from the freqs (least frequents: 'other')
#sk_reduced <- ifelse(as.character(tolower(skills2$skill_specialty)) %in% as.character(freq.main.skills$sk), tolower(skills2$skill_specialty), 'other')

# correct skills (ex.: 'a' as 'other')
# sk_reduced <- ifelse(sk_reduced=='a', 'other', sk_reduced)

# create new column with the reduced list of skills
#skills2$sk_reduced <- as.factor(sk_reduced)

### transpositions from long to wide format

#create index for each skill by ID
skills2 <- skills2 %>% group_by(employee_id) %>% mutate(sk_nr = row_number())
skills2 <- data.frame(skills2)
skills2$sk_nr <- paste0('sk_', skills2$sk_nr)
#from long to wide
skills.wide <- dcast(skills2, employee_id ~ sk_nr, value.var = 'skill_specialty')
skills.wide[sapply(skills.wide, is.character)] <- lapply(skills.wide[sapply(skills.wide, is.character)],
                                                         as.factor) #convert vars again into factors to use na.tree.replace to recode NAs
#which IDs are repeated
skills.dup.id <- skills2 %>% group_by(employee_id) %>% summarise(count=n()) %>% filter(count>1)
#to see number of keywords by ID (it will show the max, giving the number of new 'sk_' vars created)
orderBy(~-count, skills.dup.id)

# perc distribution of NAs

# skills.wide[skills.wide=='NA'] <- NA # if NAs are 'NA'
NA_perc.sk <- sapply(skills.wide, function(y) round(sum(length(which(is.na(y))))*100/length(y), 1))
NA_perc.sk <- data.frame(NA_perc.sk)
orderBy(~-NA_perc.sk, NA_perc.sk)

# to delete vars with more than a certain $ of NAs
skills.wide_small <- skills.wide[, colSums(is.na(skills.wide)) < nrow(skills.wide) * 0.5]

# or when 'NA'
NAfact_perc.sk <- sapply(skills.wide, function(y) round(as.numeric(length(y[y=='NA'])*100/length(y)), 1))

skills.wide_small[sapply(skills.wide_small, is.character)] <- lapply(skills.wide_small[sapply(skills.wide_small, is.character)],
                                                                     as.factor) #convert vars again into factors to use na.tree.replace to recode NAs
skills.wide_small <- na.tree.replace(skills.wide_small)



# certif

#certif cleaning

# remove punctuation
certif$certification <- gsub("[[:punct:]]", "", as.character(certif$certification))
# remove numbers
certif$certification <- gsub("[[:digit:]]", "", as.character(certif$certification))
# remove term "---"
#certif$certification<- gsub("--- ", "", as.character(tolower(certif$certification)))
# remove brackets
certif$certification <- gsub("\\(|\\)", "", as.character(tolower(certif$certification)))
# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
certif$certification <- trim(as.character(tolower(certif$certification)))
# blanks as 'others'
certif$certification <- ifelse(certif$certification=='', 'other', certif$certification)

# corrections
# certif$certification <- replace(certif$certification, agrep('---', tolower(certif$certification)), '---')

# combine levels of skills
certif$certif <- combine.levels(certif$certification, minlev = 0.002)
certif$certif <- tolower(certif$certif)

### transpositions from long to wide format

#create index for each certification by ID
certif <- certif %>% group_by(employee_id) %>% mutate(cert_nr = row_number())
certif <- data.frame(certif)
certif$cert_nr <- paste0('cert_', certif$cert_nr)
#from long to wide
certif.wide <- dcast(certif, employee_id ~ cert_nr, value.var = 'certif')
certif.wide[sapply(certif.wide, is.character)] <- lapply(certif.wide[sapply(certif.wide, is.character)],
                                                         as.factor) #convert vars again into factors to use na.tree.replace to recode NAs
#which IDs are repeated
certif.dup.id <- certif %>% group_by(employee_id) %>% summarise(count=n()) %>% filter(count>1)
#to see number of certifications by ID (it will show the max, giving the number of new 'cert_' vars created)
orderBy(~-count, certif.dup.id)

# perc distribution of NAs

# certif.wide[certif.wide=='NA'] <- NA # if NAs are 'NA'
NA_perc.cert <- sapply(certif.wide, function(y) round(sum(length(which(is.na(y))))*100/length(y), 1))
NA_perc.cert <- data.frame(NA_perc.cert)
orderBy(~-NA_perc.cert, NA_perc.cert)

# to delete vars with more than a certain $ of NAs
certif.wide_small <- select(certif.wide_small, employee_id, cert_1)
certif.wide_small_2 <- certif.wide[, colSums(is.na(certif.wide)) < nrow(certif.wide) * 0.6]

# or when 'NA'
NAfact_perc.cert <- sapply(certif.wide, function(y) round(as.numeric(length(y[y=='NA'])*100/length(y)), 1))

certif.wide_small <- na.tree.replace(certif.wide_small)



#lang

lang.wide <- dcast(lang, employee_id ~ languages, value.var='language_level')
colnames(lang.wide)[2:21] <- paste0("lang_",colnames(lang.wide)[2:21])
lang.wide[sapply(lang.wide, is.character)] <- lapply(lang.wide[sapply(lang.wide, is.character)], 
                                                     as.factor) 
lang.wide <- na.tree.replace(lang.wide)

### (eventually, replace levels by 1 and NAs by 0)

# lang.wide[lang.wide=='NA'] <- NA # if NAs are 'NA'
NA_perc.lang <- sapply(lang.wide, function(y) round(sum(length(which(is.na(y))))*100/length(y), 1))
NA_perc.lang <- data.frame(NA_perc.lang)
orderBy(~-NA_perc.lang, NA_perc.lang)

# to delete vars with more than a certain $ of NAs
lang.wide_small <- lang.wide[, colSums(is.na(lang.wide)) < nrow(lang.wide) * 0.75]

# or when 'NA'
NAfact_perc.lang <- sapply(lang.wide, function(y) round(as.numeric(length(y[y=='NA'])*100/length(y)), 1))




#diplomas

#recode levels for education_type
diplomas$education_type <- plyr::mapvalues(diplomas$education_type,
                                           from = c("High-School level", "High-School diploma", '1 university year degree',
                                                    '2 university years degree', '3 university years degree', '4 university years degree',
                                                    '5 university years degree', '6 university years degree', '7 university years degree',
                                                    '8 university years degree'),
                                           to = c("High-School", "High-School", 'less than 3 yrs', 'less than 3 yrs', '3 yrs',
                                                  '4-5 yrs', '4-5 yrs', 'more than 5 yrs', 'more than 5 yrs', 'more than 5 yrs'))

#diplomas cleaning

# remove punctuation
diplomas$dip_specialization_name <- gsub("[[:punct:]]", "", as.character(diplomas$dip_specialization_name))
# remove numbers
diplomas$dip_specialization_name <- gsub("[[:digit:]]", "", as.character(diplomas$dip_specialization_name))
# remove term "---"
#diplomas$dip_specialization_name<- gsub("--- ", "", as.character(tolower(diplomas$dip_specialization_name)))
# remove brackets
diplomas$dip_specialization_name <- gsub("\\(|\\)", "", as.character(tolower(diplomas$dip_specialization_name)))
# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
diplomas$dip_specialization_name <- trim(as.character(tolower(diplomas$dip_specialization_name)))
# blanks as 'others'
diplomas$dip_specialization_name <- ifelse(diplomas$dip_specialization_name=='', 'other', diplomas$dip_specialization_name)

# corrections
# diplomas$dip_specialization_name <- replace(diplomas$dip_specialization_name, agrep('---', tolower(diplomas$dip_specialization_name)), '---')

# combine levels of certifications
diplomas$dip_specialization_name <- combine.levels(diplomas$dip_specialization_name, minlev = 0.003)
#corrections
diplomas$dip_specialization_name <- tolower(diplomas$dip_specialization_name) #correcting 'OTHERS'

### transpositions from long to wide format

#create index for each diplomasication by ID
diplomas <- diplomas %>% group_by(employee_id) %>% mutate(dip_nr = row_number())
diplomas <- data.frame(diplomas)
diplomas$dip_nr <- paste0('dip_', diplomas$dip_nr)
#from long to wide
diplomas.wide <- dcast(diplomas, employee_id ~ dip_nr, value.var = 'dip_specialization_name')
diplomas.wide[sapply(diplomas.wide, is.character)] <- lapply(diplomas.wide[sapply(diplomas.wide, is.character)],
                                                             as.factor) #convert vars again into factors to use na.tree.replace to recode NAs
#which IDs are repeated
diplomas.dup.id <- diplomas %>% group_by(employee_id) %>% summarise(count=n()) %>% filter(count>1)
#to see number of diplomasications by ID (it will show the max, giving the number of new 'dip_' vars created)
orderBy(~-count, diplomas.dup.id)

# perc distribution of NAs

# diplomas.wide[diplomas.wide=='NA'] <- NA # if NAs are 'NA'
NA_perc.dip <- sapply(diplomas.wide, function(y) round(sum(length(which(is.na(y))))*100/length(y), 1))
NA_perc.dip <- data.frame(NA_perc.dip)
orderBy(~-NA_perc.dip, NA_perc.dip)

# to delete vars with more than a certain $ of NAs
diplomas.wide_small <- diplomas.wide[, colSums(is.na(diplomas.wide)) < nrow(diplomas.wide) * 0.95]

# or when 'NA'
NAfact_perc.dip <- sapply(diplomas.wide, function(y) round(as.numeric(length(y[y=='NA'])*100/length(y)), 1))

diplomas.wide_small[sapply(diplomas.wide_small, is.character)] <- lapply(diplomas.wide_small[sapply(diplomas.wide_small, is.character)],
                                                                         as.factor) #convert vars again into factors to use na.tree.replace to recode NAs
diplomas.wide_small <- na.tree.replace(diplomas.wide_small)







# exp.skills (partial)

exp.skills.wide <- dcast(exp.skills, employee_id ~ exp_skill_competency_line)
colnames(exp.skills.wide)[2:16] <- paste0("exp.sk_",colnames(exp.skills.wide)[2:16])


# exp.keywords (partial)

exp.keywords.wide <- dcast(exp.keywords, employee_id ~ exp_keyword_categ)
colnames(exp.keywords.wide)[2:7] <- paste0("exp.kw_",colnames(exp.keywords.wide)[2:7])


###

# still to do:
experiences
exp.skills
exp.keywords

###


### joins


# full outer join of resources & CF

res.CF_full <- merge(x = resources, y = CF, by = "employee_id", all = TRUE)
res.CF_full <- na.tree.replace(res.CF_full)
save(res.CF_full, file='C:/Users/Altran/Desktop/BD/29-08/R files/res.CF_full.RData', ascii=T)
load('C:/Users/Altran/Desktop/BD/29-08/R files/res.CF_full.RData')

# left join of resources & CF

res.CF <- merge(x = resources, y = CF, by = "employee_id", all.x = TRUE)
res.CF <- na.tree.replace(res.CF)
save(res.CF, file='C:/Users/Altran/Desktop/BD/29-08/R files/res.CF.RData', ascii=T)
load('C:/Users/Altran/Desktop/BD/29-08/R files/res.CF.RData')



# join of res.CF_full & diplomas.wide_small

res.CF.dip <- merge(x = res.CF_full, y = diplomas.wide_small, by = "employee_id", all.x = TRUE)
res.CF.dip <- na.tree.replace(res.CF.dip)
save(res.CF.dip, file='C:/Users/Altran/Desktop/BD/29-08/R files/res.CF.dip.RData', ascii=T)
load('C:/Users/Altran/Desktop/BD/29-08/R files/res.CF.dip.RData')



# join of res.CF.dip & lang.wide_small

res.CF.dip.lang <- merge(x = res.CF.dip, y = lang.wide_small, by = "employee_id", all.x = TRUE)
res.CF.dip.lang <- na.tree.replace(res.CF.dip.lang)
save(res.CF.dip.lang, file='C:/Users/Altran/Desktop/BD/29-08/R files/res.CF.dip.lang.RData', ascii=T)
load('C:/Users/Altran/Desktop/BD/29-08/R files/res.CF.dip.lang.RData')



# join of res.CF.dip.lang & skills.wide_small

res.CF.dip.lang.sk <- merge(x = res.CF.dip.lang, y = skills.wide_small, by = "employee_id", all.x = TRUE)
res.CF.dip.lang.sk <- na.tree.replace(res.CF.dip.lang.sk)
save(res.CF.dip.lang.sk, file='C:/Users/Altran/Desktop/BD/29-08/R files/res.CF.dip.lang.sk.RData', ascii=T)
load('C:/Users/Altran/Desktop/BD/29-08/R files/res.CF.dip.lang.sk.RData')



# join of res.CF.dip.lang.sk & k2.wide_small
res.CF.dip.lang.sk.kw <- merge(x = res.CF.dip.lang.sk, y = k2.wide_small, by = "employee_id", all.x = TRUE)
res.CF.dip.lang.sk.kw <- na.tree.replace(res.CF.dip.lang.sk.kw)
save(res.CF.dip.lang.sk.kw, file='C:/Users/Altran/Desktop/BD/29-08/R files/res.CF.dip.lang.sk.kw.RData', ascii=T)
load('C:/Users/Altran/Desktop/BD/29-08/R files/res.CF.dip.lang.sk.kw.RData')



# join of res.CF.dip.lang.sk.kw & certif.wide_small

all.main.joins <- merge(x = res.CF.dip.lang.sk.kw, y = certif.wide_small, by = "employee_id", all.x = TRUE)
#removing rows with linx_pub_status='Initiated'
#all.main.joins <- filter(all.main.joins, linx_pub_status=='Published' | is.na(linx_pub_status)==T)
all.main.joins <- na.tree.replace(all.main.joins)
save(all.main.joins, file='C:/Users/Altran/Desktop/BD/29-08/R files/all.main.joins.RData', ascii=T)
load('C:/Users/Altran/Desktop/BD/29-08/R files/all.main.joins.RData')

# perc distribution of NAs

NA_perc.ALL <- sapply(all.main.joins, function(y) round(sum(length(which(is.na(y))))*100/length(y), 1))
NA_perc.ALL <- data.frame(NA_perc.ALL)
orderBy(~-NA_perc.ALL, NA_perc.ALL)

# to delete vars with more than a certain $ of NAs
all.main.joins_small <- all.main.joins[, colSums(is.na(all.main.joins)) < nrow(all.main.joins) * 0.75]

# or when 'NA'
# NAfact_perc.dip <- sapply(all.main.joins, function(y) round(as.numeric(length(y[y=='NA'])*100/length(y)), 1))

# all.main.joins_small <- na.tree.replace(all.main.joins_small)

# drop vars
all.main.joins_small <- select(all.main.joins_small, -hierar_mgr, -lang_portuguese,
                               -summary, -nationality, -profile_title)
NA_perc.ALL_small <- sapply(all.main.joins_small, function(y) round(sum(length(which(is.na(y))))*100/length(y), 1))
NA_perc.ALL_small <- data.frame(NA_perc.ALL_small)
orderBy(~-NA_perc.ALL_small, NA_perc.ALL_small)

# clean vars

#location into dummy / join location & location_2 to replace NAs in location
all.main.joins_small$location <- ifelse(all.main.joins_small$location =='Portugal - Lisbon',
                                        'Lisbon', 'other')
all.main.joins_small$location_2 <- ifelse(all.main.joins_small$location_2 =='Portugal - Lisbon',
                                        'Lisbon', 'other')
all.main.joins_small$location <- ifelse(is.na(all.main.joins_small$location) == T,
                                        all.main.joins_small$location_2, all.main.joins_small$location)
setnames(all.main.joins_small, old='location', new='at_lisbon')
all.main.joins_small <- select(all.main.joins_small, -location_2)
all.main.joins_small$at_lisbon <- ifelse(all.main.joins_small$at_lisbon == "Lisbon", 1, 0)
all.main.joins_small$at_lisbon <- as.factor(all.main.joins_small$at_lisbon)

#join sub_division & sub_division_2 to replace NAs in sub_division
all.main.joins_small$sub_division <- as.character(all.main.joins_small$sub_division)
all.main.joins_small$sub_division_2 <- as.character(all.main.joins_small$sub_division_2)
all.main.joins_small$sub_division <- ifelse(is.na(all.main.joins_small$sub_division) == T,
                                        all.main.joins_small$sub_division_2, all.main.joins_small$sub_division)
all.main.joins_small <- select(all.main.joins_small, -sub_division_2)
#clean levels
all.main.joins_small$sub_division <- gsub("altran |portugal - | - portugal", "", tolower(all.main.joins_small$sub_division))
all.main.joins_small$sub_division <- gsub("finance", "fs", tolower(all.main.joins_small$sub_division))
all.main.joins_small$sub_division <- gsub("government industry ait", "government, industry & ait", tolower(all.main.joins_small$sub_division))

#set has_certif
all.main.joins_small$has_certif <- ifelse(is.na(all.main.joins_small$cert_1) == T, 0, 1)

#dates
all.main.joins_small$hire_date <- as.Date(all.main.joins_small$hire_date, format='%Y/%m/%d')
all.main.joins_small$leave_date <- as.Date(all.main.joins_small$leave_date, format='%Y/%m/%d')
all.main.joins_small$career_start_date <- as.Date(all.main.joins_small$career_start_date, format='%Y/%m/%d')

#dip_1
all.main.joins_small$dip_1 <- replace(all.main.joins_small$dip_1 , agrep('health biomedical', tolower(all.main.joins_small$dip_1 )), 'engenharia biomédica')



save(all.main.joins_small, file='C:/Users/Altran/Desktop/BD/29-08/R files/all.main.joins_small.RData', ascii=T)







# save tables as csv files
write.csv2(resources, file='C:/Users/Altran/Desktop/BD/29-08/R output/resources.csv')
write.csv2(CF, file='C:/Users/Altran/Desktop/BD/29-08/R output/CF.csv')
write.csv2(diplomas, file='C:/Users/Altran/Desktop/BD/29-08/R output/diplomas.csv')
write.csv2(lang, file='C:/Users/Altran/Desktop/BD/29-08/R output/lang.csv')
write.csv2(certif, file='C:/Users/Altran/Desktop/BD/29-08/R output/certif.csv')
write.csv2(skills, file='C:/Users/Altran/Desktop/BD/29-08/R output/skills.csv')
write.csv2(keywords, file='C:/Users/Altran/Desktop/BD/29-08/R output/keywords.csv')
write.csv2(experiences, file='C:/Users/Altran/Desktop/BD/29-08/R output/experiences.csv')
write.csv2(exp.skills, file='C:/Users/Altran/Desktop/BD/29-08/R output/exp.skills.csv')
write.csv2(exp.keywords, file='C:/Users/Altran/Desktop/BD/29-08/R output/exp.keywords.csv')
write.csv2(all.main.joins, file='C:/Users/Altran/Desktop/BD/29-08/R output/all.main.joins.csv')
write.csv2(all.main.joins_small, file='C:/Users/Altran/Desktop/BD/29-08/R output/all.main.joins_small.csv')




# order diplomas by date, then do index (check for differences)



# investigate removing observations with DC as CFStatus=='Initiated' (ie not published) in CF


# recode keywords in google sheets (to separate strings, then apply fuzzy matching function; check for differences)

# ex with keywords table
#' search for:
#' package stringr & stringdist


# contingencies tables
mt <- xtabs(~ BusinessDepName+ClientSector, experiences)
ftable(mt)
summary(mt) #Chi-square



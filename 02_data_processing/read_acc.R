library(data.table) # For speed instead of data.frames we are using data.tables
library(lubridate)
options(digits.secs=5)

################################################ 
### 1. READ ACCELEROMETER DATA FROM FOLDERS ####
################################################
# Set data folder
dir.data = paste0(getwd(),"/01_data/csv/activity/")
# Get all files in data folder
files = list.files(path = dir.data, pattern = "*\\.csv$", full.names = F)
# Disconsider animal OCT02 - It was predated and collar found 1km from the tunnel
files = files[files != "OCT02.csv"]

tuco_acc = vector("list", length(files)) # Initialize a empty list
for (i in 1:length(files)) {
    # 1. READ FILE
    message("\nReading file:", files[i], "...")
    tuco_acc[[i]] = fread(paste0(dir.data,"/",files[i]),
                          header = F,   # ignore file's header
                          skip = 1 ,    # skip first line corresponding to header
                          sep = "auto", # set sep to auto. Some files use comma, others space
                          fill = T,
                          select = 2:5, # ignore id column
                          col.names = c("datetime", "X", "Y", "Z"),
                          showProgress = T,
                          data.table = T)
    
    # 2. FIX ID AND PARSE DATETIME
    tuco_acc[[i]][, ID := strsplit(files[i], ".", fixed = T)[[1]][1]]
    tuco_acc[[i]][, datetime := parse_date_time(datetime, "dmYHMOS", tz = "America/Argentina/La_Rioja")]
    
    # DELETE FIRST AND LAST DAY OF RECORDING
    tuco_acc[[i]][, day_number := frank(as_date(datetime), ties.method = "dense")]
    tuco_acc[[i]] = tuco_acc[[i]][day_number > 1 & day_number < last(day_number)] 
    tuco_acc[[i]][, day_number := frank(as_date(datetime), ties.method = "dense")]
    
    
    # DELETE PARTICULAR DATA
    # delete last 7 days of #FEV05 because it was brought to the lab and continued recording for a week
    # delete last 2 days of #JUL23 due to recapture efforts
    # delete last 5 days of #JUL16 due to recapture efforts
    if(files[i] == "FEV05.csv"){
        tuco_acc[[i]] = tuco_acc[[i]][day_number < last(day_number) - 5]
    }
    if(files[i] == "JUL23.csv"){
        tuco_acc[[i]] = tuco_acc[[i]][day_number < last(day_number) - 2]
    }
    if(files[i] == "JUL16.csv"){
        tuco_acc[[i]] = tuco_acc[[i]][day_number < last(day_number) - 5]
    }
}
# Bind the whole list to create a unique data.table with all animals' data
tuco_acc = rbindlist(tuco_acc)
setcolorder(tuco_acc, c("ID", "day_number", "datetime", "X", "Y", "Z"))

# Save created data.frame as a .rds file
#saveRDS(tuco_acc, "data_rds/activity/tuco_10hz_raw.rds")

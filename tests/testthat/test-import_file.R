## These won't work on Travis, unless we can host them somewhere online
## somewhere with a consistent address. For now we can skip them. Depending on
## if we find a solution or not, when it comes to CRAN submission there is a
## skip_on_cran() option too.

## Everything after this is skipped
skip_on_travis()

# General -----------------------------------------------------------------

## error fot file not existing
expect_error(import_file("~random/path/to/file"), "File does not exist - please check file path.")

## error fot file not recognised
expect_error(import_file("~/Dropbox/respR_import_test_files/z_unsupported_for_now/pyroscience-aquaresp.txt"),
             "Source file cannot be identified.")


# Firesting Pyro ----------------------------------------------------------

path01 <- "~/Dropbox/respR_import_test_files/firesting_pyro/pyro01.txt" # Anna Andreassen
path02 <- "~/Dropbox/respR_import_test_files/firesting_pyro/pyro02.txt" # Lauren Rowsey 1
path03 <- "~/Dropbox/respR_import_test_files/firesting_pyro/pyro03.txt" # Lauren Rowsey 2

# Earlier firmware, without date column. Passes midnight, so good for updating format_time to detect this
# Also doesn't have numeric time column, like other firmwares
path04 <- "~/Dropbox/respR_import_test_files/firesting_pyro/pyro04.txt" # Matt Guzzo 1
# Earlier firmware, without date or numeric time column. Passes midnight TWICE
path05 <- "~/Dropbox/respR_import_test_files/firesting_pyro/pyro05.txt" # Matt Guzzo 2

# Next one has whole extra dataset below first set at row 20700. Don't know how
# common this is. Maybe we support in future. For now gives error message and stops.
path06 <- "~/Dropbox/respR_import_test_files/firesting_pyro/pyro06.txt" # 01112017 - sensor A.txt

# OK
path07 <- "~/Dropbox/respR_import_test_files/firesting_pyro/pyro07.txt" # 01112017 - sensor D.txt

# Next one has a comment in Comment column and is double spaced
path08 <- "~/Dropbox/respR_import_test_files/firesting_pyro/pyro08.txt" # A1_01020304.txt

# Next one is double spaced
path09 <- "~/Dropbox/respR_import_test_files/firesting_pyro/pyro09.txt" # file_MMR.txt

# OK
path10 <- "~/Dropbox/respR_import_test_files/firesting_pyro/pyro10.txt" # pyroscience.txt from FishResp

## does not error
expect_error(import_file(path01), NA)
expect_error(import_file(path02), NA)
expect_error(import_file(path03), NA)
expect_error(import_file(path04), NA)
expect_error(import_file(path05), NA)
expect_error(import_file(path06), "Import halted.")
expect_error(import_file(path07), NA)
expect_error(import_file(path08), NA)
expect_error(import_file(path09), NA)
expect_error(import_file(path10), NA)

## exact values
imp <- import_file(path01)
expect_equal(as.numeric(imp[1,3]), 1.3)
expect_equal(as.numeric(imp[65204,26]), 5.188)

imp <- import_file(path02)
expect_equal(as.numeric(imp[1,3]), 0)
expect_equal(as.numeric(imp[21491,25]), 7.199)

imp <- import_file(path03)
expect_equal(as.numeric(imp[2,3]), 3)
expect_equal(as.numeric(imp[1531,25]), 7.168)

imp <- import_file(path04)
expect_equal(as.numeric(imp[1,3]), 9.93)
expect_equal(as.numeric(imp[86262,18]), 8.14)

imp <- import_file(path05)
expect_equal(as.numeric(imp[1,2]), 11.222)
expect_equal(as.numeric(imp[36727,17]), 5.48)

## path06 not supported yet because contains multiple datasets

imp <- import_file(path07)
expect_equal(as.numeric(imp[2,3]), 1.55)
expect_equal(as.numeric(imp[79568,25]), 4.067)

imp <- import_file(path08)
expect_equal(as.numeric(imp[2,3]), 2.24)
expect_equal(as.numeric(imp[21092,28]), 6.001)
expect_equal(as.character(imp[1690,4]), "timer on") # imports comment ok

imp <- import_file(path09)
expect_equal(as.numeric(imp[2,3]), 1.37)
expect_equal(as.numeric(imp[2172,25]), 6.058)

imp <- import_file(path10)
expect_equal(as.numeric(imp[2,4]), 7.3012)
expect_equal(as.numeric(imp[3600,10]), 5.243)



# Neofox ------------------------------------------------------------------

path01 <- "~/Dropbox/respR_import_test_files/neofox/neofox01.csv" # Derek Somo
path02 <- "~/Dropbox/respR_import_test_files/neofox/neofox02.csv" # Mine - ACACTB11.csv
path03 <- "~/Dropbox/respR_import_test_files/neofox/neofox03.csv" # Mine - AFHC10.csv
path04 <- "~/Dropbox/respR_import_test_files/neofox/neofox04.csv" # Mine - AFHC18.csv
path05 <- "~/Dropbox/respR_import_test_files/neofox/neofox05.csv" # Mine - CTRL23.csv
path06 <- "~/Dropbox/respR_import_test_files/neofox/neofox06.csv" # Mine - CTRL55.csv
path07 <- "~/Dropbox/respR_import_test_files/neofox/neofox07.csv" # Mine - LEPCTA02.csv
path08 <- "~/Dropbox/respR_import_test_files/neofox/neofox08.csv" # Mine - LEPCTD06.csv
path09 <- "~/Dropbox/respR_import_test_files/neofox/neofox09.csv" # Mine - OFLA03.csv
path10 <- "~/Dropbox/respR_import_test_files/neofox/neofox10.csv" # Mine - OFLA12.csv
path11 <- "~/Dropbox/respR_import_test_files/neofox/neofox11.csv" # Mine - TONRTD02.csv
path12 <- "~/Dropbox/respR_import_test_files/neofox/neofox12.csv" # Mine - TONRTE13.csv

# imports
expect_error(import_file(path01), NA)
expect_error(import_file(path02), NA)
expect_error(import_file(path03), NA)
expect_error(import_file(path04), NA)
expect_error(import_file(path05), NA)
expect_error(import_file(path06), NA)
expect_error(import_file(path07), NA)
expect_error(import_file(path08), NA)
expect_error(import_file(path09), NA)
expect_error(import_file(path10), NA)
expect_error(import_file(path11), NA)
expect_error(import_file(path12), NA)

# values
imp <- import_file(path01)
expect_equal(as.numeric(imp[1,2]), 95.75888)
expect_equal(as.numeric(imp[10089,5]), 99.63232)

imp <- import_file(path02)
expect_equal(as.numeric(imp[1,2]), 99.30429)
expect_equal(as.numeric(imp[6031,5]), 101.8447)

imp <- import_file(path03)
expect_equal(as.numeric(imp[1,2]), 100.0138)
expect_equal(as.numeric(imp[3453,5]), 100.2642)

imp <- import_file(path04)
expect_equal(as.numeric(imp[1,2]), 101.9792)
expect_equal(as.numeric(imp[22840,5]), 99.75)

imp <- import_file(path05)
expect_equal(as.numeric(imp[1,2]), 103.5505)
expect_equal(as.numeric(imp[5980,5]), 101.793)

imp <- import_file(path06)
expect_equal(as.numeric(imp[1,2]), 99.6987)
expect_equal(as.numeric(imp[6864,5]), 102.9155)

imp <- import_file(path07)
expect_equal(as.numeric(imp[1,2]), 97.27502)
expect_equal(as.numeric(imp[49996,5]), 99.99487)

imp <- import_file(path08)
expect_equal(as.numeric(imp[1,2]), 101.4413)
expect_equal(as.numeric(imp[51906,5]), 101.5442)

imp <- import_file(path09)
expect_equal(as.numeric(imp[1,2]), 105.2127)
expect_equal(as.numeric(imp[3484,5]), 99.96631)

imp <- import_file(path10)
expect_equal(as.numeric(imp[1,2]), 101.8483)
expect_equal(as.numeric(imp[3638,5]), 100.0615)

imp <- import_file(path11)
expect_equal(as.numeric(imp[1,2]), 98.35919)
expect_equal(as.numeric(imp[5175,5]), 101.2205)

imp <- import_file(path12)
expect_equal(as.numeric(imp[1,2]), 100.5918)
expect_equal(as.numeric(imp[5172,5]), 102.9783)


# Vernier csv -------------------------------------------------------------

path01 <- "~/Dropbox/respR_import_test_files/vernier_csv/vernier_csv_01.csv" # Mine - tst1.csv
path02 <- "~/Dropbox/respR_import_test_files/vernier_csv/vernier_csv_02.csv" # Mine - tst2.csv
path03 <- "~/Dropbox/respR_import_test_files/vernier_csv/vernier_csv_03.csv" # Mine - tst3.csv
path04 <- "~/Dropbox/respR_import_test_files/vernier_csv/vernier_csv_04.csv" # Mine - tst4.csv
path05 <- "~/Dropbox/respR_import_test_files/vernier_csv/vernier_csv_05.csv" # Mine - tst5.csv
path06 <- "~/Dropbox/respR_import_test_files/vernier_csv/vernier_csv_06.csv" # Mine - tst6.csv
path07 <- "~/Dropbox/respR_import_test_files/vernier_csv/vernier_csv_07.csv" # Mine - tst7.csv
path08 <- "~/Dropbox/respR_import_test_files/vernier_csv/vernier_csv_08.csv" # Mine - tst8.csv
path09 <- "~/Dropbox/respR_import_test_files/vernier_csv/vernier_csv_09.csv" # Mine - tst9.csv
path10 <- "~/Dropbox/respR_import_test_files/vernier_csv/vernier_csv_10.csv" # Mine - tst10.csv
path11 <- "~/Dropbox/respR_import_test_files/vernier_csv/vernier_csv_11.csv" # Mine - tst11.csv
path12 <- "~/Dropbox/respR_import_test_files/vernier_csv/vernier_csv_12.csv" # Mine - tst12.csv
path13 <- "~/Dropbox/respR_import_test_files/vernier_csv/vernier_csv_13.csv" # Mine - tst13.csv
path14 <- "~/Dropbox/respR_import_test_files/vernier_csv/vernier_csv_14.csv" # Mine - tst14.csv
path15 <- "~/Dropbox/respR_import_test_files/vernier_csv/vernier_csv_15.csv" # Mine - tst15.csv
path16 <- "~/Dropbox/respR_import_test_files/vernier_csv/vernier_csv_16.csv" # Mine - tst16.csv
path17 <- "~/Dropbox/respR_import_test_files/vernier_csv/vernier_csv_17.csv" # Mine - tst17.csv
path18 <- "~/Dropbox/respR_import_test_files/vernier_csv/vernier_csv_18.csv" # Mine - tst18.csv
path19 <- "~/Dropbox/respR_import_test_files/vernier_csv/vernier_csv_19.csv" # Mine - tst19.csv
path20 <- "~/Dropbox/respR_import_test_files/vernier_csv/vernier_csv_20.csv" # Mine - tst20.csv
path21 <- "~/Dropbox/respR_import_test_files/vernier_csv/vernier_csv_21.csv" # Mine - tst21.csv
path22 <- "~/Dropbox/respR_import_test_files/vernier_csv/vernier_csv_22.csv" # Mine - Random Nucella Untitled.csv
path23 <- "~/Dropbox/respR_import_test_files/vernier_csv/vernier_csv_23.csv" # Mine - small_snail.csv
path24 <- "~/Dropbox/respR_import_test_files/vernier_csv/vernier_csv_24.csv" # Mine - nucella.csv
path25 <- "~/Dropbox/respR_import_test_files/vernier_csv/vernier_csv_25.csv" # Mine - nucella2.csv



# imports
expect_error(import_file(path01), NA)
expect_error(import_file(path02), NA)
expect_error(import_file(path03), NA)
expect_error(import_file(path04), NA)
expect_error(import_file(path05), NA)
expect_error(import_file(path06), NA)
expect_error(import_file(path07), NA)
expect_error(import_file(path08), NA)
expect_error(import_file(path09), NA)
expect_error(import_file(path10), NA)
expect_error(import_file(path11), NA)
expect_error(import_file(path12), NA)
expect_error(import_file(path13), NA)
expect_error(import_file(path14), NA)
expect_error(import_file(path15), NA)
expect_error(import_file(path16), NA)
expect_error(import_file(path17), NA)
expect_error(import_file(path18), NA)
expect_error(import_file(path19), NA)
expect_error(import_file(path20), NA)
expect_error(import_file(path21), NA)
expect_error(import_file(path22), NA)
expect_error(import_file(path23), NA)
expect_error(import_file(path24), NA)
expect_error(import_file(path25), NA)


# exact values
imp <- import_file(path05)
expect_equal(round(as.numeric(imp[1,2]), 4), 8.3157)
expect_equal(round(as.numeric(imp[58,12]), 4), 99.0743)
# columns
expect_equal(ncol(imp), 12)
expect_true(is.na(imp[[58,5]])) # NA correctly filled in for shorter columns

imp <- import_file(path09)
expect_equal(round(as.numeric(imp[1,2]), 4), 8.2330)
expect_equal(round(as.numeric(imp[98,5]), 4), 22.4746)
expect_equal(ncol(imp), 5)

imp <- import_file(path11)
expect_equal(round(as.numeric(imp[1,2]), 4), 8.2272)
expect_equal(round(as.numeric(imp[117,5]), 4), 23.6938)
expect_equal(ncol(imp), 5)
expect_match(colnames(imp)[1], "exp 1:") # custom Run name is imported correctly to column name

imp <- import_file(path12)
expect_equal(round(as.numeric(imp[1,2]), 4), 8.2109)
expect_equal(ncol(imp), 10)
expect_match(colnames(imp)[1], "test") # custom Run name is imported correctly to column name
expect_match(colnames(imp)[6], "Run") # original Run name is in other column names

## col names of other data types are correct
imp <- import_file(path14)
expect_match(colnames(imp)[2], "Position")
expect_match(colnames(imp)[3], "Velocity")
expect_match(colnames(imp)[4], "Acceleration")
expect_match(colnames(imp)[8], "Potential")

## col names in correct order as determined by LabQuest system
## i.e. in this e.g. DO2 comes before DO. Determined by order probes are plugged in.
imp <- import_file(path15)
expect_match(colnames(imp)[2], "Run 1: Dissolved Oxygen 2 \\(mg/L\\)")
expect_match(colnames(imp)[4], "Run 1: Dissolved Oxygen \\(%\\)")

## Time data are in mins
imp <- import_file(path19)
expect_match(colnames(imp)[1], "Run 1: Time \\(min\\)")

## Time data are in ms
imp <- import_file(path21)
expect_match(colnames(imp)[1], "Run 1: Time \\(ms\\)")

## Actual Experiment files
imp <- import_file(path22)
expect_equal(round(as.numeric(imp[1,2]), 4), 101.3072)
expect_equal(round(as.numeric(imp[39823,3]), 4), 98.9883)
expect_equal(ncol(imp), 3)

imp <- import_file(path23)
expect_equal(round(as.numeric(imp[1,2]), 4), 98.8149)
expect_equal(round(as.numeric(imp[6238,4]), 4), 13.6184)
expect_equal(ncol(imp), 4)

imp <- import_file(path24)
expect_equal(round(as.numeric(imp[1,2]), 4), 100.0943)
expect_equal(round(as.numeric(imp[68047,4]), 4), 11.9977)
expect_equal(ncol(imp), 4)

imp <- import_file(path25)
expect_equal(round(as.numeric(imp[1,2]), 4), 99.4645)
expect_equal(round(as.numeric(imp[65631,3]), 4), 12.3602)
expect_equal(ncol(imp), 3)



# Vernier txt -------------------------------------------------------------

path01 <- "~/Dropbox/respR_import_test_files/vernier_txt/vernier_txt_01.txt" # Mine - tst1.txt
path02 <- "~/Dropbox/respR_import_test_files/vernier_txt/vernier_txt_02.txt" # Mine - tst2.txt
path03 <- "~/Dropbox/respR_import_test_files/vernier_txt/vernier_txt_03.txt" # Mine - tst3.txt
path04 <- "~/Dropbox/respR_import_test_files/vernier_txt/vernier_txt_04.txt" # Mine - tst4.txt
path05 <- "~/Dropbox/respR_import_test_files/vernier_txt/vernier_txt_05.txt" # Mine - tst5.txt
path06 <- "~/Dropbox/respR_import_test_files/vernier_txt/vernier_txt_06.txt" # Mine - tst6.txt
path07 <- "~/Dropbox/respR_import_test_files/vernier_txt/vernier_txt_07.txt" # Mine - tst7.txt
path08 <- "~/Dropbox/respR_import_test_files/vernier_txt/vernier_txt_08.txt" # Mine - tst8.txt
path09 <- "~/Dropbox/respR_import_test_files/vernier_txt/vernier_txt_09.txt" # Mine - tst9.txt
path10 <- "~/Dropbox/respR_import_test_files/vernier_txt/vernier_txt_10.txt" # Mine - tst10.txt
path11 <- "~/Dropbox/respR_import_test_files/vernier_txt/vernier_txt_11.txt" # Mine - tst11.txt
path12 <- "~/Dropbox/respR_import_test_files/vernier_txt/vernier_txt_12.txt" # Mine - tst12.txt
path13 <- "~/Dropbox/respR_import_test_files/vernier_txt/vernier_txt_13.txt" # Mine - tst13.txt
path14 <- "~/Dropbox/respR_import_test_files/vernier_txt/vernier_txt_14.txt" # Mine - tst14.txt
path15 <- "~/Dropbox/respR_import_test_files/vernier_txt/vernier_txt_15.txt" # Mine - tst15.txt
path16 <- "~/Dropbox/respR_import_test_files/vernier_txt/vernier_txt_16.txt" # Mine - tst16.txt
path17 <- "~/Dropbox/respR_import_test_files/vernier_txt/vernier_txt_17.txt" # Mine - tst17.txt
path18 <- "~/Dropbox/respR_import_test_files/vernier_txt/vernier_txt_18.txt" # Mine - tst18.txt
path19 <- "~/Dropbox/respR_import_test_files/vernier_txt/vernier_txt_19.txt" # Mine - tst19.txt
path20 <- "~/Dropbox/respR_import_test_files/vernier_txt/vernier_txt_20.txt" # Mine - tst20.txt
path21 <- "~/Dropbox/respR_import_test_files/vernier_txt/vernier_txt_21.txt" # Mine - tst21.txt
path22 <- "~/Dropbox/respR_import_test_files/vernier_txt/vernier_txt_22.txt" # Mine - Random Nucella Untitled.txt

# imports
expect_error(import_file(path01), NA)
expect_error(import_file(path02), NA)
expect_error(import_file(path03), NA)
expect_error(import_file(path04), NA)
expect_error(import_file(path05), NA)
expect_error(import_file(path06), NA)
expect_error(import_file(path07), NA)
expect_error(import_file(path08), NA)
expect_error(import_file(path09), NA)
expect_error(import_file(path10), NA)
expect_error(import_file(path11), NA)
expect_error(import_file(path12), NA)
expect_error(import_file(path13), NA)
expect_error(import_file(path14), NA)
expect_error(import_file(path15), NA)
expect_error(import_file(path16), NA)
expect_error(import_file(path17), NA)
expect_error(import_file(path18), NA)
expect_error(import_file(path19), NA)
expect_error(import_file(path20), NA)
expect_error(import_file(path21), NA)
# this one has gmbl in header (name of file), so was misidentified until
# I reordered indentify inputs so vernier txt came first.
expect_error(import_file(path22), NA)


# exact values
imp <- import_file(path05)
expect_equal(round(as.numeric(imp[1,2]), 2), 8.32)
# columns
expect_equal(ncol(imp), 12)
expect_true(is.na(imp[[58,5]])) # NA correctly filled in for shorter columns

imp <- import_file(path09)
expect_equal(round(as.numeric(imp[1,2]), 2), 8.23)
expect_equal(ncol(imp), 5)

imp <- import_file(path11)
expect_equal(round(as.numeric(imp[1,2]), 2), 8.23)
expect_equal(ncol(imp), 5)
expect_match(colnames(imp)[1], "exp 1:") # custom Run name is imported correctly to column name

imp <- import_file(path12)
expect_equal(round(as.numeric(imp[1,2]), 2), 8.21)
expect_equal(ncol(imp), 10)
expect_match(colnames(imp)[1], "test") # custom Run name is imported correctly to column name
expect_match(colnames(imp)[6], "Run") # original Run name is in other column names

## col names of other data types are correct
imp <- import_file(path14)
expect_match(colnames(imp)[2], "Position")
expect_match(colnames(imp)[3], "Velocity")
expect_match(colnames(imp)[4], "Acceleration")
expect_match(colnames(imp)[8], "Potential")

## Time data are in mins
imp <- import_file(path19)
expect_match(colnames(imp)[1], "Run 1: Time \\(min\\)")

## Time data are in ms
imp <- import_file(path21)
expect_match(colnames(imp)[1], "Run 1: Time \\(ms\\)")

## Actual Experiment files
imp <- import_file(path22)
expect_equal(round(as.numeric(imp[1,2]), 2), 101.31)
expect_equal(ncol(imp), 3)



# Vernier qmbl ------------------------------------------------------------

path01 <- "~/Dropbox/respR_import_test_files/vernier_qmbl/vernier_qmbl_01.qmbl" # Mine - tst1.qmbl
path02 <- "~/Dropbox/respR_import_test_files/vernier_qmbl/vernier_qmbl_02.qmbl" # Mine - tst2.qmbl
path03 <- "~/Dropbox/respR_import_test_files/vernier_qmbl/vernier_qmbl_03.qmbl" # Mine - tst3.qmbl
path04 <- "~/Dropbox/respR_import_test_files/vernier_qmbl/vernier_qmbl_04.qmbl" # Mine - tst4.qmbl
path05 <- "~/Dropbox/respR_import_test_files/vernier_qmbl/vernier_qmbl_05.qmbl" # Mine - tst5.qmbl
path06 <- "~/Dropbox/respR_import_test_files/vernier_qmbl/vernier_qmbl_06.qmbl" # Mine - tst6.qmbl
path07 <- "~/Dropbox/respR_import_test_files/vernier_qmbl/vernier_qmbl_07.qmbl" # Mine - tst7.qmbl
path08 <- "~/Dropbox/respR_import_test_files/vernier_qmbl/vernier_qmbl_08.qmbl" # Mine - tst8.qmbl
path09 <- "~/Dropbox/respR_import_test_files/vernier_qmbl/vernier_qmbl_09.qmbl" # Mine - tst9.qmbl
path10 <- "~/Dropbox/respR_import_test_files/vernier_qmbl/vernier_qmbl_10.qmbl" # Mine - tst10.qmbl
path11 <- "~/Dropbox/respR_import_test_files/vernier_qmbl/vernier_qmbl_11.qmbl" # Mine - tst11.qmbl
path12 <- "~/Dropbox/respR_import_test_files/vernier_qmbl/vernier_qmbl_12.qmbl" # Mine - tst12.qmbl
path13 <- "~/Dropbox/respR_import_test_files/vernier_qmbl/vernier_qmbl_13.qmbl" # Mine - tst13.qmbl
path14 <- "~/Dropbox/respR_import_test_files/vernier_qmbl/vernier_qmbl_14.qmbl" # Mine - tst14.qmbl
path15 <- "~/Dropbox/respR_import_test_files/vernier_qmbl/vernier_qmbl_15.qmbl" # Mine - tst15.qmbl
path16 <- "~/Dropbox/respR_import_test_files/vernier_qmbl/vernier_qmbl_16.qmbl" # Mine - tst16.qmbl
path17 <- "~/Dropbox/respR_import_test_files/vernier_qmbl/vernier_qmbl_17.qmbl" # Mine - tst17.qmbl
path18 <- "~/Dropbox/respR_import_test_files/vernier_qmbl/vernier_qmbl_18.qmbl" # Mine - tst18.qmbl
path19 <- "~/Dropbox/respR_import_test_files/vernier_qmbl/vernier_qmbl_19.qmbl" # Mine - tst19.qmbl
path20 <- "~/Dropbox/respR_import_test_files/vernier_qmbl/vernier_qmbl_20.qmbl" # Mine - tst20.qmbl
path21 <- "~/Dropbox/respR_import_test_files/vernier_qmbl/vernier_qmbl_21.qmbl" # Mine - tst21.qmbl
# These two don't exist - were gmbl
#path22 <- "~/Dropbox/respR_import_test_files/vernier_qmbl/vernier_qmbl_22.qmbl" # Mine - Random Nucella Untitled.qmbl
#path23 <- "~/Dropbox/respR_import_test_files/vernier_qmbl/vernier_qmbl_23.qmbl" # Mine - small_snail.qmbl
path24 <- "~/Dropbox/respR_import_test_files/vernier_qmbl/vernier_qmbl_24.qmbl" # Mine - nucella.qmbl
path25 <- "~/Dropbox/respR_import_test_files/vernier_qmbl/vernier_qmbl_25.qmbl" # Mine - nucella2.qmbl



# imports
expect_error(import_file(path01), NA)
expect_error(import_file(path02), NA)
expect_error(import_file(path03), NA)
expect_error(import_file(path04), NA)
expect_error(import_file(path05), NA)
expect_error(import_file(path06), NA)
expect_error(import_file(path07), NA)
expect_error(import_file(path08), NA)
expect_error(import_file(path09), NA)
expect_error(import_file(path10), NA)
expect_error(import_file(path11), NA)
expect_error(import_file(path12), NA)
expect_error(import_file(path13), NA)
expect_error(import_file(path14), NA)
expect_error(import_file(path15), NA)
expect_error(import_file(path16), NA)
expect_error(import_file(path17), NA)
expect_error(import_file(path18), NA)
expect_error(import_file(path19), NA)
expect_error(import_file(path20), NA)
expect_error(import_file(path21), NA)
#expect_error(import_file(path22), NA) # doesn't exist - raw file was gmbl
#expect_error(import_file(path23), NA)# doesn't exist - raw file was gmbl
expect_error(import_file(path24), NA)
expect_error(import_file(path25), NA)


# exact values
imp <- import_file(path05)
expect_equal(round(as.numeric(imp[1,2]), 4), 8.3157)
expect_equal(round(as.numeric(imp[58,12]), 4), 99.0743)
# columns
expect_equal(ncol(imp), 12)
expect_true(is.na(imp[[58,5]])) # NA correctly filled in for shorter columns

## This one has columns in different order to csv
imp <- import_file(path09)
expect_equal(round(as.numeric(imp[1,2]), 4), 8.2330)
expect_equal(round(as.numeric(imp[98,4]), 4), 22.4746) ## NB diff column - 4 not 5
expect_equal(ncol(imp), 5)

imp <- import_file(path11)
expect_equal(round(as.numeric(imp[1,2]), 4), 8.2272)
expect_equal(round(as.numeric(imp[117,5]), 4), 23.6938)
expect_equal(ncol(imp), 5)
expect_match(colnames(imp)[1], "exp 1:") # custom Run name is imported correctly to column name

imp <- import_file(path12)
expect_equal(round(as.numeric(imp[1,2]), 4), 8.2109)
expect_equal(ncol(imp), 10)
expect_match(colnames(imp)[1], "test") # custom Run name is imported correctly to column name
expect_match(colnames(imp)[6], "Run") # original Run name is in other column names

## col names of other data types are correct
imp <- import_file(path14)
expect_match(colnames(imp)[2], "Position")
expect_match(colnames(imp)[3], "Velocity")
expect_match(colnames(imp)[4], "Acceleration")
expect_match(colnames(imp)[8], "Potential")

## Time data are in mins
imp <- import_file(path19)
expect_match(colnames(imp)[1], "\\(min\\)")

## Time data are in ms
imp <- import_file(path21)
expect_match(colnames(imp)[1], "\\(ms\\)")

## Actual Experiment files
## NOTE different column order than csv
imp <- import_file(path24)
expect_equal(round(as.numeric(imp[1,3]), 4), 100.0943)
expect_equal(round(as.numeric(imp[68047,2]), 4), 11.9977)
expect_equal(ncol(imp), 4)

## This one same column order as csv
imp <- import_file(path25)
expect_equal(round(as.numeric(imp[1,2]), 4), 99.4645)
expect_equal(round(as.numeric(imp[65631,3]), 4), 12.3602)
expect_equal(ncol(imp), 3)




# Vernier gmbl ------------------------------------------------------------

path22 <- "~/Dropbox/respR_import_test_files/z_unsupported_but_return_msg/vernier_gmbl/vernier_gmbl_22.gmbl" # Mine - Random Nucella Untitled.gmbl
path23 <- "~/Dropbox/respR_import_test_files/z_unsupported_but_return_msg/vernier_gmbl/vernier_gmbl_23.gmbl" # Mine - small_snail.gmbl

# Errors
expect_error(import_file(path22), "Vernier gmbl files not yet supported.")
expect_error(import_file(path23), "Vernier gmbl files not yet supported.")

## These gmbl files used to work with parse_vernier_raw but not any more. Some
## prolem with fread on first line.


# Vernier multiple --------------------------------------------------------

# Check the three methods import same data correctly
# Pick a few files at random
# And data locations at random

path_csv <- "~/Dropbox/respR_import_test_files/vernier_csv/vernier_csv_05.csv"
path_txt <- "~/Dropbox/respR_import_test_files/vernier_txt/vernier_txt_05.txt"
path_qmbl <- "~/Dropbox/respR_import_test_files/vernier_qmbl/vernier_qmbl_05.qmbl"

data_csv <- import_file(path_csv)
data_txt <- import_file(path_txt)
data_qmbl <- import_file(path_qmbl)

## need to as.numeric because of different column names
expect_equal(as.numeric(data_csv[4,9]),
             as.numeric(data_txt[4,9]))
expect_equal(as.numeric(data_csv[4,9]),
             as.numeric(data_qmbl[4,9]))


path_csv <- "~/Dropbox/respR_import_test_files/vernier_csv/vernier_csv_15.csv"
path_txt <- "~/Dropbox/respR_import_test_files/vernier_txt/vernier_txt_15.txt"
path_qmbl <- "~/Dropbox/respR_import_test_files/vernier_qmbl/vernier_qmbl_15.qmbl"

data_csv <- import_file(path_csv)
data_txt <- import_file(path_txt)
data_qmbl <- import_file(path_qmbl)

## need to round here as txt files are not same resolution
## Also here they are in different columns
expect_equal(round(as.numeric(data_csv[24,4]), 1),
             round(as.numeric(data_txt[24,2]), 1))
expect_equal(round(as.numeric(data_csv[24,4]), 1),
             round(as.numeric(data_qmbl[24,2]), 1))

path_csv <- "~/Dropbox/respR_import_test_files/vernier_csv/vernier_csv_20.csv"
path_txt <- "~/Dropbox/respR_import_test_files/vernier_txt/vernier_txt_20.txt"
path_qmbl <- "~/Dropbox/respR_import_test_files/vernier_qmbl/vernier_qmbl_20.qmbl"

data_csv <- import_file(path_csv)
data_txt <- import_file(path_txt)
data_qmbl <- import_file(path_qmbl)

## need to round here as txt files are not same resolution
## Also here they are in different columns
expect_equal(round(as.numeric(data_csv[24,2]), 1),
             round(as.numeric(data_txt[24,2]), 1))
expect_equal(round(as.numeric(data_csv[24,2]), 1),
             round(as.numeric(data_qmbl[24,2]), 1))





# Presens OXY10 -----------------------------------------------------------

path01 <- "~/Dropbox/respR_import_test_files/presens_oxy10/presens_oxy10_01.txt" # semicolon-sep 1.txt
path02 <- "~/Dropbox/respR_import_test_files/presens_oxy10/presens_oxy10_02.txt" # semicolon-sep 2.txt
## Originals of these next two had weird extra separated column. Looked like someone had pasted O2 data in there.
## I edited the files to remove them. Suspect they are not in original exported file.
path03 <- "~/Dropbox/respR_import_test_files/presens_oxy10/presens_oxy10_03.txt" # tab-sep 1.txt
path04 <- "~/Dropbox/respR_import_test_files/presens_oxy10/presens_oxy10_04.txt" # tab-sep 2.txt
path05 <- "~/Dropbox/respR_import_test_files/presens_oxy10/presens_oxy10_05.txt" # tab-sep 3.txt

# imports
expect_error(import_file(path01), NA)
expect_error(import_file(path02), NA)
expect_error(import_file(path03), NA)
expect_error(import_file(path04), NA)
expect_error(import_file(path05), NA)

# values
imp <- import_file(path01)
expect_equal(as.numeric(imp[1,4]), 101.78)
expect_equal(as.numeric(imp[272,4]), 91.52)
expect_equal(ncol(imp), 7)
# col names
expect_match(colnames(imp)[5], "Phase")
expect_match(colnames(imp)[6], "Amp")
expect_match(colnames(imp)[7], "Temp C")

imp <- import_file(path02)
expect_equal(as.numeric(imp[1,4]), 8.255)
expect_equal(as.numeric(imp[1179,4]), 7.197)
expect_equal(ncol(imp), 7)

imp <- import_file(path03)
expect_equal(as.numeric(imp[1,4]), 101.01)
expect_equal(as.numeric(imp[261,4]), 92.2)
expect_equal(ncol(imp), 7)

imp <- import_file(path04)
expect_equal(as.numeric(imp[1,4]), 100.75)
expect_equal(as.numeric(imp[245,4]), 89.37)
expect_equal(ncol(imp), 7)

imp <- import_file(path05)
expect_equal(as.numeric(imp[1,4]), 99.11)
expect_equal(as.numeric(imp[246,4]), 92.84)
expect_equal(ncol(imp), 7)






# Presens OXY4 ------------------------------------------------------------

path01 <- "~/Dropbox/respR_import_test_files/presens_oxy4/presens_oxy4_01.txt" # presens-ch1.txt - from FishResp
path02 <- "~/Dropbox/respR_import_test_files/presens_oxy4/presens_oxy4_02.txt" # presens-ch2.txt - from FishResp
path03 <- "~/Dropbox/respR_import_test_files/presens_oxy4/presens_oxy4_03.txt" # presens-ch3.txt - from FishResp
path04 <- "~/Dropbox/respR_import_test_files/presens_oxy4/presens_oxy4_04.txt" # presens-ch4.txt - from FishResp

# imports
expect_error(import_file(path01), NA)
expect_error(import_file(path02), NA)
expect_error(import_file(path03), NA)
expect_error(import_file(path04), NA)

# values
imp <- import_file(path01)
expect_equal(as.numeric(imp[1,4]), 96.97)
expect_equal(as.numeric(imp[1464,4]), 79.28)
expect_equal(ncol(imp), 8)
# col names
expect_match(colnames(imp)[5], "Phase")
expect_match(colnames(imp)[6], "Amp")
expect_match(colnames(imp)[7], "Temp C")

imp <- import_file(path02)
expect_equal(as.numeric(imp[1,4]), 99.7)
expect_equal(as.numeric(imp[1464,5]), 33.5)
expect_equal(ncol(imp), 8)

imp <- import_file(path03)
expect_equal(ncol(imp), 8)
expect_match(colnames(imp)[5], "Phase")
expect_match(colnames(imp)[6], "Amp")
expect_match(colnames(imp)[7], "Temp C")

imp <- import_file(path04)
expect_equal(ncol(imp), 8)
expect_match(colnames(imp)[5], "Phase")
expect_match(colnames(imp)[6], "Amp")
expect_match(colnames(imp)[7], "Temp C")








# Presens Oxyview ---------------------------------------------------------

path01 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_01.csv" # Jayslen Serrano
path02 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_02.csv" # Jayslen Serrano
path03 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_03.csv" # Jayslen Serrano
path04 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_04.csv" # Jayslen Serrano
path05 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_05.csv" # Jayslen Serrano
path06 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_06.csv" # Jayslen Serrano
path07 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_07.csv" # Jayslen Serrano
path08 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_08.csv" # Jayslen Serrano
path09 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_09.csv" # Jayslen Serrano
path10 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_10.csv" # Jayslen Serrano
path11 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_11.csv" # Jayslen Serrano
path12 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_12.csv" # Jayslen Serrano
path13 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_13.csv" # Jayslen Serrano
path14 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_14.csv" # Jayslen Serrano
path15 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_15.csv" # Jayslen Serrano
path16 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_16.csv" # Jayslen Serrano
path17 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_17.csv" # Jayslen Serrano
path18 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_18.csv" # Jayslen Serrano
path19 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_19.csv" # Jayslen Serrano
path20 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_20.csv" # Jayslen Serrano
path21 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_21.csv" # Jayslen Serrano
path22 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_22.csv" # Jayslen Serrano
path23 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_23.csv" # Jayslen Serrano
path24 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_24.csv" # Jayslen Serrano
path25 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_25.csv" # Jayslen Serrano
path26 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_26.csv" # Jayslen Serrano
path27 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_27.csv" # Jayslen Serrano
path28 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_28.csv" # Jayslen Serrano
path29 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_29.csv" # Jayslen Serrano
path30 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_30.csv" # Jayslen Serrano
path31 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_31.csv" # Jayslen Serrano
path32 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_32.csv" # Jayslen Serrano
path33 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_33.csv" # Jayslen Serrano
path34 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_34.csv" # Jayslen Serrano
path35 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_35.csv" # Jayslen Serrano
path36 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_36.csv" # Jayslen Serrano
path37 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_37.csv" # Jayslen Serrano
path38 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_38.csv" # Jayslen Serrano
path39 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_39.csv" # Jayslen Serrano
path40 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_40.csv" # Jayslen Serrano
## Next  all txt versions - Nik Wu
path41 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_41.txt" # Fish two_PCRIT_Niky_software fail.txt
path42 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_42.txt" # Fish four_BACKGROUND_Niky.txt
path43 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_43.txt" # Fish four_MAX_REST_Niky.txt
path44 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_44.txt" # Fish four_PCRIT_Niky.txt
path45 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_45.txt" # Fish one_MAX_REST_Daniel.txt
path46 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_46.txt" # Fish one_PCRIT_Daniel.txt
path47 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_47.txt" # Fish three_MAX_REST_Daniel.txt
path48 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_48.txt" # Fish three_PCRIT_Daniel.txt
path49 <- "~/Dropbox/respR_import_test_files/presens_oxyview/presens_oxyview_49.txt" # Fish two_MAX_REST_Niky.txt

# imports
expect_error(import_file(path01), NA)
expect_error(import_file(path02), NA)
expect_error(import_file(path03), NA)
expect_error(import_file(path04), NA)
expect_error(import_file(path05), NA)
expect_error(import_file(path06), NA)
expect_error(import_file(path07), NA)
expect_error(import_file(path08), NA)
expect_error(import_file(path09), NA)
expect_error(import_file(path10), NA)
expect_error(import_file(path11), NA)
expect_error(import_file(path12), NA)
expect_error(import_file(path13), NA)
expect_error(import_file(path14), NA)
expect_error(import_file(path15), NA)
expect_error(import_file(path16), NA)
expect_error(import_file(path17), NA)
expect_error(import_file(path18), NA)
expect_error(import_file(path19), NA)
expect_error(import_file(path20), NA)
expect_error(import_file(path21), NA)
expect_error(import_file(path22), NA)
expect_error(import_file(path23), NA)
expect_error(import_file(path24), NA)
expect_error(import_file(path25), NA)
expect_error(import_file(path26), NA)
expect_error(import_file(path27), NA)
expect_error(import_file(path28), NA)
expect_error(import_file(path29), NA)
expect_error(import_file(path30), NA)
expect_error(import_file(path31), NA)
expect_error(import_file(path32), NA)
expect_error(import_file(path33), NA)
expect_error(import_file(path34), NA)
expect_error(import_file(path35), NA)
expect_error(import_file(path36), NA)
expect_error(import_file(path37), NA)
expect_error(import_file(path38), NA)
expect_error(import_file(path39), NA)
expect_error(import_file(path40), NA)
# txt versions
expect_error(import_file(path41), NA)
expect_error(import_file(path42), NA)
expect_error(import_file(path43), NA)
expect_error(import_file(path44), NA)
expect_error(import_file(path45), NA)
expect_error(import_file(path46), NA)
expect_error(import_file(path47), NA)
expect_error(import_file(path48), NA)
expect_error(import_file(path49), NA)

## Just check a few

# values
imp <- import_file(path01)
expect_equal(as.numeric(imp[1,4]), 296.930)
expect_equal(as.numeric(imp[3165,7]), 27.8)
expect_equal(ncol(imp), 8)
# col names
expect_match(colnames(imp)[5], "phase")
expect_match(colnames(imp)[6], "amp")
expect_match(colnames(imp)[7], "tempC")

# values
imp <- import_file(path34)
expect_equal(as.numeric(imp[1,4]), 256.501)
expect_equal(as.numeric(imp[3165,7]), 27.1)
expect_equal(ncol(imp), 8)
# col names
expect_match(colnames(imp)[5], "phase")
expect_match(colnames(imp)[6], "amp")
expect_match(colnames(imp)[7], "tempC")

## TXT VERSIONS
# values
imp <- import_file(path48)
expect_equal(as.numeric(imp[2,3]), 0.016)
expect_equal(as.numeric(imp[57923,7]), 24.20)
expect_equal(ncol(imp), 8)
# col names
expect_match(colnames(imp)[5], "phase")
expect_match(colnames(imp)[6], "amp")
expect_match(colnames(imp)[7], "tempC")


# Presens Generic ---------------------------------------------------------

## This is a multiplate file. Important with these to not remove all columns.
## Should be 24 O2 recordings even if some are empty
path01 <- "~/Dropbox/respR_import_test_files/presens_generic/presens_generic_01.txt" # presens OD24.txt
## next, the above file just copied and extension changed to csv.
path02 <- "~/Dropbox/respR_import_test_files/presens_generic/presens_generic_01.csv" # presens OD24.csv

# imports
expect_error(import_file(path01), NA)
expect_error(import_file(path02), NA)

# values
imp <- import_file(path01)
expect_equal(as.numeric(imp[1,4]), 100.01)
expect_equal(as.numeric(imp[3705,7]), 13.43)
expect_equal(ncol(imp), 31)
# col names
expect_match(colnames(imp)[5], "A3")
expect_match(colnames(imp)[16], "C2")
expect_match(colnames(imp)[30], "T_internal_C")

# values - csv file version
imp <- import_file(path02)
expect_equal(as.numeric(imp[1,4]), 100.01)
expect_equal(as.numeric(imp[3705,7]), 13.43)
expect_equal(ncol(imp), 31)
# col names
expect_match(colnames(imp)[5], "A3")
expect_match(colnames(imp)[16], "C2")
expect_match(colnames(imp)[30], "T_internal_C")




# Minidot -----------------------------------------------------------------

path01 <- "~/Dropbox/respR_import_test_files/minidot/minidot01.txt" # miniDOT.TXT
## next, the above file just copied and extension changed to csv.
path02 <- "~/Dropbox/respR_import_test_files/minidot/minidot01.csv" # miniDOT.TXT

# imports
expect_error(import_file(path01), NA)
expect_error(import_file(path02), NA)

# values
imp <- import_file(path01)
expect_equal(as.numeric(imp[1,1]), 1523906520)
expect_equal(as.numeric(imp[19369,8]), 0.973000)
expect_equal(ncol(imp), 8)
# col names
expect_match(colnames(imp)[5], "Temperature \\(deg C\\)")
expect_match(colnames(imp)[6], "Dissolved Oxygen \\(mg/l\\)")
expect_match(colnames(imp)[7], "Dissolved Oxygen Saturation \\(%\\)")

# values
imp <- import_file(path02)
expect_equal(as.numeric(imp[1,1]), 1523906520)
expect_equal(as.numeric(imp[19369,8]), 0.973000)
expect_equal(ncol(imp), 8)
# col names
expect_match(colnames(imp)[5], "Temperature \\(deg C\\)")
expect_match(colnames(imp)[6], "Dissolved Oxygen \\(mg/l\\)")
expect_match(colnames(imp)[7], "Dissolved Oxygen Saturation \\(%\\)")

## import both versions the same
expect_identical(import_file(path01),
                 import_file(path02))




# Loligo/Presens 24-well Multiplate Excel ---------------------------------

path01 <- "~/Dropbox/respR_import_test_files/multiplate_excel/multiplate_excel_01.xls" # 11DEC17_Etive_preacclim_Oxygen.xls
## only one of these is oxygen but others work too (others amplitude and pahse)
## NOTE - xlsx not xls
path02 <- "~/Dropbox/respR_import_test_files/multiplate_excel/multiplate_excel_02.xlsx" # pocillopora_368_connelly_032619_b_Ampl.xlsx - Mike Connelly
path03 <- "~/Dropbox/respR_import_test_files/multiplate_excel/multiplate_excel_03.xlsx" # pocillopora_368_connelly_032619_b_Oxygen.xlsx - Mike Connelly
path04 <- "~/Dropbox/respR_import_test_files/multiplate_excel/multiplate_excel_04.xlsx" # pocillopora_368_connelly_032619_b_Phase.xlsx - Mike Connelly

# imports
expect_error(import_file(path01), NA)
expect_error(import_file(path02), NA)
expect_error(import_file(path03), NA)
expect_error(import_file(path04), NA)

# values
imp <- import_file(path01)
expect_equal(as.numeric(imp[3,4]), 104.85)
expect_equal(as.numeric(imp[349,30]), 10.01)
expect_equal(ncol(imp), 31)
# col names
expect_match(colnames(imp)[5], "A3")
expect_match(colnames(imp)[28], "p \\[mbar\\]")
expect_match(colnames(imp)[30], "T_internal \\[°C\\]")

# values
imp <- import_file(path02)
expect_equal(as.numeric(imp[3,4]), 66371)
expect_equal(as.numeric(imp[116,27]), 24.35)
expect_equal(ncol(imp), 28)
# col names
expect_match(colnames(imp)[5], "A3")
expect_match(colnames(imp)[27], "Temperature")

# values
imp <- import_file(path03)
expect_equal(as.numeric(imp[3,4]), 86.16)
expect_equal(as.numeric(imp[116,27]), 24.35)
expect_equal(ncol(imp), 28)
# col names
expect_match(colnames(imp)[5], "A3")
expect_match(colnames(imp)[27], "Temperature")

# values
imp <- import_file(path04)
expect_equal(as.numeric(imp[3,4]), 47.86)
expect_equal(as.numeric(imp[116,27]), 24.35)
expect_equal(ncol(imp), 28)
# col names
expect_match(colnames(imp)[5], "A3")
expect_match(colnames(imp)[27], "Temperature")




# Loligo AutoResp Witrox --------------------------------------------------

## single channel files
path01 <- "~/Dropbox/respR_import_test_files/loligo_autoresp_witrox/loligo_autoresp_witrox_01.txt" # sardine test 5 small tunnel.txt
path02 <- "~/Dropbox/respR_import_test_files/loligo_autoresp_witrox/loligo_autoresp_witrox_02.txt" # small tunnel 08092017.4.txt
path03 <- "~/Dropbox/respR_import_test_files/loligo_autoresp_witrox/loligo_autoresp_witrox_03.txt" # small tunnel 08102017.txt
path04 <- "~/Dropbox/respR_import_test_files/loligo_autoresp_witrox/loligo_autoresp_witrox_04.txt" # small tunnel 08112017.2.txt
path05 <- "~/Dropbox/respR_import_test_files/loligo_autoresp_witrox/loligo_autoresp_witrox_05.txt" # small tunnel 08112017.txt
path06 <- "~/Dropbox/respR_import_test_files/loligo_autoresp_witrox/loligo_autoresp_witrox_06.txt" # witrox-sardine3.txt
path07 <- "~/Dropbox/respR_import_test_files/loligo_autoresp_witrox/loligo_autoresp_witrox_07.txt" # witrox-sardine4.txt
path08 <- "~/Dropbox/respR_import_test_files/loligo_autoresp_witrox/loligo_autoresp_witrox_08.txt" # witrox-smalltunnel1.txt
## multichannel files
path09 <- "~/Dropbox/respR_import_test_files/loligo_autoresp_witrox/loligo_autoresp_witrox_09.txt" # SMR_raw.txt from FishResp
path10 <- "~/Dropbox/respR_import_test_files/loligo_autoresp_witrox/loligo_autoresp_witrox_10.txt" # AMR_raw.txt from FishResp
path11 <- "~/Dropbox/respR_import_test_files/loligo_autoresp_witrox/loligo_autoresp_witrox_11.txt" # post_raw.txt from FishResp
path12 <- "~/Dropbox/respR_import_test_files/loligo_autoresp_witrox/loligo_autoresp_witrox_12.txt" # pre_raw.txt from FishResp
path13 <- "~/Dropbox/respR_import_test_files/loligo_autoresp_witrox/loligo_autoresp_witrox_13.txt" # Pcrit42052_raw.txt - Benjamin Negrete
path14 <- "~/Dropbox/respR_import_test_files/loligo_autoresp_witrox/loligo_autoresp_witrox_14.txt" # SMR420523_raw.txt - Benjamin Negrete
path15 <- "~/Dropbox/respR_import_test_files/loligo_autoresp_witrox/loligo_autoresp_witrox_15.txt" # SMRPcrit410317_raw.txt - Benjamin Negrete
path16 <- "~/Dropbox/respR_import_test_files/loligo_autoresp_witrox/loligo_autoresp_witrox_16.txt" # L12_22C_Fish1_Jun6_raw.txt - lbpotts_github
path17 <- "~/Dropbox/respR_import_test_files/loligo_autoresp_witrox/loligo_autoresp_witrox_17.txt" # 20.03.2018_R_T17_LT_raw.txt - Louise Archer
path18 <- "~/Dropbox/respR_import_test_files/loligo_autoresp_witrox/loligo_autoresp_witrox_18.txt" # anch01_raw_test.txt - mine
path19 <- "~/Dropbox/respR_import_test_files/loligo_autoresp_witrox/loligo_autoresp_witrox_19.txt" # anch01_raw.txt - mine
path20 <- "~/Dropbox/respR_import_test_files/loligo_autoresp_witrox/loligo_autoresp_witrox_20.txt" # Blue_V13AP_swim challenge_8-17-2016_raw.txt - mine
path21 <- "~/Dropbox/respR_import_test_files/loligo_autoresp_witrox/loligo_autoresp_witrox_21.txt" # Metabolic Scope 8-23-2016_raw.txt - mine
path22 <- "~/Dropbox/respR_import_test_files/loligo_autoresp_witrox/loligo_autoresp_witrox_22.txt" # squid4_022017_raw.txt - mine
path23 <- "~/Dropbox/respR_import_test_files/loligo_autoresp_witrox/loligo_autoresp_witrox_23.txt" # post_raw.txt - from FishResp (guppy)
path24 <- "~/Dropbox/respR_import_test_files/loligo_autoresp_witrox/loligo_autoresp_witrox_24.txt" # pre_raw.txt - from FishResp (guppy)
path25 <- "~/Dropbox/respR_import_test_files/loligo_autoresp_witrox/loligo_autoresp_witrox_25.txt" # SMR_raw.txt - from FishResp (guppy)

# imports
expect_error(import_file(path01), NA)
expect_error(import_file(path02), NA)
expect_error(import_file(path03), NA)
expect_error(import_file(path04), NA)
expect_error(import_file(path05), NA)
expect_error(import_file(path06), NA)
expect_error(import_file(path07), NA)
expect_error(import_file(path08), NA)
expect_error(import_file(path09), NA)
expect_error(import_file(path10), NA)
expect_error(import_file(path11), NA)
expect_error(import_file(path12), NA)
expect_error(import_file(path13), NA)
expect_error(import_file(path14), NA)
expect_error(import_file(path15), NA)
expect_error(import_file(path16), NA)
expect_error(import_file(path17), NA)
expect_error(import_file(path18), NA)
expect_error(import_file(path19), NA)
expect_error(import_file(path20), NA)
expect_error(import_file(path21), NA)
expect_error(import_file(path22), NA)
expect_error(import_file(path23), NA)
expect_error(import_file(path24), NA)
expect_error(import_file(path25), NA)

# values
imp <- import_file(path01)
expect_equal(as.numeric(imp[3,4]), 29.95)
expect_equal(as.numeric(imp[1228,6]), 74.5)
expect_equal(ncol(imp), 6)
# col names
expect_match(colnames(imp)[2], "Time_stamp_code")
expect_match(colnames(imp)[3], "Barometric_pressure")

imp <- import_file(path02)
expect_equal(ncol(imp), 6)
expect_match(colnames(imp)[2], "Time_stamp_code")
expect_match(colnames(imp)[3], "Barometric_pressure")

imp <- import_file(path03)
expect_equal(ncol(imp), 6)
expect_match(colnames(imp)[2], "Time_stamp_code")
expect_match(colnames(imp)[3], "Barometric_pressure")

imp <- import_file(path04)
expect_equal(ncol(imp), 6)
expect_match(colnames(imp)[2], "Time_stamp_code")
expect_match(colnames(imp)[3], "Barometric_pressure")

imp <- import_file(path05)
expect_equal(ncol(imp), 6)
expect_match(colnames(imp)[2], "Time_stamp_code")
expect_match(colnames(imp)[3], "Barometric_pressure")

imp <- import_file(path06)
expect_equal(ncol(imp), 6)
expect_match(colnames(imp)[2], "Time_stamp_code")
expect_match(colnames(imp)[3], "Barometric_pressure")

imp <- import_file(path07)
expect_equal(ncol(imp), 6)
expect_match(colnames(imp)[2], "Time_stamp_code")
expect_match(colnames(imp)[3], "Barometric_pressure")

imp <- import_file(path08)
expect_equal(as.numeric(imp[3,4]), 29.24)
expect_equal(as.numeric(imp[685,6]), 93)
expect_equal(ncol(imp), 6)
expect_match(colnames(imp)[2], "Time_stamp_code")
expect_match(colnames(imp)[3], "Barometric_pressure")

## multichannel files
imp <- import_file(path09)
expect_equal(as.numeric(imp[3,5]), 30.2843)
expect_equal(as.numeric(imp[3235,5]), 31.3)
expect_equal(ncol(imp), 47)
expect_match(colnames(imp)[5], "CH1_O2_input_phase")
expect_match(colnames(imp)[47], "User_event")

imp <- import_file(path10)
expect_equal(ncol(imp), 47)
expect_match(colnames(imp)[5], "CH1_O2_input_phase")
expect_match(colnames(imp)[47], "User_event")

imp <- import_file(path11)
expect_equal(ncol(imp), 47)
expect_match(colnames(imp)[5], "CH1_O2_input_phase")
expect_match(colnames(imp)[47], "User_event")

imp <- import_file(path12)
expect_equal(ncol(imp), 47)
expect_match(colnames(imp)[5], "CH1_O2_input_phase")
expect_match(colnames(imp)[47], "User_event")

imp <- import_file(path13)
expect_equal(ncol(imp), 47)
expect_match(colnames(imp)[5], "CH1_O2_input_phase")
expect_match(colnames(imp)[47], "User_event")

imp <- import_file(path14)
expect_equal(ncol(imp), 47)
expect_match(colnames(imp)[5], "CH1_O2_input_phase")
expect_match(colnames(imp)[47], "User_event")

imp <- import_file(path15)
expect_equal(ncol(imp), 47)
expect_match(colnames(imp)[5], "CH1_O2_input_phase")
expect_match(colnames(imp)[47], "User_event")

imp <- import_file(path16)
expect_equal(ncol(imp), 47)
expect_match(colnames(imp)[5], "CH1_O2_input_phase")
expect_match(colnames(imp)[47], "User_event")

imp <- import_file(path17)
expect_equal(ncol(imp), 47)
expect_match(colnames(imp)[5], "CH1_O2_input_phase")
expect_match(colnames(imp)[47], "User_event")

imp <- import_file(path18)
expect_equal(ncol(imp), 47)
expect_match(colnames(imp)[5], "CH1_O2_input_phase")
expect_match(colnames(imp)[47], "User_event")

imp <- import_file(path19)
expect_equal(ncol(imp), 47)
expect_match(colnames(imp)[5], "CH1_O2_input_phase")
expect_match(colnames(imp)[47], "User_event")

imp <- import_file(path20)
expect_equal(ncol(imp), 47)
expect_match(colnames(imp)[5], "CH1_O2_input_phase")
expect_match(colnames(imp)[47], "User_event")

imp <- import_file(path21)
expect_equal(ncol(imp), 47)
expect_match(colnames(imp)[5], "CH1_O2_input_phase")
expect_match(colnames(imp)[47], "User_event")

imp <- import_file(path22)
expect_equal(as.numeric(imp[3,5]), 29.61)
expect_equal(as.numeric(imp[3235,5]), 33.4986)
expect_equal(ncol(imp), 47)
expect_match(colnames(imp)[5], "CH1_O2_input_phase")

imp <- import_file(path23)
expect_equal(ncol(imp), 47)
expect_match(colnames(imp)[5], "CH1_O2_input_phase")
expect_match(colnames(imp)[47], "User_event")

imp <- import_file(path24)
expect_equal(ncol(imp), 47)
expect_match(colnames(imp)[5], "CH1_O2_input_phase")
expect_match(colnames(imp)[47], "User_event")

imp <- import_file(path25)
expect_equal(ncol(imp), 47)
expect_match(colnames(imp)[5], "CH1_O2_input_phase")
expect_match(colnames(imp)[47], "User_event")
expect_match(colnames(imp)[47], "User_event")


# Loligo Metadata Files ---------------------------------------------------

path01 <- "~/Dropbox/respR_import_test_files/z_unsupported_but_return_msg/loligo_metadata_files/20.03.2018_R_T17_LT.txt"
path02 <- "~/Dropbox/respR_import_test_files/z_unsupported_but_return_msg/loligo_metadata_files/anch01.txt"
path03 <- "~/Dropbox/respR_import_test_files/z_unsupported_but_return_msg/loligo_metadata_files/Blue_V13AP_swim challenge_8-17-2016.txt"
path04 <- "~/Dropbox/respR_import_test_files/z_unsupported_but_return_msg/loligo_metadata_files/Metabolic Scope 8-23-2016.txt"
path05 <- "~/Dropbox/respR_import_test_files/z_unsupported_but_return_msg/loligo_metadata_files/Pcrit42052.txt"
path06 <- "~/Dropbox/respR_import_test_files/z_unsupported_but_return_msg/loligo_metadata_files/SMR420523.txt"
path07 <- "~/Dropbox/respR_import_test_files/z_unsupported_but_return_msg/loligo_metadata_files/SMRPcrit410317.txt"
path08 <- "~/Dropbox/respR_import_test_files/z_unsupported_but_return_msg/loligo_metadata_files/squid4_022017.txt"

expect_error(import_file(path01), "Currently these files are unsupported in respR.")
expect_error(import_file(path02), "Currently these files are unsupported in respR.")
expect_error(import_file(path03), "Currently these files are unsupported in respR.")
expect_error(import_file(path04), "Currently these files are unsupported in respR.")
expect_error(import_file(path05), "Currently these files are unsupported in respR.")
expect_error(import_file(path06), "Currently these files are unsupported in respR.")
expect_error(import_file(path07), "Currently these files are unsupported in respR.")
expect_error(import_file(path08), "Currently these files are unsupported in respR.")

# QBox Aqua ---------------------------------------------------------------

## Is identified as Vernier csv file and imports ok.
## System must be some sort of version of Vernier/use their tech
path01 <- "~/Dropbox/respR_import_test_files/qbox_aqua/qbox_aqua_01.csv" # from FishResp

# imports
expect_error(import_file(path01), NA)

# values
imp <- import_file(path01)
expect_equal(as.numeric(imp[1,2]), 7.501285244)
expect_equal(as.numeric(imp[3312,19]), 7.084495385)
expect_equal(ncol(imp), 19)
# col names
expect_match(colnames(imp)[5], "Latest: Pressure \\(atm\\)")
expect_match(colnames(imp)[6], "Latest: Sin")
expect_match(colnames(imp)[19], "Latest: DOcor \\(mg/L\\)")


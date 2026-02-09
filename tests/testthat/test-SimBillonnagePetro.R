
test_that("Test de la fonction SimBillonnagePetro() avec DHP2015", {
  fic_intrant <- data.frame(Placette=1, Espece=c("ERS", "BOJ", "ERR", "BOP", "HEG", "CHR"), DHPcm=c(24,25,26,27,28,29))
  fic_intrant <- fic_intrant %>% mutate(TigeID = seq_len(nrow(fic_intrant)))

  expect_no_error(SimBillonnagePetro(Data=fic_intrant, type="DHP2015"))

})

test_that("Test de la fonction SimBillonnagePetro() avec MSCR", {
  fic_intrant <- data.frame(Placette=1, Espece=c("ERS", "BOJ", "ERR", "BOP", "HEG", "CHR"), DHPcm=c(24,25,26,27,28,29), MSCR=c("M","S","C","R","C","R"))
  fic_intrant <- fic_intrant %>% mutate(TigeID = seq_len(nrow(fic_intrant)))

  expect_no_error(SimBillonnagePetro(Data=fic_intrant, type="MSCR"))

})

test_that("Test de la fonction SimBillonnagePetro() avec ABCD2015", {
  fic_intrant <- data.frame(Placette=1, Espece=c("ERS", "BOJ", "ERR", "BOP", "HEG", "CHR"), DHPcm=c(24,25,26,27,28,29), ABCD=c("A","B","C","D","A","B"))
  fic_intrant <- fic_intrant %>% mutate(TigeID = seq_len(nrow(fic_intrant)))

  expect_no_error(SimBillonnagePetro(Data=fic_intrant, type="ABCD2015"))

})

test_that("Test de la fonction SimBillonnagePetro() avec 1234", {
  fic_intrant <- data.frame(Placette=1, Espece=c("ERS", "BOJ", "ERR", "BOP", "HEG", "CHR"), DHPcm=c(24,25,26,27,28,29),
                            vigu0=c("ViG","ViG","ViG","NONVIG","NONVIG","NONVIG"),
                            prod0=c("sciage","pate","sciage","pate","sciage","pate"))
  fic_intrant <- fic_intrant %>% mutate(TigeID = seq_len(nrow(fic_intrant)))

  expect_no_error(SimBillonnagePetro(Data=fic_intrant, type="1234"))

})


test_that("Test de la fonction SimBillonnagePetro() avec DHP et eco", {
  fic_intrant <- data.frame(Placette=1, Espece=c("ERS", "BOJ"), DHPcm=c(24,29), eco=c("2O","3O"))
  fic_intrant <- fic_intrant %>% mutate(TigeID = seq_len(nrow(fic_intrant)))

  expect_no_error(SimBillonnagePetro(Data=fic_intrant, type="DHP"))

})

test_that("Test de la fonction SimBillonnagePetro() avec ABCD et eco", {
  fic_intrant <- data.frame(Placette=1, Espece=c("ERS", "BOJ"), DHPcm=c(24,29), eco=c("2O","3O"), ABCD=c("A","B"))
  fic_intrant <- fic_intrant %>% mutate(TigeID = seq_len(nrow(fic_intrant)))

  expect_no_error(SimBillonnagePetro(Data=fic_intrant, type="ABCD"))

})

test_that("Test de la fonction SimBillonnagePetro() avec ABCD sans eco", {
  fic_intrant <- data.frame(Placette=1, Espece=c("ERS", "BOJ","ERS"), DHPcm=c(24,29,29), reg_eco=c("2a","3b","5a"), ABCD=c("A","B","B"))
  fic_intrant <- fic_intrant %>% mutate(TigeID = seq_len(nrow(fic_intrant)))

  expect_no_error(SimBillonnagePetro(Data=fic_intrant, type="ABCD"))

})


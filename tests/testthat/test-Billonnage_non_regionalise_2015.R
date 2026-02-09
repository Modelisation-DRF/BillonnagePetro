test_that("Test de la fonction ABCD_DHP2015() avec MSCR", {

  Billonage <- readRDS(test_path("fixtures", "Billonage_MSCR_test.rds")) # une placette de 81 lignes tous des ERS, 7 annees, tous vivants, dhp=23+
  Billonage <- Billonage %>% mutate(TigeID = seq_len(nrow(Billonage)))

  fic_intrant <- Billonage %>% dplyr::select(Placette, ArbreID, Espece, DHPcm, MSCR, TigeID)

  result_attendu <- Billonage %>% filter(!is.na(MSCR)) %>% mutate(DER=NA, type="MSCR_P") %>% dplyr::select(TigeID, type, F2, F3, F4, P, F1, DER)

  result <- ABCD_DHP2015(data=fic_intrant, type="MSCR")

  #expect_equal(result, result_attendu)
  expect_true(isTRUE(all.equal(result, result_attendu, check.attributes = FALSE)))

})


test_that("Test de la fonction ABCD_DHP2015() avec DHP", {
  fic_intrant <- data.frame(Placette=1, Espece=c("ERS", "BOJ", "ERR", "BOP", "HEG", "CHR"), DHPcm=c(24,25,26,27,28,29))
  fic_intrant <- fic_intrant %>% mutate(TigeID = seq_len(nrow(fic_intrant)))

  expect_no_error(ABCD_DHP2015(data=fic_intrant, type="DHP2015"))

})

test_that("Test de la fonction ABCD_DHP2015() avec MSCR", {
  fic_intrant <- data.frame(Placette=1, Espece=c("ERS", "BOJ", "ERR", "BOP", "HEG", "CHR"), DHPcm=c(24,25,26,27,28,29), MSCR=c("M","S","C","R","C","R"))
  fic_intrant <- fic_intrant %>% mutate(TigeID = seq_len(nrow(fic_intrant)))

  expect_no_error(ABCD_DHP2015(data=fic_intrant, type="MSCR"))

})

test_that("Test de la fonction ABCD_DHP2015() avec ABCD", {
  fic_intrant <- data.frame(Placette=1, Espece=c("ERS", "BOJ", "ERR", "BOP", "HEG", "CHR"), DHPcm=c(24,25,26,27,28,29), ABCD=c("A","B","C","D","A","B"))
  fic_intrant <- fic_intrant %>% mutate(TigeID = seq_len(nrow(fic_intrant)))

  expect_no_error(ABCD_DHP2015(data=fic_intrant, type="ABCD2015"))

})

test_that("Test de la fonction ABCD_DHP2015() avec 1234", {
  fic_intrant <- data.frame(Placette=1, Espece=c("ERS", "BOJ", "ERR", "BOP", "HEG", "CHR"), DHPcm=c(24,25,26,27,28,29),
                            vigu0=c("ViG","ViG","ViG","NONVIG","NONVIG","NONVIG"),
                            prod0=c("sciage","pate","sciage","pate","sciage","pate"))
  fic_intrant <- fic_intrant %>% mutate(TigeID = seq_len(nrow(fic_intrant)))

  expect_no_error(ABCD_DHP2015(data=fic_intrant, type="1234"))

})

# test_that("Test de la fonction ABCD_DHP215() avec DHP et essences et dhp non supposés", {
#   fic_intrant <- data.frame(Placette=1, Espece=c("ERS", "SAB","BOJ"), DHPcm=c(24,25,12))
#   fic_intrant <- fic_intrant %>% mutate(bilonID = seq_len(nrow(fic_intrant)))
#
#   ABCD_DHP215(data=fic_intrant, type="DHP2015")
#   # la ligne avec le SAB n'est plus là: ok, il y a un filtre sur les essences dans le code
#   # mais la lignes avec le BOJ de 12 cm est là avec des volumes, car il n'y a pas de filtre le dhp
#
# })


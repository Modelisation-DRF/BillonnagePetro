test_that("Test de la fonction ABCD_DHP_region() avec DHP", {
  fic_intrant <- data.frame(Placette=1, Espece=c("ERS", "BOJ"), DHPcm=c(24,29), eco=c("2O","3O"))
  fic_intrant <- fic_intrant %>% mutate(TigeID = seq_len(nrow(fic_intrant)))

  expect_no_error(ABCD_DHP_region(data=fic_intrant, type="DHP"))

})

test_that("Test de la fonction ABCD_DHP_regio() avec ABCD", {
  fic_intrant <- data.frame(Placette=1, Espece=c("ERS", "BOJ"), DHPcm=c(24,29), eco=c("2O","3O"), ABCD=c("A","B"))
  fic_intrant <- fic_intrant %>% mutate(TigeID = seq_len(nrow(fic_intrant)))

  expect_no_error(ABCD_DHP_region(data=fic_intrant, type="ABCD"))

})

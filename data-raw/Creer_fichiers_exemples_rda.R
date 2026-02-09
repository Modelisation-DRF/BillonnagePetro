# Créer les fichiers d'exemple


liste_arbres_ex <- data.frame(Placette=1, reg_eco="2a",
                              Espece=c("ERS", "BOJ", "ERR", "BOP", "HEG", "CHR"),
                              DHPcm=c(24,25,26,27,28,29),
                              vigu0=c("ViG","ViG","ViG","NONVIG","NONVIG","NONVIG"),
                              prod0=c("sciage","pate","sciage","pate","sciage","pate"),
                              MSCR=c("R","R","R","M","M","M"),
                              ABCD=c("C","C","C","D","D","D")
                              )
liste_arbres_ex <- liste_arbres_ex %>% mutate(TigeID = seq_len(nrow(liste_arbres_ex)))

usethis::use_data(liste_arbres_ex,
                  internal=FALSE, overwrite = TRUE)


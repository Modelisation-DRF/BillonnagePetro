#'Fonction qui modifie la région écologique en vue de son utilisation par
#'le module de billonnage Petro régionalisé
#'
#'
#' @param data Un dataframe qui contient en ligne les arbres dont on veut prévoir
#'             les rendements en produit à l'aide du module de billonnage Petro
#'             régionalisé, qui contient les variables reg_eco et Espece
#'
#' @return Retourne un  dataframe qui contient en ligne les arbres dont on veut
#'          prévoir les rendements en produits à l'aide de Petro régionalisé avec le
#'          champ "eco" qui corespond à un groupement de régions écologiques
#'
#' @keywords internal
#'
ConvertisseurEco<- function (data){

  select=dplyr::select

  data$eco <- ifelse(data$Espece == "BOJ",
                     ifelse(data$reg_eco %in% c("1a", "2a", "2b", "2c", "3a", "3b", "SV"), "3O",
                            ifelse(data$reg_eco %in% c("3c", "3d"), "3E",
                                   ifelse(data$reg_eco %in% c("4a", "4b"), "4O_b",
                                          ifelse(data$reg_eco %in% c("4c"), "4O_c",
                                                 ifelse(data$reg_eco %in% c("4d", "4e", "4f", "4g","4h", "DU"), "4E", # mettre explicitement la liste qui va dans 4E
                                                        NA)
                                          )
                                   )
                            )
                     ),
                     ifelse(data$Espece == "ERS", # mettre explicitement ERS
                            ifelse(data$reg_eco %in% c("1a", "2a", "2b", "2c"), "2O",
                                   ifelse(data$reg_eco %in% c("3a"), "3O_a",
                                          ifelse(data$reg_eco %in% c("3b", "3c", "SV"), "3O_b",
                                                 ifelse(data$reg_eco %in% c("4a", "4b"), "4O_b",
                                                        ifelse(data$reg_eco %in% c("4c"), "4O_c",
                                                               ifelse(data$reg_eco %in% c("3d", "4d", "4e", "4f", "4g","4h", "DU"), "4E", # mettre explicitement la liste qui va dans 4E
                                                                      NA)
                                                        )
                                                 )
                                          )
                                   )
                            ),
                     NA) # mettre explicitement un NA si pas ERS ou BOJ
  )

 return(data)
}








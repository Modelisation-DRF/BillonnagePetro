#' Fonction principale de répartition par produits des arbres de 6 essences d'arbres feuillus
#'
#' @description Fonction principale qui prévoit la répartition par produits des arbres de 6 essences d'arbres feuillus avec les équations
#' Petro de Havreljuk et al. (2015) ou avec les équations Petro régionalisées de Havreljuk et al. (2025).
#'
#' @param Data Un dataframe qui contient en ligne les arbres dont on veut prévoir les rendements en produit Petro.
#'             Le dataframe doit contenir une colonne TigeID qui numérote individuellement chacune des lignes et
#'             les colonnes Espece et DHPcm.
#' \itemize{
#'    \item Pour type=DHP et ABCD, il faut aussi la colonne eco (regroupement de régions écologiques de Havreljuk et al. (2025) ou reg_eco.
#'          Si reg_eco est fournie, elle sera convertie en regroupements.
#'    \item Pour type=ABCD ou ABCD2015, il faut aussi une colonne ABCD.
#'    \item Pour type=MSCR, il faut aussi une colonne MSCR.
#'    \item Pour type=1234, il faut aussi les colonnes prod0 et vigu0.
#'    }
#'
#'
#' @param type Le type d'équations à utiliser
#' \itemize{
#'    \item "DHP" pour utiliser les équations régionalisées de 2025 basées seulement sur le DHP pour ERS et BOJ, pour les autres essences DHP2015 sera utilisé
#'    \item "ABCD" pour utiliser les équations régionalisées de 2025 basées sur ABCD pour ERS et BOJ, pour les autres essences ABCD2015 sera utilisé
#'    \item "1234" pour utiliser les équations de 2015 basées sur 1234
#'    \item "MSCR" pour utiliser les équations de 2015 basées sur MSCR
#'    \item "DHP2015" pour utiliser les équations de 2015 basées seulement sur le DHP
#'    \item "ABCD2015" pour utiliser les équations de 2015 basées sur ABCD
#' }
#'
#' @return Retourne un dataframe avec l'estimation du volume (m3/tige) par classe de produits pour chacun des arbres
#'  ERS, BOJ, ERR, BOP, HEG, CHR. Le data contient 6 colonnes chacune contenant le volume d'un produit: DER (déroulage),
#'  F1, F2, F3, F4 et P (pâte). Le data contient aussi une colonne type qui indique le type d'équation utilisé avec le suffixe _R ou _P
#'  ajouté à la fin pour indiquer l'utilisation de la version régionalisée (_R) ou de la version originale de Petro (_P).
#'
#' @details Les équations régionaliées (type DHP et ABCD) ne doivent êtres appliquées que sur les régions écologiques:
#'          "1a", "2a", "2b", "2c", "3a", "3b", "3c", "3d", "4a", "4b", "4c", "4d", "4e", "4f", "4g","4h", "DU", "SV".
#'
#'          Les équations ne s'appliquent qu'aux arbres avec un dhp>23, les autres seront supprimés.
#'          Les équations ne s'appliquent qu'aux espèces: "ERS", "BOJ", "ERR", "BOP", "HEG", "CHR", les autres seront supprimées.
#'
#' @examples
#' vol_billon <- SimBillonnagePetro(Data=liste_arbres_ex, type="ABCD")
#'
#' @export
#'

SimBillonnagePetro <- function (Data , type){

  select=dplyr::select

  Data<- Data %>% filter(DHPcm >23) %>%
    mutate(type =NA)

  data<- Data %>% filter(Espece %in% c("ERS", "BOJ", "ERR", "BOP", "HEG", "CHR") )

  if (nrow(data) == 0) {

    Data<- Data %>% mutate(erreur = "Code d'essence \uE0 l'ext\uE9rieur de la plage de valeurs possibles pour billonage")

    return(Data)
  }

                          ##### ABCD#####
  if(!"eco" %in% colnames(data)){
    data <-ConvertisseurEco(data)
  }

  if (type %in% c("ABCD", "ABCD2015") && all(is.na(data$ABCD))) {
    type <- ifelse(type == "ABCD", "DHP", "DHP2015")
  }




  final <- data.frame()
  if(type %in% c("ABCD","DHP")){

    if(type == "ABCD"){
      # Séparer les arbres possédant la qualité ABCD des autres
     data_ABCD <- data %>% filter(!is.na(ABCD))
     data_pas_ABCD <-data %>% filter(is.na(ABCD))

     regional_ABCD <- data_ABCD %>% filter(Espece %in% c("ERS", "BOJ"))

     non_regional_2015_ABCD <- data_ABCD %>% filter(!Espece %in% c("ERS", "BOJ"))

     # Billonnage régionalisé pour les arbres possédant la qualité ABCD

     regional_result_ABCD <- data.frame()
     if (nrow(regional_ABCD) > 0) {
     regional_result_ABCD <-ABCD_DHP_region(data=regional_ABCD, type ="ABCD" )
     }

     #Billonnage non régionalisé pour les arbres possédant la qualité ABCD

     non_regional_2015_result_ABCD <- data.frame()

     if (nrow(non_regional_2015_ABCD) > 0) {
     non_regional_2015_result_ABCD <- ABCD_DHP2015(data=non_regional_2015_ABCD, type ="ABCD2015")
     }

     regional_pas_ABCD <- data_pas_ABCD %>% filter(Espece %in% c("ERS", "BOJ"))
     non_regional_2015_pas_ABCD <- data_pas_ABCD %>% filter(!Espece %in% c("ERS", "BOJ"))

     # Billonnage régionalisé pour les arbres ne  possédant pas la qualité ABCD
     # donc Billonage éffectuer avec DHP

     regional_result_pas_ABCD <- data.frame()

     if (nrow(regional_pas_ABCD) > 0) {
     regional_result_pas_ABCD <-ABCD_DHP_region(data=regional_pas_ABCD, type ="DHP" )
     }

     # Billonnage non régionalisé pour les arbres ne  possédant pas la qualité ABCD
     # donc Billonage éffectuer avec DHP
     non_regional_2015_result_pas_ABCD<- data.frame()

     if (nrow(non_regional_2015_pas_ABCD) > 0) {
     non_regional_2015_result_pas_ABCD <- ABCD_DHP2015(data=non_regional_2015_pas_ABCD, type ="DHP2015")
     }

     finl1 <-rbind(regional_result_ABCD,non_regional_2015_result_ABCD)
     finl2 <-rbind(regional_result_pas_ABCD,non_regional_2015_result_pas_ABCD)
     final <-rbind(finl2,finl1)

    }else{

    regional <- data %>% filter(Espece %in% c("ERS", "BOJ"))
    regional_result <- data.frame()

    if (nrow(regional) > 0) {
    regional_result <-ABCD_DHP_region(data=regional, type =type )
    }

    non_regional_2015 <- data %>% filter(!Espece %in% c("ERS", "BOJ"))
    non_regional_2015_result <- data.frame()

    if (nrow(non_regional_2015) > 0) {
    non_regional_2015_result <- ABCD_DHP2015(data=non_regional_2015, type ="DHP2015")
    }

    final <-rbind(regional_result,non_regional_2015_result)
   }
  }else{


    if(type=="ABCD2015"){

      data_ABCD <- data %>% filter(!is.na(ABCD))
      data_pas_ABCD <-data %>% filter(is.na(ABCD))

      final_ABCD<- data.frame()

      if (nrow(data_ABCD) > 0) {
      final_ABCD <- ABCD_DHP2015(data=data_ABCD, type ="ABCD2015")
      }

      final_DHP <- data.frame()
      if (nrow(data_pas_ABCD) > 0) {
      final_DHP<- ABCD_DHP2015(data=data_pas_ABCD, type ="DHP2015")
      }

      final<-rbind(final_ABCD,final_DHP)

    }else{

      final <- ABCD_DHP2015(data=data, type =type)
    }


  }


  final<-final %>% select(DER,F1,F2,F3,F4,P,TigeID,type)


  return (final)


}

#' Fonction interne qui prévoit la répartition par produits des arbres ERS et BOJ à l'aide des nouvelles
#' équations de Petro régionalisées de Havreljuk et al. (2025).
#'
#' @param data Un dataframe qui contient en ligne les arbres dont on veut prévoir
#'             les rendements en produit à l'aide du module de billonnage Petro
#'             régionalisé.
#'             Le dataframe doit contenir une colonne TigeID qui numérote individuellement chacune des lignes
#'             et une colonne, Espece, colonne eco qui contient le groupe de régions écologiques,
#'             une colonne DHPcm et optionnelement une colonne ABCD.
#'             Les équations ne s'appliquent qu'aux espèces: "ERS", "BOJ", les autres seront supprimés
#'             Les équations ne s'appliquent qu'aux arbres avec un dhp>23, les autres seront supprimés
#'             Les équations ne doivent êtres appliquées que sur les régions écologiques:
#'            "1a", "2a", "2b", "2c", "3a", "3b", "3c", "3d", "4a", "4b", "4c", "4d", "4e", "4f", "4g","4h", "DU", "SV"
#' @param type "ABCD" pour utiliser les équations basées sur ABCD
#'             "DHP" pour utiliser les équations basées seulement sur le DHP
#' @return Retourne un dataframe avec l'estimation du volume par classe de produit
#'          pour chacun des arbres BOJ et ERS de 23 cm
#'          colonnes: TigeID, type, F1, F2, F3, F4, P, DER
#'
#' @keywords internal
#'

ABCD_DHP_region<- function (data, type){

  select=dplyr::select

  # filtrer les dhp et les essences
  data <- data %>% filter(DHPcm>23) %>% filter(Espece %in% c("ERS", "BOJ"))

  if (type == "ABCD"){

    CovParmPetroABCD <- CovParmPetroABCD %>% filter(Cov>0)


    Vol_Billon<-ParaPetroFinal_New %>%
      filter(Module=="Vol") %>%
      mutate(betaVol=ParameterEstimate) %>%
      select(-Module,-ParameterEstimate) %>%
      group_by(Essence_billon, Produit) %>%
      pivot_wider(names_from = Effet, values_from  = betaVol, names_prefix = "Vol")

    Pres_Billon <- ParaPetroFinal_New %>%
      filter(Module=="Pres") %>%
      mutate(betaPres=ParameterEstimate) %>%
      select(-Module,-ParameterEstimate) %>%
      group_by(Essence_billon, Produit) %>%
      pivot_wider(names_from = Effet, values_from  = betaPres, names_prefix = "Pres") %>%
      full_join(Vol_Billon, by=c("Essence_billon","Produit", "QualiteABCD", "eco")) %>%
      mutate(Presdhpcm2_classepetro=ifelse(is.na(Presdhpcm2_classepetro)==TRUE,0,Presdhpcm2_classepetro),
             Voldhpcm2_classepetro=ifelse(is.na(Voldhpcm2_classepetro)==TRUE,0,Voldhpcm2_classepetro))
    #names(Pres_Billon)

    par_qual <- Pres_Billon %>%
      filter(!is.na(QualiteABCD)) %>%
      select(c(Essence_billon, Produit, QualiteABCD, Presclassepetro_qual, Volclassepetro_qual))

    par_eco <- Pres_Billon %>%
      filter(!is.na(eco)) %>%
      select(c(Essence_billon, Produit, eco, Presclassepetro_eco, Volclassepetro_eco))

    par_num <- Pres_Billon %>%
      filter(is.na(eco) & is.na(QualiteABCD)) %>%
      select(c(Essence_billon, Produit, Presdhpcm_classepetro, Presdhpcm2_classepetro, Voldhpcm_classepetro, Voldhpcm2_classepetro))


    sim_ABCD_DHP <- data %>%
      mutate(Essence_billon=Espece) %>% #ajout
      #filter(Essence_billon %in% c("ERS", "BOJ")) %>%
      rename(QualiteABCD=ABCD) %>%
      left_join(par_eco, by=c("Essence_billon", "eco"), relationship="many-to-many") %>%
      left_join(par_qual, by=c("Essence_billon", "QualiteABCD", "Produit"), relationship="many-to-many") %>%
      left_join(par_num, by=c("Essence_billon", "Produit"), relationship="many-to-many") %>%
      inner_join(CovParmPetroABCD, by=c("Essence_billon", "Produit")) %>%
      mutate(Cov=ifelse(is.na(Cov)==TRUE,0,Cov)) %>%
      mutate(BetaPres= Presclassepetro_eco+
               Presclassepetro_qual+
               DHPcm*Presdhpcm_classepetro+
               DHPcm^2*Presdhpcm2_classepetro,
             BetaVol=  Volclassepetro_eco+
               Volclassepetro_qual+
               DHPcm*Voldhpcm_classepetro+
               DHPcm^2*Voldhpcm2_classepetro,
             Pres=exp(BetaPres)/(1+exp(BetaPres)),
             Vol=exp(BetaVol+0.5*Cov),
             VolBillonM3=Pres*Vol) %>%
      mutate (Stm2ha=pi*(DHPcm/200)^2,type = "ABCD_R",ABCD = QualiteABCD ) %>%
      select(Produit,VolBillonM3,TigeID,type) %>%
      pivot_wider(names_from = Produit, values_from = VolBillonM3)

    if(!"F1" %in% names(sim_ABCD_DHP) ){
      sim_ABCD_DHP<-sim_ABCD_DHP %>% mutate(F1 = NA)
    }
    if(!"DER" %in% names(sim_ABCD_DHP)){
      sim_ABCD_DHP <-sim_ABCD_DHP %>%
        mutate(DER=NA)
    }
    if(!"F2" %in% names(sim_ABCD_DHP) ){
      sim_ABCD_DHP<-sim_ABCD_DHP %>% mutate(F2 = NA)
    }
    if(!"F3" %in% names(sim_ABCD_DHP)){
      sim_ABCD_DHP <-sim_ABCD_DHP %>%
        mutate(F3=NA)
    }
    if(!"F4" %in% names(sim_ABCD_DHP) ){
      sim_ABCD_DHP<-sim_ABCD_DHP %>% mutate(F4 = NA)
    }
    if(!"P" %in% names(sim_ABCD_DHP)){
      sim_ABCD_DHP <-sim_ABCD_DHP %>%
        mutate(P=NA)
    }




  }else if(type == "DHP"){      ######## DHP ##########


    CovParmPetro_DHP <- CovParmPetro_DHP %>% filter(Cov>0)

    Vol_Billon<-ParaPetroFinal_dhp %>%
      filter(Module=="Vol") %>%
      mutate(betaVol=ParameterEstimate) %>%
      select(-Module,-ParameterEstimate) %>%
      group_by(Essence_billon, Produit) %>%
      pivot_wider(names_from = Effet, values_from  = betaVol, names_prefix = "Vol")

    Pres_Billon <- ParaPetroFinal_dhp %>%
      filter(Module=="Pres") %>%
      mutate(betaPres=ParameterEstimate) %>%
      select(-Module,-ParameterEstimate) %>%
      group_by(Essence_billon, Produit) %>%
      pivot_wider(names_from = Effet, values_from  = betaPres, names_prefix = "Pres") %>%
      full_join(Vol_Billon, by=c("Essence_billon","Produit", "eco"),relationship = "many-to-many") %>%
      mutate(Presdhpcm2__classepetro=ifelse(is.na(Presdhpcm2__classepetro)==TRUE,0,Presdhpcm2__classepetro),
             Voldhpcm2_classepetro=ifelse(is.na(Voldhpcm2_classepetro)==TRUE,0,Voldhpcm2_classepetro))
    #names(Pres_Billon)

    par_eco <- Pres_Billon %>%
      filter(!is.na(eco)) %>%
      select(c(Essence_billon, Produit, eco, Presclassepetro_eco, Volclassepetro_eco))

    par_num <- Pres_Billon %>%
      filter(is.na(eco)) %>%
      select(c(Essence_billon, Produit, Presdhpcm__classepetro, Presdhpcm2__classepetro, Voldhpcm_classepetro, Voldhpcm2_classepetro))



    sim_ABCD_DHP <- data %>%
      mutate(Essence_billon=Espece) %>% #ajout
      #filter(Essence_billon %in% c("ERS", "BOJ")) %>%
      left_join(par_eco, by=c("Essence_billon", "eco"), relationship = "many-to-many") %>%
      #left_join(par_qual, by=c("Essence_billon", "CLASSE_DE_"="QualiteABCD", "Produit")) %>%
      left_join(par_num, by=c("Essence_billon", "Produit")) %>%
      inner_join(CovParmPetro_DHP, by=c("Essence_billon", "Produit")) %>%
      mutate(Cov=ifelse(is.na(Cov)==TRUE,0,Cov)) %>%
      mutate(BetaPres= Presclassepetro_eco+
               (DHPcm/10)*Presdhpcm__classepetro+ #mise a l'echelle du dhp
               (DHPcm/10)^2*Presdhpcm2__classepetro, #mise a l'echelle du dhp
             BetaVol=  Volclassepetro_eco+
               DHPcm*Voldhpcm_classepetro+
               DHPcm^2*Voldhpcm2_classepetro,
             Pres=exp(BetaPres)/(1+exp(BetaPres)),
             Vol=exp(BetaVol+0.5*Cov),
             VolBillonM3=Pres*Vol) %>%
      mutate (Stm2ha=pi*(DHPcm/200)^2,type = "DHP_R") %>%
      select(Produit,VolBillonM3,TigeID,type) %>%
      # select(Espece, DHPcm, eco, QualiteABCD, Produit, Essence_billon,VolBillonM3 ) %>%
      pivot_wider(names_from = Produit, values_from = VolBillonM3)

    if(!"F1" %in% names(sim_ABCD_DHP) ){
      sim_ABCD_DHP<-sim_ABCD_DHP %>% mutate(F1 = NA)
    }
    if(!"DER" %in% names(sim_ABCD_DHP)){
      sim_ABCD_DHP <-sim_ABCD_DHP %>%
        mutate(DER=NA)
    }
    if(!"F2" %in% names(sim_ABCD_DHP) ){
      sim_ABCD_DHP<-sim_ABCD_DHP %>% mutate(F2 = NA)
    }
    if(!"F3" %in% names(sim_ABCD_DHP)){
      sim_ABCD_DHP <-sim_ABCD_DHP %>%
        mutate(F3=NA)
    }
    if(!"F4" %in% names(sim_ABCD_DHP) ){
      sim_ABCD_DHP<-sim_ABCD_DHP %>% mutate(F4 = NA)
    }
    if(!"P" %in% names(sim_ABCD_DHP)){
      sim_ABCD_DHP <-sim_ABCD_DHP %>%
        mutate(P=NA)
    }
  }

  return (sim_ABCD_DHP )

}

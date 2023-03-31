#' @title Estimation des positions des digues (points hauts) de chaque côté du profil en travers
#' 
#' @description ENG : Estimate the bank elevation of each side of the cross sections
#' 
#' FR : Cette fonction est issue du script 03-ProcessXS.R.
#' 
#' Elle permet de détecter les points hauts de chaque côté des profils en travers.
#' Elle permet de vérifier que le point haut qui a été détecté est effectivement le point le plus haut (cas du petit 
#' remblais juste avant le grand remblais). 
#' Elle vérifie que le point le plus haut détecté n'est pas sous l'eau.
#' Elle créé des profils en travers allant jusqu'à +Inf (rive gauche) ou -Inf (rive droite) si aucun point haut n'est découvert. 
#' 
#' 
#' Détails de la fonction au niveau des variables utilisées :
#'  
#' Bief_MNT : dataframe où sont stockés les altitudes Z pour chaque point des profils en travers (100 points par profil).
#' Bief_MNT a été créé à partir de Bief_Results_Iter (qui contient SL_m, CHAR_m, Q_m3_s, K, H, Z_M, NBief, NSection).
#' La colonne IPOSI ordonne les points de 1 à 100 puis de 101 à 200 etc : le numéro 1 est l'extrémité en rive GAUCHE, 
#' le numéro 100 est l'extrémité en rive DROITE.
#' 
#' Bief_MNT_NL : identification des rives à problème en passant NSection une à une
#' 
#' Extremum : stocke les points hauts G et D au niveau des NSection à problème
#' 
#' Ensuite on stocke Extremum dans Bief_MNT_NL (jointure) et on remplace les NA par +Inf pour la rive GAUCHE et -Inf 
#' pour la rive DROITE
#' 
#' Enfin on stocke les résultats dans les colonnes PG pour la rive gauche et PD pour la rive droite du tableau Bief_Position_Iter.
#' Bief_Position_Iter contient donc la hauteur des digues/berges à chaque NSection.
#' 
#' 
#' La fonction estimate_bank_posi est appelée par la fonction run_hydrau_enlarge du script 04-RunHydrau.R.
#' Et run_hydrau_enlarge n'est appelé que dans l'étape 1 de 99-RunReach.R (soit pour la création des profils en travers 
#' phase 1 uniquement).
#'  
#' @param Bief_Results_Iter la fonction s'applique sur un tableau de données du niveau d'eau / dataframe with water level
#' @param Bief_MNT la fonction s'applique sur un tableau de données avec l'altitude à chaque point de chaque profil en travers 
#' / dataframe with elevation data for each section
#' @param Bief_Position_Iter la fonction met à jour ce tableau de données qui contient la position des digues rive gauche (PG) 
#' et rive droite (PD) / dataframe with position of right and left banks
#' @param w la fonction s'applique sur une valeur donnée aux profils en travers qui sont sous l'eau / value given to the sections 
#' that are below the water surface
#' @return la fonction retourne un tableau de donnée avec les positions des digues rive gauche et rive droite / dataframe with 
#' position of right and left banks

estimate_bank_posi = function(Bief_Results_Iter, Bief_MNT, Bief_Position_Iter, w)   
{

  ## get elevation data and surface water level
  Bief_MNT %<>% 
    dplyr::left_join(dplyr::select(Bief_Results_Iter, NSection, Z_M), by="NSection")         
    # LPo : left_join = jointure gauche 
    # LPo : Copie Z_M et NSection (issu de Bief_Results_Iter) dans Bief_MNT, par la variable de jointure "Nsection"
    # LPo : Bief_MNT contient donc : NSection, POSI, IPOSI, POSI_C, RIVE, Z, X, Y, Z_M
  
  
  ## find which section needs to be checked                                           
  # LPo : On stocke les XS (et la rive correspondante) qui ont besoin d'être élargies jusqu'à un point haut 
  Bief_MNT_NL =                                                                       
    Bief_MNT %>%                                                                       
    dplyr::left_join(Bief_Position_Iter, by="NSection") %>%                                  
    dplyr::filter(POSI >= PG & POSI <= PD) %>%                                               
    dplyr::group_by(NSection, RIVE) %>%                                                      
    dplyr::summarise(n_up = sum(Z > Z_M)) %>%                                                
    dplyr::filter(n_up < 1)                                                                  
  # LPo : POSI = distance cumulée entre les points le long du profil 
  # LPo : en établissant que le premier point rive gauche est positionné à 1m
  # LPo : PG = point haut rive gauche (par défaut 80 m) 
  # LPo : PD = point haut rive droite (par défaut 120 m)
  # LPo : Variable Bief_MNT_NL :
  # LPo : Jointure de PG et PD dans Bief_MNT avec la variable de jointure "Nsection"
  # LPo : Puis groupement par rive (gauche ou droite) et par NSection
  # LPo : Création de la colonne n_up qui stocke la somme des altitudes Z supérieures à Z_M 
  # LPo : (Z_M = côte NGF de la hauteur d'eau, Z = altitude à chaque point)
  # LPo : Filtre : on conserve seulement les n_up inférieurs à 1
  

  ## check if the bank positions are already at their max                             
  # LPo : vérifier si le point haut des digues est le vrai point haut
  Bief_MNT_NL %<>%                                                                    
    dplyr::left_join(Bief_Position_Iter, by="NSection") %>%                                  
    dplyr::mutate(isEX = ifelse(RIVE=="G" & !is.finite(PG), 1, 0),                           
           isEX = ifelse(RIVE=="D" & !is.finite(PD), 1, isEX)) %>%                     
    dplyr::filter(!isEX) %>%                                                                
    dplyr::select(NSection, RIVE)  
  # LPo : Dans Bief_MNT_NL :
  # LPo : Jointure à gauche de Bief_Position_Iter(PG, PD et w_iter) avec la variable de jointure "Nsection"
  # LPo : is.finite = retourne un vecteur de la même taille que PG ou PD 
  # LPo : en précisant si les éléments sont finis ou non finis
  # LPo : isEX = "is.extremum" ?
  # LPo : isEX si rive droite et si PD est fini, écrire 1, sinon écrire la valeur de isEX
  # LPo : On filtre pour ne conserver que les valeurs de la colonne isEX égales à 1
  # LPo : On conserve uniquement les colonnes RIVE et NSection

   cat(paste0(Sys.time()," -- ", length(which(Bief_MNT_NL$RIVE == "G")),"G ",
              length(which(Bief_MNT_NL$RIVE == "D")),"D ", "/",dim(Bief_Position_Iter)[1]," cross-sections enlarged \n\n"))
   
  ## return if there is no section under water
  if(nrow(Bief_MNT_NL)==0) return(Bief_Position_Iter)                                 
  # LPo : Si Bief_MNT_NL est vide, cela signifie que l'on a trouvé le point haut (de la digue)
  # LPo : Alors retourner Bief_Position_Iter comme résultats de estimate_bank_posi
  

  ## find the highest dry points above the water level                                
  Extremum =                                            
    Bief_MNT %>%                                                                      
    dplyr::inner_join(Bief_MNT_NL,  by = c("NSection", "RIVE")) %>%                          
    dplyr::group_by(NSection, RIVE) %>%                                                      
    dplyr::arrange(abs(POSI_C)) %>%                                                          
    dplyr::mutate(isUP = Z > lead(Z, default = -999)) %>%       ## highest points                  
    dplyr::filter(Z > Z_M, isUP) %>%                            ## highest points above water level   
    dplyr::slice(1) %>%                                         ## points close to the center of the section    
    dplyr::ungroup() %>%                                                                     
    dplyr::select(NSection, RIVE, POSI)
    # LPo : Variable Extremum :
    # LPo : inner_join = jointure Bief_MNT_NL et Bief_MNT par NSection et RIVE 
    # LPo : Groupement par Nsection et par RIVE
    # LPo : abs = valeur absolue. 
    # LPo : Ordonne POSI_C par la valeur absolue.
    # LPo : lead = Décalage ?  valeur par default = -999
    # LPo : slice = Ordonne par position ordinale dans le tableau (= dans l'ordre croissant)
    # LPo : Dégroupement
    # LPo : Sélection des colonnes NSection, RIVE, POSI uniquement
    # LPo : Extremum = contient NSection, RIVE et POSI (dataframe)
    # LPo : (contient seulement 12 lignes, contre 24 pour Bief_MNT_NL)
   
  
  ## if there is no dry points, return +Inf and -Inf
  Bief_MNT_NL %<>% dplyr::left_join(Extremum, by = c("NSection", "RIVE"))                    
  # LPo : Jointure gauche pour copier dans Bief_MNT_NL le df Extremum par les variables de jointure Nsection, RIVE 
  # LPo : Ajoute des NA dans la colonne POSI pour les 12 lignes restantes
  
  Bief_MNT_NL %<>%                                                                    
    dplyr::mutate(POSI = ifelse(is.na(POSI), +Inf, POSI),                                    
           POSI = ifelse(!is.finite(POSI) & RIVE=="G", -Inf, POSI)) 

  # Bief_MNT_NL %<>% tidyr::pivot_wider(RIVE, POSI)
  Bief_MNT_NL %<>% tidyr::spread(RIVE, POSI)
  # LPo : Dans Bief_MNT_NL :
  # LPo : S'il y a des valeurs nulles dans POSI, mettre +Inf, sinon laisser la valeur de POSI 
  # LPo : S'il y a +Inf dans POSI rive gauche, mettre -Inf
  # LPo : Donc : rive droite = +Inf et rive gauche = -Inf ; pour que la XS continue au loin ?
  # LPo : spread = ordonne par RIVE, POSI   
  
  ## put the values in the dataframe
  Bief_Position_Iter %<>% dplyr::left_join(Bief_MNT_NL, by="NSection")                       
  # LPo : Jointure des résultats Bief_MNT_NL dans Bief_Position_Iter par la variable de jointure "Nsection"
  
  if(! "G" %in% names(Bief_Position_Iter)) Bief_Position_Iter$G = NA                  
  # LPo : Si la colonne ne s'appelle pas G dans Bief_Position_Iter alors créer une colonne G et la remplir de NA
  if(! "D" %in% names(Bief_Position_Iter)) Bief_Position_Iter$D = NA                  
  # LPo : Si la colonne ne s'appelle pas D dans Bief_Position_Iter alors créer une colonne D et la remplir de NA
  
  
  ## update the bank positions                                                        
  Bief_Position_Iter %<>%
    dplyr::mutate(w_iter = ifelse(!is.na(D) | !is.na(G), w, w_iter)) %>%                     
    dplyr::mutate(PG = ifelse(is.na(G), PG, G),
           PD = ifelse(is.na(D), PD, D)) %>%
    dplyr::select(NSection, PG, PD, w_iter)
  # LPo : Ajout dans Bief_Position_Iter des nouveaux points hauts
  # LPo : PG (qui est par défaut de 80 m au départ) prend la valeur qui était contenu dans Extremum pour la rive GAUCHE
  # LPO : PD (qui est par défaut à 120 m au départ) prend la valeur qui était contenu dans Extremum pour la rive DROITE
  # LPO : w est la valeur donnée au sections dont l'extrémité est sous l'eau
  
  return(Bief_Position_Iter)                                                          
  # LPo : Retourne la variable Bief_Position_Iter qui contient les nouveaux points hauts = PG et PD
  # LPo : (seulement 15 résultats pour le bief 1) 
}



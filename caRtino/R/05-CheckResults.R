#' Find root
#' @noRd 
find_root = function(X, Z , Z_M, L)                                                     
  # LPo : racines de quoi? 
{
  if(L==1) racine = 0
  else
  {
    Z = Z - Z_M
    interpol = approxfun(X, Z, method="linear")                                         
    # LPo : interpolation de l'altitude entre chaque XS
    racines = rootSolve::uniroot.all(interpol, interval=range(X))
    racine = max(racines)
  }
  return(racine)
}

#' @title Estimer la largeur de la zone inondée
#' @description ENG : Estimate the width of water surface
#' 
#' FR : Cette fonction est issue du script 05-CheckResults.R.
#' 
#' La fonction estimate_water_width permet d'estimer la largeur de la zone inondée (largeur au miroir).
#' 
#' Elle estime les largeurs maximales et minimales des digues de chaque rive. 
#' 
#' Elle récupère les points d'intersection entre l'altitude et la zone inondée. Permet d'avoir donc les extrémités 
#' de la zone recouverte d'eau. Pour se faire, elle appelle la fonction find_root du script 05-CheckResults.R. ???
#' 
#' Elle calcule ensuite la surface mouillée au niveau de chaque profil en travers.
#' 
#' La fonction estimate_water_width est appelée dans la fonction postpro_hydrau du script 05-CheckResults.R. 
#'                                                                                                     
#' @param Bief_Results_Iter la fonction s'applique sur un tableau de données qui contient le niveau d'eau 
#' / dataframe with water level
#' @param Bief_MNT_Iter la fonction s'applique sur un tableau de données qui contient l'altitude à chaque profil en travers 
#' / dataframe with elevation data for each section
#' @return la fonction retourne un tableau de données avec les largeurs des digues rive gauche et rive droite ???
#' / dataframe with widths of right and left banks

estimate_water_width = function(Bief_Results_Iter, Bief_MNT_Iter)                       
{
  ## get elevation data and water level
  Bief_res =                                                                            
  # LPo : Bief_res = variable où sont stockés les altitudes et hauteurs d'eau
    Bief_Results_Iter %>% 
    dplyr::select(NSection, Z_M) %>%
    left_join(Bief_MNT_Iter, by="NSection") %>%                                         
    # LPo : jointure (gauche) dans Bief_res de Bief_MNT_Iter par la variable de jointure "NSection"
    mutate(POSI_CA = abs(POSI_C)) %>% ## distance to the center point
    arrange(NSection, POSI_CA)
  
  ## estimate min and max values                                                        
  # LPo : estime les valeurs minimales et maximales (de largeurs des digues???)
  Largeurs_MinMax = 
    Bief_res %>%
    group_by(NSection, RIVE) %>%
    dplyr::summarise(minL = min(POSI_CA), maxL = max(POSI_CA))                                 
  # LPo : summarise = réduit des valeurs multiples à une seule valeur (min et max)
  # LPo : minL? maxL? = min et max de la largeur des digues?
  
  
  ## estimate the intersection between elevation and water surface                      
  # LPo : estime l'extrémité de la zone inondable
  inter_eau_profil = 
    Bief_res %>%
    dplyr::group_by(NSection, RIVE) %>%
    dplyr::summarise(largeur = find_root(POSI_CA, Z, Z_M, length(POSI_CA)))                    
  # LPo : applique la fonction find_root du script 05-CheckResults.R
 
  
  ## estimate the width of water surface for each section and each side                 
  # LPo : estime la largeur de la zone inondable pour chaque rive de toutes les XS
  inter_eau_profil %<>% 
    left_join(Largeurs_MinMax, by = c("NSection", "RIVE")) %>%                          
    # LPo : jointure (gauche) dans inter_eau_profil des Largeurs_MinMax, par segments de cours d'eau ("NSection") et par "RIVE"
    dplyr::mutate(L = ifelse(!is.infinite(largeur), largeur, maxL))

    
  ## update left and right widths                                                       
  # LPo : met à jour les largeurs
  Bief_Largeurs = 
    inter_eau_profil %>% 
    ungroup() %>% 
    dplyr::select(NSection, RIVE, L) %>% 
    tidyr::spread(RIVE, L) %>%                                                          
    # LPo : spread = ordonne par (RIVE, L)
    dplyr::select(NSection, LG=G, LD=D) %>%                                      
    dplyr::mutate(LM = LG + LD)                                                                
  # LPo : LM = largeur totale des profils en travers
  
  return(Bief_Largeurs)
}


#' @title Post-traitement des simulations hydrauliques
#' @description ENG : Post-processing of the simulation
#' 
#' FR : Cette fonction est issue du script 05-CheckResults.R.
#' 
#' La fonction postpro_hydrau appelle la fonction graph_water_disch du script 09-ExportGraphs.R pour créer un graphique 
#' du niveau d'eau le long du cours d'eau (profil en long).
#' 
#' La fonction postpro_hydrau appelle la fonction estimate_water_width du script 05-CheckResults.R. Elle permet donc 
#' d'estimer la largeur de la zone inondée, et la surface mouillée au niveau de chaque profil en travers.
#' 
#' @param hydrau la fonction s'applique sur la liste des résultats / list of results
#' @param name_res la fonction s'applique sur le fichier résultats / name of the result file
#' @param name_run la fonction s'applique sur la variable nom qui comprend le débit + le bief + le BV étudié 
#' / name of the run file
#' @return la fonction retourne un tableau de données des résultats formatés / dataframe with formatted outputs
postpro_hydrau = function(hydrau, name_res, name_run)
{
  ## get elevation data and distance along the stream
  z_ref = hydrau$mnt %>% dplyr::group_by(NSection) %>% dplyr::summarise(Z_REF=min(Z))                 
  # LPo : la valeur minimale d'altitude pour chaque bief = Z_REF
  hydrau$res %<>% left_join(z_ref, by="NSection")                                       
  # LPo : jointure (gauche) de z_ref dans hydrau$res par segments de cours d'eau (variable de jointure = NSection)
  
  ## write results                                                                      
  # LPo : écriture du csv
  write.csv2(hydrau$res,  
             file=paste(name_res, "_Res.csv", sep=""), row.names=F)                   
  
  ## graph                                                                              
  # LPo : appelle la fonction graph_water_disch du script 09-ExportGraphs.R
  graph_water_dist(hydrau$res, name_run, name_res)
  
  ## estimation of the water surface width
  Bief_Largeurs = estimate_water_width(hydrau$res, hydrau$mnt)                          
  # LPo : applique la fonction estimate_water_width 

  ## format output
  Tab_Bief_Result = cbind(hydrau$res,                                                   
  # LPo : Tab_Bief_Result = variable qui stocke les résultats
                          hydrau$posi[, -1],
                          Bief_Largeurs[, -1])
  return(Tab_Bief_Result)
}


  
  
  
  
  
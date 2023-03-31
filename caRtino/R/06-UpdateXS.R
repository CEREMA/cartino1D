#' @title Interpolation entre les profils en travers
#' @description ENG : Estimate new positions and widths of the cross sections by linear interpolation             
#' 
#' FR : Cette fonction est issue du script 06-UpdateXS.R.
#' 
#' Elle permet l'interpolation linéaire des profils en travers.
#' Elle créé de nouveaux profils en travers, afin de densifier le nombre de profils en travers, et estime leurs largeurs.
#' 
#' @param Tab_Bief_Result dataframe with results
#' @return dataframe with new positions along the stream and widths
#' 
estimate_posi_width = function(Tab_Bief_Result)                                                
{
  ## get some variables
  Data_table_Bief =                                                                            
  # LPo : Data_table_Bief = sélection des résultats précédemment obtenus
    Tab_Bief_Result %>%
    dplyr::select(NSection,LM,LD,LG,DIST)                                                      
    # LPo : LM = LG + LD = largeur totale de la XS

  
  ## define two linear interpolation functions - width = f(distance)                           
  interpol_LD =                                                                                
  # LPo : fonction d'interpolation côté droit
    approxfun(Data_table_Bief$DIST,
              Data_table_Bief$LD, 
              method="linear", rule=2)
  interpol_LG =                                                                                
  # LPo : fonction d'interpolation côté gauche
    approxfun(Data_table_Bief$DIST,
              Data_table_Bief$LG,
              method="linear", rule=2)
  
  ## estimate new positions along the stream and associated widths                             
  # LPo : estime les nouveaux profils (position, distance, écarts cumulés) et largeurs associées (application du DPNC)
  
  i=0; dist_iter=0; lm = Data_table_Bief$LM[1]
  dist_max = max(Data_table_Bief$DIST)
  tmp=NULL;                                                                                    
  # LPo : création du fichier temporaire "tmp" (=NULL au départ)
  
  while (dist_iter < (dist_max))                                                               
  # LPo : tant que dist_iter < dist_max (dist_max = la distance totale du premier au dernier profil)
  {
    ## estimate new spacing and position based on the width of the cross section               
    # LPo : estime l'écart entre les nouvelles XS
    ecart_espace = round(min(max(lm * DPNC, min_ecart), max_ecart))
    dist_iter = min(dist_iter + ecart_espace, dist_max)                                        
    # LPo : ajouter ecart_espace (calculé avec DPNC) à dist_ter, chercher le min entre cette somme et la dist_max
    
    ## estimate the widths at this new position                                                
    # LPo : estime la largeur à cette nouvelle position
    ld = interpol_LD(dist_iter)                                                                
    # LPo : application de interpol_LD (calcul de la longueur droite)
    lg = interpol_LG(dist_iter)                                                                
    # LPo : application de interpol_LG (calcul de la longueur gauche)
    lm = ld + lg
    # LPo : largeur au miroir (variable temporaire) = ld+lg
    i = i+1
    tmp[[i]] = c(i, lm, ld, lg, dist_iter)
    # LPo : copie des résultats dans tmp
  }                                                                                            
  # LPo : fin de la boucle quand dist_iter >= dist_max
 
  tmp = data.frame(do.call(rbind, tmp))
  # LPo : transforme tmp en data.frame
  names(tmp) = names(Data_table_Bief)                                                          
  # LPo : copie des noms de colonnes de Data_table_Bief pour les coller dans tmp
  
  ## round the values to be consistent with spatial discretisation
  tmp %<>% 
    dplyr::mutate(LD = pmax(2*delta, delta * ceiling(LD / delta)),                                    
           # LPo : ceiling = retourne les éléments correspondant à LD/delta dans un vecteur numérique
           LG = pmax(2*delta, delta * ceiling(LG / delta)))                                    
           # LPo : ceiling = retourne les éléments correspondant à LG/delta dans un vecteur numérique
  
  ## estimate XY centers of the cross sections along the stream                                
  # LPo : détermine les points centraux des nouvelles XS
  tmp$X = interpol_bief_X(tmp$DIST)
  tmp$Y = interpol_bief_Y(tmp$DIST)
  
  return(tmp)                                                                                  
}                                                                                              
# LPo : fin de la fonction estimate_posi_width



#' exclude section based on their position along the stream                                    
#' exclut les XS selon leurs positions (cas des ponts)
#' @noRd 
exclude_sections = function(Profils_Iter)                                                      
{
  i_out = which(interpol_bief_bridge(Profils_Iter$DIST) == 0)
  if(length(i_out)>0)
  {
    Profils_Iter %<>% 
      slice(-i_out) %>%
      mutate(NSection=1:n())
  }
  return(Profils_Iter)
}


#' @title Densification selon la charge hydraulique
#' @description ENG : Densification according to hydraulic energy
#'
#'
#' Cette fonction est issue du script 10-EnergyDensification.R
#' 
#' Une option, dans le script CARTINO_PARAM.R, qui s'appelle "do_densification", doit être activée
#' par l'utilisateur (= TRUE) pour que cette fonction s'applique.
#' 
#' Elle permet, tout d'abord, d'identifier les profils en travers où la différence de charge du 
#' profil amont au profil aval est supérieure à une valeur fixée par l'utilisateur. Ce paramètre 
#' est inséré dans la première colonne de la matrice dCharUtil (= dCharUtil[,1]) du script CARTINO_PARAM.R.
#' 
#  Elle permet également d'insérer un paramètre de distance minimale à respecter pour insérer un 
#' profil intermédiaire.Ce paramètre est inséré dans la seconde colonne de la matrice dCharUtil 
#' (= dCharUtil[,2]) du script CARTINO_PARAM.R.
#' 
#' De plus, l'utilisateur peut insérer un nombre maximal de profils en travers intermédiaires 
#' à ne pas dépasser. Ce paramètre est inséré dans le vecteur dCharUtil[3] du script CARTINO_PARAM.R.  
#' 
#' Elle permet ensuite de créer automatiquement ces profils en travers intermédiaires.
#' 
#' Elle calcule les valeurs d'angle, LG, LD, X, Y, DIST, ECART, PG, PD pour chacun des profils rajoutés.
#' 
#' Ces résultats sont stockés l'un après l'autre un dans une liste puis rassemblés sous forme de matrice. 
#' 
#' Le champ NSection est renuméroté pour classer les profils en travers dans le bon ordre bief par bief.
#' 
#' Enfin, les résultats sont ré-insérés dans le script 99-RunReach.R pour qu'à chaque tour de la phase 2 
#' les profils densifiés selon la charge puissent être élargis et que leurs angles puissent être lissés.  
#'
#' Les points centraux XY des XS ne sont pas positionnés sur le squelette mais cela ne semble pas poser problème.

energy_densification = function(hydrau, xs_station, xs_table_iter, stream_df, i_reach, num_bv, num_reach, i_densi)
{  

  # Création de la liste qui va venir stocker les nouveaux profils intermediaires
  # List for the new sections
  List_PTi = list()
  indi=1
  
  # Variables
  dataframe1 = hydrau$res
  dataframe2 = xs_station           
  dataframe3 = xs_table_iter
  
  #st_write(dataframe2, file.path(rep_area, "test_xs_station.gpkg"), driver = "GPKG")
  
  compteur = 0
  
  for (i in 1:(dim(dataframe1)[1]-1))
  {
    
    # Si la différence de charge entre deux XS est supérieure à 0.3 mètres
    # et la distance entre deux sections consécutives est superieure à 5 mètres
    # If DeltaEnergy > dCharUtil[1] (value definied by the user)
    # and DeltaDIST > dCharUtil[2] (minimal distance definied by the user)
    if (((dataframe1$CHAR_m[i] - dataframe1$CHAR_m[i+1]) > dCharUtil[1]) & 
        ((dataframe1$DIST[i+1] - dataframe1$DIST[i]) > dCharUtil[2]))
    {
      
      compteur = compteur+1
      # Calcul du nombre de XS intermédiaires à rajouter
      # Calculate the number of cross sections to append
      nbre_profils_interm = min(min(floor((dataframe1$CHAR_m[i] - dataframe1$CHAR_m[i+1])/dCharUtil[1]),
                                    floor((dataframe1$DIST[i+1] - dataframe1$DIST[i])/ dCharUtil[2])),
                                dCharUtil[3])
    
      # Création des profils intermédiaires
      # Intermediate cross sections creation
      if (nbre_profils_interm>0)
      {
        for (alphai in 1:nbre_profils_interm)
        {
          alpha=alphai/(nbre_profils_interm+1)
          
          # Récupère la géométrie des deux profils en travers (sf object)
          # Get the geometry of the two existing cross sections (sf object)
          geom_XS1 = dataframe2$geometry[i]
          geom_XS2 = dataframe2$geometry[i+1]
          
          # Extrait les coordonnées des points (grâce à st_coordinates) et les stocke dans un dataframe
          # Extraction of the extremities coordinates 
          geom_matrix_XS1 = as.data.frame(st_coordinates(geom_XS1))
          geom_matrix_XS2 = as.data.frame(st_coordinates(geom_XS2))
          
          # Calcul des coordonnées des extrémités des nouveaux profils intermédiaires (plot)
          # Find the extremities of the new intermediate section (for the plot)
          X_new = ((alpha*geom_matrix_XS1$X[1:2]+(1-alpha)*geom_matrix_XS2$X[1:2])) 
          Y_new = ((alpha*geom_matrix_XS1$Y[1:2]+(1-alpha)*geom_matrix_XS2$Y[1:2]))  
          
          # Copie des données dans une nouvelle variable 
          # Intermediate dataframe
          dataframe2_data=dataframe2
          
          
          # # Plot du réseau hydrographique
          # # Stream network plot
          # plot(stream_sf_$new_Lines["ID"])
          # 
          # # Plot des points centraux des profils
          # # XY of existing sections plot
          # points(dataframe2_data$X, dataframe2_data$Y, pch="+", 
          #        xlim=c(min(dataframe2_data$X)-100, max(dataframe2_data$X)+100),
          #        ylim=c(min(dataframe2_data$Y)-100, max(dataframe2_data$Y)+100))
          # 
          # # Plot des extrémités des profils existants et création d'un segment entre les extrémités
          # # Extremities of existing sections, and existing lines plot
          # points(geom_matrix_XS1[,1],geom_matrix_XS1[,2], col="green", pch = "+")
          # points(geom_matrix_XS2[,1], geom_matrix_XS2[,2], col="green", pch = "+")
          # # lines(geom_matrix_XS1[,1], geom_matrix_XS1[,2], col="green")
          # # lines(geom_matrix_XS2[,1], geom_matrix_XS2[,2], col="green")
          
          # Calcul
          # Application de la formule sur la ligne (quasi) entière de dataframe2_data
          # Calculation
          st_geometry(dataframe2_data)=NULL
          tab_new=alpha*dataframe2_data[i,-c(2,3,11,14)]+(1-alpha)*dataframe2_data[i+1,-c(2,3,11,14)]  
          
          # Ecriture d'une variable contenant les extrémités sous forme de points (facultatif)
          # Extremities
          pointA = cbind(X_new[1],Y_new[1])
          pointB = cbind(X_new[2],Y_new[2])               
          
          # # Plot des nouvelles extrémités du profil intermédiaire  
          # # Plot of the new extremities
          # points(X_new[1],Y_new[1], col="red", pch="+", lty=2) 
          # points(X_new[2],Y_new[2], col="red", pch="+", lty=2)
          
          # # Plot du segment entre ces deux extrémités
          # # Plot of the new lines 
          # lines(X_new, Y_new, col= "red")
          
          
          
          # Calcul de l'angle
          # Angle calculation
          tab_new$ANGLE= atan2((Y_new[1]-Y_new[2]),(X_new[1]-X_new[2]))
          # = tab_new$ANGLE= atan((Y_new[1]-Y_new[2])/(X_new[1]-X_new[2]))
          
          # Création d'une colonne pour convertir radians en degrés (facultatif)
          # Conversion in degree (optional)
          tab_new=cbind(tab_new, "ANGLE_degres" = tab_new$ANGLE*57.3)
          
          # Ordonne les colonnes de tab_new, dans un nouveau data.frame : tab_new2
          # Organization of the fields
          w_iter=0
          NBief=dataframe2_data[i,11]
          smooth2=dataframe2_data[i,2]
          
          tab_new2=data.frame(cbind(tab_new[,1],smooth2[],w_iter[],tab_new[,c(2:8)],NBief[],tab_new[,c(9:10)]))
          colnames(tab_new2)=colnames(dataframe2_data)
          
          
          # Stocke les nouveaux profils intermédiaires sous forme de liste de data.frame
          # List of dataframes including new sections
          List_PTi[[indi]]=tab_new2[,]
          indi=indi+1
          
        }
      }
    }
  }
  # Transforme la liste en matrice
  # Matrix with the new sections
  tab_add=do.call(rbind,List_PTi)
  
  # S'il n'y a plus de nouveaux profils intermédiaires (dernier tour de la boucle)
  # If there are no more new sections
  if (is.null(tab_add))
  {
    tab_add = dataframe3
    tab_angle = NaN
    
    return(list(tab_energy_densi = tab_add, tab_angle = tab_angle, compteur = compteur))
    
  }
  
  # S'il y a des nouveaux profils intermédiaires (qui viennent d'être créés)
  # If there are new sections
  else
    
  {
    
    # Préparation du data.frame tab_add2 : ordonne, renomme et ne retient que les colonnes 
    # utiles de la matrice tab_add
    # (en prévision du match avec hydrau$profils)
    # Organization of the data.frame
    LM=0
    tab_add2 = cbind(tab_add[, "NSection"], LM, tab_add[, cbind("LD", "LG", "DIST" , "X", "Y")])
    colnames(tab_add2) = c("NSection", "LM", "LD", "LG", "DIST", "X", "Y") 
    
    
    # Rajout des nouveaux profils dans le dataframe tab_add2, enregistrement sous forme d'un nouveau dataframe
    # Add new sections in tab_add2
    tab_densi = data.frame()
    # if (i_densi == 1)
    # {
    #   # if (opt_mod=="flutor" & Strick[1]==20 & num_bv=="01" & i_reach==2 )
    #   # bug ici
    #   # tab_densi = rbind(tab_add2,dataframe3[,-c(8:10)])
    #   
      dataframe3_ = as.data.frame(dataframe3)
      dataframe3_ = dataframe3_[, cbind("NSection", "LM", "LD", "LG", "DIST", "X", "Y")]
      tab_densi = rbind(tab_add2,dataframe3_)
    # } else
    # {
    #   dataframe3_ = as.data.frame(dataframe3)
    #   dataframe3_ = dataframe3_[, cbind("NSection", "LM", "LD", "LG", "DIST", "X", "Y")]
    #   tab_densi = rbind(tab_add2,dataframe3_)
    # }
    
    tab_densi_ord= tab_densi[order(tab_densi[,5],decreasing=F), ]
    tab_densi_ord[,1] = 0
    tab_densi_ord[,1] = 1:length(tab_densi_ord[,1])
    
    
    list_geom_densif= list()
    
    for (i_tab_densi in 1:nrow(tab_densi_ord))
    {
      geometry_energy_densif = st_point(c(as.numeric(tab_densi_ord[i_tab_densi,6]), as.numeric(tab_densi_ord[i_tab_densi,7])))
      
      geometry_energy_densif = st_sf(data.frame("geometry" = st_sfc(geometry_energy_densif)))
      
      list_geom_densif[[i_tab_densi]]=geometry_energy_densif[,]
      i_tab_densi=i_tab_densi+1
    }
    
    tab_geometry_energy_densif = do.call(rbind, list_geom_densif)
    
    tab_densi2= st_sf(tab_densi_ord, "geometry"=tab_geometry_energy_densif$geometry)
    
    
    for (i_profil in 1:nrow(tab_densi2))
    {
      
      if (i_profil == 1)
      {
        tab_densi2 %>% dplyr::mutate("ECART" = 0)
        tab_densi2[i_profil, "ECART"] = tab_densi2[i_profil, "DIST"]
      }
      else
      {
        tab_densi2[i_profil,"ECART"] = tab_densi2[i_profil, "DIST"] - tab_densi2[i_profil-1, "DIST"]
        
      }
    } 

    # Stock des angles dans une variable qui est réinsérée dans le while (densification) 
    # lorsque i_densi = 2 (dans 99-RunReach.R)
    # For the angles
    tab_ang = data.frame(tab_add[,"ANGLE"], tab_add[,"DIST"]) #tab_densi2
    colnames(tab_ang) = c("ANGLE", "DIST")
    
    
    dataframe3_b = as.data.frame(dataframe3)
    dataframe3_b = dataframe3_b[, cbind("ANGLE", "DIST")]
    tab_angle_ = as.data.frame(rbind(tab_ang, dataframe3_b))
    
    tab_angle = tab_angle_[order(tab_angle_[,"DIST"],decreasing=F), ]
    tab_angle = cbind(tab_angle, "NSection" = (1:nrow(tab_angle)))
    
  }
  
  return(list(tab_energy_densi = tab_densi2, tab_angle = tab_angle, compteur = compteur))
}
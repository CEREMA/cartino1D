#' @title Création du réseau hydrographique - Prolongation des biefs après les zones de confluence
#' @description FR : Cette fonction est issue du script 00-StreamNetwork.R.
#' La fonction create_streamnet est appelée dans la première boucle du script CARTINO.R (boucle par secteur).
#' 
#' create_streamnet permet de créer le réseau hydrographique par secteur.
#' Cette étape est fondamentale, car sans réseau hydrographique, les étapes suivantes ne peuvent s'exécuter.
#' 
#' create_streamnet permet également, afin d'éviter les complexités hydrauliques des zones de confluence, 
#' de prolonger chaque bief après les zones de confluence. 
#'  
#' Dans le fichier paramètres (script CARTINO_PARAM.R), l'utilisateur entre dans "name_stream" le nom du shapefile 
#' contenant le réseau hydrographique.Exemple : Si la couche shapefile s'appelle BDCarthage.shp, l'utilisateur entrera 
#' name_stream = "BDCarthage". 
#'
#' Le shapefile sera lu par CARTINO, les différents secteurs seront ensuite séparés les uns des autres et enfin, 
#' la fonction create_streamnet pourra entrer en action.
#' 
#' 
#' La fonction create_streamnet permet donc, dans cet ordre précis :
#' 
#' - De ré-orienter l'amont et l'aval des biefs du réseau hydrographique, si celui-ci a été créé avec taudem notamment (facultatif) ;
#' 
#' - De convertir le réseau hydrographique lu depuis le shapefile en "objet sf" (linestring) avec le SCR défini, grâce au package sf ;
#'  
#' - De créer les jonctions du réseau (point amont et point aval des biefs) ;
#' 
#' - D'identifier les jonctions où sera effectuée la prolongation des biefs ;
#' 
#' - De prolonger les biefs (les géométries des biefs prolongés sont stockées dans la variable new_Lines) ;
#' 
#' - De calculer les longueurs des biefs prolongés en calculant l'écart des points qui les composent et la distance cumulée ;
#' 
#' La fonction create_streamnet retourne la géométrie des biefs prolongés, les écarts et la distance cumulée entre chaque point 
#' de construction du cours d'eau, ainsi que les jonctions. 
#' 
#' Les biefs prolongés classés de l'amont à l'aval sont stockés dans la variable Nbief.
#' 
#' 
#' Une fois que create_streamnet a lu le réseau hydrographique et a créé les objets nécessaires au bon fonctionnement de l'outil Cartino, 
#' la fonction exp_streamnet (du script 08_ExportGIS.R) est appelée dans le script CARTINO.R.
#' Celle-ci permet de créer le fichier "StreamNet.gpkg" qui est un fichier geopackage ouvrable dans un SIG (donnée de sortie).
#' Lorsque le geopackage s'ouvre, il contient deux couches (de résultats) : celle du réseau comprenant les différents biefs prolongés
#' et celle des jonctions (points de confluences). 
#'         
#' Ces deux fonctions sont liées l'une à l'autre : La fonction create_streamnet permet de créer la couche du réseau hydrographique,
#' ainsi que la couche des jonctions, tandis que la fonction exp_streamnet permet de les exporter sous forme de geopackage.         
#'
#' Le squelette entier (réseau hydrographique) et les jonctions seront également nécessaires pour la fonction graph_streamnet 
#' (du script 09-ExportGraphs.R), si l'option est activée dans le script CARTINO_PARAM.R (ne fonctionne pas actuellement). 
#' 
#' ENG : Create a stream network based on a spatial line.
#' Each stream is prolongated after the first intersection.       
#' @param stream_sf tableau de données qui lit le réseau hydrographique / dataframe with results
#' @param dist_down longueur de prolongation des biefs (en mètres) (à définir dans CARTINO_PARAM.R) 
#' / length of the prolongation for downstream boundary condition (see CARTINO_PARAM.R)
#' @return la fonction retourne un tableau de données avec les biefs prolongés et les jonctions 
#' / dataframe with new positions along the stream and widths

create_streamnet = function(stream_sf, dist_down) 
{
  ## change upside down if the lines were created with taudem 
  if(opt_reseau_aval)
  {
    for(i in 1:nrow(stream_sf))
    {
      coords_to_inverse<-as.data.frame(shp_reseau@lines[[i]]@Lines[[1]]@coords)       
      # LPo : @ = slots 
      coords_inversed<-coords_to_inverse[rev(rownames(coords_to_inverse)),]           
      # LPo : rev = inverse la géométrie des cours d'eau ?
      shp_reseau@lines[[i]]@Lines[[1]]@coords[,1]<-coords_inversed[,1]
      shp_reseau@lines[[i]]@Lines[[1]]@coords[,2]<-coords_inversed[,2]
    }
  }
  
  
  ## manipulate spatial object 
  shp_ = stream_sf %>% st_zm() %>% st_union() %>% st_line_merge() %>% st_cast("LINESTRING") 
  # LPo : x %>% y équivaut à y=f(x)
  # LPo : stream_sf = Contient le réseau hydrographique (objet sf qui est un tibble) 
  # LPo : avec plusieurs LINESTRING dans chaque bief
  # LPo : Il contient les colonnes : ID, FROM, TO, ID_, junctions1, NB, intersect2, length, geometry
  # LPo : st_cast = convertit un type de géométrie en un autre type de géométrie 
  # LPo : Ici conversion en LINESTRING (lignes spatiales objet sf)
  # LPo : st_ligne_merge et st_union : Fusion des LINESTRING pour former les biefs
  # LPo : (un sfg pour chaque bief) : à partir de différentes géométries, créé une géométrie simple
  # LPo : st_zm = Supprime les dimensions Z ou M
  # LPo : shp_ = Variable résultat : contient le réseau hydrographique avec un LINESTRING pour chaque bief
  
  shp_ = st_sf(ID=1:length(shp_), geom=shp_)   
  # LPo : Rajout d'une colonne de caractérisitiques dans shp_ : la colonne "ID"
  # LPo : et les géométries sont stockées dans la colonne "geom"
  
  
  ## identify watershed, when streams intersect
  buffer = shp_ %>% st_buffer(dist = 10) %>% st_union() %>% st_cast("POLYGON")
  # LPo : st_cast = Convertit un type de géométrie en un autre type de géométrie (ici en POLYGON)
  # LPo : st_union = Unifie les biefs pour qu'il n'y ait qu'un seul buffer pour chaque BV
  # LPo : st_buffer = Crée un tampon (POLYGON) autour du LINESTRING
  # LPo : buffer = Variable résultat : un tampon (POLYGON) par BV :
  # LPo : Elargissement autour du réseau de la valeur définie dans "dist" (résultat sous forme d'objet sf)
  
  buffer = st_sf(NBasVers=1:length(buffer), geom=buffer)
  # LPo : Rajout d'une colonne de caractérisitiques : 
  # LPo : le numéro de bassin versant (NBasVers) à l'objet sf "buffer"
  
  shp_ %<>% st_join(buffer)
  # LPo : st_join = Jointure entre la table de "shp_" et la table du tampon "buffer" 
  # LPo : La variable shp_ comprend désormais 2 champs en plus de la géométrie : 
  # LPo : l'ID et le numéro de bassin versant (NBasVers)
  # LPo : Concernant les géométries : cela reste un LINESTRING par bief
  
  
  ## streamnet with lines and endpoints
  struc_reseau = shp_ %>% as_Spatial() %>% shp2graph::readshpnw()
  # LPo : readshpnw = Permet la lecture du réseau hydrographique en tant que SpatialLinesDataFrame 
  # LPo : Cette ligne récupère toutes les informations sur la structure du réseau et les stocke dans struc_reseau
  # LPo : as_Spatial = Convertit un sf en un SpatialLinesDataFrame 
  # LPo : struc_reseau = Variable qui contient 7 dataframes (liste de dataframes) : 
  # LPo : struc_reseau est donc un SpatialLinesDataFrame : 
  # LPo : Il contient la structure du réseau (les points de jonctions et le squelette)
  
  junctions = st_as_sf(data.frame(matrix(unlist(struc_reseau[[2]][,2]), ncol=2, byrow=T)), 
                       coords = c("X1","X2"))
  # LPo : struc_reseau = Le SpatialLinesDataFrame qui contient toutes les informations sur le réseau
  # LPo : unlist() = Convertit une liste en un vecteur contenant tous les éléments de cette liste
  # LPo : matrix() = Crée une matrice
  # LPo : byrow=T = Argument de matrix() : si byrow=T la matrice est remplie par lignes (et non par colonnes)
  # LPo : ncol=2 = Argument de matrix() : le nombre souhaité de colonne
  # LPo : data.frame() = Crée un tableau de données
  # LPo : st_as_sf = Convertit un dataframe en un "objet sf"
  # LPo : coords = Argument de st_as_sf(). En cas de données sous forme de points, cela permet de nommer ou 
  # LPo : de dénombrer les colonnes contenant les coordonnées
  # LPo : Donc dans la variable junctions, on récupère les informations concernant les jonctions 
  # LPo : ([[2]] = deuxième dataframe du SpatialLinesDataFrame (liste de dataframes)
  # LPo : cad deuxième dataframe de struc_reseau et on récupère [,2] sa deuxième colonne)
  # LPo : junctions est donc un objet sf qui contient la géométrie des points de jonctions (objet sf POINT).
  
  st_crs(junctions) = st_crs(stream_sf)
  # LPo : st_crs = Identification du SCR d'un objet sf
  # LPo : Cette ligne permet d'appliquer le même SCR à la variable qui contient les jonctions, que le SCR 
  # LPo : de la variable qui contient le réseau hydrographique (ici 2154)
  
  
  ## find downstream reach for prolongation
  struc_reseau_ = struc_reseau[[3]] %>% data.frame()
  # LPo : Cette ligne récupère le troisième dataframe de la liste de dataframe struc_reseau 
  # LPo : Il s'agit d'un dataframe contenant 3 colonnes :
  # LPo : - le numéro du bief, 
  # LPo : - d'où vient le bief (numéro de bief amont) 
  # LPo : - et où il va (numéro de jonction à l'aval du bief). 
  # LPo : Résultat stocké dans la variable struc_reseau_
  
  names(struc_reseau_) = c("NSection", "FROM", "TO")
  # LPo : Nomme ces trois colonnes respectivement "NSection", "FROM" et "TO"
  
  struc_reseau_ %<>% mutate(ID_ = NSection)
  # LPo : mutate = Permet d'ajouter une colonne dans un data.frame
  # LPo : ID_ est ajouté à la table de struc_reseau_ et est identique au champ NSection
  
  struc_reseau_ %<>% 
    dplyr::select(TO) %>% 
    left_join(dplyr::select(struc_reseau_, ID_, FROM), by=c("TO"="FROM")) %>%
    bind_cols(dplyr::select(struc_reseau_, NSection))
  # LPo : select {package dplyr} = Permet de sélectionner uniquement les colonnes que l'on souhaite
  # LPo : Sélectionne la colonne "TO" (numéro de jonction à l'aval du bief)
  # LPo : left_join = Jointure à gauche. "by" permet de définir la variable de jointure (ici lorsque "TO"="FROM")
  # LPo : Copie dans le champ ID_ de struc_reseau_ le numéro du bief où le bief se déverse 
  # LPo : et supprime le champ "FROM"
  # LPo : struc_reseau_ contient désormais 
  # LPo : - le champ NSection (le numéro du bief), 
  # LPo : - le champ "TO" qui identifie le numéro de la jonction avale du bief,
  # LPo : - et le champ ID_ qui est le numéro du bief où le bief étudié se déverse
  

  
  ## create a new prolongated reach
  # LPo : Boucle pour prolonger les biefs (ne s'applique pas sur le tronçon aval du réseau)
  new_lines = list()                              
  # LPo : Création d'une liste vide new_lines
  
  for (i in 1:nrow(struc_reseau_))                
  # LPo : Pour toutes les lignes de struc_reseau_ (donc bief 1 puis bief 2 etc.)
  {
    if (!is.na(struc_reseau_[i, "ID_"]))          
    # LPo : Boucle sur les valeurs de la colonne ID_ de struc_reseau_ s'il n'y a pas de valeurs NA dedans : 
    # LPo : Détecte donc tous les biefs, sauf le tout dernier tronçon aval du réseau
    {
      ## les lignes sont orientées amont-aval
      tmp_dist =                                                                                 
        shp_[struc_reseau_[i, "ID_"],] %>% st_cast("POINT", warn=F) %>% st_coordinates()     
      # LPo : Pour toutes les lignes du champ ID_ du dataframe struc_reseau_ (bief dans lequel le bief se déverse), 
      # LPo : on extrait les coordonnées des points de construction du bief dans lequel le bief étudié se déverse :
      # LPo : Coordonnées stockés dans tmp_dist (matrice)
      
      tmp_dist %<>%
        data.frame() %>% 
        # LPo : tmp_dist devient un data.frame
        mutate(dist = sqrt( ((X-lag(X))**2 + (Y-lag(Y))**2) ),        
               # LPo : lag permet de décaler une série 
               # LPo : Dans dist sont stockées les distances entre 
               # LPo : chaque point de construction du cours d'eau
               # LPo : Création d'une nouvelle colonne (grâce à mutate) qui s'appelle dist
               dist = ifelse(is.na(dist), 0, dist),                   
               # LPo : dans "dist", les valeurs NA sont remplacées par 0
               cumdist = cumsum(dist)) %>%                            
               # LPo : cumdist = Calcul de la distance cumulée
        filter(cumdist < dist_down)                                   
      ## we only keep a part of the stream downstream   
      # LPo : On conserve uniquement la distance cumulée inférieure 
      # LPo : à la valeur de dist_bief_aval 
      # LPo : (fixée par l'utilisateur dans CARTINO_PARAM.R 
      # LPo : dans dist_bief_aval qui devient dist_down dans CARTINO.R)
      # LPo : tmp_dist = Contient les coordonnées des points de construction,
      # LPo : la distance entre eux ainsi que la distance cumulée 
      # LPo : sur les 500 premiers mètres (=dist_down)
      # LPo : du bief dans lequel le bief étudié se déverse
      # LPo : cad les 500 mètres (=dist_down) à l'aval du bief étudié
      
      
      tmp_ =                                                                                      
        shp_[struc_reseau_[i, "NSection"],] %>% st_cast("POINT", warn=F) %>% st_coordinates()     
      # LPo : Extrait les points de construction du au bief étudié (que l'on veut prolonger)
      # LPo : qui était stocké dans shp_ sous forme de LINESTRING
      
      tmp_line = st_linestring(rbind(tmp_, as.matrix(tmp_dist[, c("X", "Y")])))                   
      # LPo : Combine tmp_dist (l'amont du bief aval) et tmp_ (le bief amont que l'on veut prolonger) 
      # LPo : tmp_line est un objet sf qui contient la géométrie du bief prolongé
      
      tmp_new = shp_[struc_reseau_[i, "NSection"], ]                                              
      # LPo : tmp_new = Récupère la géométrie du bief que l'on veut prolonger
      
      st_geometry(tmp_new) = st_sfc(tmp_line); st_crs(tmp_new) = st_crs(stream_sf)                
      # LPo : La géométrie de tmp_new devient celle de tmp_line (LINESTRING, objet sf) :
      # LPo : Le bief est prolongé
      # LPo : De plus on récupère le même SCR que stream_sf pour l'appliquer sur tmp_new (ici2154)
      
      new_lines[[i]] = tmp_new                                                                    
      # LPo : On stocke le bief prolongé dans la liste new_lines, un par un grâce à la boucle
      
    } else new_lines[[i]] = shp_[struc_reseau_[i, "NSection"], ]                            
    # LPo : else : S'il y a des valeurs NA dans la colonne ID_ de struc_reseau_
    # LPo : (comme dans le cas du tronçon tout à l'aval) : 
    # LPo : Copie la ligne de struc_reseau_ directement dans new_lines 
  }
  
  new_Lines = do.call(rbind, new_lines)                            
  # LPo : Copie les résultats de la liste new_lines dans la matrice new_Lines 
  # LPo : Créée à l'aide de do.call 
  
  # LPo : MORCEAU DE CODE POUR REMPLACER LA LIGNE : new_Lines %<>% group_by(NBasVers) %>% mutate(NBief=1:n())
  # new_Lines2 = list()
  # 
  # for (i_bv in 1:length(unique(new_Lines$NBasVers)))
  # {
  #   indice_bv = unique(new_Lines$NBasVers)[i_bv]
  #   
  #   bv = new_Lines[which(new_Lines$NBasVers == indice_bv),]
  #   n_bv = nrow(bv)
  #   
  #   bv = data.frame(bv, "NBief" = 1:n_bv)
  #   
  #   new_Lines2[[i_bv]] = bv[,]
  #   i_bv=i_bv+1
  # }
  # 
  # new_Lines = st_sf(do.call(rbind, new_Lines2))
  
  
  
  new_Lines %<>% dplyr::group_by(NBasVers) %>% dplyr::mutate(NBief=1:n())
  # LPO : LIGNE D'ORIGINE : new_Lines %<>% group_by(NBasVers) %>% mutate(NBief=1:n())  
  # LPo : Groupe les biefs d'un même BV et les numérote de l'amont à l'aval 
  # LPo : (= rajout de la colonne NBief, qui comprend donc les biefs numérotés de l'amont à l'aval par BV)
  
  ## estimate the distance along the reach                                   
  # LPo : calcule les longueurs des biefs qui ont été prolongés, 
  # LPo : en calculant l'écart entre les points qui les composent
  df_stream =                                                                 
    new_Lines %>% sf::st_coordinates() %>% data.frame() %>% dplyr::group_by(L1) %>%
    dplyr::mutate(ecart = sqrt((X-lag(X))^2+(Y-lag(Y))^2)) %>%                      
    dplyr::mutate(ecart = ifelse(is.na(ecart), 0, ecart)) %>%                       
    dplyr::mutate(dist_cum = cumsum(ecart))
  # LPo : LIGNE D'ORIGINE : df_stream = new_Lines %>% st_coordinates() %>% data.frame() %>% group_by(L1) %>%
  # LPo : Groupe les points par "L1" (=le numéro du bief)
  # LPo : df_stream est un tibble où sont stockés les coordonnées
  # LPo : des points de construction du réseau (extraits avec st_coordinates)
  # LPo : et qui contient également les écarts entre chaque point du réseau et la distance cumulée
  
    
  #  LPo : 
 # df_stream = data.frame(df_stream, ecart = sqrt((df_stream$X-lag(df_stream$X))^2+(df_stream$Y-lag(df_stream$Y))^2)) #%>%                      
 #    # LPo : Calcule l'écart entre les points des biefs
 #    
 #  df_stream$ecart = ifelse(is.na(df_stream$ecart), 0, df_stream$ecart) #%>%                       
 #    # LPo : Dans "ecart", les valeurs NA sont remplacées par 0
 #    
 #   df_stream = data.frame(df_stream, dist_cum = cumsum(df_stream$ecart))                                         
  # LPo : Distance cumulée = somme des écarts

  
  
  ## graph
  if (do_graph) graph_streamnet(new_Lines, junctions)                                  
  # LPo : Si do_graph = TRUE, alors exécuter la fonction graph_streamnet du script 09-ExportGraphs.R
  
  
  return(list(new_Lines=new_Lines, df_stream=df_stream, junctions=junctions))          
  # LPo : Fin de la fonction create_streamnet
  # LPo : Retourne la liste comprenant la géométrie des nouveaux biefs prolongés, 
  # LPo : les écarts entre les points des biefs, 
  # LPo : la distance cumulée, ainsi que les jonctions (= tous les résultats)

  
  
  }



#' estimate where to exclude cross sections based on their positions
#' @noRd 
prep_exclude_sections = function(nfile_bridge, dist_extra, shp_reach)
{
  sf_bridge = sf::read_sf(nfile_bridge)
  sf_bridge = st_crop(sf_bridge, stream_sf)
  pts_along = st_line_sample(shp_reach, density=1) %>% st_cast("POINT")
  pts_along = st_buffer(pts_along, 0.5)
  sf_bridge_int = st_intersects(pts_along, sf_bridge)
  n_int = which(sapply(sf_bridge_int, length)>0)
  if(length(n_int)>0)
  {
    dist_ex = NULL
    for (i in 1:length(n_int))
    {
      dist_ex = c(dist_ex, seq(n_int[i]-dist_extra, n_int[i]+dist_extra))
    }
    dist_ex = unique(dist_ex)
  }
  vec_dist = 0:st_length(shp_reach)
  val_dist = rep(1, length=length(vec_dist))
  val_dist[dist_ex] = 0
  interpol_bief_bridge = approxfun(vec_dist, val_dist)
  return(interpol_bief_bridge)
}


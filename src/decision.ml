(**

   Chers programmeuses et programmeurs de λman, votre mission consiste
   à compléter ce module pour faire de vos λmen les meilleurs robots
   de la galaxie. C'est d'ailleurs le seul module qui vous pouvez
   modifier pour mener à bien votre mission.

   La fonction à programmer est

         [decide : memory -> observation -> action * memory]

   Elle est appelée à chaque unité de temps par le Λserver avec de
   nouvelles observations sur son environnement. En réponse à ces
   observations, cette fonction décide quelle action doit effectuer le
   robot.

   L'état du robot est représenté par une valeur de type [memory].  La
   fonction [decide] l'attend en argument et renvoie une nouvelle
   version de cette mémoire. Cette nouvelle version sera passée en
   argument à [decide] lors du prochain appel.

*)

open World
open Space

(** Le Λserver transmet les observations suivantes au λman: *)
type observation = World.observation

(** Votre λman peut se déplacer : il a une direction D et une vitesse V.

    La direction est en radian dans le repère trigonométrique standard :

    - si D = 0. alors le robot pointe vers l'est.
    - si D = Float.pi /. 2.  alors le robot pointe vers le nord.
    - si D = Float.pi alors le robot pointe vers l'ouest.
    - si D = 3 * Float.pi / 2 alors le robot pointe vers le sud.
    (Bien entendu, ces égalités sont à lire "modulo 2 * Float.pi".)

    Son déplacement en abscisse est donc V * cos D * dt et en ordonnée
   V * sin D * dt.

    Votre λman peut communiquer : il peut laisser du microcode sur sa
   position courante pour que d'autres λmen puissent le lire.  Un
   microcode s'autodétruit au bout d'un certain nombre d'unités de
   temps mais si un microcode est laissé près d'un autre microcode
   identique, ils fusionnent en un unique microcode dont la durée de
   vie est le somme des durées de vie des deux microcodes initiaux.
   Construire un microcode demande de l'énergie au robot : chaque
   atome lui coûte 1 point d'énergie. Heureusement, l'énergie augmente
   d'1 point toutes les unités de temps.

    Pour terminer, votre λman peut couper des arbres de Böhm. Les
   arbres de Böhm ont un nombre de branches variables. Couper une
   branche prend une unité de temps et augmente le score de 1
   point. Si on ramène cette branche au vaisseau, un second point est
   accordé.

    Pour finir, le monde est malheureusement très dangereux : on y
   trouve des bouches de l'enfer dans lesquelles il ne faut pas tomber
   ainsi que des champs de souffrances où la vitesse de votre robot
   est modifiée (de -50% à +50%).

*)

type action =
  | Move of Space.angle * Space.speed
  (** [Move (a, v)] est l'angle et la vitesse souhaités pour la
     prochaine unité de temps. La vitesse ne peut pas être négative et
     elle ne peut excéder la vitesse communiquée par le serveur. *)

  | Put of microcode * Space.duration
  (** [Put (microcode, duration)] pose un [microcode] à la position courante
      du robot. Ce microcode s'autodétruira au bout de [duration] unité de
      temps. Par contre, s'il se trouve à une distance inférieure à
      [Space.small_distance] d'un microcode similaire, il est fusionné
      avec ce dernier et la durée de vie du microcode résultant est
      la somme des durées de vide des deux microcodes. *)

  | ChopTree
  (** [ChopTree] coupe une branche d'un arbre de Böhm situé une distance
      inférieure à [Space.small_distance] du robot. Cela augmente le score
      de 1 point. *)

  | Wait
  (** [Wait] ne change rien jusqu'au prochain appel. *)

  | Die of string
  (** [Die] est produit par les robots dont on a perdu le signal. *)

[@@deriving yojson]

(**

   Le problème principal de ce projet est le calcul de chemin.

   On se dote donc d'un type pour décrire un chemin : c'est une
   liste de positions dont la première est la source du chemin
   et la dernière est sa cible.

*)
type path = Space.position list

(** Version lisible des chemins. *)
let string_of_path path =
  String.concat " " (List.map string_of_position path)

(**

   Nous vous proposons de structurer le comportement du robot
   à l'aide d'objectifs décrits par le type suivant :

*)
type objective =
  | Initializing            (** Le robot doit s'initialiser.       *)
  | Chopping                (** Le robot doit couper des branches. *)
  | GoingTo of path * path
  (** Le robot suit un chemin. Le premier chemin est la liste des
      positions restantes tandis que le second est le chemin initial.
      On a donc que le premier chemin est un suffixe du second. *)

(** Version affichable des objectifs. *)
let string_of_objective = function
  | Initializing -> "initializing"
  | Chopping -> "chopping"
  | GoingTo (path, _) ->
     Printf.sprintf
       "going to %s" (String.concat " " (List.map string_of_position path))

(**

  Comme dit en introduction, le robot a une mémoire qui lui permet de
   stocker des informations sur le monde et ses actions courantes.

  On vous propose de structurer la mémoire comme suit:

*)
type memory = {
    known_world : World.t option;      (** Le monde connu par le robot.     *)
    graph       : Graph.t;             (** Un graphe qui sert de carte.     *)
    objective   : objective;           (** L'objectif courant du robot.     *)
    targets     : Space.position list; (** Les points où il doit se rendre. *)
}

(**

   Initialement, le robot ne sait rien sur le monde, n'a aucune cible
   et doit s'initialiser.

*)
let initial_memory = {
    known_world = None;
    graph       = Graph.empty;
    objective   = Initializing;
    targets     = [];
}

(**

   Traditionnellement, la fonction de prise de décision d'un robot
   est la composée de trois fonctions :

   1. "discover" qui agrège les observations avec les observations
      déjà faites dans le passé.

   2. "plan" qui met à jour le plan courant du robot en réaction
      aux nouvelles observations.

   3. "next_action" qui décide qu'elle est l'action à effectuer
       immédiatement pour suivre le plan.

*)

(** [discover] met à jour [memory] en prenant en compte les nouvelles
    observations. *)
let discover visualize observation memory =
   let seen_world = World.world_of_observation observation in
   let known_world =
      match memory.known_world with
      | None -> seen_world
      | Some known_world -> World.extends_world known_world seen_world
   in
   if visualize then Visualizer.show ~force:true known_world;
   Visualizer.show_graph memory.graph;
   { memory with known_world = Some known_world }


(**

   Pour naviguer dans le monde, le robot construit une carte sous la
   forme d'un graphe.

   Les noeuds de ce graphe sont des positions clées
   du monde. Pour commencer, vous pouvez y mettre les sommets des
   polygones de l'enfer, le vaisseau, le robot et les arbres.

   Deux noeuds sont reliés par une arête si le segment dont ils
   sont les extremités ne croisent pas une bouche de l'enfer.

*)

(*Renvoie une node qui n'est pas en enfer par rapport a pos*)
let correctHellNode pos world = match pos with
      | (x,y) -> if(World.inside_hell world ((x +. 1.),(y +. 1.))) then
                     ((x +. (-1.)),(y +. (-1.))) 
                 else if(World.inside_hell world ((x +. (-1.)),(y +. 1.))) then
                     ((x +. 1.),(y +. (-1.)))
                 else if(World.inside_hell world ((x +. 1.),(y +. (-1.)))) then
                     ((x +. (-1.)),(y +. 1.))
                 else if(World.inside_hell world ((x +. (-1.)),(y +. (-1.)))) then
                     ((x +. 1.),(y +. 1.))
                 else
                     (x,y)
   
let suffering_segments world = List.(
  Space.polygons world.space (( <> ) Hell)
  |> map Space.polygon_segments
  |> flatten
)

let addHellToGraph graph world =
   let segmentsHell = World.hell_segments world in
   let segmentsSuffering = suffering_segments world in
      
      let rec aux seg graph = match seg with
         | [] -> graph
         | x :: y -> let graph = Graph.add_node graph (correctHellNode (fst x) world) in
                     let graph = Graph.add_node graph (correctHellNode (snd x) world) in
                     aux y graph; 
   in let graph = aux segmentsHell graph in
   aux segmentsSuffering graph


let rec addTreesToGraph graph t = match t with
  | [] -> graph
  | x :: y -> if x.branches > 0
              then Graph.add_node (addTreesToGraph graph y) x.tree_position
              else addTreesToGraph graph y

let addNodesToGraph observation memory =
   let graph = Graph.empty in
   let graph = match memory.known_world with
      | Some t -> addHellToGraph graph (Option.get memory.known_world)
      | None -> graph
   in let graph = addTreesToGraph graph observation.trees in
   let graph = Graph.add_node graph observation.position in
   let graph = Graph.add_node graph observation.spaceship in
   graph

(*Retourne vrai si edge passe par l'enfer*)
let isEdgeInHell edge memory =
   let p1 = let t = fun (x,y,z) -> x in t edge in
   let q1 = let t = fun (x,y,z) -> y in t edge in
   
   List.exists (fun x -> Space.segment_intersects (p1,q1) ((fst x),(snd x))) (World.hell_segments (Option.get memory.known_world))

let sq x = x *. x
let distance (x, y) (x', y') = sqrt (sq (x' -. x) +. sq (y' -. y))

let applySufferingToEdge edge memory = match edge with
   | (x,y,z) -> let rec aux weight pos = 
                  if (Space.close pos y ((distance x y) /. 10.)) then (x,y,weight)
                  else
                  aux (weight +. ((weight /. 10.) *. World.suffering (Option.get memory.known_world) pos)) (((fst pos) +. ((fst y -. fst x) /. 10.)),((snd pos) +. ((snd y -. snd x) /. 10.)))
                 in aux z x

(*Retourne un graphe où les edges list ont été ajoutés*)
let rec addEdgeListToGraph list graph memory = match list with
   | [] -> graph
   | x :: y -> addEdgeListToGraph y (Graph.add_edge graph x) memory

   (*Retourne une liste de edge valides où node est connecté avec les nodes de la liste*)
let rec connectNodeToList node list memory = match list with
   | [] -> []
   | x :: y -> if not ((isEdgeInHell (node, x, 1.) memory)) then [(node, x, (distance node x))] @ connectNodeToList node y memory else connectNodeToList node y memory

(*Renvoie un edge donné avec le poid du suffering appliqué*)



(*Ajoute la liste des edges a un graphe déjà formé de nodes*)
let addEdges graph memory =
   let rec list nodeList = match nodeList with
      | [] -> []
      | x :: y -> connectNodeToList x y memory @ list y 
   in addEdgeListToGraph (list (Graph.nodes graph)) graph memory

let visibility_graph observation memory =
   let graph = addNodesToGraph observation memory in
   let graph = addEdges graph memory in
   graph

(**

   [plan] doit mettre à jour la mémoire en fonction de l'objectif
   courant du robot.

   Si le robot est en train de récolter du bois, il n'y a rien à faire
   qu'attendre qu'il est fini.

   Si le robot est en train de suivre un chemin, il faut vérifier que
   ce chemin est encore valide compte tenu des nouvelles observations
   faites, et le recalculer si jamais ce n'est pas le cas.

   Si le robot est en phase d'initialisation, il faut fixer ses cibles
   et le faire suivre un premier chemin.

*)

(*Si le robot est en train de chop, on ne change pas le plan
  Si le robot se rends a une position (x,y), on tente de decouvrir les environs en mettant a jour son plan
  Si le robot est en train de s'initialiser, alors on lui donne toutes les informations dont il a besoin et on le fait revenir au vaisseau*)
module K = struct
   type t = position * position list
   let compare = compare
end

module P = struct
   type t = float
   let compare = compare
   let infinity = infinity
   let to_string t = string_of_float t
end

module PQueue = PriorityQueue.Make (K) (P)

type dijkstraNode = position * position list

(*Prends un edge en argument et renvoie le Some node corresponds a l'autre coté du robot*)
let sideOfEdge edge originNode = match edge with
   | (x,y,z) -> if (x = originNode) then y else x

let weigthOfEdge edge = match edge with
   | (x,y,z) -> z

(*Renvoie true ou false is le node donné est un tree*)
let isNodeTree graph node observation = match (World.tree_at observation.trees node) with
   | Some t -> if t.branches > 0 then true else false
   | None   -> false

let noMoreTrees graph observation = match (List.find_opt (fun x -> x.branches > 0) observation.trees) with
   | Some t -> false
   | None   -> true

(*Mets a jour la queue donné avec les nouveaux nodes de poid adapté depuis le node donné*)
let updateAndPush queue graph dnode prio observation = 
   
   match dnode with
      | ((x,y),z) -> let rec aux q list = (match list with
         | edge :: l -> if ((close (sideOfEdge edge (x,y)) observation.position 1.) || ((weigthOfEdge edge) +. prio) = prio)then aux q l else (aux (PQueue.insert q ((sideOfEdge edge (x,y)),z @ [sideOfEdge edge (x,y)]) ((weigthOfEdge edge) +. prio)) l)
         | [] -> q) in
      aux queue (Graph.out graph (x,y))

      
(*Loop principale de djisktra, objective = True means search for a tree, False for the mothership*)
let rec dijkstraLoop queue graph observation =
   let min = PQueue.get_min queue in
   let q = PQueue.remove_min queue in
   match min with 
      | Some (prio,((x,y),z)) -> if ((isNodeTree graph (x,y) observation) || ((noMoreTrees graph observation = true) && ((x,y) = observation.spaceship))) then
                              z
                          else
                              dijkstraLoop (updateAndPush q graph ((x,y),z) prio observation) graph observation  
      | None   -> []

(*Fonction principale*)
let dijkstra graph observation =
   let q1 = PQueue.empty in
   let q2 = PQueue.insert q1 (observation.position,[]) 0. in
   dijkstraLoop q2 graph observation
   
(*
   Il nous suffit maintenant de trouver le chemin le plus rapide pour
   aller d'une source à une cible dans le graphe.
*)

let shortest_path graph observation : path =
   dijkstra graph observation


let plan visualize observation memory = match memory.objective with
   | Chopping -> memory
   | GoingTo(x,y) -> {memory with graph = visibility_graph observation memory}
   | Initializing -> 
   let g = visibility_graph observation memory in{
      known_world = memory.known_world;
      graph       = g;
      targets     = World.tree_positions observation.trees;
      objective   = GoingTo(shortest_path g observation,[observation.spaceship]);
    }

(**

   Next action doit choisir quelle action effectuer immédiatement en
   fonction de l'objectif courant.

   Si l'objectif est de s'initialiser, la plannification a mal fait
   son travail car c'est son rôle d'initialiser le robot et de lui
   donner un nouvel objectif.

   Si l'objectif est de couper du bois, coupons du bois! Si on vient
   de couper la dernière branche, alors il faut changer d'objectif
   pour se déplacer vers une autre cible.

   Si l'objectif est de suivre un chemin, il faut s'assurer que
   la vitesse et la direction du robot sont correctes.

*)

let getX = function (x,y) -> x
let getY = function (x,y) -> y

let isValidPath graph source target = match List.find_opt (fun x -> (sideOfEdge x source) = target) (Graph.out graph source) with
   | Some t -> true
   | None -> false
   
let angleOfPos pos pos2 =
   let cx = (getX pos) -. (getX pos2) in
   let cy = (getY pos) -. (getY pos2) in
   (Float.atan2 cy cx)

let next_action visualize observation memory = match memory.objective with
   | Initializing -> (Wait, memory)
   | Chopping -> (match (World.tree_at observation.trees observation.position) with
      | Some t -> if(t.branches > 0) then
                     (ChopTree, memory)
                  else
                     (Wait, {memory with objective = Initializing})
      | None -> (Wait, {memory with objective = Initializing}))

   | GoingTo(x,y) -> match x with
      | a :: b -> if (Space.close observation.position a 1.) then
                     (Move (Space.angle_of_float 0., Space.speed_of_float 0.), {memory with objective = GoingTo(b,y @ [a])})
                  else if ((isValidPath memory.graph observation.position a) = false) then
                     ((Move (Space.angle_of_float 0., Space.speed_of_float 0.)), {memory with objective = Initializing})
                  else (Move(Space.angle_of_float (angleOfPos a observation.position), Space.speed_of_float 300.), {memory with objective = GoingTo(x,y)})
      | [] -> (Move (Space.angle_of_float 0., Space.speed_of_float 0.), {memory with objective = Chopping})
      

(**

   Comme promis, la fonction de décision est la composition
   des trois fonctions du dessus.

*)
let decide visualize observation memory : action * memory =
  let memory = discover visualize observation memory in
  let memory = plan visualize observation memory in
  next_action visualize observation memory

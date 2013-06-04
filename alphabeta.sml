functor AlphaBeta (Settings : sig
                                  structure G : GAME
                                  val search_depth : int
                  end) : PLAYER where type Game.move = Settings.G.move
                 				where type Game.state = Settings.G.state =
struct
  structure Game = Settings.G
  
  open Game

  structure ShEst = ShowEst(Game)

  type edge = (Game.move * Game.est)

  datatype value =
      BestEdge of edge
    | Pruned

  fun le (x,y) =
      case x = y of
          true => true
        | false => (* they're not equal *)
              case (x,y) of
                  (Game.Definitely (Game.Winner Game.Minnie), _) => true
                | (_, Game.Definitely (Game.Winner Game.Maxie)) => true
                | (Game.Guess x, Game.Definitely Game.Draw) => x <= 0
                | (Game.Definitely Game.Draw, Game.Guess x) => 0 <= x
                | (Game.Guess x, Game.Guess y) => x <= y
                | (_, _) => false

  fun compare(x,y) =
    case (le(x,y), le(y,x)) of
        (true,true) => EQUAL
      | (true,false) => LESS
      | (false,true) => GREATER
      | (false,false) => raise Fail "No relationship"

  fun lt (x,y) = le (x,y) andalso not (compare(x,y) = EQUAL)

  fun valueToString (v : value) : string =
      case v of 
          Pruned => "Pruned" 
        | BestEdge (_,e) => "Value(" ^ ShEst.toString e ^ ")"

  type alphabeta = value * value (* invariant: alpha < beta *)
  fun abToString (a,b) = "(" ^ valueToString a ^ "," ^ valueToString b ^ ")"

  (* for alpha, we want max(alpha,Pruned) to be alpha, i.e.
     Pruned <= alpha for any alpha;
     otherwise order by the estimates on the edges
     *)
  fun alpha_is_less_than (alpha : value, v : Game.est) : bool =
      case alpha of
          Pruned => true
        | BestEdge(_,alphav) => lt(alphav,v)
  fun maxalpha (v1,v2) : value =
      case (v1,v2) of
          (Pruned,y) => y
        | (x,Pruned) => x
        | (BestEdge(_,e1), BestEdge(_,e2)) => 
              case lt (e1,e2) of true => v2 | false => v1

  (* for beta, we want min(beta,Pruned) to be beta, i.e.
     beta <= Pruned for any beta;
     otherwise order by the estimates on the edges
     *)
  fun beta_is_greater_than (v : Game.est, beta : value) : bool =
      case beta of
          Pruned => true
        | BestEdge(_,betav) => lt(v,betav)
  fun minbeta (v1,v2) : value =
      case (v1,v2) of
          (Pruned,y) => y
        | (x,Pruned) => x
        | (BestEdge(_,e1), BestEdge(_,e2)) => 
              case lt (e1,e2) of true => v1 | false => v2

  (* Task 4.1 *)
  fun updateAB (state : Game.state) 
               ((alpha, beta) : alphabeta)
               (value : value) : alphabeta = 
    let val p = Game.player state
       in
        case p of Maxie (* The node is Maxie! *) =>
            ((maxalpha(alpha,value)),beta)
         | Minnie (* The node is Minnie! *) =>
            (alpha,(minbeta(beta,value)))
       end

  (* Task 4.2 *)
  fun value_for (state : Game.state) ((alpha, beta) : alphabeta) : value = 
    case (Game.player state) of Maxie => alpha | Minnie => beta

  datatype result = Value of value | ParentPrune   (* an evaluation result *)
  fun resultToString r = 
      case r of Value v => valueToString v | ParentPrune => "ParentPrune"

  (* Task 4.3 *)
  fun check_bounds ((alpha,beta) : alphabeta)
                   (state : Game.state)
                   (incomingMove : Game.move)
                   (v : Game.est) : result = 
    if (alpha_is_less_than(alpha,v))andalso(beta_is_greater_than(v,beta)) then
        Value(BestEdge(incomingMove,v))
    else if not(alpha_is_less_than(alpha,v)) then case (Game.player state) of
        Maxie => ParentPrune
         | Minnie => Value(Pruned)
    else (* MM >= B! *) case (Game.player state) of
        Maxie => Value(Pruned)
         | Minnie => ParentPrune

  (* Task 4.4 *)
  fun evaluate (depth : int) 
               (ab : alphabeta)
               (state : Game.state)
               (incomingMove : Game.move) : result = 
    (* check boundaries (game over or depth 0) *)
    case (Game.status state) of Over(v) =>
        check_bounds (ab) (state) (incomingMove) (Game.estimate state)
     | In_play => 
        case depth of 0 => 
            check_bounds (ab) (state) (incomingMove) (Game.estimate state)
    (* We're good, search! *)
         | _ => case (search (depth) (ab) (state) (Game.moves(state))) of
                    Pruned => Value(Pruned)
                     | BestEdge(move,est) => check_bounds (ab) (state)
                        (incomingMove) (est)

  and search (depth : int) 
             (ab : alphabeta)
             (state : Game.state) 
             (moves : Game.move Seq.seq):value =
        case Seq.showl(moves) of Seq.Nil => value_for(state) (ab)
         | Seq.Cons(x,xs) => 
                case evaluate (depth-1) (ab) 
                    (Game.make_move(state,x)) 
                        (x) of ParentPrune => Pruned
                | Value(v) => (search (depth) (updateAB state ab v) 
                    (state) (xs))

  (* Task 4.5 *)
  fun next_move s = case search (Settings.search_depth) 
    (Pruned,Pruned) (s) (Game.moves s) of Pruned => raise Fail "Dead End"
        | BestEdge(move,est) => move
end

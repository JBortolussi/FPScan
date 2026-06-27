(** A module type for relational domains. *)
exception BotEnv
        
(** Module type for relational domains. *)
module type Domain = sig
  (** Name of the domain. Used to select it via command line
      arguments. Should be of the form \[a-z\]\[a-zA-Z0-9_\]*. *)
  val name : string

  (** Underlying non relational domain when available *)
  val nonrel_base : (module NonRelational.Domain) option

  (** Specify whether the relational domain can address bool variables
      and associated partitioning *)
  val is_partitioned : unit -> bool

  (** [parse_param s] parses parameters in [s]. If [s] is of the form
      "dom:s'" where dom doesn't contain ':' and is not [name], the
      parameter should be ignored. If dom is [name], only s' should be
      considered ([Utils.select_param name] can be used for that). A
      warning should be issued for unrecognised parameters (use
      [Utils.warn_unknown_param]). *)
  val parse_param : string -> unit

  (** Outputs some help about the options recognized by
      [parse_param]. *)
  val fprint_help : Format.formatter -> unit

  (** Type of abstract values. *)
  type t

  (** Prints an abstract value. *)
  val fprint : Format.formatter -> t -> unit
    
  val json : t -> Yojson.t

  (** {2 Lattice Structure} *)

  (** Order on type [t]. [t] with this order must be a lattice. *)
  val order : t -> t -> bool

  val top : Ast.Var.Set.t -> t
  val bottom : Ast.Var.Set.t -> t
  val is_bottom : t -> bool

  val get_vars: t -> Ast.Var.Set.t
  (** Infimums of the lattice (when the relational domain focuses on given set
      of variables). *)

  val join : t -> t -> t
  val meet : t -> t -> t
  (** Least upper bound and greatest lower bound of the lattice. *)

  (** Update the scenarios for the variables given to read_input() instruction *)
  val read_input : t -> Ast.Var.t list -> t

  (** Injection of constraints on state variables *)
  val read_state : t -> Ast.Var.t list -> t

  (** Widening to ensure termination of the analyses. *)
  val widening : t -> t -> t

  (** {2 Abstract Operators} *)

  (** Abstract semantics of assignments and guards. *)

  (** [assignment n e t] returns a [t'] such that:
      {[[|n = e;|](\gamma(t)) \subseteq \gamma(t').]} *)
  val assignment : Name.t -> Ast.expr -> t -> t

  (** Used for backward propagation of constraints on a state variable (read_state instruction) in set based simulation mode (option -sc). 
  Works only when using the polka domains. Returns a [t'] such that : {[\gamma(t) \subseteq [|n = e;|](\gamma(t')).]}.
  Boolean value indicates if the it is the first iteration of the backpropagation, and the NameSet indicates the input variables in the program*)
  val backward_assignment : bool -> Scenario.NameSet.t -> Name.t -> Ast.expr -> t -> t

  (** [guard e t] returns a t' such that:
      {[[|e >= 0|](\gamma(t)) \subseteq \gamma(t').]} el are additional expression to be enforced in t'. *)
  val guard : Ast.expr -> t -> t

  (* Export functions: returns a list of pairs ((v,context), bounds)
     where context, when defined specified if a given boolean variable is true or false *)
  val to_bounds: t -> ((Ast.Var.t * (Ast.Var.t * bool) option) * Bounds.t) list

  (* Similarly for properties *)
  val to_properties: t -> ((Ast.Var.t * (Ast.Var.t * bool) option) * Value_properties.t) list
  
  (** Projects abstract values on others abstract values following a given map *)
  val project_values : t -> t -> (Ast.expr * Ast.Var.t) list -> t
end

(** Product functor for relational domains. *)
module Prod (D1: Domain) (D2: Domain) : Domain 

(** Functor to build relational domains from non relational ones
    (by pointwise extension). *)
module MakeRelational (D : NonRelational.Domain) : Domain

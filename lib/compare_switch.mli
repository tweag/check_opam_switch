val compare_with_current_switch : string -> unit
(** Compares the [opam_export_path] argument with the current switch.
    We check that packages from the [installed] section, and the overlays (which look like inlined .opam files) are present in the current switch. *)

(** Code from dune that compute the current workspace root *)
module Workspace_root : sig
  module Kind = Workspace_root.Kind

  type t = {
    dir : string;
    to_cwd : string list;
    reach_from_root_prefix : string;
    kind : Kind.t;
  }

  val create : default_is_cwd:bool -> specified_by_user:string option -> t
  (** Build the t containing the workspace root *)

  val get_root_argument_opt : unit -> string option
  (** Read an optional [--root] option from the command line *)
end

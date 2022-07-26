(**
   Wrapper around [dune] that first checks that the current switch
   contains all the packages specified in the opam.export
   file located at [$DUNE_WRAPPER_OPAM_FILE] if set or at the root of the dune project.
 *)

let () =
  let opam_export_path =
    match Sys.getenv_opt "DUNE_WRAPPER_OPAM_FILE" with
    | Some p -> p
    | None ->
        (* if [DUNE_WRAPPER_OPAM_FILE] is not set we look at the root of the dune project *)
        let specified_by_user =
          Compare_switch.Workspace_root.get_root_argument_opt ()
        in
        let wr =
          Compare_switch.Workspace_root.create ~default_is_cwd:true
            ~specified_by_user
        in
        wr.dir ^ "/opam.export"
  in
  let () = Compare_switch.compare_with_current_switch opam_export_path in
  Unix.execvp "dune" Sys.argv

(** Based on the workspace_root.ml file from dune *)

module StringSet = Set.Make (String)

module Kind = struct
  type t = Explicit | Dune_workspace | Dune_project | Cwd

  let priority = function
    | Explicit -> 0
    | Dune_workspace -> 1
    | Dune_project -> 2
    | Cwd -> 3

  let lowest_priority = max_int

  let of_dir_contents files =
    if StringSet.mem "dune-workspace" files then Some Dune_workspace
    else if StringSet.mem "dune-project" files then Some Dune_project
    else None
end

type t = {
  dir : string;
  to_cwd : string list;
  reach_from_root_prefix : string;
  kind : Kind.t;
}

module Candidate = struct
  type t = { dir : string; to_cwd : string list; kind : Kind.t }
end

let find () =
  let cwd = Sys.getcwd () in
  let rec loop counter ~(candidate : Candidate.t option) ~to_cwd dir :
      Candidate.t option =
    match Sys.readdir dir with
    | exception Sys_error msg ->
        let () =
          Format.printf
            "[WARNING] dune_wrapper:\n\
             Unable to read directory %s. Will not look for root in parent \
             directories.\n"
            dir
        in
        let () = Format.printf "Reason: %s\n" msg in
        let () =
          Format.printf
            "To remove this warning, set your root explicitly using --root.\n"
        in
        candidate
    | files ->
        let files = StringSet.of_list (Array.to_list files) in
        let candidate =
          let candidate_priority =
            match candidate with
            | Some c -> Kind.priority c.kind
            | None -> Kind.lowest_priority
          in
          match Kind.of_dir_contents files with
          | Some kind when Kind.priority kind <= candidate_priority ->
              Some { Candidate.kind; dir; to_cwd }
          | _ -> candidate
        in
        cont counter ~candidate dir ~to_cwd
  and cont counter ~candidate ~to_cwd dir =
    if counter > String.length cwd then candidate
    else
      let parent = Filename.dirname dir in
      if parent = dir then candidate
      else
        let base = Filename.basename dir in
        loop (counter + 1) parent ~candidate ~to_cwd:(base :: to_cwd)
  in
  loop 0 ~to_cwd:[] cwd ~candidate:None

let create ~default_is_cwd ~specified_by_user =
  match
    match specified_by_user with
    | Some dn -> Some { Candidate.kind = Explicit; dir = dn; to_cwd = [] }
    | None -> (
        let cwd = { Candidate.kind = Cwd; dir = "."; to_cwd = [] } in
        match find () with
        | Some s -> Some s
        | None -> if default_is_cwd then Some cwd else None)
  with
  | Some { Candidate.dir; to_cwd; kind } ->
      {
        kind;
        dir;
        to_cwd;
        reach_from_root_prefix =
          String.concat "" (List.map (fun s -> s ^ "/") to_cwd);
      }
  | None ->
      failwith
      @@ String.concat "\n"
           [
             "I cannot find the root of the current workspace/project.";
             "If you would like to create a new dune project, you can type:";
             "";
             "    dune init project NAME";
             "";
             "Otherwise, please make sure to run dune inside an existing \
              project or workspace. For more information about how dune \
              identifies the root of the current workspace/project, please \
              refer to \
              https://dune.readthedocs.io/en/stable/usage.html#finding-the-root";
           ]

module CLI = Minicli.CLI

let get_root_argument_opt () =
  let _argc, args = CLI.init () in
  let root_opt = CLI.get_string_opt [ "--root" ] args in
  let () = CLI.finalize () in
  root_opt

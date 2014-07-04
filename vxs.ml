(*
 * Copyright (C) 2006-2012 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

(* Test of http put *)
open Lwt

module X = Xen_api_lwt_unix
open Cohttp_lwt_unix
module Cli = Cmdliner
    
type common_opts = { host_: string option; username_: string option; password_: string option}

let opt_str = function None -> "None" |	Some v -> v

let config_host copts file_opts =
  try 
    Host.({host = (Options.default_opt_no_none file_opts.Options.host copts.host_ "host");
           username = opt_str (Options.default_opt file_opts.Options.username copts.username_);
           password = opt_str (Options.default_opt file_opts.Options.password copts.password_)})
  with Options.Option_not_set opt ->
    Printf.printf "Error: you need to give a value to option: %s\n" opt;
    exit 1

let config copts =
  let file_opts = Options.get_file_opts () in
  config_host copts file_opts

let config_pool copts nfs_server nfs_path =
  let file_opts = Options.get_file_opts () in
  let host = config_host copts file_opts in
  let nfs_server = Options.default_opt file_opts.Options.nfs_server nfs_server in
  let nfs_path = Options.default_opt file_opts.Options.nfs_path nfs_path in
  (host, nfs_server, nfs_path)


(* From opam *)
let mk_subdoc ?(names="COMMANDS") commands =
  `S names ::
    List.map (fun (cs,_,d) ->
      let bold s = Printf.sprintf "$(b,%s)" s in
    let cmds = String.concat ", " (List.map bold cs) in
    `I (cmds, d)
   ) commands
    
let mk_subcommands_aux ?(name="COMMAND") my_enum commands default initial_pos =
  let command =
    let doc =
      Cli.Arg.info ~docv:name ~doc:
        (Printf.sprintf
           "Name of the sub-command. See the $(b,%sS) section for more info.%s"
           name
           (match default with
           | None   -> ""
           | Some d -> " " ^ d))
        [] in
    let commands =
      List.fold_left
        (fun acc (cs,f,_) -> List.map (fun c -> c,f) cs @ acc)
        [] commands in
    Cli.Arg.(required & pos initial_pos (some & my_enum commands) None & doc) in
  let params =
    let doc = Cli.Arg.info ~doc:"Optional parameters." [] in
    Cli.Arg.(value & pos_right initial_pos string [] & doc) in
  command, params

let mk_subcommands ?name commands initial_pos =
  mk_subcommands_aux ?name Cli.Arg.enum commands None initial_pos

let get_all_templates copts branch iso name cs_name =
  if branch = None && iso = None && name = None && cs_name = None then
    []
  else begin
    let aux () =
      let host_config = config copts in
      Printf.printf "About to get templates\n%!";
      lwt templates = Xs_ops.get_xenserver_templates_main host_config in
      let open Xs_ops in
      return (List.filter (fun t ->
	(match cs_name with
	| Some n -> 
	  Printf.printf "Checking: %s vs %s\n%!" t.vxs_name n;
	  t.vxs_name = n && t.vxs_ty = Cloudstack
	| None -> true) && 
	  (match name with
	  | Some n -> t.vxs_name = n
	  | None -> true) &&
	  (match branch with
	  | Some b -> (match t.vxs_ty with Pxe b' -> b = b' | _ -> false)
	  | None -> true) &&
	  (match iso with
	  | Some i -> (match t.vxs_ty with Mainiso i' -> i = i' | _ -> false)
	  | None -> true))
		templates)
    in
    Lwt_main.run (aux ())
  end

let get_template_uuid copts branch iso name uuid =
  Printf.printf "Template branch %s iso %s template %s uuid %s\n"
    (opt_str branch) (opt_str iso) (opt_str name) (opt_str uuid);
  match uuid with
  | Some uuid -> uuid
  | None -> 
    let templates = get_all_templates copts branch iso name None in
    if (List.length templates) <> 1 then
      begin
        Printf.printf "I expected exactly one template to match your request.\n";
        if List.length templates = 0 then begin
          Printf.printf "No templates matched the query.\n";
        end else begin
          Printf.printf "I found the following templates:\n";
          List.iter (fun x ->
            Printf.printf "* %s %s installed at %s\n" x.Xs_ops.vxs_uuid x.Xs_ops.vxs_name x.Xs_ops.vxs_install_time;
          ) templates;
        end;
	exit 1
      end;
    let ret = (List.hd templates).Xs_ops.vxs_uuid in
    Printf.printf "Template uuid:%s\n" ret;
    ret

let get_cs_uuid copts cs_name =
  Printf.printf "In get_cs_uuid\n%!";
  let templates = get_all_templates copts None None None cs_name in
  if (List.length templates) <> 1 then
    begin
      Printf.printf "Expecting 1 template: got %d.\n%! (cs_name=%s)" (List.length templates) 
      (match cs_name with Some x -> x | None -> "(none)");
      exit 1
    end
  else (List.hd templates).Xs_ops.vxs_uuid

let pool_create copts nhosts nfs_server nfs_path branch iso template_name uuid pool_name rpms =
  Printf.printf "pool_create nhost %d nfs_server %s nfs_path %s branch %s iso %s template %s uuid %s pool %s\n"
    nhosts (opt_str nfs_server) (opt_str nfs_path) (opt_str branch) (opt_str iso) (opt_str template_name) (opt_str uuid) 
    pool_name;
  Printf.printf "add-rpms %s\n" (String.concat ", " rpms);
  let template_uuid = get_template_uuid copts branch iso template_name uuid in
  let (host,nfs_server,nfs_path) = config_pool copts nfs_server nfs_path in
  let aux () =
    lwt () = if (List.length rpms) > 0 then begin
      lwt (_,uuid) = Xs_ops.template_clone host template_uuid (template_uuid ^ "_temp") in
      Printf.printf "created temporary template %s\n" uuid;
      lwt () = Xs_ops.add_rpms host uuid rpms in
      lwt () = Xs_ops.create_pool host uuid pool_name nhosts nfs_server nfs_path in 
      lwt () = Xs_ops.template_destroy host uuid in
      Printf.printf "destroyed temporary template %s\n" uuid;
      Lwt.return ()
    end else 
	lwt () = Xs_ops.create_pool host template_uuid pool_name nhosts nfs_server nfs_path in 
	Lwt.return ()
    in
    Lwt.return ()
  in
  Lwt_main.run (aux ())

let template_destroy copts branch iso template_name uuid =
  Printf.printf "template_destroy\n";
  let template_uuid = get_template_uuid copts branch iso template_name uuid in
  let host = config copts in
  let aux () =
    lwt () = Xs_ops.template_destroy host template_uuid in 
    return ()
  in
  Lwt_main.run (aux ())

let template_cache copts =
  Printf.printf "template_cache_regenerate\n";
  let host = config copts in
  let aux () =
    lwt () = Xs_ops.regenerate_template_cache host in
    return ()
  in
  Lwt_main.run (aux ())

let template_clone copts branch iso template_name uuid new_name =
  Printf.printf "template_clone\n";
  let template_uuid = get_template_uuid copts branch iso template_name uuid in
  let host = config copts in
  let aux () =
    lwt (_,uuid) = Xs_ops.template_clone host template_uuid new_name in 
    Printf.printf "template-clone %s\n" uuid;
    return ()
  in
  Lwt_main.run (aux ())

let template_install copts source nofakev6d disk mem =
  let host_config = config copts in
  let branch = match source with 
    | Xs_ops.Pxe branch -> branch 
    | Xs_ops.Mainiso _ -> "trunk-ring3" in
  let aux () =
    lwt vm_uuid = Xs_ops.create_xenserver_template host_config source disk mem in
    Printf.printf "%s\n" vm_uuid;
    let nofakev6d = match branch with
      | "trunk-ring3" | "clearwater" | "clearwater-ring3" -> true
      | _ -> nofakev6d in
    if not nofakev6d then begin
      let rpm = Printf.sprintf "/usr/groups/admin/web/www.uk.xensource.com/html/carbon/%s/latest/xe-phase-1/v6-test.rpm" branch in
      lwt () = Xs_ops.add_rpms host_config vm_uuid [rpm] in
      return ()
    end else
      return ()
  in
  Lwt_main.run (aux ())

let template_create_cli copts branch iso nofakev6d disk mem =
  match iso with
  | Some file -> template_install copts (Xs_ops.Mainiso file) nofakev6d disk mem
  | None -> 
    match branch with 
    | Some branch -> template_install copts (Xs_ops.Pxe branch) nofakev6d disk mem
    | None -> template_install copts (Xs_ops.Pxe "trunk-ring3") nofakev6d disk mem

let template_list copts branch iso latest minimal =
  let aux () =
    let host_config = config copts in
    lwt templates = Xs_ops.get_xenserver_templates_main host_config in
    let open Xs_ops in
    let templates = if latest && (List.length templates) > 0 then
	let sorted = List.fast_sort (fun t1 t2 -> - (String.compare t1.vxs_install_time t2.vxs_install_time)) templates in
	[(List.hd sorted)]
      else List.filter (fun t -> 
	(match branch with 
	| Some b -> (match t.vxs_ty with Pxe b' -> b = b' | _ -> false)
	| None -> true) &&
	  (match iso with 
	  | Some i -> (match t.vxs_ty with Mainiso i' -> i = i' | _ -> false)
	  | None -> true))
	templates in
    if not minimal then
      Printf.printf "%-20s | %-36s | %-20s | %-30s |\n" "NAME" "UUID" "INSTALL TIME" "INSTALL TYPE";
    List.iter (fun t ->
      if minimal then
	Printf.printf "%s\n" t.vxs_uuid
      else 
	Printf.printf "%-20s | %-36s | %-20s | %-30s |\n" t.vxs_name t.vxs_uuid t.vxs_install_time (Rpc.to_string (rpc_of_installty t.vxs_ty))) templates;
    Lwt.return ()
  in
  Lwt_main.run (aux ())


let help common_opts man_format cmds topic = match topic with
  | None -> `Help (`Pager, None) (* help about the program. *)
  | Some topic ->
    let topics = "topics" :: "patterns" :: "environment" :: cmds in
    let conv, _ = Cli.Arg.enum (List.rev_map (fun s -> (s, s)) topics) in
    match conv topic with
    | `Error e -> `Error (false, e)
    | `Ok t when t = "topics" -> List.iter print_endline topics; `Ok ()
    | `Ok t when List.mem t cmds -> `Help (man_format, Some t)
    | `Ok t ->
      let page = (topic, 7, "", "", ""), [`S topic; `P "Say something";] in
      `Ok (Cli.Manpage.print man_format Format.std_formatter page)

let common_opts_sect = "COMMON OPTIONS"
let help_secs = [
 `S common_opts_sect;
 `P "These options are common to all commands.";
 `S "MORE HELP";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";`Noblank;]

(* Options common to all commands *)

let common_opts host_ username_ password_ = { host_; username_; password_ }

let common_opts_t =
  let docs = common_opts_sect in
  let host =
    let doc = "Hostname to connect to." in
    Cli.Arg.(value & opt (some string) None & info ["H"; "host"] ~docs ~doc ~docv:"HOST")
  in
  let user =
    let doc = "Username to log in with." in
    Cli.Arg.(value & opt (some string) (Some "root") & info ["U"; "user"] ~docs ~doc)
  in
  let pw =
    let doc = "Password to log in with." in
    Cli.Arg.(value & opt (some string) (Some "xenroot") & info ["P"; "password"] ~docs ~doc)
  in
  Cli.Term.(pure common_opts $ host $ user $ pw)

let branch_opt () =
  let doc = "Branch for the template specifier." in
  Cli.Arg.(value & opt (some string) None & info ["b"; "branch"]
             ~docv:"BRANCH" ~doc)

let iso_opt () =
  let doc = "Iso for the template specifier." in
  Cli.Arg.(value & opt (some string) None & info ["i"; "iso"]
             ~docv:"ISO" ~doc)
  
let template_name_opt () =
  let doc = "Name of the template." in
  Cli.Arg.(value & opt (some string) None & info ["n"]
             ~docv:"TEMPLATE_NAME" ~doc)

let uuid_opt () =
  let doc = "UUID of the template." in
  Cli.Arg.(value & opt (some string) None & info ["U"; "uuid"] ~doc ~docv:"UUID")

let cs_name_opt () =
  let doc = "Name of the CS template." in
  Cli.Arg.(value & opt (some string) None & info ["cs"] ~doc ~docv:"CS_NAME")

(* Commands *)


let template_create_cmd =
  let branch = branch_opt () in
  let iso = iso_opt () in
  let nov6d =
    let doc = "Do not install the fake v6d." in
    Cli.Arg.(value & flag & info ["n"; "nofakev6d"] ~doc)
  in
  let disk =
    let doc = "Disk size in Gb." in
    Cli.Arg.(value & opt int64 40L & info ["d"] ~doc ~docv:"DISK")
  in
  let memory =
    let doc = "Memory size in Mb." in
    Cli.Arg.(value & opt int64 2048L & info ["m"] ~doc ~docv:"MEM")
  in
  let doc = "Create a Virtual Xen Server Template" in
  let man = [
    `S "DESCRIPTION";
    `P "Install a Virtual Xen Server as a Template"] @ help_secs
  in
  Cli.Term.(pure template_create_cli $ common_opts_t $ branch $ iso $
      nov6d $ disk $ memory),
  Cli.Term.info "template-create" ~sdocs:common_opts_sect ~doc ~man
    
let default_cmd =
  let doc = "Virtual XenServer Management Toolkit" in
  let man = help_secs in
  Cli.Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ common_opts_t)),
  Cli.Term.info "vxs" ~version:"0.2" ~sdocs:common_opts_sect ~doc ~man


let cmds = [ template_create_cmd ]

let () = 
  Printexc.record_backtrace true;
  try
    match Cli.Term.eval_choice default_cmd cmds with
    | `Error _ -> exit 1 | _ -> exit 0
  with e -> Printf.printf "Error: exception %s\n" (Printexc.to_string e)

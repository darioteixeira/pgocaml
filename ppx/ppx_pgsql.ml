(* PG'OCaml - type safe interface to PostgreSQL.
 * Copyright (C) 2005-2016 Richard Jones and other authors.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this library; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *)

open PGOCaml_aux
open Printf

open Migrate_parsetree
open Migrate_parsetree.Ast_407.Ast_mapper
open Migrate_parsetree.Ast_407.Ast_helper
open Migrate_parsetree.Ast_407.Asttypes
open Migrate_parsetree.Ast_407.Parsetree
open Migrate_parsetree.Ast_407.Longident

let nullable_name = "nullable"
let unravel_name = "unravel"
let typname_name = "typname"

(* We need a database connection while compiling.  If people use the
 * override flags like "database=foo", then we may connect to several
 * databases at once.  Keep track of that here.  Note that in the normal
 * case we have just one database handle, opened to the default
 * database (as controlled by environment variables such as $PGHOST,
 * $PGDATABASE, etc.)
 *)
type key = PGOCaml.connection_desc

let connections : (key, unit PGOCaml.t) Hashtbl.t = Hashtbl.create 16

(** [get_connection key] Find the database connection specified by [key],
  *  otherwise attempt to create a new one from [key] and return that (or an
  *  error).
  *)
let get_connection ~loc key =
  match Hashtbl.find_opt connections key with
  | Some connection ->
      let open Rresult in
      Ok connection
  | None ->
      (* Create a new connection. *)
      try
        let dbh = PGOCaml.connect ~desc:key () in
        (* Prepare the nullable test - see result conversions below. *)
        let nullable_query = "select attnotnull from pg_attribute where attrelid = $1 and attnum = $2" in
        PGOCaml.prepare dbh ~query:nullable_query ~name:nullable_name ();

        (* Prepare the unravel test. *)
        let unravel_query = "select typtype, typbasetype from pg_type where oid = $1" in
        PGOCaml.prepare dbh ~query:unravel_query ~name:unravel_name ();

        (* Prepare the type name query. *)
        let typname_query = "select typname from pg_type where oid = $1" in
        PGOCaml.prepare dbh ~query:typname_query ~name:typname_name ();

        Hashtbl.add connections key dbh;
        Rresult.Ok dbh
      with
        | err ->
          Error ("Could not make the connection "
            ^ PGOCaml.connection_desc_to_string key
            ^ ", error: "
            ^ Printexc.to_string err
          , loc)

(* Wrapper around [PGOCaml.name_of_type].
*)
let name_of_type_wrapper dbh oid =
  try
    PGOCaml.name_of_type oid
  with PGOCaml.Error _ as exc ->
    let params = [ Some (PGOCaml.string_of_oid oid) ] in
    let rows = PGOCaml.execute dbh ~name:typname_name ~params () in
    match rows with
      | [ [ Some "citext" ] ] -> "string"
      | [ [ Some "hstore" ] ] -> "hstore"
      | _ -> raise exc


(* By using CREATE DOMAIN, the user may define types which are essentially aliases
 * for existing types.  If the original type is not recognised by PG'OCaml, this
 * functions recurses through the pg_type table to see if it happens to be an alias
 * for a type which we do know how to handle.
*)
let unravel_type dbh orig_type =
  let rec unravel_type_aux ft =
    try
      name_of_type_wrapper dbh ft
    with PGOCaml.Error _ as exc ->
      let params = [ Some (PGOCaml.string_of_oid ft) ] in
      let rows = PGOCaml.execute dbh ~name:unravel_name ~params () in
      match rows with
        | [ [ Some typtype ; _ ] ] when typtype = "e" -> "string"
        | [ [ Some typtype ; Some typbasetype ] ] when typtype = "d" ->
          unravel_type_aux (PGOCaml.oid_of_string typbasetype)
        | _ ->
          raise exc
  in unravel_type_aux orig_type

(* Return the list of numbers a <= i < b. *)
let rec range a b =
  if a < b then a :: range (a+1) b else []

let rex =
    let open Re in
    [
    char '$';
    opt (group (char '@'));
    opt (group (char '?'));
    group (
      alt [
        seq [
          alt [char '_'; rg 'a' 'z'];
          rep (alt [char '_'; char '\''; rg 'a' 'z'; rg 'A' 'Z'; rg '0' '9'])
        ];
        seq [
          char '{';
          rep (diff any (char '}'));
          char '}'
        ]
      ]
    )
    ] |> seq |> compile

let loc_raise _loc exn =
  raise exn

let const_string ~loc str =
  { pexp_desc = Pexp_constant (Pconst_string (str, None));
    pexp_loc = loc;
    pexp_attributes = []; }

let parse_flags flags loc =
  let f_execute = ref false in
  let f_nullable_results = ref false in
  let host = ref None in
  let port = ref None in
  let user = ref None in
  let password = ref None in
  let database = ref None in
  let unix_domain_socket_dir = ref None in
  let comment_src_loc = ref (PGOCaml.comment_src_loc ()) in
  let show = ref None in
  List.iter (
    function
    | "execute" -> f_execute := true
    | "nullable-results" -> f_nullable_results := true
    | "show" -> show := Some "show"
    | str when String.starts_with str "host=" ->
        let host' = String.sub str 5 (String.length str - 5) in
        host := Some host'
    | str when String.starts_with str "port=" ->
        let port' = int_of_string (String.sub str 5 (String.length str - 5)) in
        port := Some port'
    | str when String.starts_with str "user=" ->
        let user' = String.sub str 5 (String.length str - 5) in
        user := Some user'
    | str when String.starts_with str "password=" ->
        let password' = String.sub str 9 (String.length str - 9) in
        password := Some password'
    | str when String.starts_with str "database=" ->
        let database' = String.sub str 9 (String.length str - 9) in
        database := Some database'
    | str when String.starts_with str "unix_domain_socket_dir=" ->
        let socket = String.sub str 23 (String.length str - 23) in
        unix_domain_socket_dir := Some socket
    | str when String.starts_with str "comment_src_loc=" ->
        let comment_src_loc' = String.sub str 19 (String.length str - 19) in
        begin match comment_src_loc' with
        | "yes" | "1" | "on" -> comment_src_loc := true
        | "no" | "0" | "off" -> comment_src_loc := false
        | _ -> loc_raise loc (Failure "Unrecognized value for option 'comment_src_loc'")
        end
    | str when String.starts_with str "show=" ->
        let shownam = String.sub str 5 (String.length str - 5) in
        show := Some shownam
    | str ->
      loc_raise loc (
        Failure ("Unknown flag: " ^ str)
      )
    ) flags;
  let f_execute = !f_execute in
  let f_nullable_results = !f_nullable_results in
  let host = !host in
  let user = !user in
  let password = !password in
  let database = !database in
  let port = !port in
  let unix_domain_socket_dir = !unix_domain_socket_dir in
  let key = PGOCaml.describe_connection ?host ?user ?password ?database ?port ?unix_domain_socket_dir () in
  key, f_execute, f_nullable_results, !comment_src_loc, !show

let mk_conversions ~loc ~dbh results =
  List.mapi (
    fun i (result, nullable) ->
      let field_type = result.PGOCaml.field_type in
      let fn = unravel_type dbh field_type in
      let fn = fn ^ "_of_string" in
      let col =
        let cname = "c" ^ string_of_int i in
        Exp.ident { txt = Lident cname; loc }
      in
      let sconv = [%expr match [%e col] with Some x -> x | None -> "-"][@metaloc loc] in
      if nullable then
          ([%expr
            PGOCaml_aux.Option.map
              PGOCaml.(
                [%e
                  Exp.ident { txt = Lident fn; loc }
                ]
              )
              [%e col]
          ]
          [@metaloc loc])
        , sconv
      else
          ([%expr PGOCaml.([%e Exp.ident { txt = Lident fn; loc }])
            (try PGOCaml_aux.Option.get [%e col] with
              | _ -> failwith "ppx_pgsql's nullability heuristic has failed - use \"nullable-results\""
            )
          ][@metaloc loc])
        , sconv
    )
    results

let coretype_of_type ~loc ~dbh oid =
  let typ =
    match unravel_type dbh oid with
    | "timestamp" -> Longident.Ldot(Ldot(Lident "CalendarLib", "Calendar"), "t")
    | nam -> Lident nam
  in
  { ptyp_desc = Ptyp_constr({txt = typ; loc}, [])
  ; ptyp_loc = loc
  ; ptyp_attributes = []
  }

(** produce a list pattern to match the result of a query *)
let mk_listpat ~loc results =
  List.fold_right
    (fun i tail ->
        let var = Pat.var @@ { txt = "c"^string_of_int i; loc } in
        ([%pat? [%p var]::[%p tail]][@metaloc loc])
    )
    (range 0 (List.length results))
    ([%pat? []][@metaloc loc])

let pgsql_expand ~genobject ?(flags = []) loc dbh query =
  let open Rresult in
  let (key, f_execute, f_nullable_results, comment_src_loc, show) = parse_flags flags loc in
  let query =
    if comment_src_loc
    then
      let start = loc.Location.loc_start in
      let open Lexing in
      (Printf.sprintf
        "-- '%s' L%d\n"
        start.pos_fname
        start.pos_lnum)
      ^ query
    else
      query
  in
  (* Connect, if necessary, to the database. *)
  get_connection ~loc key
  >>= fun my_dbh ->
  (* Split the query into text and variable name parts using Re.split_full.
   * eg. "select id from employees where name = $name and salary > $salary"
   * would become a structure equivalent to:
   * ["select id from employees where name = "; "$name"; " and salary > ";
   * "$salary"].
   * Actually it's a wee bit more complicated than that ...
  *)
  let split =
    let f = function
      | `Text text -> `Text text
      | `Delim subs -> `Var Re.Group.(get subs 3, test subs 1, test subs 2)
    in List.map f (Re.split_full rex query) in

  (* Go to the database, prepare this statement, and find out exactly
   * what the parameter types and return values are.  Exceptions can
   * be raised here if the statement is bad SQL.
  *)
  let (params, results), varmap =
    (* Rebuild the query with $n placeholders for each variable. *)
    let next = let i = ref 0 in fun () -> incr i; !i in
    let varmap = Hashtbl.create 8 in
    let query = String.concat "" (
        List.map (
          function
          | `Text text -> text
          | `Var (varname, false, option) ->
            let i = next () in
            Hashtbl.add varmap i (varname, false, option);
            sprintf "$%d" i
          | `Var (varname, true, option) ->
            let i = next () in
            Hashtbl.add varmap i (varname, true, option);
            sprintf "($%d)" i
        ) split
      ) in
    let varmap = Hashtbl.fold (
        fun i var vars -> (i, var) :: vars
      ) varmap [] in
    try
      PGOCaml.prepare my_dbh ~query ();
      PGOCaml.describe_statement my_dbh (), varmap
    with
      exn -> loc_raise loc exn in

  (* If the PGSQL(dbh) "execute" flag was used, we will actually
   * execute the statement now.  Normally this would never be used, but
   * some statements need to be executed, particularly CREATE TEMPORARY
   * TABLE.
  *)
  if f_execute then ignore (PGOCaml.execute my_dbh ~params:[] ());

  (* Number of params should match length of map, otherwise something
   * has gone wrong in the substitution above.
  *)
  if List.length varmap <> List.length params then
    loc_raise loc (
      Failure ("Mismatch in number of parameters found by database. " ^
               "Most likely your statement contains bare $, $number, etc.")
    );

  (* Generate a function for converting the parameters.
   *
   * See also:
   * http://archives.postgresql.org/pgsql-interfaces/2006-01/msg00043.php
  *)
  let params =
    List.fold_right
      (fun (i, { PGOCaml.param_type = param_type }) tail ->
         let varname, list, option = List.assoc i varmap in
         let varname =
           if String.starts_with varname "{"
           then String.sub varname 1 (String.length varname - 2)
           else varname
         in
         let varname =
           (Migrate_parsetree.Parse.expression
             Migrate_parsetree.Versions.ocaml_407
             (Lexing.from_string varname))
         in
         let varname = {varname with pexp_loc = loc} in
         let fn = "string_of_" ^ (unravel_type my_dbh param_type) in
         let fn = Exp.ident ~loc { txt = Lident fn; loc } in
         let head =
           match list, option with
           | false, false -> [%expr [Some (PGOCaml.([%e fn]) [%e varname])]][@metaloc loc]
           | false, true -> [%expr [PGOCaml_aux.Option.map PGOCaml.([%e fn]) [%e varname]]][@metaloc loc]
           | true, false -> [%expr List.map (fun x -> Some (PGOCaml.([%e fn]) x)) [%e varname]][@metaloc loc]
           | true, true -> [%expr List.map (fun x -> PGOCaml_aux.Option.map PGOCaml.([%e fn])) [%e varname]][@metaloc loc]
         in
         ([%expr [%e head]::[%e tail]][@metaloc loc])
      )
      (List.combine (range 1 (1 + List.length varmap)) params)
      ([%expr []][@metaloc loc])
  in

  (* Substitute expression. *)
  let expr =
    let split = List.fold_right (
        fun s tail ->
          let head = match s with
            | `Text text -> ([%expr `Text [%e const_string ~loc text]][@metaloc loc])
            | `Var (varname, list, option) ->
              let list =
                if list then ([%expr true][@metaloc loc]) else ([%expr false][@metaloc loc]) in
              let option =
                if option then ([%expr true][@metaloc loc]) else ([%expr false][@metaloc loc]) in
              ([%expr `Var ([%e const_string ~loc varname],
                           [%e list],
                           [%e option])][@metaloc loc]) in
          ([%expr [%e head] :: [%e tail]][@metaloc loc])
      ) split ([%expr []][@metaloc loc]) in
    [%expr
       (* let original_query = $str:query$ in * original query string *)
      let dbh = [%e dbh] in
      let params : string option list list = [%e params] in
      let split = [%e split] in (* split up query *)

      (* Rebuild the query with appropriate placeholders.  A single list
       * param can expand into several placeholders.
      *)
      let i = ref 0 in (* Counts parameters. *)
      let j = ref 0 in (* Counts placeholders. *)
      let query = String.concat "" (
          List.map (
            function
            | `Text text -> text
            | `Var (_varname, false, _) ->    (* non-list item *)
              let () = incr i in        (* next parameter *)
              let () = incr j in        (* next placeholder number *)
              "$" ^ string_of_int j.contents
            | `Var (_varname, true, _) -> (* list item *)
              let param = List.nth params i.contents in
              let () = incr i in        (* next parameter *)
              "(" ^
              String.concat "," (
                List.map (
                  fun _ ->
                    let () = incr j in (* next placeholder number *)
                    "$" ^ string_of_int j.contents
                ) param
              ) ^
              ")"
          ) split) in

      (* Flatten the parameters to a simple list now. *)
      let params = List.flatten params in

      (* Get a unique name for this query using an MD5 digest. *)
      let name = "ppx_pgsql." ^ Digest.to_hex (Digest.string query) in

      (* Get the hash table used to keep track of prepared statements. *)
      let hash =
        try PGOCaml.private_data dbh
        with
        | Not_found ->
          let hash = Hashtbl.create 17 in
          PGOCaml.set_private_data dbh hash;
          hash
      in
      (* Have we prepared this statement already?  If not, do so. *)
      let is_prepared = Hashtbl.mem hash name in
      PGOCaml.bind
        (if not is_prepared then
           PGOCaml.bind (PGOCaml.prepare dbh ~name ~query ()) (fun () ->
               Hashtbl.add hash name true;
               PGOCaml.return ()
             )
         else
           PGOCaml.return ()) (fun () ->
            (* Execute the statement, returning the rows. *)
            PGOCaml.execute_rev dbh ~name ~params ())
    ][@metaloc loc] in

  (** decorate the results with the nullability heuristic *)
  let results' =
    match results with
    | Some results ->
      Some (
        List.map
          (fun result ->
            match (result.PGOCaml.table, result.PGOCaml.column) with
            | Some table, Some column ->
              (* Find out whether the column is nullable from the
               * database pg_attribute table.
              *)
              let params =
                [ Some (PGOCaml.string_of_oid table);
                  Some (PGOCaml.string_of_int column) ] in
              let _rows =
                PGOCaml.execute my_dbh ~name:nullable_name ~params () in
              let not_nullable =
                match _rows with
                | [ [ Some b ] ] -> PGOCaml.bool_of_string b
                | _ -> false in
              result, f_nullable_results || not not_nullable
            | _ -> result, f_nullable_results || true (* Assume it could be nullable. *))
          results
      )
    | None ->
      None
  in
  let mkexpr ~convert ~list = [%expr
    PGOCaml.bind [%e expr] (fun _rows ->
        PGOCaml.return
          (let original_query = [%e const_string ~loc query] in
            List.rev_map (
              fun row ->
                match row with
                | [%p list] -> [%e convert]
                | _ ->
                  (* This should never happen, even if the schema changes.
                      * Well, maybe if the user does 'SELECT *'.
                  *)
                  let msg = "ppx_pgsql: internal error: " ^
                            "Incorrect number of columns returned from query: " ^
                            original_query ^
                            ".  Columns are: " ^
                            String.concat "; " (
                              List.map (
                                function
                                | Some str -> Printf.sprintf "%S" str
                                | None -> "NULL"
                              ) row
                            ) in
                  raise (PGOCaml.Error msg)
            ) _rows))
  ][@metaloc loc]
  in
  (* If we're expecting any result rows, then generate a function to
   * convert them.  Otherwise return unit.  Note that we can only
   * determine the nullability of results if they correspond to real
   * columns in a table, otherwise the type will always be 'type option'.
  *)
  match (genobject, results') with
  | true, Some results ->
    let list = mk_listpat ~loc results in
    let fields =
      List.map
        (fun ({PGOCaml.name; field_type; _}, nullable) ->
          name, coretype_of_type ~loc ~dbh:my_dbh field_type, nullable)
        results
    in
    let convert =
      List.fold_left2
        (fun (lsacc, showacc) (name, _, _) (conv, sconv) ->
          let hd =
            { pcf_desc = Pcf_method(
                  {txt = name; loc}
                , Public
                , Cfk_concrete(Fresh, conv)
              )
            ; pcf_loc = loc
            ; pcf_attributes = []
            }
          in
          let ename = const_string ~loc name in
          let showacc =
            [%expr
              let fields = ([%e ename], [%e sconv]) :: fields in
              [%e showacc]
            ][@metaloc loc]
          in
          (hd :: lsacc, showacc)
        )
        ( []
        , [%expr
            List.fold_left
              (fun buffer (name, value) ->
                let () = Buffer.add_string buffer name in
                let () = Buffer.add_char buffer ':' in
                let () = Buffer.add_char buffer ' ' in
                let () = Buffer.add_string buffer value in
                let () = Buffer.add_char buffer '\n' in
                buffer
              )
              (Buffer.create 16)
              fields
            |> Buffer.contents
          ][@metaloc loc]
        )
        fields
        (mk_conversions ~loc ~dbh:my_dbh results)
      |> fun (fields, fshow) ->
          let fshow =
            [%expr let fields = [] in [%e fshow]][@metaloc loc]
          in
          let fields =
            match show with
            | Some txt ->
              { pcf_desc = Pcf_method(
                    {txt; loc}
                  , Public
                  , Cfk_concrete(Fresh, fshow)
                )
              ; pcf_loc = loc
              ; pcf_attributes = []
              }
              :: fields
            | None ->
              fields
          in
          Exp.mk (
            Pexp_object({
              pcstr_self = Pat.any ~loc ()
            ; pcstr_fields = fields
            })
          )
    in
    let expr = mkexpr ~convert ~list in
    Ok expr
  | true, None ->
    Error("It doesn't make sense to make an object to encapsulate results that aren't coming", loc)
  | false, Some results ->
    let list = mk_listpat ~loc results in
    let convert =
      let conversions =
        mk_conversions ~loc ~dbh:my_dbh results
        |> List.map fst
      in
      (* Avoid generating a single-element tuple. *)
      match conversions with
      | [] -> [%expr ()][@metaloc loc]
      | [a] -> a
      | conversions -> Exp.tuple conversions
    in
    Ok(mkexpr ~convert ~list)
  | false, None ->
    Ok ([%expr PGOCaml.bind [%e expr] (fun _rows -> PGOCaml.return ())][@metaloc loc])

let expand_sql ~genobject loc dbh extras =
     let query, flags =
       match List.rev extras with
       | [] -> assert false
       | query :: flags -> query, flags in
     try pgsql_expand ~genobject ~flags loc dbh query
     with
     | Failure s -> Error(s, loc)
     | PGOCaml.Error s -> Error(s, loc)
     | PGOCaml.PostgreSQL_Error (s, fields) ->
       let fields' = List.map (fun (c, s) -> Printf.sprintf "(%c: %s)" c s) fields in
       Error ("Postgres backend error: " ^ s ^ ": " ^ s ^ String.concat "," fields', loc)
     | exn -> Error("Unexpected PG'OCaml PPX error: " ^ Printexc.to_string exn, loc)

(* Returns the empty list if one of the elements is not a string constant *)
let list_of_string_args mapper args =
  let maybe_strs =
    List.map
      (function
        | (Nolabel, {pexp_desc = Pexp_constant (Pconst_string (str, None)); _})
          -> Some str
        | (_, other) ->
          match mapper other with
          | {pexp_desc = Pexp_constant (Pconst_string (str, None)); _}
            -> Some str
          | _ -> None
      )
      args
  in
  if List.mem None maybe_strs then
    []
  else
    List.map (function Some x -> x | None -> assert false) maybe_strs

let pgocaml_rewriter _config _cookies =
  { default_mapper with
    expr = fun mapper expr ->
      let unsupported loc =
        { expr with
          pexp_desc = Pexp_extension (
              extension_of_error @@
              Location.error ~loc (Printf.sprintf "Something unsupported")
            )
        }
      in
      match expr with
      | { pexp_desc =
            Pexp_extension (
              { txt ; loc }
            , PStr [{ pstr_desc = Pstr_eval ({pexp_desc = Pexp_apply (dbh, args); pexp_loc = qloc; _}, _); _}]
            )
        ; _
        } when String.starts_with txt "pgsql" ->
        let open Rresult in
        let genobject = txt = "pgsql.object" in
        ( match list_of_string_args (default_mapper.expr mapper) args with
          | [] -> unsupported loc
          | args ->
            let x = expand_sql ~genobject loc dbh args in
            ( match x with
              | Rresult.Ok ({ pexp_desc; pexp_loc = _ ; pexp_attributes }) ->
                {pexp_desc; pexp_loc = qloc; pexp_attributes}
              | Error(s, loc) ->
                { expr with
                  pexp_desc = Pexp_extension (
                    extension_of_error @@
                    Location.error ~loc ("PG'OCaml PPX error: " ^ s))
                ; pexp_loc = loc
                }
            )
        )
      | { pexp_desc =
            Pexp_extension (
              { txt = "pgsql"; loc }
            , _)
        ; _
        } ->
        unsupported loc
      | other ->
        default_mapper.expr mapper other
  }

(*let migration =
  Versions.migrate Versions.ocaml_407 Versions.ocaml_current

let _ =
  Migrate_parsetree.Compiler_libs.Ast_mapper.register
    "pgocaml"
    (fun args -> migration.copy_mapper (pgocaml_mapper args))
*)

let () =
  Driver.register ~name:"pgocaml" ~reset_args:(fun _ -> ()) ~args:[]
    Versions.ocaml_407 pgocaml_rewriter

(* PG'OCaml - type safe interface to PostgreSQL.
 * Copyright (C) 2005-2009 Richard Jones and other authors.
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

open Camlp4.PreCast
open PGOCaml_aux
open Printf

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
type key = {
  (* None in any of these fields means use the default. *)
  host : string option;
  port : int option;
  user : string option;
  password : string option;
  database : string option;
  unix_domain_socket_dir : string option;
}

let connections : (key, unit PGOCaml.t) Hashtbl.t = Hashtbl.create 13

let get_connection key =
  try
    Hashtbl.find connections key
  with
    Not_found ->
      (* Create a new connection. *)
      let host = key.host in
      let port = key.port in
      let user = key.user in
      let password = key.password in
      let database = key.database in
      let unix_domain_socket_dir = key.unix_domain_socket_dir in
      let dbh =
	PGOCaml.connect
	  ?host ?port ?user ?password ?database
	  ?unix_domain_socket_dir () in

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
      dbh


(* Wrapper around [PGOCaml.name_of_type].
*)
let name_of_type_wrapper ?modifier dbh oid =
  try
    PGOCaml.name_of_type ?modifier oid
  with PGOCaml.Error msg as exc ->
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
    with PGOCaml.Error msg as exc ->
      let params = [ Some (PGOCaml.string_of_oid ft) ] in
      let rows = PGOCaml.execute dbh ~name:unravel_name ~params () in
      match rows with
        | [ [ Some typtype ; Some typbasetype ] ] when typtype = "d" ->
          unravel_type_aux (PGOCaml.oid_of_string typbasetype)
        | _ ->
          raise exc
  in unravel_type_aux orig_type

(* Return the list of numbers a <= i < b. *)
let rec range a b =
  if a < b then a :: range (a+1) b else [];;

let rex =
    let open Re in
    [
    char '$';
    opt (group (char '@'));
    opt (group (char '?'));
    group (seq [alt [char '_'; rg 'a' 'z']; rep (alt [char '_'; char '\''; rg 'a' 'z'; rg 'A' 'Z'; rg '0' '9'])]);
    ] |> seq |> compile

let pgsql_expand ?(flags = []) _loc dbh query =
  (* Parse the flags. *)
  let f_execute = ref false in
  let f_nullable_results = ref false in
  let key = ref { host = None; port = None; user = None;
		  password = None; database = None;
		  unix_domain_socket_dir = None } in
  List.iter (
    function
    | "execute" -> f_execute := true
    | "nullable-results" -> f_nullable_results := true
    | str when String.starts_with str "host=" ->
	let host = String.sub str 5 (String.length str - 5) in
	key := { !key with host = Some host }
    | str when String.starts_with str "port=" ->
	let port = int_of_string (String.sub str 5 (String.length str - 5)) in
	key := { !key with port = Some port }
    | str when String.starts_with str "user=" ->
	let user = String.sub str 5 (String.length str - 5) in
	key := { !key with user = Some user }
    | str when String.starts_with str "password=" ->
	let password = String.sub str 9 (String.length str - 9) in
	key := { !key with password = Some password }
    | str when String.starts_with str "database=" ->
	let database = String.sub str 9 (String.length str - 9) in
	key := { !key with database = Some database }
    | str when String.starts_with str "unix_domain_socket_dir=" ->
	let socket = String.sub str 23 (String.length str - 23) in
	key := { !key with unix_domain_socket_dir = Some socket }
    | str ->
	Loc.raise _loc (
	  Failure ("Unknown flag: " ^ str)
	)
  ) flags;
  let f_execute = !f_execute in
  let f_nullable_results = !f_nullable_results in
  let key = !key in

  (* Connect, if necessary, to the database. *)
  let my_dbh = get_connection key in

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
      | `Delim subs -> `Var (Re.get subs 3, Re.test subs 1, Re.test subs 2)
    in List.map f (Re.split_full rex query) in

  (* Go to the database, prepare this statement, and find out exactly
   * what the parameter types and return values are.  Exceptions can
   * be raised here if the statement is bad SQL.
   *)
  let (params, results), varmap =
    (* Rebuild the query with $n placeholders for each variable. *)
    let next = let i = ref 0 in fun () -> incr i; !i in
    let varmap = Hashtbl.create 7 in
    let query = String.concat "" (
      List.map (
	function
	| `Text text -> text
	| `Var (_varname, false, option) ->
	    let i = next () in
	    Hashtbl.add varmap i (_varname, false, option);
	    sprintf "$%d" i
	| `Var (_varname, true, option) ->
	    let i = next () in
	    Hashtbl.add varmap i (_varname, true, option);
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
      exn -> Loc.raise _loc exn in

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
    Loc.raise _loc (
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
	 let _varname, list, option = List.assoc i varmap in
	 let fn = "string_of_" ^ (unravel_type my_dbh param_type) in
	 let head =
	   match list, option with
	   | false, false ->
	     <:expr< [ Some (PGOCaml.$lid:fn$ $lid:_varname$) ] >>
	   | false, true ->
	     <:expr< [ PGOCaml_aux.Option.map PGOCaml.$lid:fn$ $lid:_varname$ ] >>
	   | true, false ->
	     <:expr< List.map (fun x -> Some (PGOCaml.$lid:fn$ x)) $lid:_varname$ >>
	   | true, true ->
	     <:expr< List.map (fun x -> PGOCaml_aux.Option.map PGOCaml.$lid:fn$ x) $lid:_varname$ >> in
	 <:expr< [ $head$ :: $tail$ ] >>
      )
      (List.combine (range 1 (1 + List.length varmap)) params)
      <:expr< [] >>
  in

  (* Substitute expression. *)
  let expr =
    let split = List.fold_right (
      fun s tail ->
	let head = match s with
	  | `Text text -> <:expr< `Text $str:text$ >>
	  | `Var (_varname, list, option) ->
	      let list =
		if list then <:expr< True >> else <:expr< False >> in
	      let option =
		if option then <:expr< True >> else <:expr< False >> in
	      <:expr< `Var $str:_varname$ $list$ $option$ >> in
	<:expr< [ $head$ :: $tail$ ] >>
    ) split <:expr< [] >> in
    <:expr<
      (* let original_query = $str:query$ in * original query string *)
      let dbh = $dbh$ in
      let params = $params$ in (* type: string option list list *)
      let split = $split$ in (* split up query *)

      (* Rebuild the query with appropriate placeholders.  A single list
       * param can expand into several placeholders.
       *)
      let i = ref 0 in (* Counts parameters. *)
      let j = ref 0 in (* Counts placeholders. *)
      let query = String.concat "" (
	List.map (
	  fun
	  [ `Text text -> text
	  | `Var _varname False _ ->	(* non-list item *)
	      let () = incr i in	(* next parameter *)
	      let () = incr j in	(* next placeholder number *)
	      "$" ^ string_of_int j.contents
	  | `Var _varname True _ -> (* list item *)
	      let param = List.nth params i.contents in
	      let () = incr i in	(* next parameter *)
	      "(" ^
		String.concat "," (
		  List.map (
		    fun _ ->
		      let () = incr j in (* next placeholder number *)
		      "$" ^ string_of_int j.contents
		  ) param
		) ^
		")"
	  ]
	) split) in

      (* Flatten the parameters to a simple list now. *)
      let params = List.flatten params in

      (* Get a unique name for this query using an MD5 digest. *)
      let name = "pa_pgsql." ^ Digest.to_hex (Digest.string query) in

      (* Get the hash table used to keep track of prepared statements. *)
      let hash =
	try PGOCaml.private_data dbh
	with
	  [ Not_found ->
	      let hash = Hashtbl.create 17 in
	      do {
		PGOCaml.set_private_data dbh hash;
		hash
	      } ] in
      (* Have we prepared this statement already?  If not, do so. *)
      let is_prepared = Hashtbl.mem hash name in
      PGOCaml.bind
        (if not is_prepared then
           PGOCaml.bind (PGOCaml.prepare dbh ~name ~query ()) (fun () ->
           do {
             Hashtbl.add hash name True;
             PGOCaml.return ()
           })
         else
           PGOCaml.return ()) (fun () ->
         (* Execute the statement, returning the rows. *)
      PGOCaml.execute_rev dbh ~name ~params ())
    >> in

  (* If we're expecting any result rows, then generate a function to
   * convert them.  Otherwise return unit.  Note that we can only
   * determine the nullability of results if they correspond to real
   * columns in a table, otherwise the type will always be 'type option'.
   *)
  match results with
  | Some results ->
      let list = List.fold_right
	(fun i tail ->
	   <:patt< [ $lid:"c"^string_of_int i$ :: $tail$ ] >>
	)
	(range 0 (List.length results))
	<:patt< [] >> in

      let conversions =
	List.mapi (
	  fun i result ->
	    let field_type = result.PGOCaml.field_type in
	    let modifier = result.PGOCaml.modifier in
	    let fn = name_of_type_wrapper ~modifier my_dbh field_type in
	    let fn = fn ^ "_of_string" in
	    let nullable =
	      f_nullable_results ||
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
		  not not_nullable
	      | _ -> true (* Assume it could be nullable. *) in
	    let col = <:expr< $lid:"c" ^ string_of_int i$ >> in
	    if nullable then
	      <:expr< PGOCaml_aux.Option.map PGOCaml.$lid:fn$ $col$ >>
	    else
	      <:expr< PGOCaml.$lid:fn$ (try PGOCaml_aux.Option.get $col$ with _ -> failwith "pa_pgsql's nullability heuristic has failed - use \"nullable-results\"") >>
	) results in

      let convert =
	(* Avoid generating a single-element tuple. *)
	match conversions with
	| [] -> <:expr< () >>
	| [a] -> <:expr< $a$ >>
	| conversion :: conversions ->
	    (*<:expr< ( $list:conversions$ ) >> in
	     * http://caml.inria.fr/pub/ml-archives/caml-list/2007/05/ab49714d974451f669cf46455627466d.en.html
	     *)
	    <:expr< ( $conversion$, $Ast.exCom_of_list conversions$ ) >> in

      <:expr<
	PGOCaml.bind $expr$ (fun _rows ->
        PGOCaml.return
          (let original_query = $str:query$ in
           List.rev_map (
             fun row ->
               match row with
                 [ $list$ -> $convert$
                 | _ ->
                     (* This should never happen, even if the schema changes.
                      * Well, maybe if the user does 'SELECT *'.
                      *)
                     let msg = "pa_pgsql: internal error: " ^
                       "Incorrect number of columns returned from query: " ^
                       original_query ^
                       ".  Columns are: " ^
                       String.concat "; " (
                         List.map (
                           fun [ Some str -> Printf.sprintf "%S" str
                           | None -> "NULL" ]
                         ) row
                       ) in
                     raise (PGOCaml.Error msg) ]
           ) _rows))
      >>

  | None ->
      <:expr<
	PGOCaml.bind $expr$ (fun _rows -> PGOCaml.return ())
      >>

open Syntax

EXTEND Gram
  expr: LEVEL "top" [
    [ "PGSQL"; "("; dbh = expr; ")";
      extras = LIST1 [ x = STRING -> x ] ->
	let query, flags =
	  match List.rev extras with
	  | [] -> assert false
	  | query :: flags -> query, flags in
	pgsql_expand ~flags _loc dbh (Camlp4.Struct.Token.Eval.string query)
    ]
  ];
END

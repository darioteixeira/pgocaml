(* Test the lowlevel interface to the database.
 * Assumes that $PGHOST, etc. are set to point to a database.
 *
 * PG'OCaml - type safe interface to PostgreSQL.
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

open Printf

IFDEF USE_BATTERIES THEN
module List = struct
  include List
  include BatList
end
ELSE
open ExtList
ENDIF


let print_row i row =
  printf "row %d: [%s]\n" i
    (String.concat "; "
      (List.map (function
		| None -> "NULL"
		| Some str -> sprintf "%S" str) row))

let print_rows rows =
  List.iteri print_row rows

let print_params_description params =
  printf "params:\n";
  List.iteri (
    fun i param ->
      printf "  parameter %d:\n" i;
      printf "    type: %ld\n" param.PGOCaml.param_type
  ) params

let print_row_description results =
  match results with
  | None ->
      printf "this statement returns no data\n"
  | Some results ->
      printf "results:\n";
      List.iteri (
	fun i result ->
	  printf "  field %d:\n" i;
	  printf "    name: %s\n" result.PGOCaml.name;
	  printf "    type: %ld\n" result.PGOCaml.field_type
      ) results

let () =
  let dbh = PGOCaml.connect () in

  (* Simple query with no parameters. *)
  let query = "select current_timestamp" in
  let name = "timestamp_query" in
  ignore (PGOCaml.prepare dbh ~query ~name ());
  let i = ref 0 in
  PGOCaml.cursor dbh ~name ~params:[] (fun row -> incr i; print_row !i row);

  (* Describe the statement. *)
  let params, results = PGOCaml.describe_statement dbh ~name () in
  print_params_description params;
  print_row_description results;

  (* A query with parameters. *)
  let query = "select $1 + $2" in
  let types = [ 23l; 23l ] in (* 23 = int4 *)
  let name = "sum_query" in
  ignore (PGOCaml.prepare dbh ~query ~name ~types ());
  let i = ref 0 in
  PGOCaml.cursor dbh ~name ~params:[Some "1"; Some "2"] (fun row -> incr i; print_row !i row);

  (* Describe the statement. *)
  let params, results = PGOCaml.describe_statement dbh ~name () in
  print_params_description params;
  print_row_description results;

  (* Create a temporary table and populate it. *)
  let query = "create temporary table employees (
                 id serial not null primary key,
                 name text not null,
                 salary numeric(8,2) not null,
                 email text
               )" in
  ignore (PGOCaml.prepare dbh ~query ());
  ignore (PGOCaml.execute dbh ~params:[] ());

  let query =
    "insert into employees (name, salary, email) values ($1, $2, $3)" in
  ignore (PGOCaml.prepare dbh ~query ());
  let params, results = PGOCaml.describe_statement dbh () in
  print_params_description params;
  print_row_description results;

  ignore (PGOCaml.execute dbh
	    ~params:[Some "Ann"; Some "10000.00"; None] ());
  ignore (PGOCaml.execute dbh
	    ~params:[Some "Bob"; Some "45000.00"; None] ());
  ignore (PGOCaml.execute dbh
	    ~params:[Some "Jim"; Some "20000.00"; None] ());
  ignore (PGOCaml.execute dbh
	    ~params:[Some "Mary"; Some "30000.00"; None] ());

  let query = "select * from employees where salary > $1 order by id" in
  ignore (PGOCaml.prepare dbh ~query ());
  let params, results = PGOCaml.describe_statement dbh () in
  print_params_description params;
  print_row_description results;
  let rows = PGOCaml.execute dbh ~params:[Some "0"] () in
  print_rows rows;

  PGOCaml.close dbh

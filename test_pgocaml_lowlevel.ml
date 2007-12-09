(* Test the lowlevel interface to the database.
 * Assumes that $PGHOST, etc. are set to point to a database.
 * $Id: test_pgocaml_lowlevel.ml,v 1.1 2006/07/19 10:21:38 rich Exp $
 *)

open Printf

open ExtList

let print_rows rows =
  List.iteri (
    fun i row ->
      printf "row %d: [%s]\n" i
	(String.concat "; "
	   (List.map (function
		      | None -> "NULL"
		      | Some str -> sprintf "%S" str) row))
  ) rows

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
  let rows = PGOCaml.execute dbh ~name ~params:[] () in
  print_rows rows;

  (* Describe the statement. *)
  let params, results = PGOCaml.describe_statement dbh ~name () in
  print_params_description params;
  print_row_description results;

  (* A query with parameters. *)
  let query = "select $1 + $2" in
  let types = [ 23l; 23l ] in (* 23 = int4 *)
  let name = "sum_query" in
  ignore (PGOCaml.prepare dbh ~query ~name ~types ());
  let rows = PGOCaml.execute dbh ~name ~params:[Some "1"; Some "2"] () in
  print_rows rows;

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

(* Example program using typesafe calls to PostgreSQL.
 * $Id: test_pgocaml.ml,v 1.1 2006/07/19 10:21:38 rich Exp $
 *)

open Printf
open Lwt

let () =
  Lwt_unix.run
	(Lwt_PGOCaml.connect () >>=
	fun dbh ->

  LWT_PGSQL(dbh) "execute" "create temporary table employees (
     id serial not null primary key,
     name text not null,
     salary int4 not null,
     email text
  )";

  let insert name salary email =
    LWT_PGSQL(dbh) "insert into employees (name, salary, email)
                values ($name, $salary, $?email)"
  in
  insert "Ann" 10_000_l None;
  insert "Bob" 45_000_l None;
  insert "Jim" 20_000_l None;
  insert "Mary" 30_000_l (Some "mary@example.com");

  LWT_PGSQL(dbh) "select id, name, salary, email from employees" >>=
  fun rows -> Lwt_util.iter (
    fun (id, name, salary, email) ->
      let email = match email with Some email -> email | None -> "-" in
      printf "%ld %S %ld %S\n" id name salary email;
			return ()
  ) rows >>=

	fun () ->
  let ids = [ 1_l; 3_l ] in
  LWT_PGSQL(dbh) "select id, name, salary, email from employees
                          where id in $@ids" >>=
  fun rows -> Lwt_util.iter (
    fun (id, name, salary, email) ->
      let email = match email with Some email -> email | None -> "-" in
      printf "%ld %S %ld %S\n" id name salary email;
			return ()
  ) rows >>=
	fun () -> 
  Lwt_PGOCaml.close dbh)

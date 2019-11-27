let init_dbh dbh =
  let () = [%pgsql
    dbh "execute"
    " DO $$ BEGIN
      CREATE DOMAIN userid AS int not null;
      EXCEPTION
        WHEN duplicate_object THEN null;
      END $$"]
  in
  let () = [%pgsql dbh "execute" "create temporary table employees
    (
    userid serial primary key,
    name text not null,
    salary int not null,
    email text
    )"]
  in
  (* assume we've already done "CREATE DOMAIN eyylmao AS int" *)
  let () = [%pgsql
    dbh "execute"
    " DO $$ BEGIN
      CREATE DOMAIN cash_money AS float;
      EXCEPTION
        WHEN duplicate_object THEN null;
      END $$"]
  (* in
  let () = [%pgsql
    dbh
    "execute"
    " DO $$ BEGIN
        CREATE TYPE tuserid AS (userid);
      EXCEPTION
        WHEN duplicate_object THEN null;
    END $$"
  ] *)
  in
  [%pgsql dbh "execute" "CREATE TEMPORARY TABLE customtable (
    userid userid NOT NULL,
    salary cash_money NOT NULL
  )"]

module Userid: sig
  type t
  val to_string : t -> string
  val from_string : string -> t
  val to_int : t -> int
end = struct
  type t = int
  let to_string = string_of_int
  let from_string = int_of_string
  let to_int x = x
end

let employee_exists dbh ?email n =
  [%pgsql dbh "SELECT EXISTS (SELECT 1 FROM employees WHERE name = $n AND email = $?email)"]

let () =
  let dbh = PGOCaml.connect () in

  init_dbh dbh;

  let insert name salary email = [%pgsql dbh "insert into employees (name, salary, email) values ($name, $salary, $?email)"] in
  insert "Ann" 10_000_l None;
  insert "Bob" 45_000_l None;
  insert "Jim" 20_000_l None;
  insert "Mary" 30_000_l (Some "mary@example.com");

  let rows = [%pgsql dbh "select userid, name, salary, email from employees"] in
  List.iter
    begin
      fun (id, name, salary, email) ->
        let email = match email with Some email -> email | None -> "-" in
        Printf.printf "%d %S %ld %S\n" (Userid.to_int id) name salary email
    end rows;

  let ids = [ 1_l; 3_l ] in
  let rows = [%pgsql.object dbh "show=pp" "select * from employees where userid in $@ids"] in
  List.iter
    begin
      fun obj ->
        print_endline obj#pp
    end rows;
  let customid = Userid.from_string "69" in
  let customcash = "$420.00" in
  let () = [%pgsql dbh "INSERT INTO customtable (userid, salary) VALUES ($customid, $customcash)"] in
  let rows' =
    [%pgsql.object
      dbh
      "show"
      "SELECT * FROM customtable"]
  in
  List.iter
    begin
      fun obj ->
        Printf.printf "%d was paid %s" (Userid.to_int obj#userid) obj#salary
    end rows';

  PGOCaml.close dbh

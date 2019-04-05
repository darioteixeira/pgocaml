let () = Printexc.record_backtrace true

let init_dbh dbh =
  [%pgsql dbh "execute" "create temporary table employees
(
id serial not null primary key,
name text not null,
salary int4 not null,
email text
)"]

let init_dbh2 dbh =
  [%pgsql dbh "execute" "create temporary table owners
(
id serial not null primary key,
name text not null,
shares int not null
)"]

let employee_exists dbh ?email n =
  [%pgsql dbh "SELECT EXISTS (SELECT 1 FROM employees WHERE name = $n AND email = $?email AND email = $?email)"]

let get_combined dbh name =
  [%pgsql.nestedobject
    dbh
    "SELECT
      employees.*, owners.*
    FROM employees LEFT JOIN owners ON employees.name = owners.name
    WHERE employees.name = $name"]

let () =
  let dbh = PGOCaml.connect () in

  init_dbh dbh;
  init_dbh2 dbh;

  let insert name salary email = [%pgsql dbh "insert into employees (name, salary, email) values ($name, $salary, $?email)"] in
  insert "Ann" 10_000_l None;
  insert "Bob" 45_000_l None;
  insert "Jim" 20_000_l None;
  insert "Mary" 30_000_l (Some "mary@example.com");

  let rows = [%pgsql dbh "select id, name, salary, email from employees"] in
  List.iter
    begin
      fun (id, name, salary, email) ->
        let email = match email with Some email -> email | None -> "-" in
        Printf.printf "%ld %S %ld %S\n" id name salary email
    end rows;

  let ids = [ 1_l; 3_l ] in
  let rows = [%pgsql dbh "select id, name, salary, email from employees where id in $@ids"] in
  List.iter
    begin
      fun (id, name, salary, email) ->
        let email = match email with Some email -> email | None -> "-" in
        Printf.printf "%ld %S %ld %S\n" id name salary email
    end rows;

  let insert_owner name =
    [%pgsql dbh "INSERT INTO owners (name, shares) VALUES ($name, 0)"]
  in
  let _ = insert_owner "Ann" in
  let x = get_combined dbh "Ann" |> List.hd in
  Printf.printf "Ann's ID's: employee: %d, owner: %d\n"
    (Int32.to_int x#employees#id)
    (Int32.to_int x#owners#id);

  let give_share name =
    let () =
      let exists =
        match [%pgsql dbh "SELECT EXISTS(SELECT * FROM owners WHERE name = $name)"] with
          | [Some x] -> x
          | _ -> false
      in
      if exists
      then insert_owner name
      else ()
    in
    let o = List.hd [%pgsql.object dbh "SELECT * FROM owners WHERE name = $name"] in
    [%pgsql dbh "UPDATE owners SET shares = ${Int32.of_int ((Int32.to_int o#shares) + 1)} WHERE name = $name"]
  in
  ignore(give_share "Ann");

  PGOCaml.close dbh

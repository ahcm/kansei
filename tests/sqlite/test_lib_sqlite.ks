use std::IO
use std::lib::Sqlite

IO = std::IO
Sqlite = std::lib::Sqlite

db_path = "tmp_sqlite_test.db"
if IO.exists(db_path)
  IO.remove(db_path)
end

db = Sqlite.open(db_path)
Sqlite.exec(db, "create table if not exists items (id integer, name text)")
Sqlite.exec(db, "delete from items")
Sqlite.exec(db, "insert into items values (1, 'alpha')")
Sqlite.exec(db, "insert into items values (2, 'beta')")

rows = Sqlite.query(db, "select id, name from items order by id")
puts rows

if IO.exists(db_path)
  IO.remove(db_path)
end

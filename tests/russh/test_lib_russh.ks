use std::IO
use std::OS
use std::lib::Russh

IO = std::IO
OS = std::OS
Russh = std::lib::Russh

key_path = "tests/russh/tmp_host_key"
if IO.exists(key_path)
  IO.remove(key_path)
end
if IO.exists(key_path + ".pub")
  IO.remove(key_path + ".pub")
end

keygen = OS.run("ssh-keygen", ["-t", "ed25519", "-N", "", "-f", key_path])
if not keygen.success
  puts "SKIP: ssh-keygen not available"
else
  chosen_port = "42331"
  server = Russh.server_start(f"127.0.0.1:{chosen_port}", key_path, "demo", "demo-pass", true)
  OS.run("sleep", ["0.2"])
  client = Russh.client_connect("127.0.0.1", i64(chosen_port), true)
  if client.auth_none("demo") == true
    ch = client.open_session()
    ch.exec("echo hi", true)

    event_count = 0
    i = 0
    while i < 40
      e = server.poll_event(200)
      if e != nil
        event_count = event_count + 1
      end
      i = i + 1
    end

    puts f"russh events: {event_count}"
    ch.close()
    client.disconnect("test done")
  else
    puts "SKIP: auth_none failed"
  end
  server.stop()
end

if IO.exists(key_path)
  IO.remove(key_path)
end
if IO.exists(key_path + ".pub")
  IO.remove(key_path + ".pub")
end

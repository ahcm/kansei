export hello::[greet, version]

VERSION = "0.1.0"

fn greet(name)
  if name == nil
    "hello"
  else
    "hello " + name
  end
end

fn version()
  VERSION
end

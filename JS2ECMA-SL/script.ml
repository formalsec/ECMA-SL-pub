let () =
  if Array.length Sys.argv < 2 then
    Format.ksprintf failwith "Usage: %s <js script>@." Sys.argv.(0)

let () =
  let code = Sys.command "npm -v > /dev/null 2>&1" in
  if code <> 0 then begin
    Format.eprintf "Please install 'npm'@.";
    exit code
  end;
  let pkg_v = "npm exec -- pkg -v > /dev/null 2>&1" in
  let code = Sys.command pkg_v in
  if code <> 0 then begin
    let code = Sys.command "npm i pkg > /dev/null 2>&1" in
    if code <> 0 then begin
      Format.eprintf "Failed to install 'pkg'@.";
      exit code
    end;
    assert (Sys.command pkg_v = 0)
  end

let () =
  let code = Sys.command "npm i -s" in
  if code <> 0 then begin
    Format.eprintf "Failed to install project dependencies@.";
    exit code
  end;
  let js_program = Sys.argv.(1) in
  let targets = [ "linux"; "macos" ] in
  List.iter
    (fun target ->
      let code =
        Format.ksprintf Sys.command
          "npm exec -- pkg %s --targets node18-%s-x64 -o ./bin/js2ecma-sl-%s > \
           /dev/null 2>&1"
          js_program target target
      in
      if code <> 0 then begin
        Format.eprintf "Failed to compile 'j2ecma-sl'@.";
        exit code
      end )
    targets;
  exit 0

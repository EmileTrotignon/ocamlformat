open Out_channel

let scoped name f = 
  let channel = open_out name in
  let r = f channel in
  close channel ; r

let write_all name ~data= 
  scoped name (fun ch -> output_string ch data)

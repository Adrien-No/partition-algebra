Printexc.record_backtrace true

open Utils.Diagram_single_uf

module type P = sig val k : int end

module Partition = Diagram (struct let k = 4 end : P)

let _ =
  let _s = Partition.s_i 3 in
  let _s' = Partition.s_i 2 in
  Partition.print (_s);
  Partition.print (_s);
  Partition.print_empty();
  Partition.print (Partition.concat _s _s')

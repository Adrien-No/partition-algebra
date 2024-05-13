Printexc.record_backtrace true

open Utils.Partition_algebra

module type P = sig val k : int end

module Partition = PartitionAlgebra (struct let k = 3 end : P)

let _ =
  let s = Partition.s_i 1 in
  let s' = Partition.s_i 1 in
  Partition.print (Partition.concat s s')

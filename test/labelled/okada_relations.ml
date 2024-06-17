let _ =
for k = 0 to 5 do
  let module D = Lib.Labelled.Make(struct let k = k end) in
  let open D in
  for i = 1 to k do
    assert (p i @ p i === p i)
  done;
  for i = 1 to k-1 do
    assert (e i @ e i === e i);
    assert (b i @ b i === b i)
  done;
  for i = 1 to k-2 do
    assert (e (i+1) @ e i @ e (i+1) === e (i+1));
    assert ((* p (i+1) @  *)p i @ p (i+1) === p (i+1) @ p i)
  done;
done

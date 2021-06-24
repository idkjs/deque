module type LIST = {
  type t('a);
  let empty: t('a);
  let cons: ('a, t('a)) => t('a);
  let uncons: t('a) => option(('a, t('a)));
  let fold_left: (('a, 'b) => 'a, 'a, t('b)) => 'a;
};

let bench = (name, f) => {
  let t0 = Unix.gettimeofday();
  let result = f();
  let t1 = Unix.gettimeofday();
  Printf.printf("%20s : %.3f s\n%!", name, t1 -. t0);
  result;
};

module Lst = {
  type t('a) = list('a);
  let empty = [];
  let cons = (x, xs) => [x, ...xs];
  let uncons =
    fun
    | [] => None
    | [x, ...xs] => Some((x, xs));
  let fold_left = List.fold_left;
};

module Test = (L: LIST) => {
  let make = n => {
    let rec go = (acc, i) =>
      if (i == 0) {
        acc;
      } else {
        go(L.cons(i, acc), i - 1);
      };

    go(L.empty, n);
  };

  let sum_foldl = xs => L.fold_left((+), 0, xs);

  let rec sum_uncons = (acc, xs) =>
    switch (L.uncons(xs)) {
    | None => acc
    | Some((x, xs)) => sum_uncons(acc + x, xs)
    };

  let sum_uncons = xs => sum_uncons(0, xs);

  let () = {
    let xs = bench("make 10m", () => make(10_000_000));
    let x = bench("sum_foldl", () => sum_foldl(xs));
    let y = bench("sum_uncons", () => sum_uncons(xs));
    assert(x == y);
    ();
  };
};

let () = Printf.printf("-- List -------------------\n%!");
module A = Test(Lst);
let () = Printf.printf("\n%!");

let () = Printf.printf("-- Dequeue ----------------\n%!");
module B = Test(Deque.Dequeue);
let () = Printf.printf("\n%!");

let () = Printf.printf("-- Steque -----------------\n%!");
module C = Test(Deque.Steque);
let () = Printf.printf("\n%!");

let () = Printf.printf("-- Deck -------------------\n%!");
module D = Test(Deque.Deck);
let () = Printf.printf("\n%!");

let () = Printf.printf("-- Deckrev ----------------\n%!");
module E = Test(Deque.Deckrev);

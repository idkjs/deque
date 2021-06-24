module type DEQUE = {
  type t('a);
  let empty: t('a);
  let cons: ('a, t('a)) => t('a);
  let snoc: (t('a), 'a) => t('a);
  let uncons: t('a) => option(('a, t('a)));
  let unsnoc: t('a) => option((t('a), 'a));
  let rev: t('a) => t('a);
  let to_list: t('a) => list('a);
  let length: t('a) => int;
  let nth: (t('a), int) => 'a;
};

module Naive = {
  type t('a) = list('a);

  let empty = [];

  let cons = (x, t) => [x, ...t];

  let uncons =
    fun
    | [] => None
    | [x, ...t] => Some((x, t));

  let unsnoc = t => {
    let rec go = acc =>
      fun
      | [] => None
      | [x] => Some((List.rev(acc), x))
      | [x, ...t] => go([x, ...acc], t);

    go([], t);
  };

  let concat = (a, b) => List.rev_append(List.rev(a), b);
  let snoc = (t, x) => concat(t, [x]);
  let rev = List.rev;
  let to_list = t => t;
  let length = List.length;
  let nth = List.nth;
};

module Bi = (A: DEQUE, B: DEQUE) => {
  type t('a) = (A.t('a), B.t('a));

  let rec uncons_to_list = t =>
    switch (B.uncons(t)) {
    | None => []
    | Some((x, t)) => [x, ...uncons_to_list(t)]
    };

  let rec unsnoc_to_list = (acc, t) =>
    switch (B.unsnoc(t)) {
    | None => acc
    | Some((t, x)) => unsnoc_to_list([x, ...acc], t)
    };
  let unsnoc_to_list = t => unsnoc_to_list([], t);

  let make = (a, b) => {
    let xs = A.to_list(a);
    let ys = B.to_list(b);
    assert(xs == ys);
    assert(xs == uncons_to_list(b));
    assert(xs == unsnoc_to_list(b));
    assert(List.length(xs) == B.length(b));
    (a, b);
  };

  let empty = make(A.empty, B.empty);

  let cons = (x, (a, b)) => make(A.cons(x, a), B.cons(x, b));

  let snoc = ((a, b), x) => make(A.snoc(a, x), B.snoc(b, x));

  let uncons = ((a, b)) =>
    switch (A.uncons(a), B.uncons(b)) {
    | (None, None) => None
    | (Some((x, a)), Some((x', b))) =>
      assert(x == x');
      Some((x, make(a, b)));
    | _ => assert(false)
    };

  let unsnoc = ((a, b)) =>
    switch (A.unsnoc(a), B.unsnoc(b)) {
    | (None, None) => None
    | (Some((a, x)), Some((b, x'))) =>
      assert(x == x');
      Some((make(a, b), x));
    | _ => assert(false)
    };

  let rev = ((a, b)) => make(A.rev(a), B.rev(b));

  let check_nth = ((a, b)) => {
    let n = B.length(b);
    assert(n == A.length(a));
    if (n == 0) {
      ();
    } else {
      let i = Random.int(n);
      let x = A.nth(a, i);
      let y = B.nth(b, i);
      assert(x == y);
    };
  };
};

module Test = (X: DEQUE) => {
  module D2 = Bi(Naive, X);

  let elt = () => Random.int(10000);

  let some_fst = t =>
    fun
    | None => t
    | Some((t, _)) => t;

  let some_snd = t =>
    fun
    | None => t
    | Some((_, t)) => t;

  let test = t =>
    switch (Random.int(6)) {
    | 0 => D2.cons(elt(), t)
    | 1 => D2.snoc(t, elt())
    | 2 => some_snd(t, D2.uncons(t))
    | 3 => some_fst(t, D2.unsnoc(t))
    | 4 => D2.rev(t)
    | 5 =>
      D2.check_nth(t);
      t;
    | _ => assert(false)
    };

  let rec test_repeatedly = (n, t) =>
    if (n <= 0) {
      ();
    } else {
      test_repeatedly(n - 1, test(t));
    };

  let () = test_repeatedly(100000, D2.empty);
};

module A = Test(Deque.Dequeue);
module B = Test(Deque.Deck);
module C = Test(Deque.Deckrev);

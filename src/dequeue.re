module Base = {
  include Dequeue_internal;

  let fold_left: type a z. ((z, a) => z, z, s(a)) => z =
    (f, z, T(t)) => {
      let list_of_buffer: type b c. ((z, b) => z, z, buffer(b, c)) => z =
        (f, z, buf) =>
          switch (buf) {
          | B0 => z
          | B1(a) => f(z, a)
          | [@implicit_arity] B2(a, b) => f(f(z, a), b)
          | [@implicit_arity] B3(a, b, c) => f(f(f(z, a), b), c)
          | [@implicit_arity] B4(a, b, c, d) => f(f(f(f(z, a), b), c), d)
          | [@implicit_arity] B5(a, b, c, d, e) =>
            f(f(f(f(f(z, a), b), c), d), e)
          };

      let rec go:
        type b1 b2 c1 c2.
          ((z, b1) => z, z, deque(b1, b2, c1), kont(b2, c2)) => z =
        (f, z, deq, kont) =>
          switch (deq) {
          | HOLE => go_kont(f, z, kont)
          | [@implicit_arity] Yellow(prefix, child, suffix) =>
            let z = list_of_buffer(f, z, prefix);
            let z = go(go_pair(f), z, child, kont);
            list_of_buffer(f, z, suffix);
          | [@implicit_arity] Green(prefix, child, suffix) =>
            let z = list_of_buffer(f, z, prefix);
            let z = go(go_pair(f), z, child, kont);
            list_of_buffer(f, z, suffix);
          | [@implicit_arity] Red(prefix, child, suffix) =>
            let z = list_of_buffer(f, z, prefix);
            let z = go(go_pair(f), z, child, kont);
            list_of_buffer(f, z, suffix);
          }

      and go_pair: type b. ((z, b) => z, z, (b, b)) => z =
        (f, z, (x, y)) => f(f(z, x), y)

      and go_kont: type b c. ((z, b) => z, z, kont(b, c)) => z =
        (f, z, kont) =>
          switch (kont) {
          | Small(buf) => list_of_buffer(f, z, buf)
          | [@implicit_arity] Y(child, kont) => go(f, z, child, kont)
          | [@implicit_arity] R(child, kont) => go(f, z, child, kont)
          | [@implicit_arity] G(child, kont) => go(f, z, child, kont)
          };

      go_kont(f, z, t);
    };

  let fold_right: type a z. ((a, z) => z, s(a), z) => z =
    (f, T(t), z) => {
      let list_of_buffer: type b c. ((b, z) => z, buffer(b, c), z) => z =
        (f, buf, z) =>
          switch (buf) {
          | B0 => z
          | B1(a) => f(a, z)
          | [@implicit_arity] B2(a, b) => f(a, f(b, z))
          | [@implicit_arity] B3(a, b, c) => f(a, f(b, f(c, z)))
          | [@implicit_arity] B4(a, b, c, d) => f(a, f(b, f(c, f(d, z))))
          | [@implicit_arity] B5(a, b, c, d, e) =>
            f(a, f(b, f(c, f(d, f(e, z)))))
          };

      let rec go:
        type b1 b2 c1 c2.
          ((b1, z) => z, deque(b1, b2, c1), z, kont(b2, c2)) => z =
        (f, deq, z, kont) =>
          switch (deq) {
          | HOLE => go_kont(f, kont, z)
          | [@implicit_arity] Yellow(prefix, child, suffix) =>
            let z = list_of_buffer(f, suffix, z);
            let z = go(go_pair(f), child, z, kont);
            list_of_buffer(f, prefix, z);
          | [@implicit_arity] Green(prefix, child, suffix) =>
            let z = list_of_buffer(f, suffix, z);
            let z = go(go_pair(f), child, z, kont);
            list_of_buffer(f, prefix, z);
          | [@implicit_arity] Red(prefix, child, suffix) =>
            let z = list_of_buffer(f, suffix, z);
            let z = go(go_pair(f), child, z, kont);
            list_of_buffer(f, prefix, z);
          }

      and go_pair: type b. ((b, z) => z, (b, b), z) => z =
        (f, (x, y), z) => f(x, f(y, z))

      and go_kont: type b c. ((b, z) => z, kont(b, c), z) => z =
        (f, kont, z) =>
          switch (kont) {
          | Small(buf) => list_of_buffer(f, buf, z)
          | [@implicit_arity] Y(child, kont) => go(f, child, z, kont)
          | [@implicit_arity] R(child, kont) => go(f, child, z, kont)
          | [@implicit_arity] G(child, kont) => go(f, child, z, kont)
          };

      go_kont(f, t, z);
    };

  let fold_left = (f, z, {length, s}) =>
    if (length >= 0) {
      fold_left(f, z, s);
    } else {
      fold_right((x, z) => f(z, x), s, z);
    }

  and fold_right = (f, {length, s}, z) =>
    if (length >= 0) {
      fold_right(f, s, z);
    } else {
      fold_left((x, z) => f(z, x), z, s);
    };

  let compare_lengths = (xs, ys) => compare(length(xs), length(ys));

  let append = (xs, ys) =>
    if (compare_lengths(xs, ys) <= 0) {
      fold_right(cons, xs, ys);
    } else {
      fold_left(snoc, xs, ys);
    };
};

include List_like.Make(Base);
include Base;

let nth: type a. (s(a), int, int) => a =
  (T(t), i, j) => {
    let buffer_length: type b c. (int, buffer(b, c)) => int =
      s =>
        fun
        | B0 => 0
        | B1(_) => s
        | B2(_) => 2 * s
        | B3(_) => 3 * s
        | B4(_) => 4 * s
        | B5(_) => 5 * s;

    let buffer: type b c. (int, int, (int, int, b) => a, buffer(b, c)) => a =
      (i, s, search) =>
        fun
        | B0 => assert(false)
        | B1(a) => search(i, s, a)
        | [@implicit_arity] B2(a, b) =>
          if (i < s) {
            search(i, s, a);
          } else {
            search(i - s, s, b);
          }
        | [@implicit_arity] B3(a, b, c) =>
          switch (i / s) {
          | 0 => search(i, s, a)
          | 1 => search(i - s, s, b)
          | 2 => search(i - 2 * s, s, c)
          | _ => assert(false)
          }
        | [@implicit_arity] B4(a, b, c, d) =>
          switch (i / s) {
          | 0 => search(i, s, a)
          | 1 => search(i - s, s, b)
          | 2 => search(i - 2 * s, s, c)
          | 3 => search(i - 3 * s, s, d)
          | _ => assert(false)
          }
        | [@implicit_arity] B5(a, b, c, d, e) =>
          switch (i / s) {
          | 0 => search(i, s, a)
          | 1 => search(i - s, s, b)
          | 2 => search(i - 2 * s, s, c)
          | 3 => search(i - 3 * s, s, d)
          | 4 => search(i - 4 * s, s, e)
          | _ => assert(false)
          };

    let rec go:
      type b1 b2 c1 c2.
        (
          int,
          int,
          int,
          (int, int, b1) => a,
          deque(b1, b2, c1),
          kont(b2, c2)
        ) =>
        a =
      (i, j, s, search, deq, kont) =>
        switch (deq) {
        | HOLE => go_kont(i, j, s, search, kont)
        | [@implicit_arity] Yellow(prefix, child, suffix) =>
          go_level(i, j, s, search, prefix, suffix, child, kont)
        | [@implicit_arity] Green(prefix, child, suffix) =>
          go_level(i, j, s, search, prefix, suffix, child, kont)
        | [@implicit_arity] Red(prefix, child, suffix) =>
          go_level(i, j, s, search, prefix, suffix, child, kont)
        }

    and go_level:
      type b1 c1 c2 c3 d3 d4.
        (
          int,
          int,
          int,
          (int, int, b1) => a,
          buffer(b1, c1),
          buffer(b1, c2),
          deque((b1, b1), c3, d3),
          kont(c3, d4)
        ) =>
        a =
      (i, j, s, search, prefix, suffix, child, kont) => {
        let prefix_len = buffer_length(s, prefix);
        let suffix_len = buffer_length(s, suffix);
        if (i < prefix_len) {
          buffer(i, s, search, prefix);
        } else if (j < suffix_len) {
          buffer(suffix_len - j - 1, s, search, suffix);
        } else {
          let (i, j, s) = (i - prefix_len, j - suffix_len, 2 * s);
          go(i, j, s, go_pair(search), child, kont);
        };
      }

    and go_pair: type b. ((int, int, b) => a, int, int, (b, b)) => a =
      (f, i, s, (x, y)) => {
        let s2 = s / 2;
        if (i < s2) {
          f(i, s2, x);
        } else {
          f(i - s2, s2, y);
        };
      }

    and go_kont:
      type b c. (int, int, int, (int, int, b) => a, kont(b, c)) => a =
      (i, j, s, search) =>
        fun
        | Small(buf) => buffer(i, s, search, buf)
        | [@implicit_arity] Y(child, kont) =>
          go(i, j, s, search, child, kont)
        | [@implicit_arity] R(child, kont) =>
          go(i, j, s, search, child, kont)
        | [@implicit_arity] G(child, kont) =>
          go(i, j, s, search, child, kont);

    let search1: (int, int, a) => a = ((_, _, x) => x: (int, int, a) => a);

    go_kont(i, j, 1, search1, t);
  };

let nth = (t, i) => {
  if (i < 0) {
    invalid_arg("Dequeue.nth");
  };
  let j = length(t) - i - 1;
  if (j < 0) {
    failwith("Dequeue.nth");
  };
  if (t.length >= 0) {
    nth(t.s, i, j);
  } else {
    nth(t.s, j, i);
  };
};

let nth_opt = (t, i) =>
  try(Some(nth(t, i))) {
  | Failure(_) => None
  };

let rec make: type a. (int, a) => kont(a, [ | `green]) =
  (n, x) =>
    switch (n) {
    | 0 => Small(B0)
    | 1 => Small(B1(x))
    | 2 => Small([@implicit_arity] B2(x, x))
    | 3 => Small([@implicit_arity] B3(x, x, x))
    | _ =>
      let n = n - 4;
      switch (n mod 2) {
      | 0 =>
        let b = [@implicit_arity] B2(x, x);
        let x2 = (x, x);
        [@implicit_arity]
        G([@implicit_arity] Green(b, HOLE, b), make(n / 2, x2));
      | 1 =>
        let p = [@implicit_arity] B3(x, x, x);
        let s = [@implicit_arity] B2(x, x);
        let x2 = (x, x);
        [@implicit_arity]
        G([@implicit_arity] Green(p, HOLE, s), make((n - 1) / 2, x2));
      | _ => assert(false)
      };
    };

let make = (n, x) => {length: n, s: T(make(n, x))};

let rec of_list: type a. list(a) => kont(a, [ | `green]) =
  fun
  | [] => Small(B0)
  | [a] => Small(B1(a))
  | [a, b] => Small([@implicit_arity] B2(a, b))
  | [a, b, c] => Small([@implicit_arity] B3(a, b, c))
  | [a, b, c, d, ...lst] => {
      let p = [@implicit_arity] B2(a, b);
      let rec go = acc => (
        fun
        | (c, d, []) => (acc, [@implicit_arity] B2(c, d))
        | (c, d, [e]) => (acc, [@implicit_arity] B3(c, d, e))
        | (c, d, [e, f, ...xs]) => go([(c, d), ...acc], (e, f, xs))
      );

      let (lst, s) = go([], (c, d, lst));
      [@implicit_arity]
      G([@implicit_arity] Green(p, HOLE, s), of_list(List.rev(lst)));
    };

let of_list = lst => {length: List.length(lst), s: T(of_list(lst))};

let singleton = x => {length: 1, s: T(Small(B1(x)))};

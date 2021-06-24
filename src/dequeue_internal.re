type buffer('a, 'color) =
  | B0: buffer('a, [ | `red])
  | B1('a): buffer('a, [ | `yellow])
  | B2('a, 'a): buffer('a, [< | `green | `yellow])
  | B3('a, 'a, 'a): buffer('a, [< | `green | `yellow])
  | B4('a, 'a, 'a, 'a): buffer('a, [ | `yellow])
  | B5('a, 'a, 'a, 'a, 'a): buffer('a, [ | `red]);

type yellow_buffer('a) =
  | Yellowish(buffer('a, [< | `green | `yellow])): yellow_buffer('a);

type any_buffer('a) =
  | Any(buffer('a, [< | `green | `yellow | `red])): any_buffer('a);

type deque('a, 'b, 'color) =
  | HOLE: deque('a, 'a, [ | `kont])
  | Yellow(
      buffer('a, [< | `green | `yellow]),
      deque(('a, 'a), 'b, [< | `yellow | `kont]),
      buffer('a, [< | `green | `yellow]),
    )
    : deque('a, 'b, [ | `yellow])
  | Green(
      buffer('a, [ | `green]),
      deque(('a, 'a), 'b, [< | `yellow | `kont]),
      buffer('a, [ | `green]),
    )
    : deque('a, 'b, [ | `green])
  | Red(
      buffer('a, [< | `green | `yellow | `red]),
      deque(('a, 'a), 'b, [< | `yellow | `kont]),
      buffer('a, [< | `green | `yellow | `red]),
    )
    : deque('a, 'b, [ | `red]);

type kont('a, 'color) =
  | Small(buffer('a, _)): kont('a, [ | `green])
  | G(deque('a, 'b, [ | `green]), kont('b, [< | `green | `red]))
    : kont('a, [ | `green])
  | Y(deque('a, 'b, [ | `yellow]), kont('b, [ | `green]))
    : kont('a, [ | `yellow])
  | R(deque('a, 'b, [ | `red]), kont('b, [ | `green]))
    : kont('a, [ | `red]);

type s('a) =
  | T(kont('a, [< | `green | `yellow])): s('a);

let green_prefix_cons:
  type a. (a, buffer(a, [ | `green])) => buffer(a, [ | `yellow]) =
  (x, buf) =>
    switch (buf) {
    |  B2(a, b) =>  B3(x, a, b)
    |  B3(a, b, c) =>  B4(x, a, b, c)
    };

let green_suffix_snoc:
  type a. (buffer(a, [ | `green]), a) => buffer(a, [ | `yellow]) =
  (buf, x) =>
    switch (buf) {
    |  B2(a, b) =>  B3(a, b, x)
    |  B3(a, b, c) =>  B4(a, b, c, x)
    };

let yellow_prefix_cons: type a. (a, yellow_buffer(a)) => any_buffer(a) =
  (x, Yellowish(buf)) =>
    switch (buf) {
    | B1(a) => Any( B2(x, a))
    |  B2(a, b) => Any( B3(x, a, b))
    |  B3(a, b, c) => Any( B4(x, a, b, c))
    |  B4(a, b, c, d) =>
      Any( B5(x, a, b, c, d))
    };

let yellow_suffix_snoc: type a. (yellow_buffer(a), a) => any_buffer(a) =
  (Yellowish(buf), x) =>
    switch (buf) {
    | B1(a) => Any( B2(a, x))
    |  B2(a, b) => Any( B3(a, b, x))
    |  B3(a, b, c) => Any( B4(a, b, c, x))
    |  B4(a, b, c, d) =>
      Any( B5(a, b, c, d, x))
    };

let buffer_cons: type a c. (a, buffer(a, c)) => kont(a, [ | `green]) =
  (x, buf) =>
    switch (buf) {
    | B0 => Small(B1(x))
    | B1(a) => Small( B2(x, a))
    |  B2(a, b) => Small( B3(x, a, b))
    |  B3(a, b, c) =>
      Small( B4(x, a, b, c))
    |  B4(a, b, c, d) =>
      Small( B5(x, a, b, c, d))
    |  B5(a, b, c, d, e) =>

      G(

        Green(
           B3(x, a, b),
          HOLE,
           B3(c, d, e),
        ),
        Small(B0),
      )
    };

let buffer_snoc: type a c. (buffer(a, c), a) => kont(a, [ | `green]) =
  (buf, x) =>
    switch (buf) {
    | B0 => Small(B1(x))
    | B1(a) => Small( B2(a, x))
    |  B2(a, b) => Small( B3(a, b, x))
    |  B3(a, b, c) =>
      Small( B4(a, b, c, x))
    |  B4(a, b, c, d) =>
      Small( B5(a, b, c, d, x))
    |  B5(a, b, c, d, e) =>

      G(

        Green(
           B3(a, b, c),
          HOLE,
           B3(d, e, x),
        ),
        Small(B0),
      )
    };

let green_uncons: type a. buffer(a, [ | `green]) => (a, yellow_buffer(a)) =
  fun
  |  B2(a, b) => (a, Yellowish(B1(b)))
  |  B3(a, b, c) => (
      a,
      Yellowish( B2(b, c)),
    );

let green_unsnoc: type a. buffer(a, [ | `green]) => (yellow_buffer(a), a) =
  fun
  |  B2(a, b) => (Yellowish(B1(a)), b)
  |  B3(a, b, c) => (
      Yellowish( B2(a, b)),
      c,
    );

let yellow_uncons: type a. yellow_buffer(a) => (a, any_buffer(a)) =
  (Yellowish(buf)) =>
    switch (buf) {
    | B1(a) => (a, Any(B0))
    |  B2(a, b) => (a, Any(B1(b)))
    |  B3(a, b, c) => (a, Any( B2(b, c)))
    |  B4(a, b, c, d) => (
        a,
        Any( B3(b, c, d)),
      )
    };

let yellow_unsnoc: type a. yellow_buffer(a) => (any_buffer(a), a) =
  (Yellowish(buf)) =>
    switch (buf) {
    | B1(a) => (Any(B0), a)
    |  B2(a, b) => (Any(B1(a)), b)
    |  B3(a, b, c) => (Any( B2(a, b)), c)
    |  B4(a, b, c, d) => (
        Any( B3(a, b, c)),
        d,
      )
    };

let buffer_uncons_unsafe: type a c. buffer(a, c) => (a, any_buffer(a)) =
  fun
  | B0 => assert(false)
  | B1(_) as buf => yellow_uncons(Yellowish(buf))
  | B2(_) as buf => yellow_uncons(Yellowish(buf))
  | B3(_) as buf => yellow_uncons(Yellowish(buf))
  | B4(_) as buf => yellow_uncons(Yellowish(buf))
  |  B5(a, b, c, d, e) => (
      a,
      Any( B4(b, c, d, e)),
    );

let buffer_unsnoc_unsafe: type a c. buffer(a, c) => (any_buffer(a), a) =
  fun
  | B0 => assert(false)
  | B1(_) as buf => yellow_unsnoc(Yellowish(buf))
  | B2(_) as buf => yellow_unsnoc(Yellowish(buf))
  | B3(_) as buf => yellow_unsnoc(Yellowish(buf))
  | B4(_) as buf => yellow_unsnoc(Yellowish(buf))
  |  B5(a, b, c, d, e) => (
      Any( B4(a, b, c, d)),
      e,
    );

let buffer_uncons: type a c. buffer(a, c) => option((a, any_buffer(a))) =
  fun
  | B0 => None
  | buf => Some(buffer_uncons_unsafe(buf));

let buffer_unsnoc: type a c. buffer(a, c) => option((any_buffer(a), a)) =
  fun
  | B0 => None
  | buf => Some(buffer_unsnoc_unsafe(buf));

let prefix_rot: type a c. (a, buffer(a, c)) => (buffer(a, c), a) =
  (x, buf) =>
    switch (buf) {
    | B0 => (B0, x)
    | B1(a) => (B1(x), a)
    |  B2(a, b) => ( B2(x, a), b)
    |  B3(a, b, c) => ( B3(x, a, b), c)
    |  B4(a, b, c, d) => (
         B4(x, a, b, c),
        d,
      )
    |  B5(a, b, c, d, e) => (
         B5(x, a, b, c, d),
        e,
      )
    };

let suffix_rot: type a c. (buffer(a, c), a) => (a, buffer(a, c)) =
  (buf, x) =>
    switch (buf) {
    | B0 => (x, B0)
    | B1(a) => (a, B1(x))
    |  B2(a, b) => (a,  B2(b, x))
    |  B3(a, b, c) => (a,  B3(b, c, x))
    |  B4(a, b, c, d) => (
        a,
         B4(b, c, d, x),
      )
    |  B5(a, b, c, d, e) => (
        a,
         B5(b, c, d, e, x),
      )
    };

type decompose('a) =
  | Underflow(option('a)): decompose('a)
  | Ok(buffer('a, [ | `green])): decompose('a)
  | Overflow(buffer('a, [ | `green]), ('a, 'a)): decompose('a);

let prefix_decompose: type a c. buffer(a, c) => decompose(a) =
  fun
  | B0 => Underflow(None)
  | B1(x) => Underflow(Some(x))
  | B2(_) as b => Ok(b)
  | B3(_) as b => Ok(b)
  |  B4(a, b, c, d) =>
     Overflow( B2(a, b), (c, d))
  |  B5(a, b, c, d, e) =>
     Overflow( B3(a, b, c), (d, e));

let suffix_decompose: type a c. buffer(a, c) => decompose(a) =
  fun
  | B0 => Underflow(None)
  | B1(x) => Underflow(Some(x))
  | B2(_) as b => Ok(b)
  | B3(_) as b => Ok(b)
  |  B4(a, b, c, d) =>
     Overflow( B2(c, d), (a, b))
  |  B5(a, b, c, d, e) =>
     Overflow( B3(c, d, e), (a, b));

let prefix23 = (opt, (b, c)) =>
  switch (opt) {
  | None =>  B2(b, c)
  | Some(a) =>  B3(a, b, c)
  };

let suffix23 = ((a, b), opt) =>
  switch (opt) {
  | None =>  B2(a, b)
  | Some(c) =>  B3(a, b, c)
  };

let prefix12 = (x, opt) =>
  switch (opt) {
  | None => B1(x)
  | Some(y) =>  B2(x, y)
  };

let green_prefix_concat:
  type a c.
    (buffer(a, c), buffer((a, a), [ | `green])) =>
    (buffer(a, [ | `green]), yellow_buffer((a, a))) =
  (buf1, buf2) =>
    switch (prefix_decompose(buf1)) {
    | Ok(buf1) => (buf1, Yellowish(buf2))
    | Underflow(opt) =>
      let (ab, buf2) = green_uncons(buf2);
      (prefix23(opt, ab), buf2);
    |  Overflow(buf1, ab) => (
        buf1,
        Yellowish(green_prefix_cons(ab, buf2)),
      )
    };

let green_suffix_concat:
  type a c.
    (buffer((a, a), [ | `green]), buffer(a, c)) =>
    (yellow_buffer((a, a)), buffer(a, [ | `green])) =
  (buf1, buf2) =>
    switch (suffix_decompose(buf2)) {
    | Ok(buf2) => (Yellowish(buf1), buf2)
    | Underflow(opt) =>
      let (buf1, ab) = green_unsnoc(buf1);
      (buf1, suffix23(ab, opt));
    |  Overflow(buf2, ab) => (
        Yellowish(green_suffix_snoc(buf1, ab)),
        buf2,
      )
    };

let prefix_concat = (buf1, buf2) =>
  switch (prefix_decompose(buf1)) {
  | Ok(buf1) =>
    let Yellowish(buf2) = buf2;
    (buf1, Any(buf2));
  | Underflow(opt) =>
    let (ab, buf2) = yellow_uncons(buf2);
    (prefix23(opt, ab), buf2);
  |  Overflow(buf1, ab) => (
      buf1,
      yellow_prefix_cons(ab, buf2),
    )
  };

let suffix_concat = (buf1, buf2) =>
  switch (suffix_decompose(buf2)) {
  | Ok(buf2) =>
    let Yellowish(buf1) = buf1;
    (Any(buf1), buf2);
  | Underflow(opt) =>
    let (buf1, ab) = yellow_unsnoc(buf1);
    (buf1, suffix23(ab, opt));
  |  Overflow(buf2, ab) => (
      yellow_suffix_snoc(buf1, ab),
      buf2,
    )
  };

type sandwich('a) =
  | Alone(option('a)): sandwich('a)
  | Sandwich('a, buffer('a, _), 'a): sandwich('a);

let buffer_unsandwich: type a c. buffer(a, c) => sandwich(a) =
  fun
  | B0 => Alone(None)
  | B1(a) => Alone(Some(a))
  |  B2(a, b) =>  Sandwich(a, B0, b)
  |  B3(a, b, c) =>  Sandwich(a, B1(b), c)
  |  B4(a, b, c, d) =>
     Sandwich(a,  B2(b, c), d)
  |  B5(a, b, c, d, e) =>
     Sandwich(a,  B3(b, c, d), e);

let buffer_halve:
  type a c. buffer(a, c) => (option(a), any_buffer((a, a))) =
  fun
  | B0 => (None, Any(B0))
  | B1(a) => (Some(a), Any(B0))
  |  B2(a, b) => (None, Any( B1((a, b))))
  |  B3(a, b, c) => (
      Some(a),
      Any( B1((b, c))),
    )
  |  B4(a, b, c, d) => (
      None,
      Any( B2((a, b), (c, d))),
    )
  |  B5(a, b, c, d, e) => (
      Some(a),
      Any( B2((b, c), (d, e))),
    );

let make_small = (prefix1, buf, suffix1) =>
  switch (prefix_decompose(prefix1), suffix_decompose(suffix1)) {
  | (Ok(p1), Ok(s1)) =>
     G( Green(p1, HOLE, s1), Small(buf))

  | (Ok(p1), Underflow(opt)) =>
    switch (buffer_unsnoc(buf), opt) {
    | (None, None) => Small(p1)
    | (None, Some(x)) => buffer_snoc(p1, x)
    | (Some((Any(rest), cd)), _) =>

      G( Green(p1, HOLE, suffix23(cd, opt)), Small(rest))
    }

  | (Underflow(opt), Ok(s1)) =>
    switch (buffer_uncons(buf), opt) {
    | (None, None) => Small(s1)
    | (None, Some(x)) => buffer_cons(x, s1)
    | (Some((cd, Any(rest))), _) =>

      G( Green(prefix23(opt, cd), HOLE, s1), Small(rest))
    }

  | (Underflow(p1), Underflow(s1)) =>
    switch (buffer_unsandwich(buf)) {
    |  Sandwich(ab, rest, cd) =>

      G(
         Green(prefix23(p1, ab), HOLE, suffix23(cd, s1)),
        Small(rest),
      )
    | Alone(opt) =>
      switch (p1, opt, s1) {
      | (None, None, None) => Small(B0)
      | (Some(a), None, None)
      | (None, None, Some(a)) => Small(B1(a))
      | (Some(a), None, Some(b))
      | (None, Some((a, b)), None) => Small( B2(a, b))
      | (Some(a), Some((b, c)), None)
      | (None, Some((a, b)), Some(c)) =>
        Small( B3(a, b, c))
      | (Some(a), Some((b, c)), Some(d)) =>
        Small( B4(a, b, c, d))
      }
    }

  | ( Overflow(p1, ab), Ok(s1)) =>
    let buf = buffer_cons(ab, buf);
     G( Green(p1, HOLE, s1), buf);

  | (Ok(p1),  Overflow(s1, ab)) =>
    let buf = buffer_snoc(buf, ab);
     G( Green(p1, HOLE, s1), buf);

  | (Underflow(opt),  Overflow(s1, ab)) =>
    let (cd, center) = suffix_rot(buf, ab);

    G( Green(prefix23(opt, cd), HOLE, s1), Small(center));

  | ( Overflow(p1, ab), Underflow(opt)) =>
    let (center, cd) = prefix_rot(ab, buf);

    G( Green(p1, HOLE, suffix23(cd, opt)), Small(center));

  | ( Overflow(p1, ab),  Overflow(s1, cd)) =>
    let (x, Any(rest)) = buffer_halve(buf);

    G(

      Green(
        p1,
         Yellow(prefix12(ab, x), HOLE, B1(cd)),
        s1,
      ),
      Small(rest),
    );
  };

let green_of_red: type a. kont(a, [ | `red]) => kont(a, [ | `green]) =
  fun
  |  R( Red(p1, HOLE, s1), Small(buf)) =>
    make_small(p1, buf, s1)
  |
    R(
       Red(p1,  Yellow(p2, child, s2), s1),
      kont,
    ) => {
      let (p1, Any(p2)) = prefix_concat(p1, Yellowish(p2));
      let (Any(s2), s1) = suffix_concat(Yellowish(s2), s1);

      G(
         Green(p1, HOLE, s1),
         R( Red(p2, child, s2), kont),
      );
    }
  |
    R(
       Red(p1, HOLE, s1),
       G( Green(p2, child, s2), kont),
    ) => {
      let (p1, Yellowish(p2)) = green_prefix_concat(p1, p2);
      let (Yellowish(s2), s1) = green_suffix_concat(s2, s1);

      G(

        Green(p1,  Yellow(p2, child, s2), s1),
        kont,
      );
    };

type not_yellow(_) =
  | Not_yellow: not_yellow([< | `green | `red]);

let ensure_green:
  type a c. (not_yellow(c), kont(a, c)) => kont(a, [ | `green]) =
  (Not_yellow, t) =>
    switch (t) {
    | Small(buf) => Small(buf)
    |  G(x, k) =>  G(x, k)
    |  R(x, k) => green_of_red( R(x, k))
    };

let yellow = (p1, child, s1, kont) =>
  T(

    Y(
       Yellow(p1, child, s1),
      ensure_green(Not_yellow, kont),
    ),
  );

let red = (p1, child, s1, kont) =>
  T(
    green_of_red(
       R( Red(p1, child, s1), kont),
    ),
  );

let cons = (x, T(t)) =>
  switch (t) {
  | Small(buf) => T(buffer_cons(x, buf))
  |  G( Green(p1, child, s1), kont) =>
    let p1 = green_prefix_cons(x, p1);
    yellow(p1, child, s1, kont);
  |  Y( Yellow(p1, child, s1), kont) =>
    let Any(p1) = yellow_prefix_cons(x, Yellowish(p1));
    red(p1, child, s1, kont);
  };

let snoc = (T(t), x) =>
  switch (t) {
  | Small(buf) => T(buffer_snoc(buf, x))
  |  G( Green(p1, child, s1), kont) =>
    let s1 = green_suffix_snoc(s1, x);
    yellow(p1, child, s1, kont);
  |  Y( Yellow(p1, child, s1), kont) =>
    let Any(s1) = yellow_suffix_snoc(Yellowish(s1), x);
    red(p1, child, s1, kont);
  };

let uncons_unsafe = (T(t)) =>
  switch (t) {
  | Small(buf) =>
    let (x, Any(buf)) = buffer_uncons_unsafe(buf);
    (x, T(Small(buf)));
  |  G( Green(p1, child, s1), kont) =>
    let (x, Yellowish(p1)) = green_uncons(p1);
    (x, yellow(p1, child, s1, kont));
  |  Y( Yellow(p1, child, s1), kont) =>
    let (x, Any(p1)) = yellow_uncons(Yellowish(p1));
    (x, red(p1, child, s1, kont));
  };

let unsnoc_unsafe = (T(t)) =>
  switch (t) {
  | Small(buf) =>
    let (Any(buf), x) = buffer_unsnoc_unsafe(buf);
    (T(Small(buf)), x);
  |  G( Green(p1, child, s1), kont) =>
    let (Yellowish(s1), x) = green_unsnoc(s1);
    (yellow(p1, child, s1, kont), x);
  |  Y( Yellow(p1, child, s1), kont) =>
    let (Any(s1), x) = yellow_unsnoc(Yellowish(s1));
    (red(p1, child, s1, kont), x);
  };

type t('a) = {
  length: int,
  s: s('a),
};

let empty = {length: 0, s: T(Small(B0))};

let is_empty = t => t.length == 0;

let length = t => abs(t.length);

let rev = t => {...t, length: - t.length};

let cons = (x, {length: n, s}) =>
  if (n >= 0) {
    {length: n + 1, s: cons(x, s)};
  } else {
    {length: n - 1, s: snoc(s, x)};
  }

and snoc = ({length: n, s}, x) =>
  if (n >= 0) {
    {length: n + 1, s: snoc(s, x)};
  } else {
    {length: n - 1, s: cons(x, s)};
  };

let uncons = ({length: n, s}) =>
  switch (n) {
  | 0 => None
  | _ when n >= 0 =>
    let (x, s) = uncons_unsafe(s);
    Some((x, {length: n - 1, s}));
  | _ =>
    let (s, x) = unsnoc_unsafe(s);
    Some((x, {length: n + 1, s}));
  };

let unsnoc = ({length: n, s}) =>
  switch (n) {
  | 0 => None
  | _ when n >= 0 =>
    let (s, x) = unsnoc_unsafe(s);
    Some(({length: n - 1, s}, x));
  | _ =>
    let (x, s) = uncons_unsafe(s);
    Some(({length: n + 1, s}, x));
  };

let is_rev = t => t.length < 0;

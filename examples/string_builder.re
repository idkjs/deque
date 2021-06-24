module type S = {
  type t;
  let empty: t;
  let (@): (t, t) => t;
  let of_string: string => t;
  let to_string: t => string;
};

module Build_string = {
  type t = string;
  let empty = "";
  let (@) = (a, b) => a ++ b;
  let of_string = s => s;
  let to_string = s => s;
};

module type FOLD = {
  type t('a);
  let fold_left: (('a, 'b) => 'a, 'a, t('b)) => 'a;
};

module Fold_to_string = (F: FOLD) => {
  let to_string = ss => {
    let n = F.fold_left((acc, s) => acc + String.length(s), 0, ss);
    let buf = Bytes.create(n);
    let _ =
      F.fold_left(
        (i, s) => {
          let n = String.length(s);
          Bytes.blit_string(s, 0, buf, i, n);
          i + n;
        },
        0,
        ss,
      );

    Bytes.to_string(buf);
  };
};

module Build_list = {
  type t = list(string);
  let empty = [];
  let (@) = (a, b) => List.append(a, b);
  let of_string = s => [s];
  include Fold_to_string(List);
};

module Build_list_safe = {
  include Build_list;
  let (@) = (a, b) => List.rev_append(List.rev(a), b);
};

module Build_dlist = {
  type t = list(string) => list(string);
  let empty = xs => xs;
  let (@) = (a, b, xs) => a(b(xs));
  let of_string = (s, xs) => [s, ...xs];
  let to_string = ss => Build_list.to_string(ss([]));
};

module Build_tree = {
  module F = {
    type t('a) =
      | Empty
      | Single('a)
      | Concat(t('a), t('a));
    let rec fold_left = (f, z) =>
      fun
      | Empty => z
      | Single(s) => f(z, s)
      |  Concat(a, b) =>
        fold_left(f, fold_left(f, z, a), b);
  };
  type t = F.t(string);
  let empty = F.Empty;
  let (@) = (a, b) =>
    switch (a, b) {
    | (F.Empty, t)
    | (t, F.Empty) => t
    | _ =>  F.Concat(a, b)
    };
  let of_string = s => F.Single(s);
  include Fold_to_string(F);
};

module Build_deque = {
  type t = Deque.t(string);
  let empty = Deque.empty;
  let (@) = (a, b) => Deque.append(a, b);
  let of_string = s => Deque.cons(s, Deque.empty);
  include Fold_to_string(Deque);
};

module Build_steque = {
  module Deque = Deque.Steque;
  type t = Deque.t(string);
  let empty = Deque.empty;
  let (@) = (a, b) => Deque.append(a, b);
  let of_string = s => Deque.cons(s, Deque.empty);
  include Fold_to_string(Deque);
};

let bench = (name, f) => {
  let t0 = Unix.gettimeofday();
  let (ok, str) =
    try (true, f()) {
    | e => (false, Printexc.to_string(e))
    };
  let t1 = Unix.gettimeofday();
  Printf.printf(
    "%16s: %.3f s -- %s\n%!",
    name,
    t1 -. t0,
    if (ok) {
      "length " ++ string_of_int(String.length(str));
    } else {
      str;
    },
  );
  if (ok) {
    Some(str);
  } else {
    None;
  };
};

module Test = (Builder: S) => {
  open Builder;

  let popen = of_string(" begin ");
  let pclose = of_string(" end ");
  let parens = x => popen @ x @ pclose;

  let parens_in =
    bench("parens") @@
    (
      () => {
        let rec go = (acc, n) =>
          if (n == 0) {
            acc;
          } else {
            go(parens(acc), n - 1);
          };

        to_string(go(empty, 20000));
      }
    );

  let () = ();

  let fibonacci =
    bench("fibonacci") @@
    (
      () => {
        let rec go = (a, b, n) =>
          if (n == 0) {
            a;
          } else {
            go(parens(a @ b), parens(a), n - 1);
          };

        to_string(go(of_string("1"), of_string("0"), 32));
      }
    );

  let () = ();
};

let header = name =>
  Printf.printf(
    "-- %s %s\n%!",
    name,
    String.make(70 - String.length(name) - 4, '-'),
  );

let () = header("String");
module Test_string = Test(Build_string);
let () = Printf.printf("\n%!");

module Test_check = (Builder: S) => {
  module T = Test(Builder);
  let () = {
    assert(T.parens_in == Test_string.parens_in);
    assert(T.fibonacci == None || T.fibonacci == Test_string.fibonacci);
  };
};

let () = header("List");
module Test_list = Test_check(Build_list);
let () = Printf.printf("\n%!");

let () = header("List_safe (no Stack_overflow on append)");
module Test_list_safe = Test_check(Build_list_safe);
let () = Printf.printf("\n%!");

let () = header("Diff list");
module Test_dlist = Test_check(Build_dlist);
let () = Printf.printf("\n%!");

let () = header("Custom tree");
module Test_tree = Test_check(Build_tree);
let () = Printf.printf("\n%!");

let () = header("Deque");
module Test_deque = Test_check(Build_deque);
let () = Printf.printf("\n%!");

let () = header("Steque");
module Test_steque = Test_check(Build_steque);
let () = Printf.printf("\n%!");

let () = header("Buffer");
module Test_buffer = {
  let popen = " begin ";
  let pclose = " end ";

  let parens_in =
    bench("parens") @@
    (
      () => {
        let buf = Buffer.create(0);
        let rec go = n =>
          if (n == 0) {
            ();
          } else {
            Buffer.add_string(buf, popen);
            go(n - 1);
            Buffer.add_string(buf, pclose);
          };

        go(20000);
        Buffer.contents(buf);
      }
    );

  let () = ();

  let fibonacci =
    bench("fibonacci") @@
    (
      () => {
        let buf = Buffer.create(0);
        let rec go =
          fun
          | 0 => Buffer.add_string(buf, "0")
          | 1 => Buffer.add_string(buf, "1")
          | n => {
              Buffer.add_string(buf, popen);
              go(n - 1);
              if (n > 2) {
                Buffer.add_string(buf, popen);
              };
              go(n - 2);
              if (n > 2) {
                Buffer.add_string(buf, pclose);
              };
              Buffer.add_string(buf, pclose);
            };

        go(32 + 1);
        Buffer.contents(buf);
      }
    );

  let () = ();

  let () = {
    assert(parens_in == Test_string.parens_in);
    assert(fibonacci == Test_string.fibonacci);
  };
};

/* $ dune exec examples/string_builder.exe

   -- String ------------------------------------------------------------
             parens: 1.478 s -- length 240000
          fibonacci: 0.430 s -- length 116432443

   -- List --------------------------------------------------------------
             parens: 7.131 s -- length 240000
          fibonacci: 0.232 s -- Stack overflow

   -- List_safe (no Stack_overflow on append) ---------------------------
             parens: 7.856 s -- length 240000
          fibonacci: 16.243 s -- length 116432443

   -- Diff list ---------------------------------------------------------
             parens: 0.003 s -- length 240000
          fibonacci: 1.963 s -- length 116432443

   -- Custom tree -------------------------------------------------------
             parens: 0.001 s -- length 240000
          fibonacci: 0.448 s -- length 116432443

   -- Deque -------------------------------------------------------------
             parens: 0.013 s -- length 240000
          fibonacci: 0.581 s -- length 116432443

   -- Steque ------------------------------------------------------------
             parens: 0.006 s -- length 240000
          fibonacci: 0.559 s -- length 116432443

   -- Buffer ------------------------------------------------------------
             parens: 0.001 s -- length 240000
          fibonacci: 0.284 s -- length 116432443

   */

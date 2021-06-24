module type S = {
  /**/**/;
  type t('a);
  /**/**/;

  /** {1 List} */;

  /** Since a double-ended queue is most similar to a list, the following
      functions are provided to match the signature and behaviour of the
      standard {! List} module.
  */;

  /** [hd xs] returns the left-most element of [xs].
      @raise Failure if the deque is empty.
  */

  let hd: t('a) => 'a;

  /** [tl xs] removes the left-most element of [xs].
      @raise Failure if the deque is empty.
  */

  let tl: t('a) => t('a);

  /** [nth xs n] returns the [n]-th element of the deque [xs]. The left-most
      element is at position 0.
      @raise Failure if the deque is too short.
      @raise Invalid_argument if [n] is negative.
  */

  let nth: (t('a), int) => 'a;

  /** [nth xs n] returns the [n]-th element of the deque [xs]. The left-most
      element is at position 0.
      @return None if the deque is too short.
      @raise Invalid_argument if [n] is negative.
  */

  let nth_opt: (t('a), int) => option('a);

  /** [make len x] replicates the value [x] in a new deque of size [len].
      @raise Invalid_argument if [len] is negative.
  */

  let make: (int, 'a) => t('a);

  /** [init len f] creates a new deque of size [len] such that its [i]th
      element is [f i], evaluated left to right.
      @raise Invalid_argument if [len] is negative.
  */

  let init: (int, int => 'a) => t('a);

  /** {1 Comparisons} */;

  /** [xs = ys] is satisfied when the elements of [xs] are in the same order,
      and are structurally equal to the elements of [ys]. */

  let (==): (t('a), t('a)) => bool;

  /** [equal eq xs ys] is [true] if the two deques have the same length
      and satisfy [eq x_i y_i] for each pair of elements of [combine xs ys]. */

  let equal: (('a, 'a) => bool, t('a), t('a)) => bool;

  /** [compare cmp xs ys] compares the deque [xs] in lexical order with [ys]
      according to the comparison function [cmp]. */

  let compare: (('a, 'a) => int, t('a), t('a)) => int;

  /** {1 Catenation} */;

  /** An alias for [append]: [xs @ ys] concatenates the two deques together. */

  let (@): (t('a), t('a)) => t('a);

  /** [rev_append xs ys] computes [append (rev xs) ys]. */

  let rev_append: (t('a), t('a)) => t('a);

  /** Concatenate a deque of deques. The elements of the argument are all
      appended together, in the same order, to give the result.
  */

  let concat: t(t('a)) => t('a);

  /** [flatten] is an alias for [concat]. */

  let flatten: t(t('a)) => t('a);

  /** {1 Iterators} */;

  /** [iter f xs] applies the function [f] in turn to each element of [xs] from
      left to right. */

  let iter: ('a => unit, t('a)) => unit;

  /** Same as [iter], but the function [f] also receives the index of each
      element as first argument (counting from 0). */

  let iteri: ((int, 'a) => unit, t('a)) => unit;

  /** [map f xs] creates a new deque where each element [x] of [xs] has been
      replaced by [f x]. */

  let map: ('a => 'b, t('a)) => t('b);

  /** Same as [map], but the function [f] also receives the index of each
      element as its first argument (counting from 0). */

  let mapi: ((int, 'a) => 'b, t('a)) => t('b);

  /** [rev_map f xs] gives the same result as [rev (map f xs)]. */

  let rev_map: ('a => 'b, t('a)) => t('b);

  /** [filter_map f xs] applies [f] to each element of [xs], filtering out the
      [None] results and returns a deque of all the [Some] values. */

  let filter_map: ('a => option('b), t('a)) => t('b);

  /** [concat_map f xs] gives the same result as [concat (map f xs)]. */

  let concat_map: ('a => t('b), t('a)) => t('b);

  /** [fold_left_map f z xs] is a combination of [fold_left] and [map] that
      threads an accumulator [z] through calls to [f].
  */

  let fold_left_map: (('a, 'b) => ('a, 'c), 'a, t('b)) => ('a, t('c));

  /** [fold_left f z xs] computes [f (... (f (f z x_0) x_1) ...) x_n] where
      [x_0...x_n] are the elements of the deque [xs] in left to right order.
  */

  let fold_left: (('a, 'b) => 'a, 'a, t('b)) => 'a;

  /** [fold_right f xs z] computes [f x_0 (f x_1 (... (f x_n z)))] where
      [x_0...x_n] are the elements of the deque [xs] in left to right order.
  */

  let fold_right: (('a, 'b) => 'b, t('a), 'b) => 'b;

  /** {1 Iterators on two deques} */;

  /** [iter2 f xs ys] calls [f x_i y_i] for each element of [xs] and [ys] at
      the same index [i], in left to right order.
      @raise Invalid_argument if the two deques have different lengths.
  */

  let iter2: (('a, 'b) => unit, t('a), t('b)) => unit;

  /** [map2 f xs ys] zips the two deques.
      @raise Invalid_argument if they have different lengths.
  */

  let map2: (('a, 'b) => 'c, t('a), t('b)) => t('c);

  /** Same as [rev (map2 f xs ys)]. */

  let rev_map2: (('a, 'b) => 'c, t('a), t('b)) => t('c);

  /** [fold_left2 f z xs ys] computes
      [f (... (f (f z x_0 y_0) x_1 y_1) ...) x_n y_n)].
  */

  let fold_left2: (('a, 'b, 'c) => 'a, 'a, t('b), t('c)) => 'a;

  /** {1 Scanning} */;

  /** [for_all f xs] checks if all elements of the deque [xs] satisfy the
      predicate [f]. It computes the conjunction [f x_0 && ... && f x_n], or
      returns [true] if the deque was empty.
  */

  let for_all: ('a => bool, t('a)) => bool;

  /** [exists f xs] checks if at least one element of the deque [xs]
      satisfies the predicate [f]. It computes the disjunction [f x_0 || ... ||
      f x_n], or returns [false] if the deque was empty.
  */

  let exists: ('a => bool, t('a)) => bool;

  /** Same as [for_all], but for a two-arguments predicate.
      @raise Invalid_argument if the two deques have different lenths.
  */

  let for_all2: (('a, 'b) => bool, t('a), t('b)) => bool;

  /** Same as [exists], but for a two-arguments predicate.
      @raise Invalid_argument if the two deques have different lenths.
  */

  let exists2: (('a, 'b) => bool, t('a), t('b)) => bool;

  /** [mem x xs] is true if and only if [x] is structurally equal [(=)] to an
      element of [xs]. */

  let mem: ('a, t('a)) => bool;

  /** [memq x xs] is true if and only if [x] is physically equal [(==)] to an
      element of [xs]. */

  let memq: ('a, t('a)) => bool;

  /** {1 Searching} */;

  /** [find f xs] returns the left-most element of [xs] that satisfies the
      predicate [f].
      @raise Not_found otherwise.
  */

  let find: ('a => bool, t('a)) => 'a;

  /** [find f xs] returns the left-most element of [xs] that satisfies the
      predicate [f].
      @return None otherwise.
  */

  let find_opt: ('a => bool, t('a)) => option('a);

  /** [find_map f xs] applies [f] to the elements of [xs] from left to right,
      and returns the first result of the form [Some v], or [None] if none
      exists.
  */

  let find_map: ('a => option('b), t('a)) => option('b);

  /** [filter f xs] returns all the elements of the deque [xs] that satisfy the
      predicate [f]. The order of the elements in the deque is preserved. */

  let filter: ('a => bool, t('a)) => t('a);

  /** Same as [filter]. */

  let find_all: ('a => bool, t('a)) => t('a);

  /** Same as [filter], but the predicate [f] also receives the index of each
      element as its first argument. */

  let filteri: ((int, 'a) => bool, t('a)) => t('a);

  /** [partition f xs] returns a pair of deques [(trues, falses)], such that
      [trues] contains all the elements of [xs] that satisfy [f] and [falses]
      the rest. The order of elements is preserved.
  */

  let partition: ('a => bool, t('a)) => (t('a), t('a));

  /** {1 Association} */;

  /** [assoc key xs] returns the left-most value associated with [key] in the
      deque of key-value pairs [xs].
      @raise Not_found if there is no value with such a [key].
  */

  let assoc: ('a, t(('a, 'b))) => 'b;

  /** [assoc key xs] returns the left-most value associated with [key] in the
      deque of key-value pairs [xs].
      @return None if there is no value with such a [key].
  */

  let assoc_opt: ('a, t(('a, 'b))) => option('b);

  /** Same as [assoc], but uses physical equality rather than structural
      equality for key comparison.
      @raise Not_found if there is no value associated with [key].
  */

  let assq: ('a, t(('a, 'b))) => 'b;

  /** Same as [assoc_opt], but uses physical equality rather than structural
      equality for key comparison.
      @return None if there is no value associated with [key].
  */

  let assq_opt: ('a, t(('a, 'b))) => option('b);

  /** [mem_assoc key xs] returns [true] when [xs] contains a pair with [key],
      and [false] otherwise. */

  let mem_assoc: ('a, t(('a, 'b))) => bool;

  /** Same as [mem_assoc], but uses physical equality rather than structural
      equality. */

  let mem_assq: ('a, t(('a, 'b))) => bool;

  /** {1 Pairs} */;

  /** [split xys] returns two deques [xs, ys] such that [xs] contains all the
      [fst] values of [xys], and [ys] all the [snd]. The order in each deque is
      preserved from the input. */

  let split: t(('a, 'b)) => (t('a), t('b));

  /** [combine xs ys] returns a single deque [xys] formed by the aligned pairs
      [(x_i, y_i)] of [xs] and [ys]. The order is preserved.
      @raise Invalid_argument if the two deques have different lengths.
  */

  let combine: (t('a), t('b)) => t(('a, 'b));

  /** {1 Sorting} */;

  /** [sort cmp xs] sorts the deque [xs] in increasing order, according to the
      comparison function [cmp].  This comparison function [cmp] must return
      [0] when its argument are equal, a positive integer if the first is
      greater and a negative integer if the first is smaller.
      See {! Array.sort} for a complete specification.
  */

  let sort: (('a, 'a) => int, t('a)) => t('a);

  /** Same as [sort], but guarantees that elements that compare equal are kept
      in their original order. */

  let stable_sort: (('a, 'a) => int, t('a)) => t('a);

  /** Same as [sort]. */

  let fast_sort: (('a, 'a) => int, t('a)) => t('a);

  /** Same as [sort], but also removes duplicates. */

  let sort_uniq: (('a, 'a) => int, t('a)) => t('a);

  /** Assuming that [xs] and [ys] are two deques already sorted in increasing
    order by the comparison function [cmp], then [merge cmp xs ys] returns a
    sorted deque containing all the elements of [xs] and [ys].  */

  let merge: (('a, 'a) => int, t('a), t('a)) => t('a);

  /** {1 Conversions} */;

  /** All conversions between collections preserve the left to right ordering
      of elements: */;

  /** [to_array xs] is an array containing all the elements of the deque [xs].
  */

  let to_array: t('a) => array('a);

  /** [of_array arr] creates a deque from the elements of the array [arr]. */

  let of_array: array('a) => t('a);

  /** [to_list xs] returns a list of the elements of the deque [xs]. */

  let to_list: t('a) => list('a);

  /** [of_list lst] creates a deque from the elements of the list [lst]. */

  let of_list: list('a) => t('a);

  /** Iterate on a deque. */

  let to_seq: t('a) => Seq.t('a);

  /** Create a deque from a sequence. */

  let of_seq: Seq.t('a) => t('a);
};

/** A double-ended queue (abbreviated {e deque}) is an ordered collection for
    which elements can be added and removed from the front and the back of the
    list. This library provides a purely functional, fully persistent
    implementation, such that the main operations are all worst-case constant
    time.

    The following datastructures were invented by Kaplan and Tarjan and are
    described in their brilliant paper
    {{: http://www.cs.tau.ac.il/~haimk/papers/jacm-deq.ps }
    "Purely Functional, Real-Time Deques with Catenation"}.
*/;

/** A double-ended queue with {b O(1)} [cons]/[uncons], [snoc]/[unsnoc]
    and [rev]; missing a fast [append]. Most similar to a finger tree as [nth]
    is also {b O(log min(i, N - i))}.  */

module Dequeue: {
  /** The type of a deque containing elements of type ['a]. */

  type t('a);

  /** The [empty] deque. */

  let empty: t('a);

  /** [is_empty xs] returns [true] when the deque [xs] contains no elements, [false] if at least one. */

  let is_empty: t('a) => bool;

  /** [cons x xs] adds an element [x] to the front of the deque [xs]. {b O(1)} */

  let cons: ('a, t('a)) => t('a);

  /** [singleton x] returns a deque containing only a single element [x]. */

  let singleton: 'a => t('a);

  /** [uncons xs] pops the left-most element of the deque [xs]. {b O(1)}
      @return [None] if the deque is empty.
  */

  let uncons: t('a) => option(('a, t('a)));

  /** [snoc xs x] adds an element [x] to the back of the deque [xs]. {b O(1)} */

  let snoc: (t('a), 'a) => t('a);

  /** [unsnoc xs] pops the right-most element of the deque [xs]. {b O(1)}
      @return [None] if the deque is empty.
  */

  let unsnoc: t('a) => option((t('a), 'a));

  /** [append xs ys] concatenates the two deques [xs] and [ys]. {b O(N)} */

  let append: (t('a), t('a)) => t('a);

  /** [rev xs] reverses the order of the elements of [xs]. {b O(1)} */

  let rev: t('a) => t('a);

  /** [length xs] returns the number of elements contained in [xs]. {b O(1)} */

  let length: t('a) => int;

  /** @inline */

  include Deque_sig.S with type t('a) := t('a);
};

/** A stack-ended queue with {b O(1)} [cons]/[uncons], [snoc] and [append]; missing [unsnoc] and [rev]. */

module Steque: {
  /** @inline */

  include (module type of Steque);

  /** @inline */

  include Deque_sig.S with type t('a) := t('a);
};

/** A double-ended queue with all operations in {b O(1)}: [cons]/[uncons], [snoc]/[unsnoc] and [append]. */

module Deck: {
  /** @inline */

  include (module type of Deck);

  /** @inline */

  include Deque_sig.S with type t('a) := t('a);
};

/** Same as {! Deck}, but [rev] is also {b O(1)}. */

module Deckrev: {
  /** @inline */

  include (module type of Deckrev);

  /** @inline */

  include Deque_sig.S with type t('a) := t('a);
};

/** For convenience, the module {! Deck} is also included here as the recommended implementation: */;

/** @inline */

include (module type of Deck);

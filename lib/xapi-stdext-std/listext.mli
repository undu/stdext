(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
module List : sig
  val setify : 'a list -> 'a list

  val subset : 'a list -> 'a list -> bool

  val set_equiv : 'a list -> 'a list -> bool

  val length : 'a list -> int [@@deprecated "Use Stdlib.List instead"]

  val hd : 'a list -> 'a [@@deprecated "Use Stdlib.List instead"]

  val tl : 'a list -> 'a list [@@deprecated "Use Stdlib.List instead"]

  val nth : 'a list -> int -> 'a [@@deprecated "Use Stdlib.List instead"]

  val rev : 'a list -> 'a list [@@deprecated "Use Stdlib.List instead"]

  val append : 'a list -> 'a list -> 'a list
    [@@deprecated "Use Stdlib.List instead"]

  val rev_append : 'a list -> 'a list -> 'a list
    [@@deprecated "Use Stdlib.List instead"]

  val concat : 'a list list -> 'a list [@@deprecated "Use Stdlib.List instead"]

  val flatten : 'a list list -> 'a list [@@deprecated "Use Stdlib.List instead"]

  val iter : ('a -> unit) -> 'a list -> unit
    [@@deprecated "Use Stdlib.List instead"]

  val map : ('a -> 'b) -> 'a list -> 'b list
    [@@deprecated "Use Stdlib.List instead"]

  val rev_map : ('a -> 'b) -> 'a list -> 'b list
    [@@deprecated "Use Stdlib.List instead"]

  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
    [@@deprecated "Use Stdlib.List instead"]

  val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
    [@@deprecated "Use Stdlib.List instead"]

  val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
    [@@deprecated "Use Stdlib.List instead"]

  val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
    [@@deprecated "Use Stdlib.List instead"]

  val rev_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
    [@@deprecated "Use Stdlib.List instead"]

  val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
    [@@deprecated "Use Stdlib.List instead"]

  val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
    [@@deprecated "Use Stdlib.List instead"]

  val for_all : ('a -> bool) -> 'a list -> bool
    [@@deprecated "Use Stdlib.List instead"]

  val exists : ('a -> bool) -> 'a list -> bool
    [@@deprecated "Use Stdlib.List instead"]

  val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
    [@@deprecated "Use Stdlib.List instead"]

  val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
    [@@deprecated "Use Stdlib.List instead"]

  val mem : 'a -> 'a list -> bool [@@deprecated "Use Stdlib.List instead"]

  val memq : 'a -> 'a list -> bool [@@deprecated "Use Stdlib.List instead"]

  val find : ('a -> bool) -> 'a list -> 'a
    [@@deprecated "Use Stdlib.List instead"]

  val filter : ('a -> bool) -> 'a list -> 'a list
    [@@deprecated "Use Stdlib.List instead"]

  val find_all : ('a -> bool) -> 'a list -> 'a list
    [@@deprecated "Use Stdlib.List instead"]

  val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
    [@@deprecated "Use Stdlib.List instead"]

  val assoc : 'a -> ('a * 'b) list -> 'b
    [@@deprecated "Use Stdlib.List instead"]

  val assq : 'a -> ('a * 'b) list -> 'b [@@deprecated "Use Stdlib.List instead"]

  val mem_assoc : 'a -> ('a * 'b) list -> bool
    [@@deprecated "Use Stdlib.List instead"]

  val mem_assq : 'a -> ('a * 'b) list -> bool
    [@@deprecated "Use Stdlib.List instead"]

  val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
    [@@deprecated "Use Stdlib.List instead"]

  val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list
    [@@deprecated "Use Stdlib.List instead"]

  val split : ('a * 'b) list -> 'a list * 'b list
    [@@deprecated "Use Stdlib.List instead"]

  val combine : 'a list -> 'b list -> ('a * 'b) list
    [@@deprecated "Use Stdlib.List instead"]

  val sort : ('a -> 'a -> int) -> 'a list -> 'a list
    [@@deprecated "Use Stdlib.List instead"]

  val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list
    [@@deprecated "Use Stdlib.List instead"]

  val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list
    [@@deprecated "Use Stdlib.List instead"]

  val merge : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
    [@@deprecated "Use Stdlib.List instead"]

  val inv_assoc : 'a -> ('b * 'a) list -> 'b
  (** Perform a lookup on an association list of (value, key) pairs. *)

  val map_tr : ('a -> 'b) -> 'a list -> 'b list
  (** A tail-recursive map. *)

  val count : ('a -> bool) -> 'a list -> int
  (** Count the number of list elements matching the given predicate. *)

  val position : ('a -> bool) -> 'a list -> int list
  (** Find the indices of all elements matching the given predicate. *)

  val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
    [@@deprecated "Use Stdlib.List instead"]
  (** Map the given function over a list, supplying the integer
      	    index as well as the element value. *)

  val iteri : (int -> 'a -> unit) -> 'a list -> unit
    [@@deprecated "Use Stdlib.List instead"]

  val iteri_right : (int -> 'a -> unit) -> 'a list -> unit

  val rev_mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
  (** Map the given function over a list in reverse order. *)

  val mapi_tr : (int -> 'a -> 'b) -> 'a list -> 'b list
  (** Tail-recursive [mapi]. *)

  val chop : int -> 'a list -> 'a list * 'a list
  (** [chop k l] splits [l] at index [k] to return a pair of lists. Raises
      invalid_arg when [i] is negative or greater than the length of [l]. *)

  val rev_chop : int -> 'a list -> 'a list * 'a list
  (** [rev_chop k l] splits [l] at index [k] to return a pair of lists, the
      first in reverse order. Raises invalid_arg when [i] is negative or
      greater than the length of [l]. *)

  val chop_tr : int -> 'a list -> 'a list * 'a list
  (** Tail-recursive [chop]. *)

  val dice : int -> 'a list -> 'a list list
  (** [dice k l] splits [l] into lists with [k] elements each. Raises
      invalid_arg if [List.length l] is not divisible by [k]. *)

  val sub : int -> int -> 'a list -> 'a list
  (** Extract the sub-list between the given indices. *)

  val remove : int -> 'a list -> 'a list
  (** Remove the element at the given index. *)

  val extract : int -> 'a list -> 'a * 'a list
  (** Extract the element at the given index, returning the element and the
      		list without that element. *)

  val insert : int -> 'a -> 'a list -> 'a list
  (** Insert the given element at the given index. *)

  val replace : int -> 'a -> 'a list -> 'a list
  (** Replace the element at the given index with the given value. *)

  val morph : int -> ('a -> 'a) -> 'a list -> 'a list
  (** Apply the given function to the element at the given index. *)

  val between : 'a -> 'a list -> 'a list
  (** Insert the element [e] between every pair of adjacent elements in the
      	    given list. *)

  val between_tr : 'a -> 'a list -> 'a list
  (** Tail-recursive [between]. *)

  val randomize : 'a list -> 'a list
  (** Generate a random permutation of the given list. *)

  val distribute : 'a -> 'a list -> 'a list list
  (** Distribute the given element over the given list, returning a list of
      	    lists with the new element in each position. *)

  val permute : 'a list -> 'a list list
  (** Generate all permutations of the given list. *)

  val rle_eq : ('a -> 'a -> bool) -> 'a list -> ('a * int) list
  (** Run-length encode the given list using the given equality function. *)

  val rle : 'a list -> ('a * int) list
  (** Run-length encode the given list using built-in equality. *)

  val unrle : (int * 'a) list -> 'a list
  (** Decode a run-length encoded list. *)

  val inner :
       (('a -> 'b -> 'c -> 'd) -> 'e -> 'f -> 'g -> 'h)
    -> 'e
    -> ('b -> 'c -> 'i)
    -> 'f
    -> 'g
    -> ('a -> 'i -> 'd)
    -> 'h
  (** Compute the inner product of two lists. *)

  val filter_map : ('a -> 'b option) -> 'a list -> 'b list
    [@@deprecated "Use Stdlib.List instead"]
  (** Applies a function f that generates optional values, to each
      	    of the items in a list A [a1; ...; am], generating a new list of
      	    non-optional values B [b1; ...; bn], with m >= n. For each value
      	    a in list A, list B contains a corresponding value b if and only
      	    if the application of (f a) results in Some b.  *)

  val is_sorted : ('a -> 'a -> int) -> 'a list -> bool
  (** Returns true if and only if the given list is in sorted order
      	    according to the given comparison function.  *)

  val intersect : 'a list -> 'a list -> 'a list
  (** Returns the intersection of two lists. *)

  val set_difference : 'a list -> 'a list -> 'a list
  (** Returns the set difference of two lists *)

  val assoc_default : 'a -> ('a * 'b) list -> 'b -> 'b
  (** Act as List.assoc, but return the given default value if the
      	    key is not in the list. *)

  val map_assoc_with_key :
    ('k -> 'v1 -> 'v2) -> ('k * 'v1) list -> ('k * 'v2) list
  (** [map_assoc_with_key op al] transforms every value in [al] based on the
      	    key and the value using [op]. *)

  (* Like Lisp cons*)
  val cons : 'a -> 'a list -> 'a list [@@deprecated "Use Stdlib.List instead"]

  val take : int -> 'a list -> 'a list
  (** [take n list] returns the first [n] elements of [list] (or less if list
      	    is shorter).*)

  val drop : int -> 'a list -> 'a list
  (** [drop n list] returns the list without the first [n] elements of [list]
      (or [] if list is shorter). *)

  val tails : 'a list -> 'a list list

  val safe_hd : 'a list -> 'a option
    [@@deprecated "Use List.nth_opt list 0 instead"]

  val replace_assoc : 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list
  (** Replace the value belonging to a key in an association list. Adds the key/value pair
      	 *  if it does not yet exist in the list. If the same key occurs multiple time in the original
      	 *  list, all occurances are removed and replaced by a single new key/value pair.
      	 *  This function is useful is the assoc list is used as a lightweight map/hashtable/dictonary. *)

  val update_assoc : ('a * 'b) list -> ('a * 'b) list -> ('a * 'b) list
  (** Includes everything from [update] and all key/value pairs from [existing] for
      	 *  which the key does not exist in [update]. In other words, it is like [replace_assoc]
      	 *  but then given a whole assoc list of updates rather than a single key/value pair. *)

  val make_assoc : ('a -> 'b) -> 'a list -> ('a * 'b) list

  val unbox_list : 'a option list -> 'a list
  (** Unbox all values from the option list. *)

  val restrict_with_default : 'v -> 'k list -> ('k * 'v) list -> ('k * 'v) list
  (** [restrict_with_default default keys al] makes a new association map
      	    from [keys] to previous values for [keys] in [al]. If a key is not found
      	    in [al], the [default] is used. *)

  val range : int -> int -> int list
  (** range lower upper = [lower; lower + 1; ...; upper - 1]
      	    Returns the empty list if lower >= upper. *)
end

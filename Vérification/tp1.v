Require Import Lia.

Module INTEGERS.

(* Define the recursive function "pow x n", computing x to the power n *)
Fixpoint pow (x : nat) (n : nat) :=
  match n with
  | 0 => 1
  | S p => x * (pow x p)
  end.

(* We check the type and the result of the evaluation *)
Check pow.

Eval compute in (pow 2 3).

(* Prove that "pow 2 3 = 8" *)
Example pow_2_3 : pow 2 3 = 8.
Proof.
  unfold pow.
  simpl.
  reflexivity.
Qed.

(* Prove by induction on n the following property.
   We will use the "lia" tactic *)
Lemma pow_1 : forall n, pow 1 n = 1.
Proof.
  intros.
  induction n.
  -unfold pow.
  reflexivity.
  -simpl.
  lia.
Qed.


(* Prove by induction on n the following property.
   We will use the "lia" tactic *)

Lemma pow_add: forall x n m, pow x (n+m) = pow x n * pow x m.
Proof.
  intros.
  induction n; simpl; lia.
Qed.

(* Prove by induction on n the following property.
   We will use the "lia" tactic *)

Lemma mul_pow : forall x y n, pow (x*y) n = pow x n * pow y n.
Proof.
  intros.
  induction n; simpl; lia.
Qed.

(* Prove by induction on n the following property.
   We will use the tactic "lia" as well as pow_1, mul_pow and pow_add *)

Lemma pow_mul : forall x n m, pow x (n*m) = pow (pow x n) m.
Proof.
  intros.
  induction n; simpl.
  -rewrite pow_1.
  reflexivity.
  -rewrite mul_pow.
  rewrite pow_add.
  rewrite IHn. (* ou lia *)
  reflexivity.
Qed.

Require Import Arith.
Definition sq n := n * n.
Lemma arith : forall a b : nat, sq (a + b) = sq a + 2 * a * b + sq b.
Proof.
intros.
unfold sq.
Search ((_+_)*(_+_)).
lia.
Qed.


Lemma arithv2 : forall a b : nat, sq (a + b) = sq a + 2 * a * b + sq b.
Proof.
intros.
unfold sq.
Search _ mult "distr".
change 2 with (1+1).
rewrite !Nat.mul_add_distr_r, !Nat.mul_add_distr_l.
Search "1" "*".
rewrite Nat.mul_1_l.
rewrite !Nat.add_assoc.
rewrite (Nat.mul_comm a b) at 3.
reflexivity.
Qed.

End INTEGERS.


Require Import List.
Import ListNotations.

Module LISTS.

(* Define the function merge taking as argument two lists l1 and l2 of elements of any type T and returning the list obtained by taking alternately an element of l1 then an element of l2. If one of the lists is empty, we take the elements of the other list. *)
Fixpoint merge {T} (l1 l2: list T): list T := 
  match l1,l2 with 
  |[],[] => []
  |[],_ => l2
  |_,[] => l1
  |t1::q1,t2::q2 => t1::t2::merge q1 q2
  end.

Compute merge [1;2;3] [4;5].
(* Exemple *)
Example merge_example : merge [1;2;3] [4;5] = [1;4;2;5;3].
Proof.
  unfold merge.
  reflexivity.
Qed.


(* Prove by induction on l1 the following property.
   We will use "destruct l2" to examine the cases l2 empty or non-empty,
   and the tactic "lia" *)
Lemma merge_len : forall T (l1 l2: list T), length (merge l1 l2) = length l1 + length l2.

Proof.
  intros T l1.
  induction l1.
  - intros l2.
    destruct l2.
    +simpl.
    reflexivity.
    +simpl.
    reflexivity.
  - intros l2.
    destruct l2.
    + simpl.
      lia.
    + simpl.
      rewrite IHl1.
      lia.
Qed.

Lemma last_rev : forall (l : list nat) dflt,
    last (rev l) dflt = hd dflt l.
Proof.
intros.
induction l.
-simpl.
reflexivity.
-simpl.
Search last.
apply last_last.
Qed.

End LISTS.








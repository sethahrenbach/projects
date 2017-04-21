Require Export Bool.
Require Export List.
Require Export Arith.
Require Export Arith.EqNat.

(* Preliminaries, selectively snagged from Software Foundations (which is MI_D
   licensed. Annoyingly, the only reason we are doing this rather than just
   importing is that the |- notation used for typing judgments in SF blocks
   the |- notation in Ltac, which is kind of a Big Problem, so we tweak the
   |- notation a little bit. *)

(****
 **** Begin preliminaries
 ****)
(**** from SfLib.v ****)

Definition admit {T: Type} : T.  Admitted.

Require String. Open Scope string_scope.

Ltac move_to_top x :=
  match reverse goal with
  | H : _ |- _ => try move x after H
  end.

Tactic Notation "assert_eq" ident(x) constr(v) :=
  let H := fresh in
  assert (x = v) as H by reflexivity;
  clear H.

Tactic Notation "Case_aux" ident(x) constr(name) :=
  first [
    set (x := name); move_to_top x
  | assert_eq x name; move_to_top x
  | fail 1 "because we are working on a different case" ].

Tactic Notation "Case" constr(name) := Case_aux Case name.
Tactic Notation "SCase" constr(name) := Case_aux SCase name.
Tactic Notation "SSCase" constr(name) := Case_aux SSCase name.
Tactic Notation "SSSCase" constr(name) := Case_aux SSSCase name.
Tactic Notation "SSSSCase" constr(name) := Case_aux SSSSCase name.
Tactic Notation "SSSSSCase" constr(name) := Case_aux SSSSSCase name.
Tactic Notation "SSSSSSCase" constr(name) := Case_aux SSSSSSCase name.
Tactic Notation "SSSSSSSCase" constr(name) := Case_aux SSSSSSSCase name.

Theorem ex_falso_quodlibet : forall (P:Prop),
  False -> P.
Proof.
  intros P contra.
  inversion contra.  Qed.

Inductive id : Type := 
  Id : nat -> id.

Theorem eq_id_dec : forall id1 id2 : id, {id1 = id2} + {id1 <> id2}.
Proof.
   intros id1 id2.
   destruct id1 as [n1]. destruct id2 as [n2].
   destruct (eq_nat_dec n1 n2) as [Heq | Hneq].
   Case "n1 = n2".
     left. rewrite Heq. reflexivity.
   Case "n1 <> n2".
     right. intros contra. inversion contra. apply Hneq. apply H0.
Defined. 

Lemma eq_id : forall (T:Type) x (p q:T), 
              (if eq_id_dec x x then p else q) = p. 
Proof.
  intros. 
  destruct (eq_id_dec x x); try reflexivity. 
  apply ex_falso_quodlibet; auto.
Qed.

Lemma neq_id : forall (T:Type) x y (p q:T), x <> y -> 
               (if eq_id_dec x y then p else q) = q. 
Proof.
  (* FILL IN HERE *) Admitted.

Definition partial_map (A:Type) := id -> option A.

Definition empty {A:Type} : partial_map A := (fun _ => None). 

Notation "'\empty'" := empty.

Definition extend {A:Type} (Gamma : partial_map A) (x:id) (T : A) :=
  fun x' => if eq_id_dec x x' then Some T else Gamma x'.

Lemma extend_eq : forall A (ctxt: partial_map A) x T,
  (extend ctxt x T) x = Some T.
Proof.
  intros. unfold extend. rewrite eq_id; auto. 
Qed.

Lemma extend_neq : forall A (ctxt: partial_map A) x1 T x2,
  x2 <> x1 ->
  (extend ctxt x2 T) x1 = ctxt x1.
Proof.
  intros. unfold extend. rewrite neq_id; auto. 
Qed.

Lemma extend_shadow : forall A (ctxt: partial_map A) t1 t2 x1 x2,
  extend (extend ctxt x2 t1) x2 t2 x1 = extend ctxt x2 t2 x1.
Proof with auto.
  intros. unfold extend. destruct (eq_id_dec x2 x1)...
Qed.


Definition relation (X:Type) := X -> X -> Prop.

Definition partial__function {X: Type} (R: relation X) :=
  forall x y1 y2 : X, R x y1 -> R x y2 -> y1 = y2. 

Definition Reflexive {X: Type} (R: relation X) :=
  forall a : X, R a a.

Definition Irreflexive {X: Type} (R: relation X) :=
  forall a : X, ~ R a a.

Definition Transitive {X: Type} (R: relation X) :=
  forall a b c : X, (R a b) -> (R b c) -> (R a c).

Definition Antisymmetric {X: Type} (R: relation X) :=
  forall a b : X, (R a b) -> (R b a) -> a = b.

Definition Symmetric {X: Type} (R: relation X) :=
  forall a b : X, (R a b) -> (R b a).

Definition Equivalence {X:Type} (R: relation X) :=
  (Reflexive R) /\ (Symmetric R) /\ (Transitive R).

(** A relation is a _partial order_ when it's reflexive,
    _anti_-symmetric, and transitive.  In the Coq standard library
    it's called just "order" for short. *)

Definition Partialorder {X:Type} (R: relation X) :=
  (Reflexive R) /\ (Antisymmetric R) /\ (Transitive R).

(** A preorder is almost like a partial order, but doesn't have to be
    antisymmetric. *)

Definition Preorder {X:Type} (R: relation X) :=
  (Reflexive R) /\ (Transitive R).

(* Reflexive Transitive Closures: RT_closure and S(imple)RT_closure *)

Inductive RT_clo {X: Type} (R: relation X) : relation X :=
    | RT_step  : forall (x y: X), 
                 R x y -> 
                 RT_clo R x y
    | RT_refl  : forall (x : X),  
                 RT_clo R x x
    | RT_trans : forall (x y z : X),
                 RT_clo R x y ->
                 RT_clo R y z ->
                 RT_clo R x z.

(* This is easier to work with because of issues that arise from indeterminism in regards to the 
   cut step in applying RT_trans: 
            RT_clo (R a b) -> RT_clo (R b c) implies RT_clo (R a c)  
   can sometimes require creative selection of the witness (R b c). 
   Here's an alternative that seems easier to use:
 *)

Inductive reflexive_transitive_closure {X:Type} (R: relation X) : relation X :=
    | rtc_refl  : forall (x : X), 
                  reflexive_transitive_closure R x x
    | rtc_step : forall (x y z : X),
                  R x y ->  
                  reflexive_transitive_closure R y z ->
                  reflexive_transitive_closure R x z.


(* Given a relation R, R is in SRT_clo.  *)
Theorem R_in_RTC : forall (X:Type) (R:relation X) (x y : X), 
        R x y -> reflexive_transitive_closure R x y.
Proof.
  intros.
  apply rtc_step with y. 
  apply H. apply rtc_refl.    
Qed.

(* the reflexive_transitive_closure is _transitive_ *)
Theorem trans_RTC : forall (X:Type) (R: relation X) (x y z : X),
                    reflexive_transitive_closure R x y  ->
                    reflexive_transitive_closure R y z ->
                    reflexive_transitive_closure R x z.
Proof.
  intros.
  induction H. assumption. 
  apply rtc_step with y. assumption.
  apply IHreflexive_transitive_closure. assumption.
Qed.


Inductive list (X:Type) : Type :=
  | nil  : list X
  | cons : X -> list X -> list X.

Arguments nil {X}.
Arguments cons {X} _ _. 

Notation "[_]" := nil (at level 0) : type_scope.
Notation "l :: r" := (cons l r) (at level 60, right associativity).
Notation "[ x , .. , y ]" := (cons x .. (cons y [_]) ..) : list_scope.

Fixpoint append (X : Type) (fs fs' : list X) : (list X) :=
  match fs with
  | [_]      => fs'
  | cons h t => cons h (append X t fs')
  end.

Fixpoint rcons (X:Type) (fs :list X) (v:X) : (list X) :=
  match fs with
  | [_]      => (v :: [_])
  | t :: b   => (t :: (rcons X b v))
  end.

Fixpoint reverse (X:Type) (fs :list X) : (list X) :=
  match fs with
  | [_]      => [_] 
  | t :: b   => rcons X (reverse X b) t
  end.

Fixpoint sizeof (X:Type) (fs : list X) : nat :=
  match fs with
  | [_]      => 0
  | t :: b   => S (sizeof X b)
  end.

Arguments sizeof {X} fs.
Arguments append {X} fs fs'.
Arguments reverse {X} fs. 
Arguments rcons {X} fs v.



(****
 **** End preliminaries
 ****)
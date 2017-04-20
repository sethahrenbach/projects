Require Import Lib.


(** printing |-- %\ensuremath{\vdash}% *)
(** printing \in %\ensuremath{\in}% *)
(** printing Gamma %\ensuremath{\Gamma}% *)

(** * Syntax

      Here is the syntax for ReWire. *)

(** ** Monads and Types *)

(** Here is the syntax for layered state monad. *)
Inductive label : Type :=
  | LNil : label
  | LRead : label
  | LWrite : label
  | LReadWrite : label.

Inductive mo : Type :=
  | MIdentity : mo  (* zero-layer state monad *)
  | MStateT   : ty -> label -> mo -> mo

(** Here is the syntax for ``ordinary'' types. *)

with ty : Type :=
  | TArrow   : ty -> ty -> ty
  | TProd    : ty -> ty -> ty
  | TSum     : ty -> ty -> ty
  | TNil     : ty
  | TMonadic : mo -> ty -> ty.

(** Mutual induction schemes for types and monads will be useful. *)
Scheme ty_mo_ind := Induction for ty Sort Prop
  with mo_ty_ind := Induction for mo Sort Prop.

Combined Scheme ty_mo_mutind from ty_mo_ind, mo_ty_ind.

(** *** Relating monads by permissiveness *)

(** Partial order on monads. M1 less permissive than M2 means that M1 has an identical
    store shape but fewer or the same permissions. *)

Inductive monad_less_permissive : mo -> mo -> Prop :=
  | LP_StateT_00 : forall T M1 M2,
                     monad_less_permissive M1 M2 ->
                     monad_less_permissive (MStateT T LNil M1)
                                           (MStateT T LNil M2)
  | LP_StateT_0R : forall T M1 M2,
                     monad_less_permissive M1 M2 ->
                     monad_less_permissive (MStateT T LNil M1)
                                           (MStateT T LRead M2)
  | LP_StateT_0W : forall T M1 M2,
                     monad_less_permissive M1 M2 ->
                     monad_less_permissive (MStateT T LNil M1)
                                           (MStateT T LWrite M2)
  | LP_StateT_0RW : forall T M1 M2,
                     monad_less_permissive M1 M2 ->
                     monad_less_permissive (MStateT T LNil M1)
                                           (MStateT T LReadWrite M2)
  | LP_StateT_RR : forall T M1 M2,
                     monad_less_permissive M1 M2 ->
                     monad_less_permissive (MStateT T LRead M1)
                                           (MStateT T LRead M2)
  | LP_StateT_WW : forall T M1 M2,
                     monad_less_permissive M1 M2 ->
                     monad_less_permissive (MStateT T LWrite M1)
                                           (MStateT T LWrite M2)
  | LP_StateT_RRW : forall T M1 M2,
                     monad_less_permissive M1 M2 ->
                     monad_less_permissive (MStateT T LRead M1)
                                           (MStateT T LReadWrite M2)
  | LP_StateT_WRW : forall T M1 M2,
                     monad_less_permissive M1 M2 ->
                     monad_less_permissive (MStateT T LWrite M1)
                                           (MStateT T LReadWrite M2)
  | LP_StateT_RWRW : forall T M1 M2,
                     monad_less_permissive M1 M2 ->
                     monad_less_permissive (MStateT T LReadWrite M1)
                                           (MStateT T LReadWrite M2)
  | LP_Identity  : monad_less_permissive MIdentity MIdentity.

Hint Constructors monad_less_permissive.

Theorem less_permissive_refl : forall M, monad_less_permissive M M.
Proof.
  intros; induction M; [| destruct l]; auto.
Qed.

Theorem less_permissive_antisymm : forall M1 M2, monad_less_permissive M1 M2 ->
                                                 monad_less_permissive M2 M1 ->
                                                 M1 = M2.
Proof.
  intros M1 M2 Hw12; induction Hw12; intros Hw21; auto; inversion Hw21; subst; rewrite (IHHw12 H0); auto.
Qed.

Theorem less_permissive_trans: forall M1 M2 M3, monad_less_permissive M1 M2 ->
                                                monad_less_permissive M2 M3 ->
                                                monad_less_permissive M1 M3.
Proof.
  induction M1; intros.
    inversion H; subst. inversion H0; subst; auto.
    inversion H; subst; inversion H0; subst; constructor; eapply IHM1; eauto.
Qed.

(* Relation on monads. "M1 disjoint with M2" means that M1 and M2 have the
   same store shape and their permission sets are disjoint. *)
Inductive monad_disjoint : mo -> mo -> Prop :=
  | Disjoint_StateT_00 : forall T M1 M2,
                           monad_disjoint M1 M2 ->
                           monad_disjoint (MStateT T LNil M1)
                                          (MStateT T LNil M2)
  | Disjoint_StateT_0R : forall T M1 M2,
                           monad_disjoint M1 M2 ->
                           monad_disjoint (MStateT T LNil M1)
                                          (MStateT T LRead M2)
  | Disjoint_StateT_0W : forall T M1 M2,
                           monad_disjoint M1 M2 ->
                           monad_disjoint (MStateT T LNil M1)
                                          (MStateT T LWrite M2)
  | Disjoint_StateT_0RW : forall T M1 M2,
                           monad_disjoint M1 M2 ->
                           monad_disjoint (MStateT T LNil M1)
                                          (MStateT T LReadWrite M2)
  | Disjoint_StateT_R0 : forall T M1 M2,
                           monad_disjoint M1 M2 ->
                           monad_disjoint (MStateT T LRead M1)
                                          (MStateT T LNil M2)
  | Disjoint_StateT_W0 : forall T M1 M2,
                           monad_disjoint M1 M2 ->
                           monad_disjoint (MStateT T LWrite M1)
                                          (MStateT T LNil M2)
  | Disjoint_StateT_RW0 : forall T M1 M2,
                           monad_disjoint M1 M2 ->
                           monad_disjoint (MStateT T LReadWrite M1)
                                          (MStateT T LNil M2)
  | Disjoint_Identity  : monad_disjoint MIdentity MIdentity.

Theorem monad_disjoint_symm : forall M1 M2, monad_disjoint M1 M2 ->
                                            monad_disjoint M2 M1.
Proof.
  induction M1; intros ? ?H; inversion H; subst; auto; constructor; auto.
Qed.

(** ** Terms and Configurations *)

Inductive tm : Type :=
  (* Lambda calculus *)
  | tvar     : id -> tm
  | tapp     : tm -> tm -> tm
  | tabs     : id -> ty -> tm -> tm  (* ty is type of binder *)

  (* Data *)
  | tunit    : tm
  | tpair    : tm -> tm -> tm
  | tpi1     : tm -> tm
  | tpi2     : tm -> tm
  | tinl     : tm -> ty -> tm        (* ty is type on right of sum *)
  | tinr     : tm -> ty -> tm        (* ty is type on left of sum *)
  | tcase    : tm -> tm -> tm -> tm

  (* State monad operations *)
  | treturn  : tm -> mo -> tm
  | tbind    : tm -> tm -> tm
  | tlift    : tm -> ty -> tm        (* ty is type of state for new layer *)
  | televate : tm -> mo -> tm        (* mo is monad we are elevating to *)
  | tget     : ty -> mo -> tm        (* ty is type of state for new layer, mo is base monad *)
  | tput     : tm -> ty -> mo -> tm  (* ty is type of state for new layer, mo is base monad *)
  .


Tactic Notation "tm_cases" tactic(first) ident(c) :=
  first;
  [ Case_aux c "tvar" | Case_aux c "tapp" | Case_aux c "tabs" 
  | Case_aux c "tunit" | Case_aux c "tpair"
  | Case_aux c "tpi1" | Case_aux c "tpi2" 
  | Case_aux c "tinl" | Case_aux c "tinr"
  | Case_aux c "tcase" | Case_aux c "treturn" | Case_aux c "tbind" | Case_aux c "tlift" 
  | Case_aux c "televate" | Case_aux c "tget" | Case_aux c "tput" ].

Definition store := list tm.
Definition configuration := (tm*store)%type.

(** * Typing Judgments *)

(** ** For terms *)
Definition context := partial_map ty.

Reserved Notation "Gamma '|--' t '\in' T" (at level 40).

Inductive has_type : context -> tm -> ty -> Prop :=
  | T_Var  : forall Gamma x T,
               Gamma x = Some T -> Gamma |-- tvar x \in T
  | T_Abs  : forall Gamma x T_11 T_12 t_12,
               extend Gamma x T_11 |-- t_12 \in T_12 ->
                 Gamma |-- tabs x T_11 t_12 \in TArrow T_11 T_12
  | T_App  : forall T_11 T_12 Gamma t_1 t_2,
               Gamma |-- t_1 \in TArrow T_11 T_12 ->
               Gamma |-- t_2 \in T_11 ->
                 Gamma |-- tapp t_1 t_2 \in T_12

  | T_Unit : forall Gamma,
               Gamma |-- tunit \in TNil
  | T_Pair : forall Gamma T_1 T_2 t_1 t_2,
               Gamma |-- t_1 \in T_1 ->
               Gamma |-- t_2 \in T_2 ->
                 Gamma |-- tpair t_1 t_2 \in TProd T_1 T_2
  | T_Pi1  : forall Gamma T_1 T_2 t,
               Gamma |-- t \in TProd T_1 T_2 ->
                 Gamma |-- tpi1 t \in T_1
  | T_Pi2  : forall Gamma T_1 T_2 t,
               Gamma |-- t \in TProd T_1 T_2 ->
                 Gamma |-- tpi2 t \in T_2
  | T_Inl  : forall Gamma T_1 T_2 t,
               Gamma |-- t \in T_1 ->
                 Gamma |-- tinl t T_2 \in TSum T_1 T_2
  | T_Inr  : forall Gamma T_1 T_2 t,
               Gamma |-- t \in T_2 ->
                 Gamma |-- tinr t T_1 \in TSum T_1 T_2
  | T_Case : forall Gamma T_l T_r T_res t_s t_l t_r,
               Gamma |-- t_s \in TSum T_l T_r ->
               Gamma |-- t_l \in TArrow T_l T_res ->
               Gamma |-- t_r \in TArrow T_r T_res ->
                 Gamma |-- tcase t_s t_l t_r \in T_res

  | T_Return  : forall Gamma T Mo t,
                  Gamma |-- t \in T ->
                    Gamma |-- treturn t Mo \in TMonadic Mo T
(*  | T_BindM : forall Gamma t t' T_1 T_2 Mo L1 L2, 
                   Gamma |-- t \in TMonadic (MStateT T_1 L1 Mo) T_1 ->
                   Gamma |-- t' \in TArrow T_1 (TMonadic (MStateT T_2 L2 Mo) TNil) ->
                   Gamma |-- tbind t t' \in TMonadic (MStateT T_2 (LReadWrite L1 L2) Mo) TNil *)
  | T_Bind : forall Gamma t Mo T_1 t' T_2,
                  Gamma |-- t  \in TMonadic Mo T_1 ->
                  Gamma |-- t' \in TArrow T_1 (TMonadic Mo T_2) ->
                    Gamma |-- tbind t t' \in TMonadic Mo T_2
  | T_Lift    : forall Gamma t Mo T T',
                  Gamma |-- t \in TMonadic Mo T' ->
                    Gamma |-- tlift t T \in TMonadic (MStateT T LNil Mo) T'
  | T_Elevate : forall Gamma Mo Mo' t T,
                  monad_less_permissive Mo Mo' ->
                  Gamma |-- t \in TMonadic Mo T ->
                    Gamma |-- televate t Mo' \in TMonadic Mo' T
  | T_Get     : forall Gamma Mo T,
                  Gamma |-- tget T Mo \in TMonadic (MStateT T LRead Mo) T
  | T_Put     : forall Gamma Mo t T,
                  Gamma |-- t \in T ->
                   Gamma |-- tput t T Mo \in TMonadic (MStateT T LWrite Mo) TNil
  

where "Gamma '|--' t '\in' T" := (has_type Gamma t T).

Tactic Notation "has_type_cases" tactic(first) ident(c) :=
  first;
  [ Case_aux c "T_Var" | Case_aux c "T_Abs" | Case_aux c "T_App" 
  | Case_aux c "T_Unit" | Case_aux c "T_Pair"
  | Case_aux c "T_Pi1" | Case_aux c "T_Pi2" 
  | Case_aux c "T_Inl" | Case_aux c "T_Inr"
  | Case_aux c "T_Case" | Case_aux c "T_Return" | Case_aux c "T_Bind" | Case_aux c "T_Lift" 
  | Case_aux c "T_Elevate" | Case_aux c "T_Get" | Case_aux c "T_Put" ].

Lemma type_unique : forall t Gamma T1 T2, Gamma |-- t \in T1 -> Gamma |-- t \in T2 -> T1 = T2.
Proof.
  induction t; intros Gamma T1 T2 Ht1 Ht2; inversion Ht1; subst; inversion Ht2; subst; try auto.
  Case "tvar".
    assert (Some T1=Some T2). rewrite <- H2. rewrite H1. auto.
    inversion H. auto.
  Case "tapp".
    assert (T_11 = T_0). eapply IHt2. eassumption. assumption.
    subst.
    assert (TArrow T_0 T2=TArrow T_0 T1). eapply IHt1. eassumption. assumption.
    inversion H. auto.
  Case "tabs".
    assert (T_12=T_0). eapply IHt; eauto.
    subst; auto.
  Case "tpair".
    assert (T_1=T_0); eauto.
    assert (T_2=T_3); eauto.
    subst; auto.
  Case "tpi1".
    assert (TProd T1 T_2=TProd T2 T_0); eauto.
    inversion H; auto.
  Case "tpi2".
    assert (TProd T_1 T1=TProd T_0 T2); eauto.
    inversion H; auto.
  Case "tinl".
    assert (T_1 = T_0); eauto.
    subst; auto.
  Case "tinr".
    assert (T_2 = T_0); eauto.
    subst; auto.
  Case "case".
    assert (TSum T_l T_r = TSum T_l0 T_r0); eauto.
    inversion H; subst.
    assert (TArrow T_r0 T1 = TArrow T_r0 T2); eauto.
    inversion H0; auto.
  Case "treturn".
    assert (T=T0); eauto.
    subst; auto.
  Case "tbindM1". (*
    pose proof (IHt1 _ _ _ H2 H3). 
    inversion H.
    pose proof (IHt2 _ _ _ H4 H6).
    inversion H0.
    subst. auto.
  Case "tbindM2".
    pose proof (IHt1 _ _ _ H2 H3).
       inversion H.
    pose proof (IHt2 _ _ _ H4 H6).
       inversion H0. subst. 
     inversion Ht1. subst. inversion Ht2. subst. admit. admit. *) pose proof (IHt2 _ _ _ H4 H6).
     inversion H. auto.
  Case "tlift". subst.
    assert (TMonadic Mo T' = TMonadic Mo0 T'0); eauto.   
    inversion H; auto.
  Case "televate".
    assert (TMonadic Mo T = TMonadic Mo0 T0); eauto.
    inversion H; auto.
Qed.

(** ** For configurations *)
Inductive store_matches_monad : store -> mo -> Prop :=
  | matches_monad_empty : store_matches_monad nil MIdentity
  | matches_monad_cons  : forall Mo l T t Sto,
                            \empty |-- t \in T ->
                            store_matches_monad Sto Mo ->
                              store_matches_monad (t::Sto) (MStateT T l Mo).

Reserved Notation "co '|>' T" (at level 40).

Inductive configuration_has_type : configuration -> ty -> Prop :=
  | configuration_has_type_intro : forall t T Mo Sto,
                                     \empty |-- t \in TMonadic Mo T ->
                                     store_matches_monad Sto Mo ->
                                       (t,Sto) |> TMonadic Mo T
where "co |> T" := (configuration_has_type co T).

Lemma less_permissive_store_matches : forall Mo1 Mo2 Sto,
                                         monad_less_permissive Mo1 Mo2 ->
                                         store_matches_monad Sto Mo1 ->
                                         store_matches_monad Sto Mo2.
Proof.
  intros Mo1 Mo2 Sto.
  generalize dependent Mo2. generalize dependent Mo1.
  induction Sto; intros Mo1 Mo2 Hless Hmatches.
    Case "empty".
      inversion Hmatches; subst; inversion Hless; auto.
    Case "cons".
      inversion Hmatches; subst; inversion Hless; subst; constructor.
        assumption. apply (IHSto Mo); auto.
        assumption. apply (IHSto Mo); auto.
        assumption. apply (IHSto Mo); auto.
        assumption. apply (IHSto Mo); auto.
         assumption. apply (IHSto Mo); auto.
        assumption. apply (IHSto Mo); auto.
        assumption. apply (IHSto Mo); auto.
        assumption. apply (IHSto Mo); auto.
         assumption. apply (IHSto Mo); auto.
Qed.

Lemma more_permissive_store_matches : forall Mo1 Mo2 Sto,
                                        monad_less_permissive Mo2 Mo1 ->
                                        store_matches_monad Sto Mo1 ->
                                        store_matches_monad Sto Mo2.
Proof.
  intros Mo1 Mo2 Sto.
  generalize dependent Mo2. generalize dependent Mo1.
  induction Sto; intros Mo1 Mo2 Hless Hmatches.
    Case "empty".
      inversion Hmatches; subst; inversion Hless; auto.
    Case "cons".
      inversion Hmatches; subst; inversion Hless; subst; constructor.
            assumption. apply (IHSto Mo); auto.
        assumption. apply (IHSto Mo); auto.
        assumption. apply (IHSto Mo); auto.
        assumption. apply (IHSto Mo); auto.
         assumption. apply (IHSto Mo); auto.
        assumption. apply (IHSto Mo); auto.
        assumption. apply (IHSto Mo); auto.
        assumption. apply (IHSto Mo); auto.
         assumption. apply (IHSto Mo); auto.
Qed.

(** * Substitution *)

Reserved Notation "'[' x ':=' s ']' t" (at level 20).

(* Substitution function. *)
Fixpoint subst (x:id) (s:tm) (t:tm) : tm :=
  match t with
  | tvar x' => 
      if eq_id_dec x x' then s else t
  | tapp t1 t2 => 
      tapp ([x:=s] t1) ([x:=s] t2)
  | tabs x' T t1 => 
      tabs x' T (if eq_id_dec x x' then t1 else ([x:=s] t1))

  | tunit =>
      tunit
  | tpair t1 t2 =>
      tpair ([x:=s] t1) ([x:=s] t2)
  | tpi1 t1 =>
      tpi1 ([x:=s] t1)
  | tpi2 t1 =>
      tpi2 ([x:=s] t1)
  | tinl t1 T =>
      tinl ([x:=s] t1) T
  | tinr t1 T =>
      tinr ([x:=s] t1) T
  | tcase t1 t2 t3 =>
      tcase ([x:=s] t1) ([x:=s] t2) ([x:=s] t3)

  | treturn t1 M =>
      treturn ([x:=s] t1) M
  | tbind t1 t2 =>
      tbind ([x:=s] t1) ([x:=s] t2)
  | tlift t1 T =>
      tlift ([x:=s] t1) T
  | televate t1 Mo =>
      televate ([x:=s] t1) Mo
  | tget T Mo =>
      tget T Mo
  | tput t1 T Mo =>
      tput ([x:=s] t1) T Mo
  end

where "'[' x ':=' s ']' t" := (subst x s t).

(** A variable _x_ appears free in a term _t_ .*)

Inductive appears_free_in : id -> tm -> Prop :=
  | afi_var : forall x,
      appears_free_in x (tvar x)
  | afi_app1 : forall x t1 t2,
      appears_free_in x t1 -> 
      appears_free_in x (tapp t1 t2)
  | afi_app2 : forall x t1 t2,
      appears_free_in x t2 -> 
      appears_free_in x (tapp t1 t2)
  | afi_abs : forall x y T11 t12,
      y <> x  ->
      appears_free_in x t12 ->
      appears_free_in x (tabs y T11 t12)
  (* pairs *)
  | afi_pair1 : forall x t1 t2,
      appears_free_in x t1 ->
      appears_free_in x (tpair t1 t2)
  | afi_pair2 : forall x t1 t2,
      appears_free_in x t2 ->
      appears_free_in x (tpair t1 t2)
  | afi_pi1 : forall x t,
      appears_free_in x t ->
      appears_free_in x (tpi1 t)
  | afi_pi2 : forall x t,
      appears_free_in x t ->
      appears_free_in x (tpi2 t)
  (* copairs *)
  | afi_tinl : forall x t TR,
      appears_free_in x t ->
      appears_free_in x (tinl t TR)
  | afi_tinr : forall x t TL,
      appears_free_in x t ->
      appears_free_in x (tinr t TL)
  | afi_tcase1 : forall x t1 t2 t3,
      appears_free_in x t1 ->
      appears_free_in x (tcase t1 t2 t3)
  | afi_tcase2 : forall x t1 t2 t3,
      appears_free_in x t2 ->
      appears_free_in x (tcase t1 t2 t3)
  | afi_tcase3 : forall x t1 t2 t3,
      appears_free_in x t3 ->
      appears_free_in x (tcase t1 t2 t3)
  (* monadic constructors *)
  | afi_treturn : forall x t M,
      appears_free_in x t ->
      appears_free_in x (treturn t M)
  | afi_tbind1 : forall x t1 t2,
      appears_free_in x t1 ->
      appears_free_in x (tbind t1 t2)
  | afi_tbind2 : forall x t1 t2,
      appears_free_in x t2 ->
      appears_free_in x (tbind t1 t2)
  | afi_tlift : forall x t T,
      appears_free_in x t ->
      appears_free_in x (tlift t T)
  | afi_televate : forall x t M,
      appears_free_in x t ->
      appears_free_in x (televate t M)
  | afi_tput : forall x t T M,
      appears_free_in x t ->
      appears_free_in x (tput t T M)
.

Hint Constructors appears_free_in.

Definition closed (t:tm) := forall x, ~ appears_free_in x t.

Lemma context_invariance : forall Gamma Gamma' t S,
      Gamma |-- t \in S  ->
      (forall x, appears_free_in x t -> Gamma x = Gamma' x)  ->
      Gamma' |-- t \in S.
Proof with eauto.
  intros. generalize dependent Gamma'.
  induction H; intros Gamma' Heqv; auto; try (econstructor;eauto;fail)...
  Case "tvar".
    apply T_Var... rewrite <- Heqv...
  Case "tabs".
    apply T_Abs... apply IHhas_type. intros y Hafi.
    unfold extend. destruct (eq_id_dec x y)...
Qed.

Lemma free_in_context : forall x t T Gamma,
      appears_free_in x t ->
        Gamma |-- t \in T ->
        (exists T', Gamma x = Some T').
Proof with eauto.
  intros x t T Gamma Hafi Htyp.
  induction Htyp; inversion Hafi; subst...
  Case "T_Abs".
    destruct IHHtyp as [T' Hctx]... exists T'.
    unfold extend in Hctx. 
    rewrite neq_id in Hctx...
Qed.

Lemma subst_preserves_typing : forall Gamma x U v t T,
      (extend Gamma x U) |-- t \in T ->
                  \empty |-- v \in U ->
                   Gamma |-- ([x:=v] t) \in T.
Proof with eauto.
  intros Gamma x U v t S Htypt Htypv. 
  generalize dependent Gamma. generalize dependent S.
  induction t; intros S Gamma Htypt; simpl; inversion Htypt; subst; try (econstructor;eauto;fail).
  Case "tvar".
    simpl. rename i into y.
    destruct (eq_id_dec x y).
    SCase "x=y".
      subst. 
      unfold extend in H1. rewrite eq_id in H1. 
      inversion H1; subst. clear H1.
      eapply context_invariance...
      intros x Hcontra.
      destruct (free_in_context _ _ S empty Hcontra) as [T' HT']...
      inversion HT'.
    SCase "x<>y".
      apply T_Var... unfold extend in H1. rewrite neq_id in H1... 
  Case "tabs".
      rename i into y. rename t into T11.
      apply T_Abs...
      destruct (eq_id_dec x y). 
      SCase "x=y".
         eapply context_invariance...
         subst. 
         intros x Hafi. unfold extend.
         destruct (eq_id_dec y x)...
      SCase "x<>y".
         apply IHt. 
         eapply context_invariance...
         intros z Hafi. 
         unfold extend.
         destruct (eq_id_dec y z)... 
         subst. rewrite neq_id... 
Qed.

(** * Values *)

(* Values. Note that computations are *always* values (much like functional
   abstractions), i.e. they do not reduce via the lambda calculus reduction
   relation (but they *will* reduce under the monadic reduction relation,
   defined later). *)
Inductive value : tm -> Prop :=
  | v_abs : forall x T t,
      value (tabs x T t)
  | v_unit : value tunit
  | v_pair : forall t1 t2,
      value t1 -> value t2 -> value (tpair t1 t2)
  | v_inl : forall t T,
      value t -> value (tinl t T)
  | v_inr : forall t T,
      value t -> value (tinr t T)
  | v_return : forall t Mo,
      value (treturn t Mo)
  | v_bind : forall t1 t2,
      value (tbind t1 t2)
  | v_lift : forall t T,
      value (tlift t T)
  | v_elevate : forall t Mo,
      value (televate t Mo)
  | v_get : forall T Mo,
      value (tget T Mo)
  | v_put : forall t T Mo,
      value (tput t T Mo).

Theorem value_dec : forall (t:tm), value t \/ ~ (value t).
  intro.
  induction t; try (right; intro H; inversion H; fail);
               try (left; constructor; fail).

  Case "T_Pair".
    destruct IHt1.
    SCase "t1 is a value".
      destruct IHt2.
      SSCase "t2 is a value".
        left; constructor; assumption.
      SSCase "t2 is not a value".
        right; intro H1; inversion H1; contradiction.
    SCase "t1 is not a value".
      right. intro H1. inversion H1; contradiction.

  Case "T_InL".
    destruct IHt.
      left; apply v_inl; assumption.
      right; intro Hval; inversion Hval; contradiction.

  Case "T_InR".
    destruct IHt.
      left; apply v_inr; assumption.
      right; intro Hval; inversion Hval; contradiction.
Qed.

Ltac decide_value e := destruct (value_dec e) as [?Hval | ?Hnotval].

(** ** Done computations *)

Inductive done_mo : configuration -> Prop :=
  | done_return : forall v Mo Sto, value v -> done_mo (treturn v Mo,Sto).

Theorem done_mo_dec (co:configuration) : done_mo co \/ ~ (done_mo co).
  destruct co as [t Sto].
  induction t; try (right; intro H; inversion H; fail).
  Case "treturn".
    destruct (value_dec t).
    SCase "t is a value".
      left; constructor; assumption.
    SCase "t is not a value".
      right; intro Hdone; inversion Hdone; contradiction.
Qed.

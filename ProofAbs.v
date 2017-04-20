Require Import Lib. Require Import ReWire. (* Require Import TEqAxioms. *) 

Reserved Notation "Gamma '||>' t1 '==' t2 '@' T" (at level 20).

Parameter typed_eq : context -> ty -> tm -> tm -> Prop.
Notation "Gamma '||>' t1 '==' t2 '@' T" := (typed_eq Gamma T t1 t2).

Notation "\( var : ty )-> term" := (tabs var ty term) 
                                    (at level 40, var at level 100, format "\( var : ty )-> term ").

Notation "'(\' x ':' T ')' '[' t ']'" := (tabs x T t) (no associativity).
(* Equational Theory for Typed Equality *)

 Axiom ty_eq_refl  : forall Gamma T t,
                         Gamma |-- t \in T ->
                         Gamma ||> t == t @ T.
                      
  Axiom ty_eq_sym   : forall Gamma T t1 t2,
                         Gamma ||> t1 == t2 @ T ->
                         Gamma ||> t2 == t1 @ T.

  Axiom ty_eq_trans : forall Gamma T t1 t2 t3,
                         Gamma ||> t1 == t2 @ T ->
                         Gamma ||> t2 == t3 @T ->
                         Gamma ||> t1 == t3 @T.

  Axiom ty_eq_cong : forall Gamma T1 T2 t1 t2 t3 t4,
                         Gamma ||> t1 == t2 @ TArrow T1 T2 ->
                         Gamma ||> t3 == t4 @ T1 ->
                         Gamma ||> tapp t1 t3 == tapp t2 t4 @ T2.

  Axiom ty_eq_abs_cong : forall gamma x t1 t2 T1 T2,
                           extend gamma x T1 ||> t1 == t2 @ T2 ->
                           gamma ||> tabs x T1 t1 == tabs x T1 t2 @(TArrow T1 T2).

  Axiom ty_eq_abs : forall gamma x t1 t2 T T',
                           gamma ||> tabs x T t1 == tabs x T t2 @TArrow T T' ->
                           extend gamma x T ||> t1 == t2 @ T'.

  Axiom ty_eq_beta_red : forall gamma x t1 t2 T1 T2,
                           extend gamma x T2 |-- t1 \in T1 ->
                           gamma |-- t2 \in T2 ->
                           gamma ||> tapp (tabs x T2 t1) t2 == [x := t2] t1 @ T1.      
  

  Axiom ty_eq_eta_red : forall gamma x t T1 T2,
                           gamma |-- t \in TArrow T1 T2 ->
                           ~ appears_free_in x t ->
                           gamma ||> tapp (tabs x T1 t) (tvar x) == t @(TArrow T1 T2).                

  Axiom ty_eq_add : forall gamma x t1 t2 T1 T2,                 
                        gamma ||> t1 == t2 @T1 ->
                        gamma x = None ->
                        extend gamma x T2 ||> t1 == t2 @T1.                                                          

  Axiom ty_eq_drop : forall gamma x t1 t2 T T',
                        extend gamma x T' |-- t1 \in T ->
                        extend gamma x T' |-- t2 \in T ->
                        extend gamma x T' ||>  t1 == t2 @T ->
                        ~ appears_free_in x t1 ->
                        ~ appears_free_in x t2 ->
                        gamma ||> t1 == t2 @T.

  Axiom ty_eq_add_copy : forall gamma x T t1 t2 T',
                       extend gamma x T ||> t1 == t2 @ T' ->
                 extend (extend gamma x T) x T ||> t1 == t2 @ T'.      

(* Not quite right (consider when x=y, R=/=S) 
  Axiom ty_eq_permute : forall gamma x y t1 t2 T S R,
                         extend (extend gamma x S) y R ||> t1 == t2 @T ->                                                         
                         extend (extend gamma y R) x S ||> t1 == t2 @T.
*)
  Axiom ty_eq_permute_neq : forall gamma x y t1 t2 T S R,
                         x <> y ->
                         extend (extend gamma x S) y R ||> t1 == t2 @T ->                                                         
                         extend (extend gamma y R) x S ||> t1 == t2 @T.

  Axiom ty_eq_permute_teq : forall gamma x y t1 t2 T S,
                         extend (extend gamma x S) y S ||> t1 == t2 @T ->                                                         
                         extend (extend gamma y S) x S ||> t1 == t2 @T.

  
  Axiom ty_eq_right_mono : forall gamma t1 t2 t3 T1 T2 Mo,
                              gamma |-- t1 \in TMonadic Mo T1 ->
                              gamma |-- t2 \in TMonadic Mo T1 ->
                              gamma |-- t3 \in TArrow T1 (TMonadic Mo T2) ->
                              gamma ||> t1 == t2 @(TMonadic Mo T1) ->
                              gamma ||> tbind t1 t3 == tbind t2 t3 @(TMonadic Mo T2). 

  Axiom ty_eq_left_mono : forall gamma t1 t2 t3 T1 T2 Mo,
                           gamma |-- t1 \in TMonadic Mo T1 ->
                           gamma |-- t2 \in TArrow T1 (TMonadic Mo T2) ->
                           gamma |-- t3 \in TArrow T1 (TMonadic Mo T2) ->
                           gamma ||> t2 == t3 @(TArrow T1 (TMonadic Mo T2)) ->
                           gamma ||> tbind t1 t2 == tbind t1 t3 @(TMonadic Mo T2).

  (*
        Gamma |-- e1 : T -> T'    Gamma |-- e2 : T -> T'   (forall e, Gamma |-- e : T -> Gamma ||> e1 e == e2 e @ T')
        -------------------------------------------------------------------------------------------------------------
                                            Gamma ||> e1 = e2 @ T -> T'
  *)

  Axiom ty_eq_extensionality : forall gamma e1 e2 x y T1 T2,
                                 (forall t1 t2, gamma ||> t1 == t2 @T1 ->
                                 gamma ||> tapp (tabs x T1 e1) t1 == tapp (tabs y T1 e2) t2 @T2) ->
                                 gamma ||> tabs x T1 e1 == tabs y T1 e2 @TArrow T1 T2.                                  
                           
  Axiom ty_eq_bind_xch : forall gamma t1 t2 t3 t4 T1 T2 Mo,
                         gamma ||> t1 == t2 @TMonadic Mo T1 ->
                         gamma ||> tbind t2 t3 == tbind t2 t4 @TMonadic Mo T2 ->
                         gamma ||> tbind t1 t3 == tbind t1 t4 @TMonadic Mo T2.


  Axiom monad_lunit : forall gamma T T' t t' Mo,
                        gamma |-- t \in T ->
                        gamma |-- t' \in TArrow T (TMonadic Mo T') ->
                                         gamma ||> tbind (treturn t Mo) t' == (tapp t' t) @(TMonadic Mo T').
                                                  

  Axiom monad_runit : forall gamma T t x Mo,
                        gamma |-- t \in (TMonadic Mo T) ->
                        gamma ||> t == tbind t (tabs x T (treturn (tvar x) Mo)) @(TMonadic Mo T).

  Axiom ty_eq_well_type_1 : forall gamma e1 e2 T,
             gamma ||> e1 == e2 @T ->
             gamma |-- e1 \in T.

  
  Axiom ty_eq_well_type_2 : forall gamma e1 e2 T,
             gamma ||> e1 == e2 @T ->
             gamma |-- e2 \in T.

  Axiom ty_eq_bind_case : forall gamma y b t1 t2 f T T' Mo,
            gamma y = None ->
            gamma |-- t1 \in TMonadic Mo T ->
            gamma |-- t2 \in TMonadic Mo T ->
            gamma |-- b \in TSum TNil TNil ->
            gamma |-- f \in TArrow T (TMonadic Mo T') ->
            gamma ||> tbind (tcase b (tabs y TNil t1) (tabs y TNil t2)) f == tcase b (tabs y TNil (tbind t1 f)) (tabs y TNil (tbind t2 f)) @TMonadic Mo T'.

  Axiom monad_assoc : forall gamma T T' T'' m f g x Mo,
                        gamma |-- m \in (TMonadic Mo T) ->
                        gamma |-- f \in TArrow T (TMonadic Mo T') ->
                        gamma |-- g \in TArrow T' (TMonadic Mo T'') ->
                        gamma x = None ->
                        gamma ||> tbind (tbind m f) g == tbind m (tabs x T (tbind (tapp f (tvar x)) g)) @(TMonadic Mo T'').

  Axiom ty_eq_first : forall gamma t1 t2 T1 T2,
                      gamma |-- t1 \in T1 ->
                      gamma |-- t2 \in T2 ->
                      gamma ||> tpi1 (tpair t1 t2) == t1 @ T1.

  Axiom ty_eq_second : forall gamma t1 t2 T1 T2,
                       gamma |-- t1 \in T1 ->
                       gamma |-- t2 \in T2 ->
                       gamma ||> tpi2 (tpair t1 t2) == t2 @ T2.

 Axiom ty_eq_pair : forall gamma tp T1 T2,
                    gamma |-- tp \in TProd T1 T2 ->
                    gamma ||> tpair (tpi1 tp) (tpi2 tp) == tp @ TProd T1 T2.


 Axiom ty_eq_elevate : forall gamma t1 t2 T Mo Mo',
                      gamma ||> t1 == t2 @TMonadic Mo T ->
                      gamma ||> televate t1 Mo' == televate t2 Mo' @TMonadic Mo' T.

 Axiom ty_eq_put_get : forall gamma x t T Mo,
                      gamma |-- t \in T ->
                      gamma ||> tbind (tput t T Mo) (tabs x TNil (tget T Mo)) == tbind (tput t T Mo) (tabs x TNil ((treturn t Mo))) @TMonadic Mo T.


Require Import Relations.
(*Require Import Morphisms.*)
(*Require Import Setoid.

Instance typed_eq_symmetric : forall (gamma : context) (T : ty), (@Symmetric _ (typed_eq gamma T)).
Proof. red; apply ty_eq_sym.  
Defined. 

Instance typed_eq_transitive : forall (gamma : context) (T : ty), (@Transitive _ (typed_eq gamma T)).
Proof. red; apply ty_eq_trans.
Defined.


Instance typed_eq_PER :  forall (gamma : context)(T : ty), (@PER _ (typed_eq gamma T)).
Proof. split; [apply typed_eq_symmetric|apply typed_eq_transitive].
Defined.

Add Parametric Relation (gamma : context)(T:ty) : tm (typed_eq gamma T)
  symmetry proved by symmetry
  transitivity proved by transitivity
  as TypedEq_Rel. *)

(*subst*)
  Axiom closed_subst : forall x s phi T,
                    \empty |-- phi \in T ->
                     [x:=s]phi = phi.          
  Axiom tvar_subst_eq : forall x s, [x := s](tvar x) = s.
  Axiom tvar_subst_neq : forall x y s, x <> y -> [x:=s](tvar y) = tvar y.
  Axiom tvar_subst : forall (x x' : id) s,
                     [x := s] (tvar x') = if eq_id_dec x x' then s else (tvar x').
  Axiom tapp_subst : forall (x : id) s t1 t2,
                     [x := s] (tapp t1 t2) = tapp ([x := s] t1) ([x := s] t2).
  Axiom tabs_subst_eq : forall x s t1 T, [x:=s](tabs x T t1) = tabs x T t1.
  Axiom tabs_subst_neq : forall x y s t1 T, x <> y -> [x:=s](tabs y T t1) = tabs y T ([x:=s]t1).
  Axiom tabs_subst : forall (x x' : id) s t1 T,
                     [x := s] (tabs x' T t1) = tabs x' T (if eq_id_dec x x' then t1 else ([x:=s] t1)).
  Axiom tunit_subst : forall (x : id) s,
                     [x:=s] tunit = tunit.
  Axiom tpair_subst : forall (x : id) s t1 t2,
                     [x:=s] (tpair t1 t2) = tpair ([x:=s] t1) ([x:=s] t2).
  Axiom tpi1_subst  : forall (x : id) s t,
                     [x:=s] (tpi1 t) = tpi1 ([x:=s] t).
  Axiom tpi2_subst : forall x s t,
                     [x:=s] (tpi2 t) = tpi2 ([x:=s] t).
  Axiom tinl_subst : forall x s t T,
                     [x:=s] (tinl t T) = tinl ([x:=s] t) T.
  Axiom tinr_subst : forall x s t T,
                     [x:=s] (tinr t T) = tinr ([x:=s] t) T.
  Axiom tcase_subst : forall x s t1 t2 t3,
                     [x:=s] (tcase t1 t2 t3) = tcase ([x:=s] t1) ([x:=s] t2) ([x:=s] t3).
  Axiom treturn_subst : forall x s t M,
                      [x:=s] (treturn t M) = treturn ([x:=s] t) M.
  Axiom tbind_subst : forall x s t1 t2,
                     [x:=s] (tbind t1 t2) = tbind ([x:=s] t1) ([x:=s] t2).
  Axiom tlift_subst : forall x s t T,
                     [x:=s] (tlift t T) = tlift ([x:=s] t) T.
  Axiom televate_subst : forall x s t M,
                         [x:=s] (televate t M) = televate ([x:=s] t) M.
  Axiom tget_subst : forall x s T M,
                     [x:=s] (tget T M) = tget T M.
  Axiom tput_subst : forall x s t T M,
                     [x:=s] (tput t T M) = tput ([x:=s] t) T M.


  Inductive appears_in : id -> tm -> Prop :=
            | ai_var : forall x, appears_in x (tvar x)
            | ai_app1 : forall x t1 t2,
                        appears_in x t1 ->
                        appears_in x (tapp t1 t2)
            | ai_app2 : forall x t1 t2,
                        appears_in x t2 ->
                        appears_in x (tapp t1 t2)
            | ai_abs1 : forall x t T, appears_in x (tabs x T t)
            | ai_abs2 : forall x y t T,
                          y<>x ->
                          appears_in x t ->
                          appears_in x (tabs y T t)
            | ai_pair1 : forall x t1 t2,
                         appears_in x t1 ->
                         appears_in x (tpair t1 t2)
            | ai_pair2 : forall x t1 t2,
                         appears_in x t2 ->
                         appears_in x (tpair t1 t2)
            | ai_tpi1 : forall x t,                          
                        appears_in x t ->
                        appears_in x (tpi1 t)
            | ai_tpi2 : forall x t,
                        appears_in x t ->
                        appears_in x (tpi2 t)
            | ai_tinl : forall x t TR,
                        appears_in x t ->
                        appears_in x (tinl t TR)
            | ai_tinr : forall x t TL,
                        appears_in x t ->
                        appears_in x (tinr t TL)
            | ai_tcase1 : forall x t1 t2 t3,
                         appears_in x t1 ->
                         appears_in x (tcase t1 t2 t3)
            | ai_tcase2 : forall x t1 t2 t3,
                         appears_in x t2 ->
                         appears_in x (tcase t1 t2 t3)
            | ai_tcase3 : forall x t1 t2 t3,
                         appears_in x t3 ->
                         appears_in x (tcase t1 t2 t3)
            | ai_treturn : forall x t M,
                          appears_in x t ->
                          appears_in x (treturn t M)
            | ai_tbind1 : forall x t1 t2,
                          appears_in x t1 ->
                          appears_in x (tbind t1 t2)
            | ai_tbind2 : forall x t1 t2,
                          appears_in x t2 ->
                          appears_in x (tbind t1 t2)
            | ai_tlift : forall x t T,
                         appears_in x t ->
                         appears_in x (tlift t T)
            | ai_televate : forall x t M,
                         appears_in x t ->
                         appears_in x (televate t M)            
            | ai_tput : forall x t T M,
                        appears_in x t ->
                        appears_in x (tput t T M). 

   Axiom get_tabs_nil : forall gamma phi v T T' Mo,
                     gamma |-- phi \in TMonadic (MStateT T LRead Mo) T' -> 
                     ~ (appears_in v phi) ->
                     gamma ||> tbind (tget T Mo) (tabs v T phi)
                           == phi @ TMonadic (MStateT T LRead Mo) T'.
   
  Axiom sub :
     forall gamma e1 e2 x T t T',
       gamma ||> e1 == e2 @ T ->
       extend gamma x T |-- t \in T' ->
       gamma ||> [x:=e1]t == [x:=e2]t @ T'.


  (* Proof Abstraction for Imperative Languages *)
  Notation "'(\' x ':' T ')' '[' t ']'" := (tabs x T t) (no associativity).
  Axiom new_var: forall v w : id,
                 exists x, x <> v /\ x <> w.

  Axiom equal_empty_everywhere : forall gamma t1 t2 T, 
       empty ||> t1 == t2 @ T ->
       gamma ||> t1 == t2 @ T.
     
  Axiom type_permute : forall gamma x v (T : ty),
           extend (extend gamma x T) v T = extend (extend gamma v T) x T.

  Axiom type_permute_neq : forall gamma x v (T T' : ty),
           extend (extend gamma x T) v T' = extend (extend gamma v T') x T.

  
 (* State Monad Laws *)

  Definition F := Id 0.
  Definition S := Id 1.
  Definition tupdate Mo TS : tm := 
       tabs F (TArrow TS TS) (tbind (televate (tget TS Mo) (MStateT TS LReadWrite Mo)) (tabs S TS (televate (tput (tapp (tvar F) (tvar S)) TS Mo) (MStateT TS LReadWrite Mo)))).

  Axiom STsequence : forall x y f g T Mo,
            empty |-- f \in TArrow T T ->
            empty |-- g \in TArrow T T ->
            empty ||> tbind (tapp (tupdate Mo T) f) (tabs x TNil (tapp (tupdate Mo T) g)) 
                   == tapp (tupdate Mo T) (tabs y T (tapp g (tapp f (tvar y)))) @ (TMonadic (MStateT T LReadWrite Mo) TNil).

  Axiom STgetget : forall gamma x y z T Mo,
            gamma  ||> tbind (tget T Mo) (tabs x T (tbind (tget T Mo) (tabs y T (treturn (tpair (tvar x) (tvar y)) (MStateT T LRead Mo))))) 
                   == tbind (tget T Mo) (tabs z T (treturn (tpair (tvar z) (tvar z)) (MStateT T LRead Mo))) @ TMonadic (MStateT T LRead Mo) (TProd T T).

  Axiom STgetupget : forall nil x y z f T Mo,
            empty |-- f \in TArrow T T ->
            empty ||> tbind (televate (tget T Mo) (MStateT T LReadWrite Mo)) (tabs x T (tbind (tapp (tupdate Mo T) f) (tabs nil TNil (tbind (televate (tget T Mo) (MStateT T LReadWrite Mo)) (tabs y T (treturn (tpair (tvar x) (tvar y)) (MStateT T LReadWrite Mo))))))) 
                   == tbind (televate (tget T Mo) (MStateT T LReadWrite Mo)) (tabs z T (tbind (tapp (tupdate Mo T) f) (tabs nil TNil (treturn (tpair (tvar z) (tapp f (tvar z))) (MStateT T LReadWrite Mo))))) 
                   @ TMonadic (MStateT T LReadWrite Mo) (TProd T T).


  Definition W := Id 4.
  Definition V := Id 3.
  Definition X := Id 0.
  Definition Y := Id 1.
  Definition Z := Id 2.

 (* Lifting Laws *)
  Axiom lift_return : forall gamma t l T1 T2 Mo, 
        gamma |-- t \in T1 ->
        gamma ||> tlift (treturn t Mo) T2 == treturn t (MStateT T2 l Mo) @TMonadic (MStateT T2 l Mo) T1.  

  Axiom lift_bind : forall gamma phi f l T1 T2 T3 Mo,
        gamma |-- phi \in TMonadic Mo T1 ->
        gamma |-- f \in TArrow T1 (TMonadic Mo T2) ->
        gamma ||> tlift (tbind phi f) T3 == tbind (tlift phi T3) (tabs Y T1 (tlift (tapp f (tvar Y)) T3)) @TMonadic (MStateT T3 l Mo) T2.



(*Misc Axioms *)

  Axiom ty_eq_televate_ret : forall gamma t Mo Mo' T,
                        gamma |-- t \in T ->
                        monad_less_permissive Mo Mo' ->
                        gamma ||> televate (treturn t Mo) Mo' == treturn t Mo' @TMonadic Mo' T.

  Axiom ty_eq_televate : forall gamma t1 t2 Mo Mo' T,
                        monad_less_permissive Mo Mo' ->
                        gamma ||> t1 == t2 @TMonadic Mo T ->
                        gamma ||> televate t1 Mo' == televate t2 Mo' @TMonadic Mo' T.

  Axiom ty_eq_elevate_bind : forall gamma t1 t2 y Mo Mo' Mo'' T T',
                        monad_less_permissive Mo Mo'' ->
                        monad_less_permissive Mo' Mo'' ->
                        gamma |-- t1 \in TMonadic Mo T ->
                        gamma |-- tabs y T t2 \in TArrow T (TMonadic Mo' T') ->
                        gamma ||> televate (tbind t1 (tabs y T t2)) Mo'' == tbind (televate t1 Mo'') (tabs y T (televate t2 Mo'')) @ TMonadic Mo'' T'.
                        

   Axiom ty_eq_put_get_ret: forall gamma t T Mo Mo' Mo'',
              gamma |-- t \in T ->
              gamma ||> tbind (televate (tput t T Mo) Mo'') (tabs Z TNil (televate (tget T Mo) Mo'')) == tbind (televate (tput t T Mo) Mo'') (tabs Z TNil (televate (treturn t Mo') Mo'')) @TMonadic Mo'' T.
  Axiom new_var' : forall x v z (T1 T2 T3 : ty), exists y, extend (extend (extend empty x T1) v T2) z T3 y = None.
   
  Theorem typable_stronger : forall Gamma Gamma' T t,
             (forall x T, Gamma x = Some T -> Gamma' x = Some T) ->
             Gamma |-- t \in T ->
             Gamma' |-- t \in T.
  Proof. Admitted.

  Theorem typable_empty_everywhere: forall Gamma T t,
             \empty |-- t \in T ->
             Gamma |-- t \in T.
  Proof.
    intros.      
    apply (typable_stronger \empty Gamma T t).
    intros x T0.  intros.  Focus 2.
    auto.
    inversion H0.
  Qed.

  Lemma extend_not_eq : forall gamma x y (T : ty),
        x <> y ->
        (extend (gamma) y T) x = gamma x.
  Proof. Admitted.
  
  Lemma extend_x_type : forall gamma x (T:ty), 
        (extend (gamma) x T) x = Some T.
  Proof. Admitted.

  Lemma less_perm_LRW : forall T Mo l,
           monad_less_permissive (MStateT T l Mo) (MStateT T LReadWrite Mo).
  Proof. intros. induction l; constructor; apply less_permissive_refl.
  Qed.

  Ltac auto_type :=    try (repeat (econstructor; try (eapply less_perm_LRW); fail));
                      try (repeat econstructor); auto; 
                      try (eapply extend_x_type); eauto; 
                      try (eapply typable_empty_everywhere; eauto; fail); 
                      try (rewrite type_permute; eapply extend_x_type);
                      try (rewrite extend_neq; auto; fail);
                      try (rewrite extend_neq; auto; rewrite extend_x_type; auto; fail);
                      try (rewrite extend_neq; auto; rewrite extend_neq; auto; apply extend_x_type; fail);
                      try (repeat (econstructor; try (eapply less_perm_LRW))).

   Definition idempotent (phi: tm) : Prop :=
    forall v w T Mo,
      empty |-- phi \in (TMonadic Mo T) /\
      (empty ||> tbind phi (tabs v T (tbind phi (tabs w T (treturn (tpair (tvar v) (tvar w)) Mo)))) ==
      tbind phi (tabs w T (treturn (tpair (tvar w) (tvar w)) Mo)) @(TMonadic Mo (TProd T T))).

   Definition non_interfering (phi psi : tm) : Prop :=
    forall v w T1 T2 Mo,
      empty |-- phi \in (TMonadic Mo T1) /\
      empty |-- psi \in (TMonadic Mo T2) /\
      empty ||>  tbind phi (tabs v T1 (tbind psi (tabs w T2 (treturn (tpair (tvar v) (tvar w)) Mo)))) ==
                 tbind psi (tabs w T2 (tbind phi (tabs v T1 (treturn (tpair (tvar v) (tvar w)) Mo)))) @(TMonadic Mo (TProd T1 T2)).
   Definition NIL := Id 100.
   Definition innocent1 (phi : tm) : Prop :=
          forall psi v T Mo,
             empty |-- psi \in TMonadic Mo T ->
             empty ||> tbind phi (tabs NIL TNil psi) == tbind psi (tabs v T (tbind phi (tabs NIL TNil (treturn (tvar v) Mo)))) @TMonadic Mo T. 
   

   Definition innocent2 (phi : tm) : Prop :=
          forall psi T Mo,
             empty |-- psi \in TMonadic Mo T ->
             empty ||> tbind phi (tabs NIL TNil psi) == psi @TMonadic Mo T. 

   Ltac push_one_subst :=  
      match goal with
       | [ |- context[[_:=_]tbind _ _]] => rewrite tbind_subst; try auto
       | [ |- context[[?X:=_]tabs ?V _ _]] => destruct (eq_id_dec X V); subst; try (rewrite tabs_subst_eq; try auto); try (rewrite tabs_subst_neq; try auto); try inversion e
       | [ |- context[[?X:=_]tvar ?X]] => rewrite tvar_subst_eq; try auto
       | [ |- context[[?X:=_]tvar ?V]] => rewrite tvar_subst_neq; try auto
       | [ |- context[[_:=_]tapp _ _]] => rewrite tapp_subst; try auto
       | [ |- context[[_:=_]treturn _ _]] => rewrite treturn_subst; try auto
       | [ |- context[[_:=_]tpair _ _]] => rewrite tpair_subst; try auto
       | [ |- context[[_:=_]tunit]] => rewrite tunit_subst
       | [ |- context[[_:=_]tpi1 _]] => rewrite tpi1_subst; try auto
       | [ |- context[[_:=_]tpi2 _]] => rewrite tpi2_subst; try auto
       | [ |- context[[_:=_]tinl _ _]] => rewrite tinl_subst; try auto
       | [ |- context[[_:=_]tinr _ _]] => rewrite tinr_subst; try auto
       | [ |- context[[_:=_]tcase _ _ _]] => rewrite tcase_subst; try auto
       | [ |- context[[_:=_]tlift _ _]] => rewrite tlift_subst; try auto
       | [ |- context[[_:=_]televate _ _]] => rewrite televate_subst; try auto
       | [ |- context[[_:=_]tget _ _]] => rewrite tget_subst; try auto
       | [ |- context[[_:=_]tput _ _ _]] => rewrite tput_subst; try auto
       | [ |- context[[_:=_]_]] => erewrite closed_subst; try eauto
    end.

   
   Ltac push_one_subst_hyp :=  
      match goal with
       | [H: context[[_:=_]tbind _ _] |-_] => rewrite tbind_subst in H; try auto
       | [H: context[[?X:=_]tabs ?V _ _] |- _] => destruct (eq_id_dec X V) in H; subst; try (rewrite tabs_subst_eq in H; try auto); try (rewrite tabs_subst_neq in H; try auto); try inversion e
       | [H: context[[?X:=_]tvar ?X] |- _] => rewrite tvar_subst_eq in H; try auto
       | [H: context[[?X:=_]tvar ?V] |- _] => rewrite tvar_subst_neq in H; try auto
       | [H: context[[_:=_]tapp _ _] |- _] => rewrite tapp_subst in H; try auto
       | [H: context[[_:=_]treturn _ _] |- _] => rewrite treturn_subst in H; try auto
       | [H: context[[_:=_]tpair _ _] |- _] => rewrite tpair_subst in H; try auto
       | [H: context[[_:=_]tunit] |- _] => rewrite tunit_subst in H
       | [H: context[[_:=_]tpi1 _] |- _] => rewrite tpi1_subst in H; try auto
       | [H: context[[_:=_]tpi2 _] |- _] => rewrite tpi2_subst in H; try auto
       | [H: context[[_:=_]tinl _ _] |- _] => rewrite tinl_subst in H; try auto
       | [H: context[[_:=_]tinr _ _] |- _] => rewrite tinr_subst in H; try auto
       | [H: context[[_:=_]tcase _ _ _] |- _] => rewrite tcase_subst in H; try auto
       | [H: context[[_:=_]tlift _ _] |- _] => rewrite tlift_subst in H; try auto
       | [H: context[[_:=_]televate _ _] |- _] => rewrite televate_subst in H; try auto
       | [H: context[[_:=_]tget _ _] |- _] => rewrite tget_subst in H; try auto
       | [H: context[[_:=_]tput _ _ _] |- _] => rewrite tput_subst in H; try auto
       | [H: context[[_:=_]_] |- _] => erewrite closed_subst in H; try eauto
   end.
 
   Example push_hyp_test : forall x s t1 t2,
           [x:=s]tapp t1 t2 = tapp ([x:=s]t1) ([x:=s]t2) -> True.
   Proof. intros. push_one_subst_hyp.
   Qed.   

   Ltac push_subst_hyp := repeat (try push_one_subst_hyp; try inversion e).

   Example push_test : forall x s t1 t2,
                       [x:=s]tapp t1 t2 = tapp ([x:=s]t1) ([x:=s]t2).
   Proof. intros. push_one_subst. Qed.

   Example push_test2 : forall x s t1 t2,
                    [x:=s]tbind t1 t2 = tbind ([x:=s]t1) ([x:=s]t2).
   Proof. intros. push_one_subst. Qed.

   Example push_test3 : forall x v s t T,
                        [x:=s]tabs v T t = if (eq_id_dec x v) then (tabs v T t) else [x:=s](tabs v T t).
   Proof. intros. push_one_subst. Qed.

   Ltac push_subst := repeat push_one_subst.

   Print ty_eq_right_mono.


   Ltac right_mono f T:=
        match goal with
        |[H: ?gamma ||> ?t1 == ?t2 @ TMonadic ?Mo ?T' |- _]
               => apply (ty_eq_right_mono gamma t1 t2 f T' T Mo) in H; auto_type; auto
        end.
   Example right_mono_test_1 : forall gamma t1 t2 f T1 T2 Mo,
            gamma |-- f \in TArrow T1 (TMonadic Mo T2) ->
            gamma |-- t1 \in TMonadic Mo T1 ->
            gamma |-- t2 \in TMonadic Mo T1 ->
            gamma ||> t1 == t2 @TMonadic Mo T1 ->
            gamma ||> tbind t1 f == tbind t2 f @TMonadic Mo T2.
   intros. right_mono f T2. Qed.

   Ltac left_mono T1:=
         match goal with
          | [ |- ?gamma ||> tbind ?t1 ?t2 == tbind ?t1 ?t3 @ TMonadic ?Mo ?T2] =>
             apply (ty_eq_left_mono gamma t1 t2 t3 T1 T2 Mo) end; auto_type.
  

   Ltac trans := 
        match goal with
        | [H1: ?gamma ||> ?t1 == ?t2 @?T, H2: ?gamma ||> ?t2 == ?t3 @?T |- _] => 
            pose proof (ty_eq_trans gamma T t1 t2 t3 H1 H2); try auto; clear H1 H2
        end. 
     
   Ltac super_trans :=
        match goal with
        | [H1: ?gamma ||> ?t1 == ?t2 @?T |- _] =>
            match goal with
            | [H2: gamma ||> t2 == ?t3 @T |- _] =>  let H := fresh in (
                                                        pose proof (ty_eq_trans gamma T t1 t2 t3 H1 H2) as H)
        end
       end; auto.

   Ltac copy_hyp H1 :=
       let H := fresh in
       generalize H1 as H; intro H.



Ltac prover :=
       repeat match goal with
              |[ |- ?g ||> tbind (tbind ?t1 (tabs ?x ?T ?t2)) ?t3 == tbind ?t1 (tabs ?x ?T (tbind (tapp (tabs ?x ?T ?t2)(tvar ?x)) ?t3))@TMonadic ?Mo ?T'] => 
                      try (try (eapply equal_empty_everywhere; eapply monad_assoc)); try push_subst; auto_type; fail
              |[ |- ?g ||> tbind (tbind ?t1 (tabs ?x ?T ?t2)) ?t3 == tbind ?t1 (tabs ?x ?T (tbind (tapp (tabs ?x ?T ?t2)(tvar ?x)) ?t3))@TMonadic ?Mo ?T'] => try (eapply monad_assoc); auto_type
              |[ |- ?g ||> tapp (tabs ?x ?T2 ?t1) ?t2 == [?x := ?t2] ?t1 @ ?T1] => try (eapply ty_eq_beta_red; try (push_subst; auto_type; fail); auto_type)
              |[ |- ?g ||> tabs ?x ?T1 ?t1 == tabs ?x ?T1 ?t2 @(TArrow ?T1 ?T2)]=> try (eapply ty_eq_abs_cong; try (push_subst; auto_type; fail); auto_type)
              |[ |- ?g ||> tbind ?t1 ?t3 == tbind ?t2 ?t3 @(TMonadic ?Mo ?T2)]=> try (eapply ty_eq_right_mono; try (push_subst; auto_type; fail); auto_type)
              |[ |- ?g ||> tbind ?t1 ?t2 == tbind ?t1 ?t3 @(TMonadic ?Mo ?T2)]=> try (eapply ty_eq_left_mono ; try (push_subst; auto_type; fail); auto_type) (*[auto_type | auto_type | push_subst; auto_type |  ])*)
              |[ |- ?g ||> tbind (treturn ?t ?Mo) ?t' == (tapp ?t' ?t) @(TMonadic ?Mo ?T')]=> try (eapply monad_lunit; auto_type)
               
             end.

   Lemma idemp_phi_left_side : forall phi f v w T1 T2 Mo, 
                   \empty |-- phi \in TMonadic Mo T1 ->
                   \empty |-- f \in TArrow (TProd T1 T1) (TMonadic Mo T2) ->
                   (* idempotent phi -> *)
                   \empty ||> tbind (tbind phi (tabs v T1 (tbind phi (tabs w T1 (treturn (tpair (tvar v) (tvar w)) Mo))))) f 
                          ==  tbind phi (tabs v T1 (tbind phi (tabs w T1 (tapp f (tpair (tvar v) (tvar w)))))) 
                          @TMonadic Mo T2.
   Proof. intros.
   
   assert (empty ||> tbind (tbind phi (tabs v T1 (tbind phi (tabs w T1 (treturn (tpair (tvar v) (tvar w)) Mo))))) f 
                 ==  tbind phi (tabs v T1 (tbind (tapp (tabs v T1 (tbind phi (tabs w T1 (treturn (tpair (tvar v) (tvar w)) Mo)))) (tvar v)) f))
                 @TMonadic Mo T2);
   prover.
  
   assert (empty ||> tbind phi (tabs v T1 (tbind (tapp (tabs v T1 (tbind phi (tabs w T1 (treturn (tpair (tvar v) (tvar w)) Mo)))) (tvar v)) f))
                 ==  tbind phi (tabs v T1 (tbind ([v:=tvar v]tbind phi (tabs w T1 (treturn (tpair (tvar v) (tvar w)) Mo))) f))
                 @TMonadic Mo T2); prover; try trans;
  try push_subst_hyp.

   Case "v=w".
        assert (empty ||> tbind phi (tabs w T1 (tbind (tbind phi (tabs w T1 (treturn (tpair (tvar w) (tvar w)) Mo))) f))
                      ==  tbind phi (tabs w T1 (tbind phi (tabs w T1 (tbind (tapp (tabs w T1 (treturn (tpair (tvar w) (tvar w)) Mo)) (tvar w)) f))))
                      @TMonadic Mo T2); prover; trans.

match goal with |[H: ?g ||> ?t1 == tbind ?p (tabs ?v ?T1 (tbind ?p (tabs ?w ?T1 (tbind (tapp (tabs ?w ?T1 (treturn (tpair (tvar ?v) (tvar ?w)) ?Mo))(tvar ?w)) ?f))))
                               @TMonadic ?Mo ?T2|- _]=> assert (g ||> tbind p (tabs v T1 (tbind p (tabs w T1 (tbind (tapp (tabs w T1 (treturn (tpair (tvar v) (tvar w)) Mo))(tvar w)) f))))
                                                                  ==  tbind p (tabs v T1 (tbind p (tabs w T1 (tbind ([w:=tvar w]treturn (tpair (tvar v) (tvar w)) Mo) f))))
                                                                  @TMonadic Mo T2) end; prover; trans; push_subst_hyp.

(*
        match goal with |[H: ?g ||> ?t1 == tbind ?p (tabs ?w ?T1 (tbind ?p (tabs ?w ?T1 (tbind (tapp (tabs ?w ?T1 (treturn (tpair (tvar ?w) (tvar ?w)) ?Mo))(tvar ?w)) ?f))))
                               @TMonadic ?Mo ?T2|- _]=> assert (g ||> tbind p (tabs w T1 (tbind p (tabs w T1 (tbind (tapp (tabs w T1 (treturn (tpair (tvar w) (tvar w)) Mo))(tvar w)) f))))
                                                                  ==  tbind p (tabs w T1 (tbind p (tabs w T1 (tbind ([w:=tvar w]treturn (tpair (tvar w) (tvar w)) Mo) f))))
                                                                  @TMonadic Mo T2) end; prover; trans; push_subst_hyp.
        
  *)      match goal with |[H: ?g ||> ?t1 == tbind ?p (tabs ?w ?T1 (tbind ?p (tabs ?w ?T1 (tbind (treturn ?tp ?Mo) ?f)))) @TMonadic ?Mo ?T2 |- _] =>
                      assert (g ||> tbind p (tabs w T1 (tbind p (tabs w T1 (tbind (treturn tp Mo) f)))) == tbind p (tabs w T1 (tbind p (tabs w T1 (tapp f tp)))) @TMonadic Mo T2) end; prover; trans.

  Case "v/=w".
        assert (empty ||> tbind phi (tabs v T1 (tbind (tbind phi (tabs w T1 (treturn (tpair (tvar v) (tvar w)) Mo))) f))
                      ==  tbind phi (tabs v T1 (tbind phi (tabs w T1 (tbind (tapp (tabs w T1 (treturn (tpair (tvar v) (tvar w)) Mo)) (tvar w)) f))))
                      @TMonadic Mo T2); prover; trans.

        match goal with |[H: ?g ||> ?t1 == tbind ?p (tabs ?v ?T1 (tbind ?p (tabs ?w ?T1 (tbind (tapp (tabs ?w ?T1 (treturn (tpair (tvar ?v) (tvar ?w)) ?Mo))(tvar ?w)) ?f))))
                               @TMonadic ?Mo ?T2|- _]=> assert (g ||> tbind p (tabs v T1 (tbind p (tabs w T1 (tbind (tapp (tabs w T1 (treturn (tpair (tvar v) (tvar w)) Mo))(tvar w)) f))))
                                                                  ==  tbind p (tabs v T1 (tbind p (tabs w T1 (tbind ([w:=tvar w]treturn (tpair (tvar v) (tvar w)) Mo) f))))
                                                                  @TMonadic Mo T2) end; prover; trans; push_subst_hyp.
        match goal with |[H: ?g ||> ?t1 == tbind ?p (tabs ?v ?T1 (tbind ?p (tabs ?w ?T1 (tbind (treturn ?tp ?Mo) ?f)))) @TMonadic ?Mo ?T2 |- _] =>
                      assert (g ||> tbind p (tabs v T1 (tbind p (tabs w T1 (tbind (treturn tp Mo) f)))) == tbind p (tabs v T1 (tbind p (tabs w T1 (tapp f tp)))) @TMonadic Mo T2) end; prover; trans.

 Qed.

        

   Lemma idemp_phi_right_side : forall phi f w T1 T2 Mo,
                   \empty |-- phi \in TMonadic Mo T1 ->
                   \empty |-- f \in TArrow (TProd T1 T1) (TMonadic Mo T2) ->
                   (* idempotent phi -> *)
                   \empty ||> tbind (tbind phi (tabs w T1 (treturn (tpair (tvar w) (tvar w)) Mo))) f 
                          ==  tbind phi (tabs w T1 (tapp f (tpair (tvar w) (tvar w)))) 
                          @TMonadic Mo T2.
   Proof. intros.

    assert (empty ||> tbind (tbind phi (tabs w T1 (treturn (tpair (tvar w) (tvar w)) Mo))) f
                  ==  tbind phi (tabs w T1 (tbind (tapp (tabs w T1 (treturn (tpair (tvar w) (tvar w)) Mo)) (tvar w)) f))
                  @TMonadic Mo T2); prover.                        

    assert (empty ||> tbind phi (tabs w T1 (tbind (tapp (tabs w T1 (treturn (tpair (tvar w) (tvar w)) Mo)) (tvar w)) f))
                  ==  tbind phi (tabs w T1 (tbind ([w:=tvar w]treturn (tpair (tvar w) (tvar w)) Mo) f))
                  @TMonadic Mo T2); try (prover; trans; push_subst_hyp).    

    assert (empty ||> tbind phi (tabs w T1 (tbind (treturn (tpair (tvar w) (tvar w)) Mo) f))
                  == tbind phi (tabs w T1 (tapp f (tpair (tvar w) (tvar w))))
                  @TMonadic Mo T2); prover; trans.   
   Qed.


   Theorem idemp_phi : forall phi f v w T1 T2 Mo,
                      \empty |-- phi \in TMonadic Mo T1 ->
                      \empty |-- f \in TArrow (TProd T1 T1) (TMonadic Mo T2) ->
                      idempotent phi ->
                      \empty ||> tbind phi (tabs v T1 (tbind phi (tabs w T1 (tapp f (tpair (tvar v) (tvar w))))))
                                 ==
                                tbind phi (tabs w T1 (tapp f (tpair (tvar w) (tvar w)))) @ TMonadic Mo T2.
   Proof.
   intros.

   assert (empty ||> tbind phi (tabs v T1 (tbind phi (tabs w T1 (tapp f (tpair (tvar v) (tvar w))))))
                 ==  tbind (tbind phi (tabs v T1 (tbind phi (tabs w T1 (treturn (tpair (tvar v) (tvar w)) Mo))))) f
                 @TMonadic Mo T2); apply ty_eq_sym; try (eapply idemp_phi_left_side); auto_type;
   

   assert (empty ||> tbind (tbind phi (tabs v T1 (tbind phi (tabs w T1 (treturn (tpair (tvar v) (tvar w)) Mo))))) f
                 ==  tbind (tbind phi (tabs w T1 (treturn (tpair (tvar w) (tvar w)) Mo))) f
                 @TMonadic Mo T2); try (eapply ty_eq_right_mono); auto_type; unfold idempotent in H1; pose proof (H1 v w T1 Mo); try destruct H3; try assumption;
                 super_trans;

   assert (empty ||> tbind (tbind phi (tabs w T1 (treturn (tpair (tvar w) (tvar w)) Mo))) f
                 ==  tbind phi (tabs w T1 (tapp f (tpair (tvar w) (tvar w))))
                 @TMonadic Mo T2); try (eapply idemp_phi_right_side);  try (eapply monad_lunit); auto_type; try apply ty_eq_sym; super_trans.  

   Qed.


   Lemma NI_leftside : forall phi psi f v w T1 T2 T3 Mo, 
        empty |-- phi \in TMonadic Mo T1 ->
        empty |-- psi \in TMonadic Mo T2 ->
        empty |-- f \in TArrow (TProd T1 T2) (TMonadic Mo T3) ->
        w <> v ->
        non_interfering phi psi ->
        empty ||> tbind (tbind phi (tabs v T1 (tbind psi (tabs w T2 (treturn (tpair (tvar v) (tvar w)) Mo))))) f ==
                  tbind phi (tabs v T1 (tbind psi (tabs w T2 (tapp f (tpair (tvar v) (tvar w)))))) @TMonadic Mo T3.
   Proof.
   intros.
   
   assert(empty ||> tbind (tbind phi (tabs v T1 (tbind psi (tabs w T2 (treturn (tpair (tvar v) (tvar w)) Mo))))) f
                ==  tbind phi (tabs v T1 (tbind (tapp (tabs v T1 (tbind psi (tabs w T2 (treturn (tpair (tvar v) (tvar w)) Mo)))) (tvar v)) f))
                @TMonadic Mo T3); prover.

   assert(empty ||> tbind phi (tabs v T1 (tbind (tapp (tabs v T1 (tbind psi (tabs w T2 (treturn (tpair (tvar v) (tvar w)) Mo)))) (tvar v)) f))
                ==  tbind phi (tabs v T1 (tbind ([v:=tvar v]tbind psi (tabs w T2 (treturn (tpair (tvar v) (tvar w)) Mo))) f))
                @TMonadic Mo T3). 
           eapply ty_eq_left_mono.  auto_type. auto_type. push_subst.  unfold not in H2. destruct H2. auto. auto_type. 
           eapply ty_eq_abs_cong; auto_type. 
           eapply ty_eq_right_mono; try (auto_type; fail). push_subst. unfold not in H2; destruct H2; auto; fail. auto_type.
           eapply ty_eq_beta_red; auto_type.
   trans. push_subst_hyp. unfold not in H2. destruct H2. auto.

   assert(empty ||> tbind phi (tabs v T1 (tbind (tbind psi (tabs w T2 (treturn (tpair (tvar v) (tvar w)) Mo))) f))
                ==  tbind phi (tabs v T1 (tbind psi (tabs w T2 (tbind (tapp (tabs w T2 (treturn (tpair (tvar v) (tvar w)) Mo)) (tvar w)) f))))
                @TMonadic Mo T3); try (eapply ty_eq_left_mono); auto_type.
        eapply ty_eq_abs_cong; auto_type.
        eapply monad_assoc; auto_type.
trans.
   assert(empty ||> tbind phi (tabs v T1 (tbind psi (tabs w T2 (tbind (tapp (tabs w T2 (treturn (tpair (tvar v) (tvar w)) Mo)) (tvar w)) f))))
                 == tbind phi (tabs v T1 (tbind psi (tabs w T2 (tbind ([w:=tvar w]treturn (tpair (tvar v) (tvar w)) Mo) f))))
                @TMonadic Mo T3).
        prover. push_subst_hyp. 
trans.
   assert(empty ||> tbind phi (tabs v T1 (tbind psi (tabs w T2 (tbind (treturn (tpair (tvar v) (tvar w)) Mo) f))))
                ==  tbind phi (tabs v T1 (tbind psi (tabs w T2 (tapp f (tpair (tvar v) (tvar w))))))
                @TMonadic Mo T3); prover; trans.
          

Qed.


   Lemma NI_rightside : forall phi psi f v w T1 T2 T3 Mo,
        empty |-- phi \in TMonadic Mo T1 ->
        empty |-- psi \in TMonadic Mo T2 ->
        empty |-- f \in TArrow (TProd T1 T2) (TMonadic Mo T3) ->
        w <> v ->
        non_interfering phi psi ->
        empty ||> tbind (tbind psi (tabs w T2 (tbind phi (tabs v T1 (treturn (tpair (tvar v) (tvar w)) Mo))))) f ==
                  tbind psi (tabs w T2 (tbind phi (tabs v T1 (tapp f (tpair (tvar v) (tvar w)))))) @TMonadic Mo T3. 
   Proof.
   intros. 

   assert (empty ||> tbind (tbind psi (tabs w T2 (tbind phi (tabs v T1 (treturn (tpair (tvar v) (tvar w)) Mo))))) f
                 ==  tbind psi (tabs w T2 (tbind (tapp (tabs w T2 (tbind phi (tabs v T1 (treturn (tpair (tvar v) (tvar w)) Mo)))) (tvar w)) f))
                 @TMonadic Mo T3); prover.    

   assert (empty ||> tbind psi (tabs w T2 (tbind (tapp (tabs w T2 (tbind phi (tabs v T1 (treturn (tpair (tvar v) (tvar w)) Mo)))) (tvar w)) f))
                 ==  tbind psi (tabs w T2 (tbind ([w:=tvar w]tbind phi (tabs v T1 (treturn (tpair (tvar v) (tvar w)) Mo))) f))
                 @TMonadic Mo T3).              
          eapply ty_eq_left_mono.  auto_type. auto_type. push_subst.  unfold not in H2. destruct H2. auto. auto_type. 
           eapply ty_eq_abs_cong; auto_type. 
           eapply ty_eq_right_mono; try (auto_type; fail). push_subst. unfold not in H2; destruct H2; auto; fail. auto_type.
           eapply ty_eq_beta_red; auto_type.
   trans. push_subst_hyp. unfold not in H2. destruct H2. auto.

   assert (empty ||> tbind psi (tabs w T2 (tbind (tbind phi (tabs v T1 (treturn (tpair (tvar v) (tvar w)) Mo))) f))
           ==  tbind psi (tabs w T2 (tbind phi (tabs v T1 (tbind (tapp (tabs v T1 (treturn (tpair (tvar v) (tvar w)) Mo)) (tvar v)) f))))                  @TMonadic Mo T3); prover; trans.

   assert (empty ||> tbind psi (tabs w T2 (tbind phi (tabs v T1 (tbind (tapp (tabs v T1 (treturn (tpair (tvar v) (tvar w)) Mo)) (tvar v)) f))))
                 ==  tbind psi (tabs w T2 (tbind phi (tabs v T1 (tbind ([v:=tvar v]treturn (tpair (tvar v) (tvar w)) Mo) f))))
                 @TMonadic Mo T3); prover; trans; push_subst_hyp.

   assert (empty ||> tbind psi (tabs w T2 (tbind phi (tabs v T1 (tbind (treturn (tpair (tvar v) (tvar w)) Mo) f))))
                 ==  tbind psi (tabs w T2 (tbind phi (tabs v T1 (tapp f (tpair (tvar v) (tvar w))))))
                 @TMonadic Mo T3); prover; trans.
Qed.


Theorem nonint_phi_psi : forall phi psi f v w T1 T2 T3 Mo,
            \empty |-- phi \in TMonadic Mo T1 ->
            \empty |-- psi \in TMonadic Mo T2 ->
            \empty |-- f \in TArrow (TProd T1 T2) (TMonadic Mo T3) ->
            non_interfering phi psi ->
            w <> v ->
            \empty ||> tbind phi (tabs v T1 (tbind psi (tabs w T2 (tapp f (tpair (tvar v) (tvar w))))))
                   ==  tbind psi (tabs w T2 (tbind phi (tabs v T1 (tapp f (tpair (tvar v) (tvar w)))))) @ TMonadic Mo T3.
   Proof.
     intros. 
     pose proof (NI_leftside phi psi f v w T1 T2 T3 Mo H H0 H1 H3 H2).
     pose proof(NI_rightside phi psi f v w T1 T2 T3 Mo H H0 H1 H3 H2).
     unfold non_interfering in H2. pose proof H2 v w T1 T2 Mo. clear H2. destruct H6. destruct H6. clear H2. clear H6.
     right_mono f T3.
     trans. apply ty_eq_sym in H4. trans.
   Qed. 

   Lemma less_perm_LRW' : forall T Mo l,
           monad_less_permissive (MStateT T l Mo) (MStateT T LReadWrite Mo).
  Proof. intros. induction l; constructor; apply less_permissive_refl.
  Qed.

  Definition Bit : ty := TSum TNil TNil.
  Definition W8 : ty := TProd Bit (TProd Bit (TProd Bit (TProd Bit (TProd Bit (TProd Bit (TProd Bit Bit)))))).
  Definition Int : ty := TProd W8 (TProd W8 (TProd W8 W8)).
  Definition TProp : ty := Bit.
  Definition Val : ty := TSum TNil (TSum Int TProp).
  Definition State : ty := TArrow W8 Val.
 
  (*Definition getloc x sto T Mo : tm := 
                tbind (tget T Mo) (tabs sto State (treturn (tapp (tvar sto) x) Mo)).
  *)
 

  Definition getloc Mo : tm := 
                tabs X W8 (tbind (tget State Mo) (tabs Y State (treturn (tapp (tvar Y) (tvar X)) (MStateT State LRead Mo)))).
  Check getloc. 
  Theorem getloc_has_type Mo : forall gamma, extend gamma X W8 |-- tapp (getloc Mo) (tvar X) \in (TMonadic (MStateT State LRead Mo) Val).
    Proof.
      intros.
      auto_type.
  Qed.

  Lemma update_typecheck : forall Mo TS, 
          empty |-- tupdate Mo TS \in TArrow (TArrow TS TS) (TMonadic (MStateT TS LReadWrite Mo) TNil). intros. unfold tupdate.
          econstructor; econstructor; econstructor; try (eapply less_perm_LRW); econstructor; try (eapply less_perm_LRW); constructor; auto_type.
  Qed.


  Definition ttrue : tm := tinl tunit TNil.
  Definition tfalse : tm := tinr tunit TNil.
  Definition andb : tm := tabs X Bit (tabs Y Bit (tcase (tvar X) (tabs Z TNil (tvar Y)) (tabs Z TNil (tfalse)))).
  Definition andp : tm := tabs X TProp (tabs Y TProp (tcase (tvar X) (tabs Z TNil (tvar Y)) (tabs Z TNil (tfalse)))).
  Definition implp : tm := tabs X TProp (tabs Y TProp (tcase (tvar X) (tabs Z TNil (tvar Y)) (tabs Z TNil (ttrue)))).
  Definition bitcompare : tm := tabs X Bit (tabs Y Bit (tcase (tvar X) (tabs Z TNil (tvar Y)) (tabs Z TNil (tcase (tvar Y) (tabs Z TNil (tfalse)) (tabs Z TNil (ttrue)))))).
  Definition bitwise : tm := tabs X W8 (tabs Y W8 (tcase (tapp (tapp bitcompare (tpi1 (tvar X)))                                                         ((tpi1 (tvar Y))))
                                                    (tabs W TNil (tcase (tapp (tapp bitcompare (tpi1 (tpi2 (tvar X))))                                     ((tpi1 (tpi2 (tvar Y)))))  
                                                     (tabs W TNil (tcase (tapp (tapp bitcompare (tpi1 (tpi2 (tpi2 (tvar X)))))                              ((tpi1 (tpi2 (tpi2 (tvar Y)))))) 
                                                      (tabs W TNil (tcase (tapp (tapp bitcompare (tpi1 (tpi2 (tpi2 (tpi2 (tvar X))))))                       ((tpi1 (tpi2 (tpi2 (tpi2 (tvar Y))))))) 
                                                       (tabs W TNil (tcase (tapp (tapp bitcompare (tpi1 (tpi2 (tpi2 (tpi2 (tpi2 (tvar X)))))))                ((tpi1 (tpi2 (tpi2 (tpi2 (tpi2 (tvar Y)))))))) 
                                                        (tabs W TNil (tcase (tapp (tapp bitcompare (tpi1 (tpi2 (tpi2 (tpi2 (tpi2 (tpi2 (tvar X))))))))         ((tpi1 (tpi2 (tpi2 (tpi2 (tpi2 (tpi2 (tvar Y))))))))) 
                                                         (tabs W TNil (tcase (tapp (tapp bitcompare (tpi1 (tpi2 (tpi2 (tpi2 (tpi2 (tpi2 (tpi2 (tvar X)))))))))  ((tpi1 (tpi2 (tpi2 (tpi2 (tpi2 (tpi2 (tpi2 (tvar Y)))))))))) 
                                                          (tabs W TNil (tcase (tapp (tapp bitcompare (tpi2 (tpi2 (tpi2 (tpi2 (tpi2 (tpi2 (tpi2 (tvar X)))))))))  ((tpi2 (tpi2 (tpi2 (tpi2 (tpi2 (tpi2 (tpi2 (tvar Y))))))))))
                                                           (tabs W TNil ttrue) (tabs W TNil tfalse))) 
                                                         (tabs W TNil tfalse))) 
                                                        (tabs W TNil tfalse))) 
                                                       (tabs W TNil tfalse))) 
                                                      (tabs W TNil tfalse))) 
                                                     (tabs W TNil tfalse))) 
                                                    (tabs W TNil tfalse))) 
                                                   (tabs W TNil tfalse))).
 
  Lemma ttrue_has_type : \empty |-- ttrue \in Bit.
  Proof. auto_type.
  Qed.

  Lemma tfalse_has_type : \empty |-- tfalse \in Bit.
  auto_type.
  Qed.

  Lemma bitcompare_has_type : \empty |-- bitcompare \in TArrow Bit (TArrow Bit Bit).
  Proof. auto_type.
  Qed.    

  Lemma bitwise_has_type : \empty |-- bitwise \in TArrow W8 (TArrow W8 (TSum TNil TNil)).
  Proof. auto_type. Qed.

  Definition sto := Z.  
  Definition tweak : tm := 
    tabs X W8
         (tabs V Val
               (tabs sto State
                     (tabs Y W8 (tcase (tapp (tapp bitwise (tvar X)) (tvar Y)) (tabs W TNil (tvar V)) (tabs W TNil (tapp (tvar sto) (tvar Y))))))).

  Theorem tweak_has_type : \empty |-- tweak \in TArrow W8 (TArrow Val (TArrow State State)).
  Proof.  unfold tweak.  auto_type.
  Qed.

  Axiom ty_eq_tapp_tweak : forall gamma x v s Val Mo,
                                         extend gamma v Val||> treturn (tapp (tapp (tapp (tapp tweak (tvar x)) (tvar v)) (tvar s)) (tvar x)) Mo
                                               ==  treturn (tvar v) Mo
                                                            @TMonadic Mo Val.

  Definition F1 := Id 9.
  Definition Delta := Id 10.
  Definition Sigma := Id 11.
  Definition Sigma' := Id 12.
  
  Lemma get_idemp_left_side : forall f T T' Mo,
        empty |-- f \in TArrow (TProd T T) (TMonadic (MStateT T LRead Mo) T') ->
        empty ||> tbind (tbind (tget T Mo) (tabs Sigma T (tbind (tget T Mo) (tabs Sigma' T (treturn (tpair (tvar Sigma) (tvar Sigma')) (MStateT T LRead Mo)))))) f 
              ==  tbind (tget T Mo) (tabs Sigma T (tbind (tget T Mo) (tabs Sigma' T (tapp f (tpair (tvar Sigma) (tvar Sigma')))))) @TMonadic (MStateT T LRead Mo) T'.
  Proof. 
    intros.  (*Admitted.*)

    assert (H1: empty ||> tbind (tbind (tget T Mo) (tabs Sigma T (tbind (tget T Mo) (tabs Sigma' T (treturn (tpair (tvar Sigma) (tvar Sigma')) (MStateT T LRead Mo)))))) f
                     ==  tbind (tget T Mo) (tabs Sigma T (tbind (tapp (tabs Sigma T (tbind (tget T Mo) (tabs Sigma' T (treturn (tpair (tvar Sigma) (tvar Sigma')) (MStateT T LRead Mo))))) (tvar Sigma)) f)) 
                     @TMonadic (MStateT T LRead Mo) T'); prover. 

    assert (empty ||> tbind (tget T Mo) (tabs Sigma T (tbind (tapp (tabs Sigma T (tbind (tget T Mo) (tabs Sigma' T (treturn (tpair (tvar Sigma) (tvar Sigma')) (MStateT T LRead Mo))))) (tvar Sigma)) f))
                  ==  tbind (tget T Mo) (tabs Sigma T (tbind ([Sigma:=tvar Sigma]tbind (tget T Mo) (tabs Sigma' T (treturn (tpair (tvar Sigma) (tvar Sigma')) (MStateT T LRead Mo)))) f))
                  @TMonadic (MStateT T LRead Mo) T'); prover; trans; push_subst_hyp.    

    assert (empty ||> tbind (tget T Mo) (tabs Sigma T (tbind (tbind (tget T Mo) (tabs Sigma' T (treturn (tpair (tvar Sigma) (tvar Sigma')) (MStateT T LRead Mo)))) f))
                  ==  tbind (tget T Mo) (tabs Sigma T (tbind (tget T Mo) (tabs Sigma' T (tbind (tapp (tabs Sigma' T (treturn (tpair (tvar Sigma) (tvar Sigma')) (MStateT T LRead Mo))) (tvar Sigma')) f))))
                  @TMonadic (MStateT T LRead Mo) T'); prover; trans.

    assert (empty ||> tbind (tget T Mo) (tabs Sigma T (tbind (tget T Mo) (tabs Sigma' T (tbind (tapp (tabs Sigma' T (treturn (tpair (tvar Sigma) (tvar Sigma')) (MStateT T LRead Mo))) (tvar Sigma')) f))))
                  == tbind (tget T Mo) (tabs Sigma T (tbind (tget T Mo) (tabs Sigma' T (tbind ([Sigma':=tvar Sigma']treturn (tpair (tvar Sigma) (tvar Sigma')) (MStateT T LRead Mo)) f))))
                 @TMonadic (MStateT T LRead Mo) T'); prover; trans.

    assert (empty ||> tbind (tget T Mo) (tabs Sigma T (tbind (tget T Mo) (tabs Sigma' T (tbind (treturn (tpair (tvar Sigma) (tvar Sigma')) (MStateT T LRead Mo)) f))))
                  ==  tbind (tget T Mo) (tabs Sigma T (tbind (tget T Mo) (tabs Sigma' T (tapp f (tpair (tvar Sigma) (tvar Sigma'))))))    
                            @TMonadic (MStateT T LRead Mo) T'); prover; trans.
  Qed. 

  Lemma get_idemp_right_side : forall f T T' Mo, 
        empty |-- f \in TArrow (TProd T T) (TMonadic (MStateT T LRead Mo) T') ->
        empty ||> tbind (tbind (tget T Mo) (tabs Sigma T (tbind (tget T Mo) (tabs Sigma' T (treturn (tpair (tvar Sigma) (tvar Sigma)) (MStateT T LRead Mo)))))) f
              ==  tbind (tget T Mo) (tabs Sigma T (tbind (tget T Mo) (tabs Sigma' T (tapp f (tpair (tvar Sigma) (tvar Sigma)))))) @TMonadic (MStateT T LRead Mo) T'.
  Proof.
  intros. 

  assert (empty ||> tbind (tbind (tget T Mo) (tabs Sigma T (tbind (tget T Mo) (tabs Sigma' T (treturn (tpair (tvar Sigma) (tvar Sigma)) (MStateT T LRead Mo)))))) f
                == tbind (tget T Mo) (tabs Sigma T (tbind (tapp (tabs Sigma T (tbind (tget T Mo) (tabs Sigma' T (treturn (tpair (tvar Sigma) (tvar Sigma)) (MStateT T LRead Mo))))) (tvar Sigma)) f))
               @TMonadic (MStateT T LRead Mo) T'); prover.

  assert (empty ||> tbind (tget T Mo) (tabs Sigma T (tbind (tapp (tabs Sigma T (tbind (tget T Mo) (tabs Sigma' T (treturn (tpair (tvar Sigma) (tvar Sigma)) (MStateT T LRead Mo))))) (tvar Sigma)) f))
                ==  tbind (tget T Mo) (tabs Sigma T (tbind ([Sigma:=tvar Sigma]tbind (tget T Mo) (tabs Sigma' T (treturn (tpair (tvar Sigma) (tvar Sigma)) (MStateT T LRead Mo)))) f))
                @TMonadic (MStateT T LRead Mo) T'); prover; trans; push_subst_hyp.              

  assert (empty ||> tbind (tget T Mo) (tabs Sigma T (tbind (tbind (tget T Mo) (tabs Sigma' T (treturn (tpair (tvar Sigma) (tvar Sigma)) (MStateT T LRead Mo)))) f))
                ==  tbind (tget T Mo) (tabs Sigma T (tbind (tget T Mo) (tabs Sigma' T (tbind (tapp (tabs Sigma' T (treturn (tpair (tvar Sigma) (tvar Sigma)) (MStateT T LRead Mo))) (tvar Sigma')) f))))
                @TMonadic (MStateT T LRead Mo) T'); prover; trans.              

  assert (empty ||> tbind (tget T Mo) (tabs Sigma T (tbind (tget T Mo) (tabs Sigma' T (tbind (tapp (tabs Sigma' T (treturn (tpair (tvar Sigma) (tvar Sigma)) (MStateT T LRead Mo))) (tvar Sigma')) f))))
                ==  tbind (tget T Mo) (tabs Sigma T (tbind (tget T Mo) (tabs Sigma' T (tbind ([Sigma':=tvar Sigma']treturn (tpair (tvar Sigma) (tvar Sigma)) (MStateT T LRead Mo)) f))))
                @TMonadic (MStateT T LRead Mo) T'); prover; trans; push_subst_hyp.

  assert (empty ||> tbind (tget T Mo) (tabs Sigma T (tbind (tget T Mo) (tabs Sigma' T (tbind (treturn (tpair (tvar Sigma) (tvar Sigma)) (MStateT T LRead Mo)) f))))
                ==  tbind (tget T Mo) (tabs Sigma T (tbind (tget T Mo) (tabs Sigma' T (tapp f (tpair (tvar Sigma) (tvar Sigma))))))  
                @TMonadic (MStateT T LRead Mo) T'); prover; trans.
Qed.

  Ltac inversion_appear := match goal with
       | [H: appears_in ?s ?t |- _] => inversion H; clear H
       end.
  Ltac disappear := repeat inversion_appear.

  Lemma get2tabs : forall f T T' Mo,
          empty |-- f \in TArrow (TProd T T) (TMonadic (MStateT T LRead Mo) T') ->
          empty ||> tbind (tbind (tget T Mo) (tabs Sigma T (treturn (tpair (tvar Sigma) (tvar Sigma)) (MStateT T LRead Mo)))) f
                ==  tbind (tbind (tget T Mo) (tabs Sigma T (tbind (tget T Mo) (tabs Sigma' T (treturn (tpair (tvar Sigma) (tvar Sigma)) (MStateT T LRead Mo)))))) f 
                @TMonadic (MStateT T LRead Mo) T'.
  Proof. intros.

  Admitted.
         
  Lemma get2tabs' : forall f T T' Mo, 
          empty |-- f \in TArrow (TProd T T) (TMonadic (MStateT T LRead Mo) T') ->
          empty ||> tbind (tbind (tget T Mo) (tabs Sigma T (treturn (tpair (tvar Sigma) (tvar Sigma)) (MStateT T LRead Mo)))) f
                ==  tbind (tbind (tget T Mo) (tabs Sigma T (tbind (tget T Mo) (tabs Sigma' T (treturn (tpair (tvar Sigma) (tvar Sigma)) (MStateT T LRead Mo)))))) f 
                @TMonadic (MStateT T LRead Mo) T'.
  Proof. Admitted.

  Lemma get_idemp : forall f T T' Mo,
           empty |-- f \in TArrow (TProd T T) (TMonadic (MStateT T LRead Mo) T') ->   
           empty ||> tbind (tget T Mo) (tabs Sigma T (tbind (tget T Mo) (tabs Sigma' T (tapp f (tpair (tvar Sigma) (tvar Sigma'))))))
                  == tbind (tget T Mo) (tabs Sigma T (tbind (tget T Mo) (tabs Sigma' T (tapp f (tpair (tvar Sigma) (tvar Sigma)))))) 
                  @TMonadic (MStateT T LRead Mo) T'.
  Proof.
  intros.
  pose proof STgetget empty Sigma Sigma' Sigma T Mo. right_mono f T'; auto_type.
  pose proof get_idemp_right_side f T T' Mo H.
  pose proof get_idemp_left_side f T T' Mo H. apply ty_eq_sym in H0. trans.
  pose proof get2tabs f T T' Mo H. trans. apply ty_eq_sym in H3. trans.
  Qed.

  Ltac state_type := match goal with
               | [|-?gamma |-- tupdate _ _ \in _] => eapply typable_empty_everywhere; eapply update_typecheck; auto_type
               | [|-?gamma |-- televate _ _ \in _] => econstructor; try (eapply less_perm_LRW); auto_type 
           end.

  Ltac auto_state_type := repeat state_type.

  Theorem televate_getloc : forall gamma Mo, extend gamma X W8 |-- televate (tapp (getloc Mo) (tvar X)) (MStateT State LReadWrite Mo) \in (TMonadic (MStateT State LReadWrite Mo) Val).
  intros. unfold getloc.
  econstructor. eapply less_perm_LRW. auto_type. 
  Qed.

  Ltac auto_type' := repeat match goal with
                  |[|- ?gamma |-- tvar ?v \in ?TM] => auto_type
                  |[|- ?gamma |-- tpair ?t ?t2 \in ?TM] => econstructor
                  |[|- ?gamma |-- tabs ?v ?T ?t \in ?TM] => econstructor
                  |[|- ?gamma |-- tapp ?t1 ?t2 \in ?TM] => econstructor
                  |[|- ?gamma |-- tbind ?t1 ?t2 \in ?TM] => econstructor
                  |[|- ?gamma |-- treturn ?t ?M \in ?TM] => econstructor
                  |[|- ?gamma |-- tlift ?t ?T \in ?TM] => econstructor
                  |[|- ?gamma |-- televate (getloc ?Mo) ?M \in ?T] => eapply televate_getloc
                  |[|- ?gamma |-- televate ?t ?M \in ?TM] => econstructor; try (eapply less_perm_LRW)
                  |[|- ?gamma |-- tupdate _ _ \in ?TM] => eapply typable_empty_everywhere; eapply update_typecheck
                  |[|- ?gamma |-- tget ?T ?M \in ?TM] => econstructor
                  |[|- ?gamma |-- tcase _ _ _ \in ?TM] => econstructor
                  |[|- _] => auto_type
                   end.


  Lemma get_update_left_side : forall f T T' delta Mo,
           empty |-- delta \in TArrow T T ->
           empty |-- f \in TArrow (TProd T T) (TMonadic (MStateT T LReadWrite Mo) T') ->   
           empty ||> tbind (tbind (televate (tget T Mo) (MStateT T LReadWrite Mo)) 
                           (tabs Sigma T (tbind (tapp (tupdate Mo T) delta) 
                              (tabs Z TNil (tbind (televate (tget T Mo) (MStateT T LReadWrite Mo)) (tabs Sigma' T (treturn (tpair (tvar Sigma) (tvar Sigma')) (MStateT T LReadWrite Mo)))))))) f
                 ==  tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
                            (tabs Sigma T (tbind (tapp (tupdate Mo T) delta) (tabs Z TNil (tbind (televate (tget T Mo) (MStateT T LReadWrite Mo)) (tabs Sigma' T (tapp f (tpair (tvar Sigma) (tvar Sigma')))))))) 
                 @TMonadic (MStateT T LReadWrite Mo) T'.
  Proof.
  intros. 
 
  assert (empty ||> tbind (tbind (televate (tget T Mo) (MStateT T LReadWrite Mo)) (tabs Sigma T (tbind (tapp (tupdate Mo T) delta) (tabs Z TNil (tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
                                                                                                                                                       (tabs Sigma' T (treturn (tpair (tvar Sigma) (tvar Sigma')) (MStateT T LReadWrite Mo)))))))) f
                    ==  tbind (televate (tget T Mo) (MStateT T LReadWrite Mo)) 
                              (tabs Sigma T (tbind (tapp (tabs Sigma T (tbind (tapp (tupdate Mo T) delta) (tabs Z TNil (tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
                                                                                                                              (tabs Sigma' T (treturn (tpair (tvar Sigma) (tvar Sigma')) (MStateT T LReadWrite Mo)))))))
                                                        (tvar Sigma)) f))
                              @TMonadic (MStateT T LReadWrite Mo) T'). eapply monad_assoc; auto_type'.

  assert (empty ||> tbind (televate (tget T Mo) (MStateT T LReadWrite Mo)) 
                (tabs Sigma T (tbind (tapp (tabs Sigma T (tbind (tapp (tupdate Mo T) delta) (tabs Z TNil (tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
                                                                                                                (tabs Sigma' T (treturn (tpair (tvar Sigma) (tvar Sigma')) (MStateT T LReadWrite Mo)))))))
                                                        (tvar Sigma)) f))
                ==  tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
                          (tabs Sigma T (tbind ([Sigma:=tvar Sigma]tbind (tapp (tupdate Mo T) delta) (tabs Z TNil (tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
                                                                                                                         (tabs Sigma' T (treturn (tpair (tvar Sigma) (tvar Sigma')) (MStateT T LReadWrite Mo)))))) f))
                @TMonadic (MStateT T LReadWrite Mo) T'). 
         eapply ty_eq_left_mono; try push_subst; auto_type'.

  eapply ty_eq_abs_cong. eapply ty_eq_right_mono; auto_type'.

   match goal with [ |- ?gamma ||> ?t == ?t1 @?T] => assert (gamma ||> t == [Sigma:=tvar Sigma]t1 @ T) end; try eapply ty_eq_beta_red; auto_type'.
        push_one_subst_hyp; auto. push_one_subst_hyp; auto. push_one_subst_hyp; auto. push_one_subst_hyp; auto. push_one_subst_hyp; auto. push_one_subst_hyp; auto. push_one_subst_hyp; auto.
         push_one_subst_hyp; auto. push_one_subst_hyp; auto. push_one_subst_hyp; auto. push_one_subst_hyp; auto. push_one_subst_hyp; auto_type'. push_one_subst_hyp; auto.

   trans.  push_one_subst_hyp. push_one_subst_hyp; try (inversion e).  push_one_subst_hyp. push_one_subst_hyp; try (inversion e). push_one_subst_hyp.
 push_one_subst_hyp. push_one_subst_hyp. push_one_subst_hyp. push_one_subst_hyp. push_one_subst_hyp. push_one_subst_hyp. push_one_subst_hyp; try state_type. push_one_subst_hyp.

  assert (empty ||> tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
                          (tabs Sigma T (tbind (tbind (tapp (tupdate Mo T) delta) (tabs Z TNil (tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
                                                                                                                         (tabs Sigma' T (treturn (tpair (tvar Sigma) (tvar Sigma')) (MStateT T LReadWrite Mo)))))) f))
                ==  tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
                          (tabs Sigma T (tbind (tapp (tupdate Mo T) delta) (tabs Z TNil (tbind (tapp (tabs Z TNil (tbind (televate (tget T Mo) (MStateT T LReadWrite Mo)) 
                                                                                                                         (tabs Sigma' T (treturn (tpair (tvar Sigma) (tvar Sigma')) (MStateT T LReadWrite Mo))))) (tvar Z)) f))))
                @TMonadic (MStateT T LReadWrite Mo) T'). 

         eapply ty_eq_left_mono; auto_type'.

  eapply ty_eq_abs_cong.
         eapply monad_assoc; auto_type'.

  trans. 

  assert (empty ||>  tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
                          (tabs Sigma T (tbind (tapp (tupdate Mo T) delta) (tabs Z TNil (tbind (tapp (tabs Z TNil (tbind (televate (tget T Mo) (MStateT T LReadWrite Mo)) 
                                                                                                                         (tabs Sigma' T (treturn (tpair (tvar Sigma) (tvar Sigma')) (MStateT T LReadWrite Mo))))) (tvar Z)) f))))
                ==  tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
                          (tabs Sigma T (tbind (tapp (tupdate Mo T) delta) (tabs Z TNil (tbind ([Z:=tvar Z]tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
                                                                                                                         (tabs Sigma' T (treturn (tpair (tvar Sigma) (tvar Sigma')) (MStateT T LReadWrite Mo)))) f))))
                @TMonadic (MStateT T LReadWrite Mo) T').
         eapply ty_eq_left_mono; try push_subst; auto_type'. eapply ty_eq_abs_cong.
        eapply ty_eq_left_mono; auto_type'. eapply ty_eq_abs_cong.
        eapply ty_eq_right_mono; auto_type'. match goal with [|- ?gamma ||> tapp ?t ?t' == ?t1 @?T] => assert (gamma ||> tapp t t' == [Z:=tvar Z]t1 @T) end.        eapply ty_eq_beta_red; auto_type'.  push_one_subst_hyp; auto.
  
   trans.  push_one_subst_hyp. push_one_subst_hyp; try (inversion e). push_one_subst_hyp. push_one_subst_hyp. push_one_subst_hyp. push_one_subst_hyp. push_one_subst_hyp.
                       push_one_subst_hyp.

   assert (empty ||> tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
                          (tabs Sigma T (tbind (tapp (tupdate Mo T) delta) (tabs Z TNil (tbind (tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
                                                                                                                         (tabs Sigma' T (treturn (tpair (tvar Sigma) (tvar Sigma')) (MStateT T LReadWrite Mo)))) f))))
                 ==  tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
                          (tabs Sigma T (tbind (tapp (tupdate Mo T) delta)
                              (tabs Z TNil (tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
                                  (tabs Sigma' T (tbind (tapp (tabs Sigma' T (treturn (tpair (tvar Sigma) (tvar Sigma')) (MStateT T LReadWrite Mo))) (tvar Sigma')) f))))))
                 @TMonadic (MStateT T LReadWrite Mo) T').
        eapply ty_eq_left_mono; auto_type'; try eapply ty_eq_abs_cong. try eapply ty_eq_left_mono; auto_type'; try eapply ty_eq_abs_cong; try eapply monad_assoc; auto_type'.

  trans.
  assert (empty ||> tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
                          (tabs Sigma T (tbind (tapp (tupdate Mo T) delta)
                              (tabs Z TNil (tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
                                  (tabs Sigma' T (tbind (tapp (tabs Sigma' T (treturn (tpair (tvar Sigma) (tvar Sigma')) (MStateT T LReadWrite Mo))) (tvar Sigma')) f))))))
                ==tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
                          (tabs Sigma T (tbind (tapp (tupdate Mo T) delta)
                              (tabs Z TNil (tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
                                  (tabs Sigma' T (tbind ([Sigma':=tvar Sigma'](treturn (tpair (tvar Sigma) (tvar Sigma')) (MStateT T LReadWrite Mo))) f))))))
                @TMonadic (MStateT T LReadWrite Mo) T').
eapply ty_eq_left_mono; auto_type'. try eapply ty_eq_abs_cong; try eapply ty_eq_left_mono; auto_type'; try eapply ty_eq_abs_cong; try eapply ty_eq_left_mono; auto_type'; 
               try eapply ty_eq_abs_cong; try eapply ty_eq_right_mono; auto_type'; try eapply ty_eq_beta_red; auto_type'.

  trans. push_subst_hyp.

  assert (empty ||> tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
                          (tabs Sigma T (tbind (tapp (tupdate Mo T) delta)
                              (tabs Z TNil (tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
                                  (tabs Sigma' T (tbind (treturn (tpair (tvar Sigma) (tvar Sigma')) (MStateT T LReadWrite Mo)) f))))))
                ==  tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
                          (tabs Sigma T (tbind (tapp (tupdate Mo T) delta)
                              (tabs Z TNil (tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
                                  (tabs Sigma' T (tapp f (tpair (tvar Sigma) (tvar Sigma'))))))))
                @TMonadic (MStateT T LReadWrite Mo) T').
        eapply ty_eq_left_mono; auto_type'; try eapply ty_eq_abs_cong; try eapply ty_eq_left_mono; auto_type'; try eapply ty_eq_abs_cong; try eapply ty_eq_left_mono; auto_type'; 
        try eapply ty_eq_abs_cong; try eapply ty_eq_left_mono; auto_type'; try eapply ty_eq_abs_cong; try eapply monad_lunit; auto_type'.

  trans.
Qed.

  Lemma get_update_right_side : forall f T T' delta Mo,
           empty |-- delta \in TArrow T T ->
           empty |-- f \in TArrow (TProd T T) (TMonadic (MStateT T LReadWrite Mo) T') ->   
           empty ||> tbind (tbind (televate (tget T Mo) (MStateT T LReadWrite Mo)) 
                           (tabs Sigma T (tbind (tapp (tupdate Mo T) delta) (tabs Z TNil (treturn (tpair (tvar Sigma) (tapp delta (tvar Sigma))) (MStateT T LReadWrite Mo)))))) f
                 ==  tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
                            (tabs Sigma T (tbind (tapp (tupdate Mo T) delta) (tabs Z TNil (tapp f (tpair (tvar Sigma) (tapp delta (tvar Sigma)))))))
                 @TMonadic (MStateT T LReadWrite Mo) T'.

 Proof.
 intros.

 assert (empty ||> tbind (tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
                         (tabs Sigma T (tbind (tapp (tupdate Mo T) delta) (tabs Z TNil (treturn (tpair (tvar Sigma) (tapp delta (tvar Sigma))) (MStateT T LReadWrite Mo)))))) f
               ==  tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
                         (tabs Sigma T (tbind (tapp (tabs Sigma T (tbind (tapp (tupdate Mo T) delta) (tabs Z TNil (treturn (tpair (tvar Sigma) (tapp delta (tvar Sigma))) (MStateT T LReadWrite Mo))))) (tvar Sigma)) f))                      
               @TMonadic (MStateT T LReadWrite Mo) T');

        try eapply monad_assoc; auto_type';
  assert (empty ||> tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
               (tabs Sigma T (tbind (tapp (tabs Sigma T (tbind (tapp (tupdate Mo T) delta) (tabs Z TNil (treturn (tpair (tvar Sigma) (tapp delta (tvar Sigma))) (MStateT T LReadWrite Mo))))) (tvar Sigma)) f))
               ==  tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
                         (tabs Sigma T (tbind ([Sigma:= tvar Sigma]tbind (tapp (tupdate Mo T) delta) (tabs Z TNil (treturn (tpair (tvar Sigma) (tapp delta (tvar Sigma))) (MStateT T LReadWrite Mo)))) f))
               @TMonadic (MStateT T LReadWrite Mo) T');
         try eapply ty_eq_left_mono; try push_subst; auto_type';

   try eapply ty_eq_abs_cong;
        try eapply ty_eq_right_mono; auto_type';

  try match goal with [|- ?g ||> tapp ?t ?t' == ?t1 @ ?T] => assert (g ||> tapp t t' == [Sigma:=tvar Sigma]t1 @ T) end; try eapply ty_eq_beta_red; auto_type'; push_one_subst_hyp; push_one_subst_hyp;
             push_one_subst_hyp; push_one_subst_hyp; push_one_subst_hyp; push_one_subst_hyp;  push_one_subst_hyp; push_one_subst_hyp; push_one_subst_hyp; push_one_subst_hyp; auto_type'.

  trans.

  assert (empty ||> tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
                 (tabs Sigma T (tbind (tbind (tapp (tupdate Mo T) delta) (tabs Z TNil (treturn (tpair (tvar Sigma) (tapp delta (tvar Sigma))) (MStateT T LReadWrite Mo)))) f))
                 == tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
                          (tabs Sigma T (tbind (tapp (tupdate Mo T) delta) (tabs Z TNil (tbind (tapp (tabs Z TNil (treturn (tpair (tvar Sigma) (tapp delta (tvar Sigma))) (MStateT T LReadWrite Mo))) (tvar Z)) f))))
                 @TMonadic (MStateT T LReadWrite Mo) T');
           try eapply ty_eq_left_mono; auto_type'; try eapply ty_eq_abs_cong; try eapply monad_assoc; auto_type'.

  trans. 

  assert (empty ||> tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
                          (tabs Sigma T (tbind (tapp (tupdate Mo T) delta) (tabs Z TNil (tbind (tapp (tabs Z TNil (treturn (tpair (tvar Sigma) (tapp delta (tvar Sigma))) (MStateT T LReadWrite Mo))) (tvar Z)) f))))        
                ==  tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
                          (tabs Sigma T (tbind (tapp (tupdate Mo T) delta) (tabs Z TNil (tbind ([Z:=tvar Z]treturn (tpair (tvar Sigma) (tapp delta (tvar Sigma))) (MStateT T LReadWrite Mo)) f))))
                          @TMonadic (MStateT T LReadWrite Mo) T').
         eapply ty_eq_left_mono; try push_subst ; auto_type'; try eapply ty_eq_abs_cong; try eapply ty_eq_left_mono; auto_type'; try eapply ty_eq_abs_cong; try eapply ty_eq_right_mono; auto_type'.
        match goal with [|-?g ||> ?t == ?t1 @ ?T] => assert (g ||> t == [Z:=tvar Z]t1 @ T) end; try eapply ty_eq_beta_red; auto_type'.  push_one_subst_hyp; push_one_subst_hyp. push_one_subst_hyp; push_one_subst_hyp.
           push_one_subst_hyp; push_one_subst_hyp. push_one_subst_hyp; push_one_subst_hyp.  push_one_subst_hyp; push_one_subst_hyp. push_one_subst_hyp; push_one_subst_hyp.

  trans. 

  assert (empty ||> tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
                          (tabs Sigma T (tbind (tapp (tupdate Mo T) delta) (tabs Z TNil (tbind (treturn (tpair (tvar Sigma) (tapp delta (tvar Sigma))) (MStateT T LReadWrite Mo)) f))))
                ==  tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
                          (tabs Sigma T (tbind (tapp (tupdate Mo T) delta) (tabs Z TNil (tapp f (tpair (tvar Sigma) (tapp delta (tvar Sigma)))))))
                @TMonadic (MStateT T LReadWrite Mo) T').
         eapply ty_eq_left_mono; auto_type'; try eapply ty_eq_abs_cong; try eapply ty_eq_left_mono; auto_type'; try eapply ty_eq_abs_cong; try eapply monad_lunit; auto_type'.

  trans.
Qed.

  Lemma get_update : forall f delta T T' Mo,
           empty |-- f \in TArrow (TProd T T) (TMonadic (MStateT T LReadWrite Mo) T') ->
           empty |-- delta \in TArrow T T ->
           empty ||> tbind (televate (tget T Mo) (MStateT T LReadWrite Mo)) (tabs Sigma T (tbind (tapp (tupdate Mo T) delta) 
                           (tabs Z TNil (tbind (televate (tget T Mo) (MStateT T LReadWrite Mo)) (tabs Sigma' T (tapp f (tpair (tvar Sigma) (tvar Sigma'))))))))
                 == tbind (televate (tget T Mo) (MStateT T LReadWrite Mo)) (tabs Sigma T (tbind (tapp (tupdate Mo T) delta) (tabs Z TNil (tapp f (tpair (tvar Sigma) (tapp delta (tvar Sigma))))))) 
                 @TMonadic (MStateT T LReadWrite Mo) T'.

   Proof.
   intros.

   assert (empty ||> tbind (tbind (televate (tget T Mo) (MStateT T LReadWrite Mo)) (tabs Sigma T (tbind (tapp (tupdate Mo T) delta) (tabs Z TNil (tbind (televate (tget T Mo) (MStateT T LReadWrite Mo))
                            (tabs Sigma' T (treturn (tpair (tvar Sigma) (tvar Sigma')) (MStateT T LReadWrite Mo)))))))) f
                 ==  tbind (tbind (televate (tget T Mo) (MStateT T LReadWrite Mo)) (tabs Sigma T (tbind (tapp (tupdate Mo T) delta) (tabs Z TNil (treturn (tpair (tvar Sigma) (tapp delta (tvar Sigma))) (MStateT T LReadWrite Mo)))))) f
                 @TMonadic (MStateT T LReadWrite Mo) T'). eapply ty_eq_right_mono; try (eapply STgetupget); auto_type'. 

   assert (empty ||> tbind (televate (tget T Mo) (MStateT T LReadWrite Mo)) (tabs Sigma T (tbind (tapp (tupdate Mo T) delta) (tabs Z TNil (tbind (televate (tget T Mo) (MStateT T LReadWrite Mo)) (tabs Sigma' T (tapp f (tpair (tvar Sigma) (tvar Sigma'))))))))
                 ==  tbind (tbind (televate (tget T Mo) (MStateT T LReadWrite Mo)) (tabs Sigma T (tbind (tapp (tupdate Mo T) delta) (tabs Z TNil (tbind (televate (tget T Mo) (MStateT T LReadWrite Mo)) 
                           (tabs Sigma' T (treturn (tpair (tvar Sigma) (tvar Sigma')) (MStateT T LReadWrite Mo)))))))) f
                 @TMonadic (MStateT T LReadWrite Mo) T'). eapply ty_eq_sym. apply get_update_left_side; auto_type.

   assert (empty ||> tbind (televate (tget T Mo) (MStateT T LReadWrite Mo)) (tabs Sigma T (tbind (tapp (tupdate Mo T) delta) (tabs Z TNil (tapp f (tpair (tvar Sigma) (tapp delta (tvar Sigma)))))))
                 ==  tbind (tbind (televate (tget T Mo) (MStateT T LReadWrite Mo)) (tabs Sigma T (tbind (tapp (tupdate Mo T) delta) (tabs Z TNil (treturn (tpair (tvar Sigma) (tapp delta (tvar Sigma))) (MStateT T LReadWrite Mo)))))) f
                 @TMonadic (MStateT T LReadWrite Mo) T'). eapply ty_eq_sym. apply get_update_right_side; auto_type.
  trans. eapply ty_eq_sym in H3. trans.
Qed.

  Lemma get_up_tweak_LS : forall  Mo,
                    extend (extend (extend empty X W8) V Val) Z TNil ||> tbind (tapp (tupdate Mo State) (tapp (tapp tweak (tvar X)) (tvar V))) (tabs Z TNil (televate (tapp (getloc Mo) (tvar X)) (MStateT State LReadWrite Mo)))
                                                                     ==  tbind (televate (tget State Mo) (MStateT State LReadWrite Mo)) (tabs S State (tbind 
                                                                                                                                           (televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo)) 
                                                                                                                                           (tabs Z TNil (televate (treturn (tvar V) (MStateT State LRead Mo)) (MStateT State LReadWrite Mo)))))
                                                                     @TMonadic (MStateT State LReadWrite Mo) Val. 
  Proof.
  intros. unfold tupdate. 
  assert (Hnv: (@empty ty) S = None). auto.
   
  assert ((extend (extend (extend empty X W8) V Val) Z TNil)||> tbind (tapp (tabs F (TArrow State State) (tbind (televate (tget State Mo) (MStateT State LReadWrite Mo)) 
                                                                                                                (tabs S State (televate (tput (tapp (tvar F) (tvar S)) State Mo) (MStateT State LReadWrite Mo))))) 
                                                                            (tapp (tapp tweak (tvar X)) (tvar V))) 
                                                                      (tabs Z TNil (televate (tapp (getloc Mo) (tvar X)) (MStateT State LReadWrite Mo)))
                                                            ==  tbind ([F:=tapp (tapp tweak (tvar X)) (tvar V)]tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                                                                     (tabs S State (televate (tput (tapp (tvar F) (tvar S)) State Mo) (MStateT State LReadWrite Mo))))
                                                                      (tabs Z TNil (televate (tapp (getloc Mo) (tvar X)) (MStateT State LReadWrite Mo)))
                                                            @TMonadic (MStateT State LReadWrite Mo) Val). eapply ty_eq_right_mono. auto_type'. push_subst.
                                                                 auto_type'. auto_type'. eapply ty_eq_beta_red; auto_type'. push_subst_hyp.

  assert ((extend (extend (extend empty X W8) V Val) Z TNil)||> tbind (tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                             (tabs S State (televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo))))
                                                                      (tabs Z TNil (televate (tapp (getloc Mo) (tvar X)) (MStateT State LReadWrite Mo)))
                                                            ==  tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                      (tabs S State (tbind (tapp (tabs S State (televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo)))
                                                                                                 (tvar S))
                                                                                           (tabs Z TNil (televate (tapp (getloc Mo) (tvar X)) (MStateT State LReadWrite Mo)))))
                                                            @TMonadic (MStateT State LReadWrite Mo) Val); try (eapply monad_assoc); auto_type'.
                                       

  trans. 

  assert ((extend (extend (extend empty X W8) V Val) Z TNil)||> tbind (televate (tget State Mo) (MStateT State LReadWrite Mo)) 
                                                                      (tabs S State (tbind (tapp (tabs S State (televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo)))
                                                                                                 (tvar S))
                                                                                           (tabs Z TNil (televate (tapp (getloc Mo) (tvar X)) (MStateT State LReadWrite Mo)))))
                                                            ==  tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                      (tabs S State (tbind ([S:= tvar S]televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo))
                                                                                           (tabs Z TNil (televate (tapp (getloc Mo) (tvar X)) (MStateT State LReadWrite Mo)))))
                                                            @TMonadic (MStateT State LReadWrite Mo) Val). eapply ty_eq_left_mono. auto_type'. auto_type'. push_one_subst. push_one_subst. push_one_subst. push_one_subst.
                           push_one_subst. push_one_subst. push_subst. auto_type'. eapply tweak_has_type. 
                                          unfold not; try intros; try inversion H.
                           eapply ty_eq_abs_cong; auto_type'. eapply ty_eq_right_mono. auto_type'. push_subst. auto_type'. eapply tweak_has_type. unfold not. intros. inversion H. auto_type'.
                           eapply ty_eq_beta_red; auto_type'.
          trans.   push_one_subst_hyp. push_one_subst_hyp. push_one_subst_hyp. push_one_subst_hyp. push_one_subst_hyp. push_one_subst_hyp. push_one_subst_hyp. push_one_subst_hyp. Focus 2. unfold not; intros; inversion H. push_one_subst_hyp; try eapply tweak_has_type.
    unfold getloc in H0.
  assert (extend (extend (extend empty X W8) V Val) Z TNil ||> tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                     (tabs S State (tbind (televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo))
                                                                                          (tabs Z TNil (televate (tapp (tabs X W8 (tbind (tget State Mo) (tabs Y State (treturn (tapp (tvar Y) (tvar X)) (MStateT State LRead Mo))))) (tvar X)) (MStateT State LReadWrite Mo)))))
                                                           ==  tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                     (tabs S State (tbind (televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo))
                                                                                          (tabs Z TNil (televate ([X:= tvar X]tbind (tget State Mo) (tabs Y State (treturn (tapp (tvar Y) (tvar X)) (MStateT State LRead Mo)))) (MStateT State LReadWrite Mo)))))
                                                           @TMonadic (MStateT State LReadWrite Mo) Val).
                    eapply ty_eq_left_mono; auto_type'.
                    eapply ty_eq_abs_cong; auto_type'.
                    eapply ty_eq_left_mono; auto_type'.
                    eapply ty_eq_abs_cong; auto_type'.
                    eapply ty_eq_televate. eapply less_perm_LRW.
                    eapply ty_eq_beta_red; auto_type'. push_subst_hyp. trans. 

  assert (extend (extend (extend empty X W8) V Val) Z TNil ||> tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                     (tabs S State (tbind (televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo))
                                                                                          (tabs Z TNil (televate (tbind (tget State Mo) (tabs Y State (treturn (tapp (tvar Y) (tvar X)) (MStateT State LRead Mo)))) (MStateT State LReadWrite Mo)))))
                                                           ==  tbind (televate (tget State Mo) (MStateT State LReadWrite Mo)) 
                                                                    (tabs S State (tbind (televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo))
                                                                                          (tabs Z TNil (tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                                                              (tabs Y State (televate (treturn (tapp (tvar Y) (tvar X)) (MStateT State LRead Mo))
                                                                                                              (MStateT State LReadWrite Mo)))))))
                                                           @TMonadic (MStateT State LReadWrite Mo) Val). eapply ty_eq_left_mono; auto_type'; eapply ty_eq_abs_cong; auto_type'; eapply ty_eq_left_mono; auto_type';
                                                                     eapply ty_eq_abs_cong; auto_type'; eapply ty_eq_elevate_bind; try (eapply less_perm_LRW); auto_type'.

  trans. 

  assert (Hx : exists x, extend (extend (extend empty X W8) V Val) Z TNil x = None). eapply new_var'. destruct Hx. 
  
  assert (extend (extend (extend empty X W8) V Val) Z TNil ||> tbind (televate (tget State Mo) (MStateT State LReadWrite Mo)) 
                                                                    (tabs S State (tbind (televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo))
                                                                                          (tabs Z TNil (tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                                                              (tabs Y State (televate (treturn (tapp (tvar Y) (tvar X)) (MStateT State LRead Mo))
                                                                                                              (MStateT State LReadWrite Mo)))))))
                                                           ==  tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                     (tabs S State (tbind (televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo))
                                                                                          (tabs Z TNil (tbind (tapp (tabs Z TNil (televate (tget State Mo) (MStateT State LReadWrite Mo))) (tvar Z))
                                                                                                       (tabs Y State (televate (treturn (tapp (tvar Y) (tvar X)) (MStateT State LRead Mo))(MStateT State LReadWrite Mo)))))))
                                                           @TMonadic (MStateT State LReadWrite Mo) Val). eapply ty_eq_left_mono; auto_type'. 
                                                                                                         try (eapply ty_eq_abs_cong); auto_type'.
                                                                                                         try (eapply ty_eq_left_mono); auto_type'.
                                                                                                         try (eapply ty_eq_abs_cong); auto_type'.
                                                                                                         eapply ty_eq_right_mono; auto_type'.
                                                                                                         eapply ty_eq_sym; eapply ty_eq_beta_red; auto_type'.
 trans. 
  
  assert (extend (extend (extend empty X W8) V Val) Z TNil ||> tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                     (tabs S State (tbind (televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo))
                                                                                          (tabs Z TNil (tbind (tapp (tabs Z TNil (televate (tget State Mo) (MStateT State LReadWrite Mo))) (tvar Z))
                                                                                                       (tabs Y State (televate (treturn (tapp (tvar Y) (tvar X)) (MStateT State LRead Mo))(MStateT State LReadWrite Mo)))))))
                                                           ==  tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                     (tabs S State (tbind (tbind (televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo))
                                                                                          (tabs Z TNil (televate (tget State Mo) (MStateT State LReadWrite Mo))))
                                                                                   (tabs Y State (televate (treturn (tapp (tvar Y) (tvar X)) (MStateT State LRead Mo)) (MStateT State LReadWrite Mo)))))
                                                           @TMonadic (MStateT State LReadWrite Mo) Val).
        eapply ty_eq_left_mono; auto_type'. eapply ty_eq_abs_cong; auto_type'.
        eapply ty_eq_permute_neq; auto_type'. unfold not; intros; inversion H0.
        eapply ty_eq_add; auto_type'.
        eapply ty_eq_sym; eapply monad_assoc; auto_type'. 
  trans. 

(*  assert (ty_eq_put_get_ret: forall gamma t T Mo Mo' Mo'',
              gamma |-- t \in T ->
              gamma ||> tbind (televate (tput t T Mo) Mo'') (tabs Z TNil (televate (tget T Mo) Mo'')) == tbind (televate (tput t T Mo) Mo'') (tabs Z TNil (televate (treturn t Mo') Mo'')) @TMonadic Mo'' T). admit. *)


  assert (extend (extend (extend empty X W8) V Val) Z TNil ||> tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                     (tabs S State (tbind (tbind (televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo))
                                                                                          (tabs Z TNil (televate (tget State Mo) (MStateT State LReadWrite Mo))))
                                                                                   (tabs Y State (televate (treturn (tapp (tvar Y) (tvar X)) (MStateT State LRead Mo)) (MStateT State LReadWrite Mo)))))
                                                           ==  tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                     (tabs S State (tbind (tbind (televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo))
                                                                                                 (tabs Z TNil (televate (treturn (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) (MStateT State LRead Mo)) (MStateT State LReadWrite Mo))))
                                                                                          (tabs Y State (televate (treturn (tapp (tvar Y) (tvar X)) (MStateT State LRead Mo)) (MStateT State LReadWrite Mo)))))
                                                           @TMonadic (MStateT State LReadWrite Mo) Val). eapply ty_eq_left_mono; auto_type'. eapply ty_eq_abs_cong; auto_type'.
                                                           eapply ty_eq_right_mono; auto_type'. eapply ty_eq_put_get_ret; auto_type'.

  trans. 

  assert(extend (extend (extend empty X W8) V Val) Z TNil ||> tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                    (tabs S State (tbind (tbind (televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo))
                                                                                                 (tabs Z TNil (televate (treturn (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) (MStateT State LRead Mo)) (MStateT State LReadWrite Mo))))
                                                                                          (tabs Y State (televate (treturn (tapp (tvar Y) (tvar X)) (MStateT State LRead Mo)) (MStateT State LReadWrite Mo)))))
                                                          ==  tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                    (tabs S State (tbind (televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo))
                                                                                         (tabs Z TNil (tbind (tapp (tabs Z TNil (televate (treturn (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) (MStateT State LRead Mo)) (MStateT State LReadWrite Mo))) (tvar Z))
                                                                                                             (tabs Y State (televate (treturn (tapp (tvar Y) (tvar X)) (MStateT State LRead Mo)) (MStateT State LReadWrite Mo)))))))
                                                          @TMonadic (MStateT State LReadWrite Mo) Val).
              eapply ty_eq_left_mono; auto_type'.
              eapply ty_eq_abs_cong; auto_type'. eapply ty_eq_permute_neq. unfold not. intros. inversion H0.
              eapply ty_eq_add; auto_type'.
              eapply monad_assoc; auto_type'.
  trans. 

  assert(extend (extend (extend empty X W8) V Val) Z TNil ||> tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                    (tabs S State (tbind (televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo))
                                                                                         (tabs Z TNil (tbind (tapp (tabs Z TNil (televate (treturn (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) (MStateT State LRead Mo)) (MStateT State LReadWrite Mo))) (tvar Z))
                                                                                                             (tabs Y State (televate (treturn (tapp (tvar Y) (tvar X)) (MStateT State LRead Mo)) (MStateT State LReadWrite Mo)))))))
                                                          ==  tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                    (tabs S State (tbind (televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo))
                                                                                         (tabs Z TNil (tbind ([Z:=tvar Z]televate (treturn (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) (MStateT State LRead Mo)) (MStateT State LReadWrite Mo))
                                                                                                             (tabs Y State (televate (treturn (tapp (tvar Y) (tvar X)) (MStateT State LRead Mo)) (MStateT State LReadWrite Mo)))))))
                                                          @TMonadic (MStateT State LReadWrite Mo)Val).
              eapply ty_eq_left_mono. auto_type'. auto_type'. econstructor. econstructor. econstructor. eapply less_perm_LRW. auto_type'. econstructor. push_subst. auto_type'. auto_type'. unfold not; intros; try inversion H0. unfold not; intros; try inversion H0.
                                     unfold not; intros; try inversion H0.
              eapply ty_eq_abs_cong; auto_type'.
              eapply ty_eq_left_mono; [auto_type' | auto_type'| push_subst; auto_type'; unfold not; intros; try inversion H0 | ] .
              eapply ty_eq_abs_cong; auto_type'.
              eapply ty_eq_right_mono; [auto_type' | push_subst; auto_type'; unfold not; intros; try inversion H0 |auto_type'|eapply ty_eq_beta_red; auto_type' ].
    trans. 
    push_one_subst_hyp; push_one_subst_hyp; push_one_subst_hyp. push_one_subst_hyp; push_one_subst_hyp; push_one_subst_hyp. push_one_subst_hyp. push_one_subst_hyp. push_one_subst_hyp. Focus 2. auto_type'.
                  Focus 2. unfold not; intros; try inversion H0.
                  Focus 2. unfold not; intros; try inversion H0.
                  Focus 2. unfold not; intros; try inversion H0.
                  Focus 2. unfold not; intros; try inversion H0.
                  Focus 2. unfold not; intros; try inversion H0.
        
  assert(extend (extend (extend empty X W8) V Val) Z TNil ||> tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                    (tabs S State (tbind (televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo))
                                                                                         (tabs Z TNil (tbind (televate (treturn (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) (MStateT State LRead Mo)) (MStateT State LReadWrite Mo))
                                                                                                             (tabs Y State (televate (treturn (tapp (tvar Y) (tvar X)) (MStateT State LRead Mo)) (MStateT State LReadWrite Mo)))))))
                                                          ==  tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                    (tabs S State (tbind (televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo))
                                                                                         (tabs Z TNil (tbind (treturn (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) (MStateT State LReadWrite Mo))
                                                                                                             (tabs Y State (televate (treturn (tapp (tvar Y) (tvar X)) (MStateT State LRead Mo)) (MStateT State LReadWrite Mo)))))))
                                                          @TMonadic (MStateT State LReadWrite Mo)Val).
               eapply ty_eq_left_mono; auto_type'.
              eapply ty_eq_abs_cong; auto_type'.
              eapply ty_eq_left_mono; auto_type'.
              eapply ty_eq_abs_cong; auto_type'.
              eapply ty_eq_right_mono; auto_type'.    
              eapply ty_eq_televate_ret; auto_type'. eapply less_permissive_refl.
  trans. 
             
  assert(extend (extend (extend empty X W8) V Val) Z TNil ||> tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                    (tabs S State (tbind (televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo))
                                                                                         (tabs Z TNil (tbind (treturn (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) (MStateT State LReadWrite Mo))
                                                                                                             (tabs Y State (televate (treturn (tapp (tvar Y) (tvar X)) (MStateT State LRead Mo)) (MStateT State LReadWrite Mo)))))))
                                                          ==  tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                    (tabs S State (tbind (televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo))
                                                                                         (tabs Z TNil (tbind (treturn (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) (MStateT State LReadWrite Mo))
                                                                                                             (tabs Y State (treturn (tapp (tvar Y) (tvar X)) (MStateT State LReadWrite Mo)))))))
                                                          @TMonadic (MStateT State LReadWrite Mo)Val).  
              eapply ty_eq_left_mono; auto_type'.
              eapply ty_eq_abs_cong; auto_type'.
              eapply ty_eq_left_mono; auto_type'.
              eapply ty_eq_abs_cong; auto_type'.
              eapply ty_eq_left_mono; auto_type'.
              eapply ty_eq_abs_cong; auto_type'.
              eapply ty_eq_televate_ret; auto_type'. eapply less_permissive_refl.
  
  trans. 

   assert(extend (extend (extend empty X W8) V Val) Z TNil ||> tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                    (tabs S State (tbind (televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo))
                                                                                         (tabs Z TNil (tbind (treturn (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) (MStateT State LReadWrite Mo))
                                                                                                             (tabs Y State (treturn (tapp (tvar Y) (tvar X)) (MStateT State LReadWrite Mo)))))))
                                                           ==  tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                    (tabs S State (tbind (televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo))
                                                                                         (tabs Z TNil (tapp (tabs Y State (treturn (tapp (tvar Y) (tvar X)) (MStateT State LReadWrite Mo)))
                                                                                                            (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S))))))
                                                           @TMonadic (MStateT State LReadWrite Mo)Val).
              eapply ty_eq_left_mono; auto_type'.
              eapply ty_eq_abs_cong; auto_type'.
              eapply ty_eq_left_mono; auto_type'.
              eapply ty_eq_abs_cong; auto_type'.
              eapply monad_lunit; auto_type'.

  trans.     
  
   assert(extend (extend (extend empty X W8) V Val) Z TNil ||> tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                    (tabs S State (tbind (televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo))
                                                                                         (tabs Z TNil (tapp (tabs Y State (treturn (tapp (tvar Y) (tvar X)) (MStateT State LReadWrite Mo)))
                                                                                                            (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S))))))
                                                           ==  tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                    (tabs S State (tbind (televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo))
                                                                                         (tabs Z TNil ([Y:=(tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S))]treturn(tapp (tvar Y) (tvar X)) (MStateT State LReadWrite Mo)))))
                                                           @TMonadic (MStateT State LReadWrite Mo)Val).
             
               eapply ty_eq_left_mono; auto_type'.
              eapply ty_eq_abs_cong; auto_type'.
              eapply ty_eq_left_mono; auto_type'.
              eapply ty_eq_abs_cong; auto_type'.
              eapply ty_eq_beta_red; auto_type'.
  trans.  push_subst_hyp.

  assert(extend (extend (extend empty X W8) V Val) Z TNil ||> tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                    (tabs S State (tbind (televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo))
                                                                                         (tabs Z TNil (treturn (tapp (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) (tvar X)) (MStateT State LReadWrite Mo)))))
                                                          ==  tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                    (tabs S State (tbind (televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo))
                                                                                         (tabs Z TNil (treturn (tvar V) (MStateT State LReadWrite Mo)))))
                                                          @TMonadic (MStateT State LReadWrite Mo) Val).  
              eapply ty_eq_left_mono; auto_type'.
              eapply ty_eq_abs_cong; auto_type'.
              eapply ty_eq_left_mono; auto_type'.
              eapply ty_eq_abs_cong; auto_type'.
             
Print ty_eq_permute_neq.              eapply ty_eq_permute_neq; auto_type'. unfold not; intros; try inversion H0. eapply ty_eq_add; auto_type.
              eapply ty_eq_add_copy; auto_type. eapply ty_eq_add; auto_type'.

 assert (ty_eq_tapp_tweak : forall gamma x v s Val Mo,
                                         extend gamma v Val||> treturn (tapp (tapp (tapp (tapp tweak (tvar x)) (tvar v)) (tvar s)) (tvar x)) Mo
                                               ==  treturn (tvar v) Mo
                                                            @TMonadic Mo Val). eapply ty_eq_tapp_tweak. eapply ty_eq_tapp_tweak.

  trans.  

  assert(extend (extend (extend empty X W8) V Val) Z TNil ||> tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                    (tabs S State (tbind (televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo))
                                                                                         (tabs Z TNil (treturn (tvar V) (MStateT State LReadWrite Mo)))))
                                                          ==  tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                    (tabs S State (tbind (televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo))
                                                                                         (tabs Z TNil (televate (treturn (tvar V) (MStateT State LRead Mo)) (MStateT State LReadWrite Mo)))))
                                                          @TMonadic (MStateT State LReadWrite Mo)Val).
             eapply ty_eq_left_mono; auto_type'.
             eapply ty_eq_abs_cong; auto_type'.
             eapply ty_eq_left_mono; auto_type'.
             eapply ty_eq_abs_cong; auto_type'.
             eapply ty_eq_sym. eapply ty_eq_televate_ret; auto_type'; eapply less_permissive_refl. unfold getloc.
   trans.
Qed.

  Lemma get_up_tweak_RS : forall  Mo,
                   extend (extend (extend empty X W8) V Val) Z TNil ||> tbind (tapp (tupdate Mo State) (tapp (tapp tweak (tvar X)) (tvar V))) 
                                                                              (tabs Z TNil (televate (treturn (tvar V) (MStateT State LRead Mo)) (MStateT State LReadWrite Mo)))
                                                                    ==  tbind (televate (tget State Mo) (MStateT State LReadWrite Mo)) 
                                                                              (tabs S State (tbind (televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo)) 
                                                                                                   (tabs Z TNil (televate (treturn (tvar V) (MStateT State LRead Mo)) (MStateT State LReadWrite Mo)))))
                                                                     @TMonadic (MStateT State LReadWrite Mo) Val. 
  Proof. intros. unfold tupdate.

  assert(extend (extend (extend empty X W8) V Val) Z TNil ||> tbind (tapp (tabs F (TArrow State State) (tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                                                              (tabs S State (televate (tput (tapp (tvar F) (tvar S)) State Mo) (MStateT State LReadWrite Mo)))))
                                                                          (tapp (tapp tweak (tvar X)) (tvar V)))
                                                                    (tabs Z TNil (televate (treturn (tvar V) (MStateT State LRead Mo)) (MStateT State LReadWrite Mo)))
                                                          ==  tbind ([F:=(tapp (tapp tweak (tvar X)) (tvar V))]tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                                                                     (tabs S State (televate (tput (tapp (tvar F) (tvar S)) State Mo) (MStateT State LReadWrite Mo))))
                                                                    (tabs Z TNil (televate (treturn (tvar V) (MStateT State LRead Mo)) (MStateT State LReadWrite Mo)))
                                                          @TMonadic (MStateT State LReadWrite Mo)Val).
                   eapply ty_eq_right_mono; [auto_type' | push_subst; auto_type' | auto_type' | eapply ty_eq_beta_red; auto_type' ].
                   push_subst_hyp.

  assert(extend (extend (extend empty X W8) V Val) Z TNil ||> tbind (tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                           (tabs S State (televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo))))
                                                                    (tabs Z TNil (televate (treturn (tvar V) (MStateT State LRead Mo)) (MStateT State LReadWrite Mo)))
                                                          ==  tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                    (tabs S State (tbind (tapp (tabs S State (televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo))) (tvar S))
                                                                                  (tabs Z TNil (televate (treturn (tvar V) (MStateT State LRead Mo)) (MStateT State LReadWrite Mo)))))
                                                          @TMonadic (MStateT State LReadWrite Mo)Val).
                  eapply monad_assoc; auto_type'.
  trans. 

  assert(extend (extend (extend empty X W8) V Val) Z TNil ||> tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                    (tabs S State (tbind (tapp (tabs S State (televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo))) (tvar S))
                                                                                  (tabs Z TNil (televate (treturn (tvar V) (MStateT State LRead Mo)) (MStateT State LReadWrite Mo)))))
                                                          ==  tbind (televate (tget State Mo) (MStateT State LReadWrite Mo))
                                                                    (tabs S State (tbind ([S:=tvar S]televate (tput (tapp (tapp (tapp tweak (tvar X)) (tvar V)) (tvar S)) State Mo) (MStateT State LReadWrite Mo))
                                                                                         (tabs Z TNil (televate (treturn (tvar V) (MStateT State LRead Mo)) (MStateT State LReadWrite Mo)))))
                                                         @TMonadic (MStateT State LReadWrite Mo)Val).
                eapply ty_eq_left_mono; [auto_type' |auto_type' | push_subst; auto_type'; unfold not; intros; inversion H
                                        |eapply ty_eq_abs_cong; auto_type'; eapply ty_eq_right_mono]. auto_type'. push_subst; auto_type'.
                     unfold not; intros; inversion H. auto_type'. 
                eapply ty_eq_beta_red; auto_type'.
  trans. 
 Qed.  


  Lemma get_up_tweak : forall Mo,
           extend (extend (extend empty X W8) V Val) Z TNil 
                 ||> tbind (tapp (tupdate Mo State) (tapp (tapp tweak (tvar X)) (tvar V))) (tabs Z TNil (televate (tapp (getloc Mo) (tvar X)) (MStateT State LReadWrite Mo)))
                 ==  tbind (tapp (tupdate Mo State) (tapp (tapp tweak (tvar X)) (tvar V))) (tabs Z TNil (televate (treturn  (tvar V) (MStateT State LRead Mo)) (MStateT State LReadWrite Mo)))
                 @TMonadic (MStateT State LReadWrite Mo) Val.
  Proof. intros. 
  pose proof get_up_tweak_LS  Mo.
  pose proof get_up_tweak_RS  Mo. eapply ty_eq_sym in H0. trans.
  Qed.

  Definition TEST := Id 99.
  Definition P1 := Id 98.
  Definition P2 := Id 97.
  Definition ITE theta u v : tm := tbind theta (tabs TEST TProp (tcase (tvar TEST) u v)).
  Definition AND phi phi' Mo : tm := tbind phi (tabs P1 TProp (tbind phi' (tabs P2 TProp (treturn (tapp (tapp andp (tvar P1)) (tvar P2)) Mo)))).
  Definition IMPL phi phi' Mo : tm := tbind phi (tabs P1 TProp (tbind phi' (tabs P2 TProp (treturn (tapp (tapp implp (tvar P1)) (tvar P2)) Mo)))).

  Lemma ite_bind_f : forall f theta x y Mo,
               empty |-- theta \in TMonadic Mo TProp -> 
               empty |-- x \in TMonadic Mo TProp ->
               empty |-- y \in TMonadic Mo TProp ->
               empty |-- f \in TArrow TProp (TMonadic Mo TProp) ->
               empty ||> tbind (ITE theta (tabs Z TNil x) (tabs Z TNil y)) f == ITE theta (tabs Z TNil (tbind x f)) (tabs Z TNil (tbind y f)) @TMonadic Mo TProp.
  Proof.
  intros. unfold ITE.

  assert(empty ||> tbind (tbind theta (tabs TEST TProp (tcase (tvar TEST) (tabs Z TNil x) (tabs Z TNil y)))) f
               ==  tbind theta (tabs TEST TProp (tbind (tapp (tabs TEST TProp (tcase (tvar TEST) (tabs Z TNil x) (tabs Z TNil y))) (tvar TEST)) f))
               @TMonadic Mo TProp); try (eapply monad_assoc); auto_type'.

  assert(empty ||> tbind theta (tabs TEST TProp (tbind (tapp (tabs TEST TProp (tcase (tvar TEST) (tabs Z TNil x) (tabs Z TNil y))) (tvar TEST)) f))
               ==  tbind theta (tabs TEST TProp (tbind ([TEST:=tvar TEST]tcase (tvar TEST) (tabs Z TNil x) (tabs Z TNil y)) f))
               @TMonadic Mo TProp); try (eapply ty_eq_left_mono). eauto. auto_type. push_subst; auto_type. auto_type. eapply ty_eq_abs_cong; auto_type. eapply ty_eq_right_mono. auto_type'. push_subst; auto_type'. auto_type. eapply ty_eq_beta_red; auto_type'.

  trans.  push_subst_hyp.

  assert(empty ||> tbind theta (tabs TEST TProp (tbind (tcase (tvar TEST) (tabs Z TNil x) (tabs Z TNil y)) f))
               ==  tbind theta (tabs TEST TProp (tcase (tvar TEST) (tabs Z TNil (tbind x f)) (tabs Z TNil (tbind y f))))
               @TMonadic Mo TProp).
        eapply ty_eq_left_mono; auto_type.
        eapply ty_eq_abs_cong; auto_type.
        eapply ty_eq_bind_case; auto_type.

  trans.
Qed.

  
(* Hoare Logic Syntax *)

Definition TNat := W8. 
Definition value := TSum TNil (TSum TNat TProp).




Inductive Expr : Type := 
    | Var : id -> Expr
    | IntE : nat -> Expr
    | NegE : Expr -> Expr
    | PlusE : Expr -> Expr -> Expr.
   

Inductive Assert : Prop :=
    | TrueA : Assert
    | FalseA : Assert
    | LeqA : Expr -> Expr -> Assert
    | AndA : Assert -> Assert -> Assert
    | NotA : Assert -> Assert.

Inductive Comm : Type :=
    | Skip : Comm
    | Assign : id -> Expr -> Comm
    | Seq : Comm -> Comm -> Comm
    | ITEC : Assert -> Comm -> Comm -> Comm.
(*    | While : Assert -> Comm -> Comm. *)

Inductive Lang : Type :=
    | C : Comm -> Lang
    | E : Expr -> Lang
    | A : Assert -> Lang.


Inductive htm : Type :=
(*values*)
  | hnil : htm
 (* | hInt : TNat -> htm 
  | hProp : TProp -> htm. *)

(*assertions*)
  | htrue : htm
  | hfalse : htm
  | hleq : Expr -> Expr -> htm
  | hand : Assert -> Assert -> htm
  | hnot : Assert -> htm

(*expressions*)
  | hvar : id -> htm
  | hneg : Expr -> htm
  | hplus : Expr -> Expr -> htm
  
(*commands*)
  | hskip : htm
  | hassign : id -> Expr -> htm
  | hseq : Comm -> Comm -> htm
  | hite : Assert -> Comm -> Comm -> htm.

Definition eeq e1 e2:= AndA (LeqA e1 e2) (LeqA e2 e1). 

Theorem eq_exp_dec : forall e1 e2 : Expr, {e1 = e2} + {e1 <> e2}.
Proof. intros. induction e1. destruct e2. destruct i. destruct i0. destruct (eq_nat_dec n n0) as [Heq | Hneq].
     Case "eq".
          subst. left. auto.
     Case "neq". right. intros contra. inversion contra. apply Hneq. apply H0.

     destruct i. destruct (eq_nat_dec n0 n) as [Heq | Hneq].
     Case "eq". right. discriminate.
     Case "eq". right. discriminate.
     right. discriminate.
     right. discriminate.
     
     destruct e2. right; discriminate.
                  destruct (eq_nat_dec n n0) as [Heq | Hneq].
                  Case "eq". left. subst. auto.
                  Case "neq". right. intros contra. inversion contra. apply Hneq. apply H0.
     right; discriminate. right; discriminate. 
   
     induction e2; try (right; discriminate).
     constructor. inversion IHe1. rewrite H in IHe1. Admitted.

Reserved Notation "'[' x '//' s ']' P" (at level 20).
Fixpoint psubst (x : id) (e : Expr) (P : Assert) := 
  match P with
  | TrueA => P
  | FalseA => P 
  | LeqA e1 e2 => if eq_exp_dec e e1 then (LeqA e e2) else if eq_exp_dec e e2 then (LeqA e1 e) else P
  | AndA P1 P2 => AndA ([x // e] P1) ([x // e]P2)
  | NotA P' => NotA ([x // e]P')
  end
  where "'[' x '//' e ']' P" := (psubst x e P). 

Inductive triple : Prop :=
  | htrip:  Assert -> Comm -> Assert -> triple
  | trip_prop : Assert -> triple.


Definition ImplyA p q: Assert :=  NotA (AndA p (NotA q)).


Inductive H_Provable : triple -> Prop :=
  | Skip_Ax : forall P,
              H_Provable (htrip P Skip P)
  | Assign_Ax : forall P x e,
              H_Provable (htrip (psubst x e P) (Assign x e) P) 
  | Seq_Ax : forall P Q R c1 c2,
              H_Provable (htrip P c1 Q) ->
              H_Provable (htrip Q c2 R) ->
              H_Provable (htrip P (Seq c1 c2) R)
  | Cond_Ax : forall P b Q c1 c2,
              H_Provable (htrip (AndA P b) c1 Q) ->
              H_Provable (htrip (AndA P (NotA b)) c2 Q) ->
              H_Provable (htrip P (ITEC b c1 c2) Q)

  | Weak_Ax : forall P P' Q Q' c,
              H_Provable (htrip P c Q) ->
              H_Provable (trip_prop (ImplyA P' P)) ->
              H_Provable (trip_prop (ImplyA Q' Q)) ->
              H_Provable (htrip P' c Q').


(*
Axiom skipper : forall P,
         triple P Skip P.

Axiom cond : forall P Q b c1 c2,
         triple (AndA P b) c1 Q ->
         triple (AndA P (NotA b)) c2 Q ->
         triple P (ITEC b c1 c2) Q.

Axiom assign : forall P x e,
         triple P (Assign x e) P. (* FIX ME *)

Axiom seq : forall P Q c1 c2,
          triple P c1 Q ->
          triple P c2 Q ->
          triple P (Seq c1 c2) Q. 

Axiom weaken : forall P P' Q Q' c,
          lang (A (ImplyA P' P)) -> (*NotA (AndA P' (NotA P)))) -> *)
          lang (A (ImplyA Q' Q)) -> (*NotA (AndA Q' (NotA Q)))) -> *)
          triple P c Q ->
          triple P' c Q'.   *)

(* 
    ******** TO DO *********
    * Labels on Monad Transformers
    **** nil, read, write, readwrite  ..........................................................................CHECK
    **** nil -> pure; read -> innocence.........................................................................
    * Def Bit : ty := TSum TNil TNil............................................................................CHECK
    **** Def W8 : ty := TProd Bit (TProd Bit (TProd Bit (TProd Bit (TProd Bit (TProd Bit (TProd Bit Bit))))))...CHECK 
    * Def Val : ty := TSum TNil (TSum Int Prop)................................................................ CHECK
    * Def Int : ty := TProd W8 (TProd W8 (TProd W8 W8)).........................................................CHECK
    * Def TProp : ty := Bit.....................................................................................CHECK
    * Def State : ty := TArrow W8 Val...........................................................................CHECK
    *
    * Lifting Laws..............................................................................................CHECK?
    *
    * Update function in ReWire ................................................................................CHECK
    **** cf tweak .............................................................................................>CHECK
    *
    * Hoare Logic
    **** syntax of HL
    **** interpreter of programs
    **** no while loops
    *
    * Prove OPS |- HL is sound.


  *)             

  
        
  

  
   
  

  




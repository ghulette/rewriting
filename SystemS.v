Set Implicit Arguments.

Inductive strategy (A : Set) : Set :=
| SRule : (A -> option A) -> strategy A
| SIdentity : strategy A
| SFailure : strategy A
| STest : strategy A -> strategy A
| SNeg : strategy A -> strategy A
| SSeq : strategy A -> strategy A -> strategy A
| SLeftChoice : strategy A -> strategy A -> strategy A
| SChoice : strategy A -> strategy A -> strategy A.

Notation "s1 ; s2" :=
  (SSeq s1 s2) (at level 80, right associativity).

Notation "s1 <+ s2" :=
  (SLeftChoice s1 s2) (at level 80, right associativity).

Inductive eval (A : Set) : strategy A -> A -> option A -> Prop :=
| E_Rule1 : forall r t t', r t = Some t' -> eval (SRule r) t (Some t')
| E_Rule2 : forall r t, r t = None -> eval (SRule r) t None
| E_Id : forall t, eval (SIdentity _) t (Some t)
| E_Fail : forall t, eval (SFailure _) t None
| E_Test1 : forall s t t', eval s t (Some t') -> eval (STest s) t (Some t)
| E_Test2 : forall s t, eval s t None -> eval (STest s) t None
| E_Neg1 : forall s t, eval s t None -> eval (SNeg s) t (Some t)
| E_Neg2 : forall s t t', eval s t (Some t') -> eval (SNeg s) t None
| E_Seq1 :
    forall s1 s2 t t' t'',
      eval s1 t (Some t') ->
      eval s2 t' (Some t'') ->
      eval (s1;s2) t (Some t'')
| E_Seq2 :
    forall s1 s2 t,
      eval s1 t None ->
      eval (s1;s2) t None
| E_Seq3 :
    forall s1 s2 t t',
      eval s1 t (Some t') ->
      eval s2 t' None ->
      eval (s1;s2) t None
| E_LeftChoice1 :
    forall s1 s2 t t',
      eval s1 t (Some t') ->
      eval (s1 <+ s2) t (Some t')
| E_LeftChoice2 :
    forall s1 s2 t t',
      eval s1 t None ->
      eval s2 t (Some t') ->
      eval (s1 <+ s2) t (Some t')
| E_LeftChoice3 :
    forall s1 s2 t,
      eval s1 t None ->
      eval s2 t None ->
      eval (s1 <+ s2) t None
| E_Choice1 :
    forall s1 s2 t t',
      eval s1 t (Some t') ->
      eval (SChoice s1 s2) t (Some t')
| E_Choice2 :
    forall s1 s2 t t',
      eval s2 t (Some t') ->
      eval (SChoice s1 s2) t (Some t')
| E_Choice3 :
    forall s1 s2 t,
      eval s1 t None ->
      eval s2 t None ->
      eval (SChoice s1 s2) t None.

Definition succeeds A (s : strategy A) (t : A) : Prop :=
  exists t', eval s t (Some t').

Definition fails A (s : strategy A) (t : A) : Prop :=
  eval s t None.

Inductive without_choice A : strategy A -> Prop :=
| WC_Rule : forall r, without_choice (SRule r)
| WC_Id : without_choice (SIdentity _)
| WC_Fail : without_choice (SFailure _)
| WC_Test : forall s, without_choice s -> without_choice (STest s)
| WC_Neg : forall s, without_choice s -> without_choice (SNeg s)
| WC_Seq :
    forall s1 s2,
      without_choice s1 ->
      without_choice s2 ->
      without_choice (s1;s2)
| WC_LeftChoice :
    forall s1 s2,
      without_choice s1 ->
      without_choice s2 ->
      without_choice (s1 <+ s2).

Definition deterministic A (s : strategy A) : Prop :=
  forall t x y, eval s t x -> eval s t y -> x = y.

Theorem without_choice_deterministic :
  forall A (s : strategy A),
    without_choice s -> deterministic s.
Proof.
  unfold deterministic.
  induction s; intros WC t x y Hx Hy;
  inversion WC; subst; clear WC;

  (* SRule, SIdentity, SFailure *)
  try (inversion Hx; inversion Hy; subst; congruence);

  (* STest, SNeg *)
  try (inversion Hx; inversion Hy; auto; subst;
       assert (Some t' = None); apply IHs with t; congruence).

  (* SSeq *)
  inversion Hx; inversion Hy; auto; subst;
  try (assert (Some t' = None); apply IHs1 with t; congruence);
  try (assert (Some t'0 = Some t') as H by (apply IHs1 with t; assumption);
       inversion H; subst; clear H;
       apply IHs2 with t'; assumption).

  (* SLeftChoice *)
  inversion Hx; inversion Hy; auto; subst;
  try (apply IHs1 with t; assumption);
  try (apply IHs2 with t; assumption).
  assert (Some t' = None); apply IHs1 with t; congruence.
  assert (Some t'0 = None); apply IHs1 with t; congruence.
Qed.

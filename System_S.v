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
      eval (s1;s2) t' (Some t'')
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

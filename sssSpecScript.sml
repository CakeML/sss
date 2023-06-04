open HolKernel boolLib bossLib Parse
open optionTheory stringTheory listTheory llistTheory wordsTheory

val _ = new_theory "sssSpec";

Datatype:
  digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
End

Type seedBits = ``:256 word``;
Type hashWord = ``:word32``;

Type PIN = ``:digit list``;

Datatype:
 consent = Accepting | Rejecting
End

Datatype:
  sssSavedState = <|
    trngEntropy: word32 llist;
    deviceKey: word32;
    seed: seedBits option;
    pin: PIN option;
    wrongPinAttempts: num
  |>
End

Datatype:
  sssMachineState =
    GenerateSeed num (* number of words confirmed *) |
    InputSeed (num list) (* words entered *) |
    InputPIN PIN (* digits entered *) (consent option) |
    PinRequired PIN (* digits entered *) (consent option) |
    Signing hashWord consent |
    Signed  hashWord
End

Type sssButtonInput = ``:(bool # bool)``;
Type sssUSBInput = ``:string``;

Type sssDisplayOutput = ``:string``;
Type sssUSBOutput = ``:string``;

Definition LeftButtonPressed_def:
  LeftButtonPressed: sssButtonInput = (T, F)
End

Definition RightButtonPressed_def:
  RightButtonPressed: sssButtonInput = (F, T)
End

Definition BothButtonsPressed_def:
  BothButtonsPressed: sssButtonInput = (T, T)
End

Definition NUM_SEED_WORDS_def:
  NUM_SEED_WORDS = 24n
End

Definition WORD_LIST_def:
  WORD_LIST = ["TODO"]
End

Definition indicesFromSeed_def:
  indicesFromSeed (seed: seedBits) = ARB: num list (* TODO *)
End

Definition wordsFromSeed_def:
  wordsFromSeed seed = MAP (λi. EL i WORD_LIST) (indicesFromSeed seed)
End

Datatype:
  sssState = <|
    ms: sssMachineState;
    ss: sssSavedState;
    dout: sssDisplayOutput;
    uout: sssUSBOutput
  |>
End

Definition NewSeedMessage_def:
  NewSeedMessage = "Generate seed?"
End

Inductive nextState:
  (s1.ms = GenerateSeed 0 /\ s1.dout = NewSeedMessage /\ s1.uout = "" /\
   s1.ss.seed = NONE /\
   LTAKE 4 (s1.ss.trngEntropy) = SOME words /\
   LDROP 4 (s1.ss.trngEntropy) = SOME rest /\
   s1.ss.pin = NONE /\
   s2 = s1 with <|
     dout := EL 0 (wordsFromSeed (concat_word_list words));
     ss := (s1.ss with <| seed := SOME (concat_word_list words); trngEntropy := rest |>)
   |>
   ==>
   nextState s1 (BothButtonsPressed, "") s2) /\
  (s1.ms = GenerateSeed n /\ s1.dout = EL n (wordsFromSeed seed) /\
   SUC n < NUM_SEED_WORDS /\ s1.ss.seed = SOME seed /\
   s1.uout = "" /\ s1.ss.pin = NONE /\
   s2 = s1 with <|
     dout := EL (SUC n) (wordsFromSeed seed);
     ms := GenerateSeed(SUC n)
   |>
   ==>
   nextState s1 (BothButtonsPressed, "") s2)
End

val _ = export_theory();

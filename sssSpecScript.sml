open HolKernel boolLib bossLib Parse
open pred_setTheory optionTheory stringTheory listTheory llistTheory wordsTheory

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
  sssWordSelector = <|
    prefix: char list;
    letterOptions: char list;
    selectedLetter: num
  |>
End

Definition NUM_SEED_WORDS_def:
  NUM_SEED_WORDS = 24n
End

Definition WORD_LIST_def:
  WORD_LIST = ["todo"]
End

Definition nextLetters_def:
  nextLetters prefix words =
  IMAGE oHD (set (FILTER (λword. isPREFIX prefix word) words))
End

Definition makeLetterOptions_def:
  makeLetterOptions letterSet =
    FILTER (λc. SOME c IN letterSet) "abcdefghijklmnopqrstuvwxyz"
    ++ (if NONE IN letterSet then "!" else "")
End

Definition emptyWordSelector_def:
  emptyWordSelector = <|
    prefix := [];
    letterOptions := makeLetterOptions (nextLetters [] WORD_LIST);
    selectedLetter := 0;
  |>
End

Datatype:
  sssMachineState =
  | GenerateSeed (num option) (* number of words shown *)
  | InputSeed (num list) (* words (by index) entered *)
  | ConfirmSeed (num option) (* number of words confirmed *) sssWordSelector
  | ConfirmPIN PIN (* digits entered *) (consent option)
  | InputPIN PIN (* digits entered *) (consent option)
  | PinRequired PIN (* digits entered *) (consent option)
  | Signing hashWord consent
  | Signed  hashWord
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

Definition ConfirmSeedMessage_def:
  ConfirmSeedMessage = "Confirm seed"
End

Definition NewPinMessage_def:
  NewPinMessage = "Enter PIN"
End

Definition initialSavedState_def:
  initialSavedState key ent = <|
    trngEntropy := ent;
    deviceKey := key;
    seed := NONE;
    pin := NONE;
    wrongPinAttempts := 0
  |>
End

Definition initialState_def:
  initialState key ent = <|
    ms := GenerateSeed NONE;
    ss := initialSavedState key ent;
    dout := NewSeedMessage;
    uout := ""
  |>
End

Inductive nextState:
  (* Generate a new seed from entropy *)
  (s1 = initialState key ent /\
   LTAKE 4 ent = SOME bits /\
   LDROP 4 ent = SOME rest /\
   s2 = s1 with <|
     dout := EL 0 (wordsFromSeed (concat_word_list bits));
     ms := GenerateSeed (SOME 0);
     ss := (s1.ss with <| seed := SOME (concat_word_list bits); trngEntropy := rest |>)
   |>
   ==>
   nextState s1 (BothButtonsPressed, "") s2) /\

  (* Show words of a newly generated seed *)
  (s1.ms = GenerateSeed (SOME n) /\
   SUC n < NUM_SEED_WORDS /\ s1.ss.seed = SOME seed /\
   s2 = s1 with <|
     dout := EL (SUC n) (wordsFromSeed seed);
     ms := GenerateSeed (SOME (SUC n))
   |>
   ==>
   nextState s1 (BothButtonsPressed, "") s2) /\

  (* Show the last word of a newly generated seed and move to confirming it *)
  (s1.ms = GenerateSeed (SOME n) /\
   SUC n = NUM_SEED_WORDS /\
   s2 = s1 with <|
     dout := ConfirmSeedMessage;
     ms := ConfirmSeed NONE emptyWordSelector
   |>
   ==>
   nextState s1 (BothButtonsPressed, "") s2) /\

  (* Confirm words: start *)
  (s1.ms = ConfirmSeed NONE ws /\
   s2 = s1 with <|
     dout := SNOC (EL 0 ws.letterOptions) ws.prefix ;
     ms := ConfirmSeed (SOME 0) ws
   |>
   ==>
   nextState s1 (BothButtonsPressed, "") s2) /\

  (* Confirm words: move to next letter *)
  (s1.ms = ConfirmSeed (SOME n) ws /\
   SUC ws.selectedLetter < LENGTH ws.letterOptions /\
   s2 = s1 with <|
     dout := SNOC (EL (SUC ws.selectedLetter) ws.letterOptions) ws.prefix ;
     ms := ConfirmSeed (SOME n) (ws with <| selectedLetter := SUC ws.selectedLetter |>)
   |>
   ==>
   nextState s1 (RightButtonPressed, "") s2) /\

  (* Confirm words: move to previous letter *)
  (s1.ms = ConfirmSeed (SOME n) ws /\
   0 < ws.selectedLetter /\
   s2 = s1 with <|
     dout := SNOC (EL (PRE ws.selectedLetter) ws.letterOptions) ws.prefix ;
     ms := ConfirmSeed (SOME n) (ws with <| selectedLetter := PRE ws.selectedLetter |>)
   |>
   ==>
   nextState s1 (LeftButtonPressed, "") s2) /\

  (* Confirm words: move to next word successfully *)
  (s1.ms = ConfirmSeed (SOME n) ws /\ SUC n < NUM_SEED_WORDS /\
   s1.ss.seed = SOME seed /\
   EL n (wordsFromSeed seed) = ws.prefix /\
   EL ws.selectedLetter ws.letterOptions = #"!" /\
   s2 = s1 with <|
     dout := [EL 0 emptyWordSelector.letterOptions];
     ms := ConfirmSeed (SOME (SUC n)) emptyWordSelector
   |>
   ==>
   nextState s1 (BothButtonsPressed, "") s2)

  (* Confirm words: fail due to wrong word *)

  (* Confirm words: confirm final word *)
End

val _ = export_theory();

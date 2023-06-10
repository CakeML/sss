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

Definition letterOptionsFromSet_def:
  letterOptionsFromSet letterSet =
    FILTER (λc. SOME c IN letterSet) "abcdefghijklmnopqrstuvwxyz"
End

Definition letterOptions_def:
  letterOptions prefix = letterOptionsFromSet (nextLetters prefix WORD_LIST)
End

Definition emptyWordSelector_def:
  emptyWordSelector = <| prefix := ""; selectedLetter := 0; |>
End

Definition displayWordSelector_def:
  displayWordSelector ws =
    SNOC (EL ws.selectedLetter (letterOptions ws.prefix)) ws.prefix
End

Datatype:
  sssMachineState =
  | GenerateSeed (num option) (* index of word being shown *)
  | InputSeed (num list option) (* words (by index) entered *)
              sssWordSelector
              (consent option)
  | ConfirmSeed (num option) (* number of words confirmed *)
                sssWordSelector
                bool (* confirming this word *)
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

Definition InputSeedMessage_Def:
  InputSeedMessage = "Input seed?"
End

Definition ConfirmSeedMessage_def:
  ConfirmSeedMessage = "Confirm seed?"
End

Definition NewPinMessage_def:
  NewPinMessage = "Enter PIN"
End

Definition NumberedWord_def:
  NumberedWord n ls = (toString n) ++ ": " ++ (EL n ls)
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

Definition initialStateWithAnySeed_def:
  initialStateWithSeed key ent seed =
    initialState key ent with ss updated_by (λss. ss with seed := seed)
End

Inductive nextState:
  (* Generate a new seed from entropy *)
  (s1 = initialStateWithSeed key ent anySeed /\
   LTAKE 4 ent = SOME bits /\
   LDROP 4 ent = SOME rest /\
   s2 = s1 with <|
     dout := NumberedWord 0 (wordsFromSeed (concat_word_list bits));
     ms := GenerateSeed (SOME 0);
     ss := (s1.ss with <| seed := SOME (concat_word_list bits); trngEntropy := rest |>)
   |>
   ==>
   nextState s1 (BothButtonsPressed, "") s2) /\

  (* Show option to go to input seed mode *)
  (s1 = initialStateWithSeed key ent anySeed /\
   s2 = s1 with <|
     dout := InputSeedMessage;
     ms := InputSeed NONE emptyWordSelector NONE
   |>
   ==>
   nextState s1 (LeftButtonPressed, "") s2) /\

  (* Show next word of a newly generated seed *)
  (s1.ms = GenerateSeed (SOME n) /\
   SUC n < NUM_SEED_WORDS /\ s1.ss.seed = SOME seed /\
   s2 = s1 with <|
     dout := NumberedWord (SUC n) (wordsFromSeed seed);
     ms := GenerateSeed (SOME (SUC n))
   |>
   ==>
   nextState s1 (RightButtonPressed, "") s2) /\

  (* Show previous word of a newly generated seed *)
  (s1.ms = GenerateSeed (SOME n) /\
   0 < n /\ s1.ss.seed = SOME seed /\
   s2 = s1 with <|
     dout := NumberedWord (PRE n) (wordsFromSeed seed);
     ms := GenerateSeed (SOME (PRE n))
   |>
   ==>
   nextState s1 (LeftButtonPressed, "") s2) /\

  (* Go back to regenerate *)
  (s1.ms = GenerateSeed (SOME 0) /\ s1.ss.seed = SOME seed /\
   s2 = s1 with <|
     dout := NewSeedMessage;
     ms := GenerateSeed NONE
   |>
   ==>
   nextState s1 (LeftButtonPressed, "") s2) /\

  (* Go forward instead *)
  (s1.ms = GenerateSeed NONE /\ s1.ss.seed = SOME seed /\
   s2 = s1 with <|
     dout := NumberedWord 0 (wordsFromSeed seed);
     ms := GenerateSeed (SOME 0)
   |>
   ==>
   nextState s1 (RightButtonPressed, "") s2) /\

  (* Show the last word of a newly generated seed and move to confirming it *)
  (s1.ms = GenerateSeed (SOME n) /\
   SUC n = NUM_SEED_WORDS /\
   s2 = s1 with <|
     dout := ConfirmSeedMessage;
     ms := ConfirmSeed NONE emptyWordSelector F
   |>
   ==>
   nextState s1 (BothButtonsPressed, "") s2) /\

  (* Confirm words: start *)
  (s1.ms = ConfirmSeed NONE ws cf /\
   s2 = s1 with <|
     dout := displayWordSelector ws ;
     ms := ConfirmSeed (SOME 0) ws cf
   |>
   ==>
   nextState s1 (BothButtonsPressed, "") s2) /\

  (* Confirm words: move to next letter *)
  (s1.ms = ConfirmSeed (SOME n) ws F /\
   SUC ws.selectedLetter < LENGTH (letterOptions ws.prefix) /\
   ws' = ws with <| selectedLetter := SUC ws.selectedLetter |> /\
   s2 = s1 with <|
     dout := displayWordSelector ws' ;
     ms := ConfirmSeed (SOME n) ws' F
   |>
   ==>
   nextState s1 (RightButtonPressed, "") s2) /\

  (* Confirm words: move to previous letter *)
  (s1.ms = ConfirmSeed (SOME n) ws F /\
   0 < ws.selectedLetter /\
   ws' = ws with <| selectedLetter := PRE ws.selectedLetter |> /\
   s2 = s1 with <|
     dout := displayWordSelector ws' ;
     ms := ConfirmSeed (SOME n) ws' F
   |>
   ==>
   nextState s1 (LeftButtonPressed, "") s2) /\

  (* Confirm words: move to confirm this word *)
  (s1.ms = ConfirmSeed (SOME n) ws F /\ SUC n < NUM_SEED_WORDS /\
   s1.ss.seed = SOME seed /\
   EL n (wordsFromSeed seed) = ws.prefix /\
   SUC ws.selectedLetter = LENGTH (letterOptions ws.prefix) /\
   s2 = s1 with <|
     dout := NumberedWord n (wordsFromSeed seed) ;
     ms := ConfirmSeed (SOME n) ws T
   |>
   ==>
   nextState s1 (RightButtonPressed, "") s2) /\

  (* Confirm words: confirm and move to next word *)
  (s1.ms = ConfirmSeed (SOME n) ws T /\ SUC n < NUM_SEED_WORDS /\
   s1.ss.seed = SOME seed /\
   EL n (wordsFromSeed seed) = ws.prefix /\
   s2 = s1 with <|
     dout := displayWordSelector emptyWordSelector ;
     ms := ConfirmSeed (SOME (SUC n)) emptyWordSelector F
   |>
   ==>
   nextState s1 (BothButtonsPressed, "") s2)

  (* Confirm words: confirm final word *)
End

val _ = export_theory();

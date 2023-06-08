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
  | GenerateSeed num (* number of words shown *)
  | InputSeed (num list) (* words (by index) entered *)
  | ConfirmSeed num (* number of words confirmed *) sssWordSelector
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

Inductive nextState:
  (* Generate a new seed from entropy *)
  (s1.ms = GenerateSeed 0 /\ s1.dout = NewSeedMessage /\ s1.uout = "" /\
   s1.ss.seed = NONE /\
   LTAKE 4 (s1.ss.trngEntropy) = SOME bits /\
   LDROP 4 (s1.ss.trngEntropy) = SOME rest /\
   s1.ss.pin = NONE /\
   s2 = s1 with <|
     dout := EL 0 (wordsFromSeed (concat_word_list bits));
     ss := (s1.ss with <| seed := SOME (concat_word_list bits); trngEntropy := rest |>)
   |>
   ==>
   nextState s1 (BothButtonsPressed, "") s2) /\

  (* Show words of a newly generated seed *)
  (s1.ms = GenerateSeed n /\ s1.dout = EL n (wordsFromSeed seed) /\
   SUC n < NUM_SEED_WORDS /\ s1.ss.seed = SOME seed /\
   s1.uout = "" /\ s1.ss.pin = NONE /\
   s2 = s1 with <|
     dout := EL (SUC n) (wordsFromSeed seed);
     ms := GenerateSeed (SUC n)
   |>
   ==>
   nextState s1 (BothButtonsPressed, "") s2) /\

  (* Show the last word of a newly generated seed and move to confirming it *)
  (s1.ms = GenerateSeed n /\ s1.dout = EL n (wordsFromSeed seed) /\
   SUC n = NUM_SEED_WORDS /\ s1.ss.seed = SOME seed /\
   s1.uout = "" /\ s1.ss.pin = NONE /\
   s2 = s1 with <|
     dout := ConfirmSeedMessage;
     ms := ConfirmSeed 0 emptyWordSelector
   |>
   ==>
   nextState s1 (BothButtonsPressed, "") s2) /\

  (* Confirm words: move to next letter *)
  (s1.ms = ConfirmSeed n ws /\ n < NUM_SEED_WORDS /\
   SUC ws.selectedLetter < LENGTH ws.letterOptions /\
   s1.uout = "" /\
   s2 = s1 with <|
     dout := SNOC (EL (SUC ws.selectedLetter) ws.letterOptions) ws.prefix ;
     ms := ConfirmSeed n (ws with <| selectedLetter := SUC ws.selectedLetter |>)
   |>
   ==>
   nextState s1 (RightButtonPressed, "") s2)
  (* Confirm words: move to previous letter *)
  (* Confirm words: move to next word *)
  (* Confirm words: confirm final word *)
End

val _ = export_theory();

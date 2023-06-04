open HolKernel boolLib bossLib Parse
open optionTheory stringTheory listTheory wordsTheory

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

val _ = export_theory();

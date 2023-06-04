open HolKernel boolLib bossLib Parse
open optionTheory listTheory wordsTheory

val _ = new_theory "sssSpec";

Datatype:
  sssSeedState = NoSeed | Seed (256 word)
End

Datatype:
  digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
End

Datatype:
 consent = Accepting | Rejecting
End

Datatype:
  sssMachineState =
    PinRequired (digit list) (consent option) |
    Sign word32 consent |
    Signed word32
End

val _ = export_theory();

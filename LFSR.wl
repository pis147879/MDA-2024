modulo=2;

SetupLFSR[key_, IV_] := Mod[key + IV, modulo];
UpdateLFSR[alpha_][stato_] := Prepend[Drop[stato, -1], Mod[Dot[alpha,  stato], modulo]];
OutputLFSR[stato_] := Last[stato];

KeystreamLFSR[alpha_][key_, IV_, length_] := Map[OutputLFSR, NestList[UpdateLFSR[alpha], SetupLFSR[key, IV], length]];


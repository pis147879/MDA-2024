modulo=2;

SetupLFSR[key_, IV_] := Mod[key + IV, modulo];
UpdateLFSR[alpha_][stato_] := Prepend[Drop[stato, -1], Mod[Dot[alpha,  stato], modulo]];
OutputLFSR[stato_] := Last[stato];

KeystreamLFSR[alpha_][key_, IV_, length_] := Map[OutputLFSR, NestList[UpdateLFSR[alpha], SetupLFSR[key, IV], length]];

(* Esercizio:  restano da implementare le funzioni 
    LFSREncryption[key_,IV_,message_]
    LFSRDecryption[key_,IV_,ciphertext_]
  
    che generano il keystream e lo combinano *aka sommano* con il messaggio
    NB se il modulo e' 2 la Encryption e la Decryption coincidono

    *)
(* AES polynomial *)
paes = FromDigits[IntegerDigits[FromDigits["11B", 16], 2, 9], x];

(* integer/polynomial conversion *)
Int2Poly[a_] := FromDigits[IntegerDigits[a, 2, 8], x];
Poly2Int[poly_] := FromDigits[Reverse@CoefficientList[poly, x, 8], 2];

(* binary fields arithmetics *)
FieldPlus[a_, b_] := FieldPlus[a, b] = BitXor[a, b];
FieldTimes[a_, b_] := FieldTimes[a, b] = Poly2Int[PolynomialMod[Int2Poly[a] Int2Poly[b], {paes, 2}]];
TestDegree[u_, v_] := Floor@Log[2, u] < Floor@Log[2, v];
FieldPlus[a_, b___] := FieldPlus[a, FieldPlus[b]];
FieldTimes[a_, b___] := FieldTimes[a, FieldTimes[b]];

(* algoritmo di inversione per il campo binario di AES*)
FieldInverse[a_] := FieldInverse[a] = Module[{u, v, g1, g2, npaes},
  {u, v, g1, g2} = {a, npaes, 1, 0};
  npaes = FromDigits["11B", 16];
  While[True,
   While[Mod[u, 2] == 0,
    	u = u/2;
    	If[Mod[g1, 2] =!= 0, g1 = BitXor[g1, npaes]];
    	g1 = g1/2;
    (*Print[{u, v, g1, g2}];*)
    ];
   If[u == 1, Return[g1]];
   If[TestDegree[u, v],
    (*Print["@swap : ", {u, v, g1, g2}];*)
    {u, v, g1, g2} = {v, u, g2, g1}];
   u = BitXor[u, v];
   g1 = BitXor[g1, g2];
   ]];

(* AES Non-linear Layer *)

f[byte_] := If[byte == 0, 0, FieldInverse[byte]];
g[byte_] := Module[{A,b},
        (
            A = Table[RotateRight[IntegerDigits[FromDigits["8F", 16], 2, 8], i], {i, 0, 7}];
            b = IntegerDigits[FromDigits["C6", 16], 2, 8];

            FromDigits[Mod[A . IntegerDigits[byte, 2, 8] + b, 2], 2]
        )
];

ginverse[byte_] := Module[{A,b},
        (
            A = Table[RotateRight[IntegerDigits[FromDigits["8F", 16], 2, 8], i], {i, 0, 7}];
            Ainv =InverseMatrix[A,Modulus->2];
            b = IntegerDigits[FromDigits["C6", 16], 2, 8];

            FromDigits[Ainv . Mod[ IntegerDigits[byte, 2, 8] + b, 2], 2]
        )
];

SRD[byte_] := SRD[byte] = g[f[byte]];
SubBytes[state_] := Map[SRD, state, {2}];

SRDINV[byte_] := SRDINV[byte] = f[ginverse[byte]];
InverseSubBytes[state_] := Map[SRDINV, state, {2}];

(* AES Mix Layer *)

ShiftRows[state_] := MapThread[RotateLeft, {state, {0, 1, 2, 3}}];
InverseShiftRows[state_] := MapThread[RotateRight, {state, {0, 1, 2, 3}}];

MCMatrix = Table[
  			RotateRight[ { 2, 3, 1 ,1 }  , i], {i, 0, 3}]

InverseMCMatrix = Table[
  			RotateRight[ { 14, 11, 13 ,9 }  , i], {i, 0, 3}]

AuxMixColumns[state_,matrix_] := Module[{columns},
   	columns = Transpose[state];
  	Map[
             Inner[FieldTimes  , matrix, #, FieldPlus] &,
   	columns]
  ];

MixColumns[state_]:=AuxMixColumns[state,MCMatrix];
InverseMixColumns[state_]:=AuxMixColumns[state,InverseMCMatrix];


AESRound[state_, rk_] := AddKey[MixColumns@ShiftRows@SubBytes@state, rk];

(* ... to be continued *)
Nk = 4 ;

UpdateKeySchedule[state_] := Append[state,
  			FieldPlus[
   				If[Mod[Length[state], Nk] == 0,
    					FieldPlus[{Mod[2^(Length[state]/Nk - 1), 256], 0, 0, 0},  
                                                                      \
                    Map[SRD, RotateLeft[state[[-1]], 1]]
     							],
    					 state[[-1]]
    				],
   				 state[[-Nk]]]];

AESKeySchedule[key_, rounds_] := 
 AESKeySchedule[key, rounds] = Module[{state0, steps, tmpks},
   state0 = 
    Partition[IntegerDigits[FromDigits[key, 16], 256, 16], 4];
   
   steps = 4 (rounds + 1) - Nk;
   tmpks = Nest[UpdateKeySchedule, state0, steps];
   Map[Transpose, Partition[tmpks, 4]]
   ];

AESEncryption[state_, key_] := Fold[AESRound, state, AESKeyschedule[key, 10]];

AESRoundInverso[state_, rk_] := InverseSubBytes@InverseShiftRows@InverseMixColumns@AddKey[ state, rk];

AESEncryption[state_, key_] := Fold[AESRoundInverse, state, Reverse@AESKeyschedule[key, 10]];


(* AES polynomial *)
paes = FromDigits[IntegerDigits[FromDigits["11B", 16], 2, 9], x];

(* integer/polynomial conversion *)
Int2Poly[a_] := FromDigits[IntegerDigits[a, 2, 8], x];
Poly2Int[poly_] := FromDigits[Reverse@CoefficientList[poly, x, 8], 2];


(* binary fields arithmetics *)
FieldPlus[a_, b_] := FieldPlus[a, b] =BitXor[a, b];
FieldTimes[a_, b_] := FieldTimes[a, b] = Poly2Int[PolynomialMod[Int2Poly[a] Int2Poly[b], {paes, 2}]];
TestDegree[u_, v_] := Floor@Log[2, u] < Floor@Log[2, v];

(* algoritmo di inversione per il campo binario di AES*)
FieldInverse[a_] := FieldInverse[a] = Module[{u, v, g1, g2, npaes},
  {u, v, g1, g2} = {a, npaes, 1, 0};
  npaes = FromDigits["11B", 16];
  While[True,
   While[Mod[u, 2] == 0,
    	u = u/2;
    	If[Mod[g1, 2] =!= 0, g1 = BitXor[g1, npaes]];
    	g1 = g1/2;
    Print[{u, v, g1, g2}];
    ];
   If[u == 1, Return[g1]];
   If[TestDegree[u, v],
    Print["@swap : ", {u, v, g1, g2}];
    {u, v, g1, g2} = {v, u, g2, g1}];
   u = BitXor[u, v];
   g1 = BitXor[g1, g2];
   ]];

(* AES Non-linear Layer *)

SubBytes[state_] := Map[SRD, state, {2}];
SRD[byte_] := SRD[byte] = g[f[byte]];
f[byte_] := If[byte == 0, 0, FieldInverse[byte]];
g[byte_] := Module[{A,b},
        (
            A = Table[RotateRight[IntegerDigits[FromDigits["8F", 16], 2, 8], i], {i, 0, 7}];
            b = IntegerDigits[FromDigits["C6", 16], 2, 8];

            FromDigits[Mod[A . IntegerDigits[byte, 2, 8] + b, 2], 2]
        )
];


(* AES Mix Layer *)

ShiftRows[state_] := MapThread[RotateLeft, {state, {0, 1, 2, 3}}];

(* ... to be continued *)
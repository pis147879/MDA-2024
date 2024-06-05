wordsize = 32;
module = 2^wordsize;

BitRotateLeft[x_, offset_] := 
 BitAnd[BitXor[BitShiftRight[x, wordsize - offset],
   BitShiftLeft[x, offset]], modulo - 1];

(*
BitRotateRight[x_,offset_]:=
 BitAnd[BitXor[BitShiftLeft[x,wordsize-offset],
   BitShiftRight[x,offset]],modulo-1]}*)



QuarterRound[y0_, y1_, y2_, y3_] := Module[{z1, z2, z3, z0},
  (
   z1 = BitAnd[BitXor[y1, BitRotateLeft[Mod[y0 + y3, modulo], 7]], 
     modulo - 1];
   z2 = BitAnd[BitXor[y2, BitRotateLeft[Mod[y0 + z1, modulo], 9]], 
     modulo - 1];
   z3 = BitAnd[BitXor[y3, BitRotateLeft[Mod[z2 + z1, modulo], 13]], 
     modulo - 1];
   z0 = BitAnd[BitXor[y0, BitRotateLeft[Mod[z3 + z2, modulo], 18]], 
     modulo - 1];
   {z0, z1, z2, z3}
   )];

ColumnRound[y_] := 
 Module[{partitioned, z, z0, z1, z2, z3, z4, z5, z6, z7, z8, z9, z10, 
   z11, z12, z13, z14, z15, shifted},
  (
   partitioned = Transpose[Partition[y, 4]] ;
   z = {z0, z1, z2, z3, z4, z5, z6, z7, z8, z9, z10, z11, z12, z13, 
     z14, z15};
   shifted = MapThread[RotateLeft, {partitioned, {0, 1, 2, 3}}];
   {{z0, z4, z8, z12}, {z5, z9, z13, z1}, {z10, z14, z2, z6}, {z15, 
      z3, z7, z11}} = Map[Apply[QuarterRound, #] &, shifted];
   z
   )];

DoubleRound[y_] := RowRound[ColumnRound[y]];

LittleEndian[state_] := FromDigits[Reverse@state, 2^8];

LittleEndianInverse[x_] := Reverse@IntegerDigits[x, 2^8, 4];

Salsa20[x_] := Module[{y, z},
  (
   y = Map[LittleEndian, Partition[x, 4]];
   z = Nest[DoubleRound, y, 10];
   Flatten@Map[LittleEndianInverse, Mod[z + y, module]]
   )];

(** Esercizio: restano da implementare le due funzioni 
    1) Salsa20Expansion[key,IV] (nel testo indicata Salsa20_key(IV) )

    2) Salsa20Encryption[key,IV,msg]

    **)


(* ::Package:: *)

(*fissiamo una scelta *)

security=5;
choice=0; (* oppure choice=1 *)
Print["Alice sceglie ",choice]; 

(* generiamo un numero casuale a protezione della schelta *)
hide=RandomInteger[2^security]; (* questa informazione nota solo ad alice*)
committment=Hash[hide+choice];  (* costruiamo il committment - alice si impegna a non cambiare la sua scelta *)

(* se la funzione di hash funziona bene *)
(* e il committment scheme e' sicuro    *)
(* allora *)
(* bob dal committment non puo' ricavare nessuna informazione sulla scelta *)
(* alice non puo' cambiare la sua scelta *)
(*  ovvero non puo' trovare un n' per cui *)
(*  il committment corrispondente con la scelta modificata *)
(*  coincide con y *)

(* Possiamo sempre cercarlo a con la ricerca esaustiva ...*)
TimeConstrained[
    n1=0;
    While[!(Hash[n1+1]==y)&&(n1<2^security),n1++];Print@n1;
    Print@Hash[n1+1];
,100]

Print["hash calcolato ",Hash[n1+1]," verifica l'uguaglianza con il committment : ",y];

(* oppure con il ragionamento *)
n1=n-1;
Print["h(n'+c') = ",Hash[n1+1]," h(n,c)=",y," ok =",Hash[n1+1]==y];






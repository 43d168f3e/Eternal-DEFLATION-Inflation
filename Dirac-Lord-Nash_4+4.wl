(* ::Package:: *)

(* ========================================================================== *)
(*                          Dirac-Lord-Nash_4+4.wl                               *)
(* ======================  a few references:   ================================ *)
(*JOURNAL OF NATHENATICAL PHYSICS, VOLUME 4, NUMBER 7, JULY 1963*)
(*"A Remarkable Representation of the 3 + 2 de Sitter Group"*)
(*P. A. M. DIrac*)
(* ========================================================================== *)
(*Proc. Camb. Phil. Soc. (1968), 64, 765*)
(*"The Dirac spinor in six dimensions"*)
(*E. A. LORD*)
(*Department of Mathematics, King's College, University of London*)
(* ========================================================================== *)
(*J. Math. Phys. 25 (2), February 1984*)
(*"Identities satisfied by the generators of the Dirac algebra"*)
(*Patrick L. Nash*)
(* ========================================================================== *)
(*IL NUOVO CIMENTO, VoL. 105 B, N. 1, Gennaio 1990*)
(*"On the Structure of the Split Octonion Algebra"*)
(*P. L. NASH*)
(*University of Texas at San Antonio, TX 78285-0663*)
(* ========================================================================== *)
(*JOURNAL OF MATHEMATICAL PHYSICS 51, 042501 (2010)*)
(*"Second gravity"*)
(*Patrick L. Nash*)
(* ========================================================================== *)
(*                                                                            *)
(*  Clifford Algebra Cl(4,4) and Spin(4,4) Representations                    *)
(*  for Split Octonions and Cartan's Triality                                 *)
(*                                                                            *)
(*  This package provides:                                                    *)
(*    1. Real 16x16 matrix representation of Cl(4,4) with generators t^A      *)
(*    2. Two real 8x8 matrix representations of Spin(4,4)                     *)
(*    3. Proof of anti-commutation relations {t^A, t^B} = 2 eta^{AB} I_16     *)
(*    4. Proof of commutation relations for spin generators S^{AB}           *)
(*                                                                            *)
(*  Mathematical Background:                                                   *)
(*    - Split octonions Os: 8D non-associative algebra over R                 *)
(*    - Signature (4,4): <x,x> = x0^2+x1^2+x2^2+x3^2-x4^2-x5^2-x6^2-x7^2     *)
(*    - Cartan's triality: V, S1, S2 are equivalent 8D representations       *)
(*                                                                            *)
(*  Usage: Get["DiracLordNash4+4.wl"]                                         *)
(*                                                                            *)
(* ========================================================================== *)
(* ============================================================================ *)
(* Patrick L. Nash, Ph.D.     (c) 2022, under GPL ; do not remove this notice   *)
(* Professor, UTSA Physics and Astronomy, Retired (UTSA)                        *)
(* Patrick299Nash  at    gmail   ...                                            *)
(* Enhanced Version 2 - Fixed HTML entity handling and partial derivatives      *)
(* blame: PLN and friends (Claude Opus 4.5 and Manus-Lite)                           *)
(* ============================================================================ *)

BeginPackage["DiracLordNash44`"];

(* ========================================================================== *)
(*                         TABLE OF CONTENTS                                  *)
(* ========================================================================== *)
(*                                                                            *)
(*  SECTION 1: Basic Definitions and Identity Matrices                        *)
(*  SECTION 2: Metric Tensors                                                 *)
(*  SECTION 3: Pauli-like 2x2 Building Blocks                                 *)
(*  SECTION 4: Self-Dual and Anti-Self-Dual 4x4 Matrices                      *)
(*  SECTION 5: 8x8 Clifford Algebra Generators Q[A] for spinor space        *)
(*  SECTION 6: Conjugate Q-bar generators                                   *)
(*  SECTION 7: 16x16 Clifford Algebra Generators T16^A[n]                     *)
(*  SECTION 8: Chirality and Volume Elements                                  *)
(*  SECTION 9: Spin(4,4) Generators S^{AB} (8x8 reducible representations)    *)
(*  SECTION 10: Verification of Anti-Commutation Relations                    *)
(*  SECTION 11: Verification of Commutation Relations                         *)
(*  SECTION 12: Helper Functions for Lagrangian Construction                  *)
(*  SECTION 13: Unit Spinor, F-matrices, Projections, Fundamental Identity    *)
(*  SECTION 14: Complete 256-Element Basis via Pauli Kronecker Products       *)
(*                                                                            *)
(* ========================================================================== *)

(* Public symbols *)
ID2::usage = "ID2 is the 2x2 identity matrix.";
ID4::usage = "ID4 is the 4x4 identity matrix.";
ID8::usage = "ID8 is the 8x8 identity matrix.";
ID16::usage = "ID16 is the 16x16 identity matrix.";

eta2244::usage = "eta2244 is the 4x4 metric with signature (2,2): diag(-1,1,-1,1).";
etaAB::usage = "etaAB is the 8x8 metric with signature (4,4): diag(1,1,1,1,-1,-1,-1,-1).";

\[Sigma]22::usage = "\[Sigma]22 is a list of four real 2x2 matrices forming a basis.";
\[Sigma]Bar22::usage = "\[Sigma]Bar22 is the conjugate basis with -I2 as first element.";

s4by4::usage = "s4by4[h] gives the h-th self-dual antisymmetric 4x4 matrix (h=1,2,3).";
t4by4::usage = "t4by4[h] gives the h-th anti-self-dual antisymmetric 4x4 matrix (h=1,2,3).";

allS4by4::usage = "gives all s4by4 self-dual antisymmetric 4x4 matrix (h=1,2,3).";
allT4by4::usage = "gives all s4by4 anti-dual antisymmetric 4x4 matrix (h=1,2,3).";
  
(* OverBar[allQ] usage is documented via allQBar below *)

allQ::usage = "allQ";
allQBar::usage = "allQBar";
Q::usage = "Q[A] gives the A-th 8x8 Clifford generator (A=0,...,7). Q[0]=ID8.";
QBar::usage = "QBar[A] (or OverBar[Q][A]) gives the conjugate of Q[A] via QBar[A] = -eta[A,A]*Transpose[Q[A]].";

T16A::usage = "T16A[n] gives the n-th 16x16 Clifford algebra generator (n=0,...,8).";

\[Sigma]8::usage = "\[Sigma]8 is the 8x8 chirality matrix: Q[1].Q[2].Q[3].";
\[Sigma]16::usage = "\[Sigma]16 is the 16x16 chirality matrix.";
Omega8::usage = "Omega8 is the 8x8 volume element: \[Sigma]8.Q[7].";
Omega16::usage = "Omega16 is the 16x16 complex structure matrix.";

(*SAB8::usage = "SAB8[A,B] incorrectly gives the (A,B) Spin(4,4) generator as an 8x8 matrix (acts on S1 or S2).";*)
SAB16::usage = "SAB16[A,B] gives the (A,B) Spin(4,4) generator as a 16x16 matrix.";
SAB::usage = "gives ALL Spin(4,4) generator as a 16x16 matrix.";

SAB1::usage = "SAB1 returns table (A,B) Spin(4,4) generator as an 8x8 matrix (acts on S1 ).";
SAB2::usage = "SAB2 returns table (A,B) Spin(4,4) generator as an 8x8 matrix (acts on S2).";

SpinorMetric8::usage = "SpinorMetric8 is the 8x8 spinor metric C = {{0,I4},{I4,0}}.";
SpinorMetric16::usage = "SpinorMetric16 is the 16x16 spinor metric.";

verifyAntiCommutation::usage = "verifyAntiCommutation[] returns True if all anti-commutation relations hold.";
verifyCommutation::usage = "verifyCommutation[] returns True if all spin generator commutation relations hold.";

(* Section 13: Unit Spinor and Lagrangian Construction *)
unit::usage = "unit is the unit type-1 spinor, an eigenspinor of \[Sigma]8 with eigenvalue +1.";
FAa::usage = "FAa is the 8x8 matrix F_A^a = \[Eta]_{AA} * (\[Tau][A] . unit)^T for Lagrangian construction.";
FaA::usage = "FaA is the list of row vectors F_a^A = unit^T . \[Sigma]8 . \[Tau]\:0304[A] for A=0,...,7.";
FForthogonality::usage = "FForthogonality is the 8x8 matrix FaA . FAa, which should equal ID8 (resolution of identity).";
splitOctonionMult::usage = "splitOctonionMult[A,B,C] gives the split octonion structure constant C^{ABC}.";
(*EA::usage = "mult tab entries";*)
eA::usage = "mult tab entries";
times::usage = "mult tab entries";
splitOctonionMultTable::usage = "splitOctonionMultTable gives the split octonion multiplication table.";
realProjection8::usage = "realProjection8 is the 8x8 real projection matrix: KroneckerProduct[unit, \[Sigma]8.unit].";
realProjection16::usage = "realProjection16 is the 16x16 real projection: {{realProjection8,0},{0,realProjection8}}.";
imaginaryPart8::usage = "imaginaryPart8[\[Psi]] returns the imaginary (non-real) part of 8-spinor \[Psi].";
imaginaryPart16::usage = "imaginaryPart16[\[CapitalPsi]] returns the imaginary part of 16-spinor \[CapitalPsi].";
FundamentalIdentity8by8::usage = "FundamentalIdentity8by8[a] verifies Tr[a]*I8 = \[CapitalSigma] \[Eta][A,A]*\[Tau][A].a.\[Tau]\:0304[A].";
testFundamentalIdentity::usage = "testFundamentalIdentity[matrix] tests fundamental identity on given matrix.";
fundamentalIdentityTest1::usage = "fundamentalIdentityTest1 is True if fundamental identity holds for ID8.";
fundamentalIdentityTest2::usage = "fundamentalIdentityTest2 is True if fundamental identity holds for \[Sigma]8.";
fundamentalIdentityTest3::usage = "fundamentalIdentityTest3 is True if fundamental identity holds for Q[1].Q[2].";

(* Section 14: 256-Element Basis *)
pauli::usage = "pauli[k] returns the k-th Pauli matrix: pauli[0]=I2, pauli[1,2,3]=PauliMatrix[1,2,3].";
pauliReal::usage = "pauliReal[k] returns the k-th REAL Pauli basis: pauliReal[2]=I*PauliMatrix[2] is real.";
Basis16::usage = "Basis16[a,b,c,d] returns the 16x16 matrix \[Sigma]_a\[CircleTimes]\[Sigma]_b\[CircleTimes]\[Sigma]_c\[CircleTimes]\[Sigma]_d (a,b,c,d\[Element]{0,1,2,3}).";
Basis16Real::usage = "Basis16Real[a,b,c,d] returns the REAL 16x16 matrix using pauliReal basis.";
Basis16Index::usage = "Basis16Index[a,b,c,d] returns linear index n = 64a+16b+4c+d \[Element] {0,...,255}.";
Basis16FromIndex::usage = "Basis16FromIndex[n] returns {a,b,c,d} from linear index n.";
Basis16ByIndex::usage = "Basis16ByIndex[n] returns the n-th basis matrix (n\[Element]{0,...,255}).";
Basis16Label::usage = "Basis16Label[a,b,c,d] returns string label like '\[Sigma]1 \[CircleTimes] I \[CircleTimes] \[Sigma]3 \[CircleTimes] \[Sigma]2'.";
ViewBasis16::usage = "ViewBasis16[a,b,c,d] displays basis matrix with label and index.";
ViewBasis16ByIndex::usage = "ViewBasis16ByIndex[n] displays the n-th basis matrix.";
GenerateAllBasis16::usage = "GenerateAllBasis16[] returns list of all 256 {index,label,matrix} triples.";
AllBasis16::usage = "AllBasis16 is a cached list of all 256 basis matrices.";
AllBasis16Real::usage = "AllBasis16Real is a cached list of all 256 REAL basis matrices.";
Basis16IndexTable::usage = "Basis16IndexTable[] displays table of all 256 indices and labels.";
ExpandInBasis16::usage = "ExpandInBasis16[M] returns 256 coefficients of M in the Pauli basis.";
NonZeroComponents16::usage = "NonZeroComponents16[M] returns non-zero basis components of matrix M.";
VerifyBasis16Orthogonality::usage = "VerifyBasis16Orthogonality[] returns True if basis is orthogonal.";
(*X::usage = "default Minkowski coored";*)
epsilon3::usage = "Levi-Civita symbol for 3 indices";
epsilon4::usage = "Levi-Civita symbol for 4 indices";

Begin["`Private`"];

(* ========================================================================== *)
(*  SECTION 1: Basic Definitions and Identity Matrices                        *)
(* ========================================================================== *)
(*X = {x0, x1, x2, x3, x4, x5, x6, x7};
Protect[X];
Protect[x0, x1, x2, x3, x4, x5, x6, x7];*)

ID2 = IdentityMatrix[2];
ID4 = IdentityMatrix[4];
ID8 = IdentityMatrix[8];
ID16 = IdentityMatrix[16];

(* Zero matrices for convenience *)
Zero4 = Array[0 &, {4, 4}];
Zero8 = Array[0 &, {8, 8}];

(* ========================================================================== *)
(*  SECTION 2: Metric Tensors                                                 *)
(* ========================================================================== *)

(* 4x4 metric with signature (2,2) for building blocks *)
eta2244 = DiagonalMatrix[{-1, 1, -1, 1}];

(* 8x8 metric with signature (4,4) for split octonions *)
(* Indices: 0,1,2,3 are timelike (+1), 4,5,6,7 are spacelike (-1) *)
etaAB = ArrayFlatten[{{ID4, Zero4}, {Zero4, -ID4}}];

(* Levi-Civita symbol for 4 indices *)
epsilon4 = Array[Signature[{##}] &, {4, 4, 4, 4}];
epsilon3 = Array[Signature[{##}]&,{3,3,3}]
(* ========================================================================== *)
(*  SECTION 3: Pauli-like 2x2 Building Blocks                                 *)
(* ========================================================================== *)

(* Real 2x2 matrices forming a Clifford algebra basis *)
(* \[Sigma]22 = {I2, \[Sigma]_1, i*\[Sigma]_2, \[Sigma]_3} where i*\[Sigma]_2 is real *)
\[Sigma]22 = {
    IdentityMatrix[2],           (* {{1,0},{0,1}} *)
    PauliMatrix[1],              (* {{0,1},{1,0}} *)
    I * PauliMatrix[2],          (* {{0,1},{-1,0}} - real! *)
    PauliMatrix[3]               (* {{1,0},{0,-1}} *)
};

(* Conjugate basis with opposite first element *)
\[Sigma]Bar22 = {
    -IdentityMatrix[2],          (* {{-1,0},{0,-1}} *)
    PauliMatrix[1],              (* {{0,1},{1,0}} *)
    I * PauliMatrix[2],          (* {{0,1},{-1,0}} *)
    PauliMatrix[3]               (* {{1,0},{0,-1}} *)
};

(* ========================================================================== *)
(*  SECTION 4: Self-Dual and Anti-Self-Dual 4x4 Matrices                      *)
(* ========================================================================== *)

(* Functions to build 4x4 blocks from 2x2 matrices via Kronecker products *)
yyy[j_] := KroneckerProduct[\[Sigma]22[[j]], \[Sigma]22[[2]]];
xxx[j_] := ArrayFlatten[{{\[Sigma]22[[j]], 0}, {0, \[Sigma]Bar22[[j]]}}];

(* Self-dual antisymmetric 4x4 matrices (h = 1,2,3) *)
(* These satisfy: (1/2)*epsilon[p,q,j1,j2]*s4by4[h][[j1,j2]] = s4by4[h][[p,q]] *)




(* Anti-self-dual antisymmetric 4x4 matrices (h = 1,2,3) *)
(* These satisfy: (1/2)*epsilon[p,q,j1,j2]*t4by4[h][[j1,j2]] = -t4by4[h][[p,q]] *)

Qa1234[h_, p_, q_] := Signature[{h, p, q, 4}];
Qb1234[h_, p_, q_] := ID4[[p, 4]]*ID4[[q, h]] - ID4[[p, h]]*ID4[[q, 4]];
SelfDualAntiSymmetric[h_, p_, q_] := Qa1234[h, p, q] - Qb1234[h, p, q] ;
AntiSelfDualAntiSymmetric[h_, p_, q_] := (Qa1234[h, p, q] + Qb1234[h, p, q] );

allS4by4=Table[s4by4[h] = Table[Table[SelfDualAntiSymmetric[h, p, q], {q, 4}], {p, 4}], {h, 1, 3} ];
allT4by4=Table[t4by4[h] = Table[Table[AntiSelfDualAntiSymmetric[h, p, q], {q, 4}], {p, 4}], {h, 1, 3} ];

(* ========================================================================== *)
(*  SECTION 5: 8x8 Clifford Algebra Generators Q[A]                         *)
(* ========================================================================== *)

(* Q[0] = identity (required for completeness) *)
Q[0] = ID8;
OverBar[Q][0] = ID8;
Table[Q[7 - h] = ArrayFlatten[{{0, t4by4[h]}, {-t4by4[h], 0}}], {h, 1, 3} ];
Table[Q[h] = ArrayFlatten[{{0, s4by4[h]}, {s4by4[h], 0}}], {h, 1, 3} ];
   
(* Q[1], Q[2], Q[3]: Built from self-dual matrices *)
(* These are symmetric: Q[h] = Transpose[Q[h]] for h = 1,2,3 *)
(*Do[
    Q[h] = ArrayFlatten[{{0, s4by4[h]}, {s4by4[h], 0}}],
    {h, 1, 3}
];
*)
(* Q[4], Q[5], Q[6]: Built from anti-self-dual matrices *)
(* These are antisymmetric: Q[h] = -Transpose[Q[h]] for h = 4,5,6 *)
(*Do[
    Q[7 - h] = ArrayFlatten[{{0, t4by4[h]}, {-t4by4[h], 0}}],
    {h, 1, 3}
];*)

(* Q[7]: The chirality-related generator, defined as product of others *)
Q[7] = Q[1] . Q[2] . Q[3] . Q[4] . Q[5] . Q[6];
\[Sigma]8 = Q[1] . Q[2] . Q[3];
(* ========================================================================== *)
(*  SECTION 6: Conjugate Q-bar Generators                                   *)
(* ========================================================================== *)

(* The conjugate generators satisfy: OverBar[Q][A] = -eta[A,A] * Transpose[Q[A]] *)
(* For A = 1,2,3: eta[A,A] = +1, so OverBar[Q][A] = -Transpose[Q[A]] *)
(* For A = 4,5,6,7: eta[A,A] = -1, so OverBar[Q][A] = Transpose[Q[A]] *)
OverBar[allQ]=Table[OverBar[Q][A1] = \[Sigma]8 . Transpose[\[Sigma]8 . Q[A1]],{A1, 1, 7}];
PrependTo[OverBar[allQ],OverBar[Q][0]];
allQ   = Table[Q[A1] ,{A1, 0, 7}];
allQBar=Table[OverBar[Q][A1],{A1, 0, 7}];



(* ========================================================================== *)
(*  SECTION 7: 16x16 Clifford Algebra Generators T16^A[n]                     *)
(* ========================================================================== *)

(* The 16x16 generators act on the full spinor space S1 \[CirclePlus] S2 *)
(* Construction: T16^A[n] = {{0, OverBar[Q][n]}, {Q[n], 0}} *)
allT16A=Table[T16A[A1] = ArrayFlatten[{{0, OverBar[Q][A1]}, {Q[A1], 0}}],{A1, 0, 7}
];

(* T16^A[8]: The 16D chirality element (product of all generators) *)
T16A[8] = T16A[0] . T16A[1] . T16A[2] . T16A[3] . T16A[4] . T16A[5] . T16A[6] . T16A[7];
AppendTo[allT16A,T16A[8]];
(* ========================================================================== *)
(*  SECTION 8: Chirality and Volume Elements                                  *)
(* ========================================================================== *)

(* 8x8 chirality matrix: \[Sigma] = Q[1].Q[2].Q[3] *)
(* This has eigenvalues +1 and -1, projecting onto type-1 and type-2 spinor spaces *)
(*\[Sigma]8 = Q[1] . Q[2] . Q[3];*)

(* Alternative representation: \[Sigma]8 = Q[4].Q[5].Q[6].Q[7] *)
(* Verification: \[Sigma]8 == Q[4].Q[5].Q[6].Q[7] should be True *)

(* 16x16 chirality matrix *)
\[Sigma]16 = T16A[0] . T16A[1] . T16A[2] . T16A[3];

\[Sigma]16 . T16A[#] == -Transpose[\[Sigma]16 . T16A[#]] & /@ Range[0, 7]

(* Relation: \[Sigma]16 == ArrayFlatten[{{-\[Sigma]8, 0}, {0, \[Sigma]8}}] *)

(* 8x8 volume element (complex structure) *)
Omega8 = \[Sigma]8 . Q[7];

(* 16x16 complex structure *)
Omega16 = T16A[0] . T16A[4] . \[Sigma]16;

(* ========================================================================== *)
(*  SECTION 9: Spin(4,4) Generators S^{AB}                                    *)
(* ========================================================================== *)

(* The spin generators are defined as commutators: S^{AB} = (1/4)(t^A.t^B - t^B.t^A) *)
(* These form the Lie algebra so(4,4) *)

(* WTF:   8x8 spin generators (act on S1 or S2 individually) *)
(*SAB8[A_, B_] := (1/4) * (Q[A] . Q[B] - Q[B] . Q[A]);*)



SAB1=Table[1/4 ( OverBar[Q][A1] . Q[B1]-OverBar[Q][B1] . Q[A1]),{A1,0, 7},{B1,0, 7}]
SAB2=Table[1/4 ( Q[A1] . OverBar[Q][B1]-Q[B1] . OverBar[Q][A1]),{A1,0, 7},{B1,0, 7}]


(* 16x16 spin generators (act on S1 \[CirclePlus] S2) *)
SAB = Table[1/4 ((T16^A)[A1] . (T16^A)[B1] - (T16^A)[B1] . (T16^A)[A1]), {A1, 0, 7}, {B1, 0, 7}];
SAB16[A_, B_] :=SAB[[A,B]];   (*(1/4) * (T16A[A] . T16A[B] - T16A[B] . T16A[A]);*)




(* Note: S^{AB} = -S^{BA} (antisymmetric) *)
(* Note: S^{AA} = 0 for all A *)

(* ========================================================================== *)
(*  SECTION 10: Verification of Anti-Commutation Relations                    *)
(* ========================================================================== *)

(* The Clifford algebra Cl(4,4) is defined by: {t^A, t^B} = 2*eta^{AB}*I *)
(* That is: t^A.t^B + t^B.t^A = 2*etaAB[[A+1,B+1]]*I *)

(* Verification function for 8x8 generators *)
verifyAntiCommutation8[] := Module[{result = True, antiComm},
    Do[
        antiComm = Q[A] . OverBar[Q][B] + Q[B] . OverBar[Q][A]//FullSimplify;
        If[antiComm != 2 * etaAB[[A + 1, B + 1]] * ID8,
            result = False;
            Print["Anti-commutation 8 fails for A=", A, ", B=", B, ", ==", antiComm];
        ],
        {A, 0, 7}, {B, 0, 7}
    ];
    result
];

(* Verification function for 16x16 generators *)
verifyAntiCommutation16[] := Module[{result = True, antiComm},
    Do[
        antiComm = T16A[A] . T16A[B] + T16A[B] . T16A[A];
        If[antiComm != 2 * etaAB[[A + 1, B + 1]] * ID16,
            result = False;
            Print["Anti-commutation 16 fails for A=", A, ", B=", B];
        ],
        {A, 0, 7}, {B, 0, 7}
    ];
    result
];

(* Combined verification *)
verifyAntiCommutation[] := verifyAntiCommutation8[] && verifyAntiCommutation16[];

(* ========================================================================== *)
(*  SECTION 11: Verification of Commutation Relations                         *)
(* ========================================================================== *)

(* The spin generators satisfy the so(4,4) Lie algebra relations: *)
(* [S^{AB}, S^{CD}] = eta^{BC}*S^{AD} - eta^{AC}*S^{BD} - eta^{BD}*S^{AC} + eta^{AD}*S^{BC} *)

(* Also, the spin generators transform the Clifford generators: *)
(* [S^{AB}, t^C] = eta^{BC}*t^A - eta^{AC}*t^B *)

verifySpinCommutation[] := Module[{result = True, lhs, rhs},
    (* Check [S^{AB}, t^C] = eta^{BC}*t^A - eta^{AC}*t^B *)
    Do[
        lhs = SAB1[A, B] . Q[C] - Q[C] . SAB1[A, B];
        rhs = etaAB[[B + 1, C + 1]] * Q[A] - etaAB[[A + 1, C + 1]] * Q[B];
        If[FullSimplify[lhs - rhs] != Array[0 &, {8, 8}],
            result = False;
            Print["Spin-Clifford commutation fails for A=", A, ", B=", B, ", C=", C];
        ],
        {A, 1, 7}, {B, A + 1, 7}, {C, 1, 7}
    ];
    result
];

verifySOCommutation[] := Module[{result = True, lhs, rhs, eta},
    eta = etaAB;
    (* Check [S^{AB}, S^{CD}] *)
    Do[
        lhs = SAB1[A, B] . SAB1[C, D] - SAB1[C, D] . SAB1[A, B];
        rhs = eta[[B + 1, C + 1]] * SAB1[A, D] - eta[[A + 1, C + 1]] * SAB1[B, D] 
            - eta[[B + 1, D + 1]] * SAB1[A, C] + eta[[A + 1, D + 1]] * SAB1[B, C];
        If[FullSimplify[lhs - rhs] != Array[0 &, {8, 8}],
            result = False;
            Print["so(4,4) commutation fails for A=", A, ", B=", B, ", C=", C, ", D=", D];
        ],
        {A, 1, 6}, {B, A + 1, 7}, {C, 1, 6}, {D, C + 1, 7}
    ];
    result
];

verifyCommutation[] := verifySpinCommutation[] && verifySOCommutation[];

(* ========================================================================== *)
(*  SECTION 12: Spinor Metrics and Helper Functions                           *)
(* ========================================================================== *)

(* 8x8 spinor metric (charge conjugation matrix) *)
(* C = {{0, I4}, {I4, 0}} satisfies C.Q[A].C^{-1} = OverBar[Q][A]^T *)
SpinorMetric8 = ArrayFlatten[{{Zero4, ID4}, {ID4, Zero4}}];

(* 16x16 spinor metric *)
SpinorMetric16 = ArrayFlatten[{{Zero8, ID8}, {ID8, Zero8}}];

(* ========================================================================== *)
(*  Helper Functions for Lagrangian Construction                              *)
(* ========================================================================== *)

(* Dirac adjoint for 8-component spinor *)
(* psiBar = psi^dagger . gamma^0 where gamma^0 corresponds to our metric structure *)
DiracAdjoint8[psi_] := Transpose[Conjugate[psi]] . \[Sigma]8;

(* Dirac adjoint for 16-component spinor *)
DiracAdjoint16[psi_] := Transpose[Conjugate[psi]] . \[Sigma]16;

(* Covariant derivative matrix (useful for kinetic terms) *)
(* This is Q[5].Q[6].Q[7], appearing in the Dirac-like operator *)
CovariantDiffMatrix8 = Q[5] . Q[6] . Q[7];
CovariantDiffMatrix16 = T16A[5] . T16A[6] . T16A[7];

(* Projectors onto type-1 and type-2 spinor spaces *)
(* P1 projects onto positive chirality, P2 onto negative chirality *)
Projector1 = (ID8 + \[Sigma]8) / 2;  (* Projects onto S1-like subspace *)
Projector2 = (ID8 - \[Sigma]8) / 2;  (* Projects onto S2-like subspace *)

(* WTF   16D projectors *)
(*Projector1Full = (ID16 + \[Sigma]16) / 2;
Projector2Full = (ID16 - \[Sigma]16) / 2;*)
Projector1Full = (ID16 + T16A[8]) / 2;
Projector2Full = (ID16 - T16A[8]) / 2;

(* ========================================================================== *)
(*  SECTION 13: Unit Spinor, F-matrices, Projections, Fundamental Identity    *)
(* ========================================================================== *)

(*
   This section defines key objects for constructing Lagrangians in the
   split octonion framework:
   
   1. unit: Type-1 eigenspinor of \[Sigma]8 with eigenvalue +1
   2. FAa: Maps vector index A to spinor indices - 8x8 matrix
   3. FaA: Maps spinor index to vector index A - list of 8 row vectors
   4. realProjection8/16: Projects spinor onto "real" (octonionic identity) part
   5. imaginaryPart8/16: Extracts "imaginary" (7 imaginary octonion units) part
   6. FundamentalIdentity8by8: Verifies the fundamental trace identity
   
   The unit spinor is central to decomposing spinors into real and imaginary
   parts with respect to the split octonion structure.
*)

(* -------------------------------------------------------------------------- *)
(*  13.1 Unit Spinor: Eigenspinor of \[Sigma]8                                       *)
(* -------------------------------------------------------------------------- *)

(*
   The unit spinor is the normalized eigenspinor of \[Sigma]8 with eigenvalue +1:
       \[Sigma]8.unit = +unit
   
   This selects a preferred direction in spinor space corresponding to the
   identity element e\:2080 of the split octonions.
   
   Explicit form: unit = (1/\[Sqrt]2)(1,0,0,0,1,0,0,0)
*)

unit = {1/Sqrt[2], 0, 0, 0, 1/Sqrt[2], 0, 0, 0};
Protect[unit];

(* Verify eigenvalue equation *)
unitEigenCheck = Simplify[\[Sigma]8 . unit - unit];  (* Should be zero vector *)

(* -------------------------------------------------------------------------- *)
(*  13.2 F-matrices: Vector-Spinor Intertwining Objects                       *)
(* -------------------------------------------------------------------------- *)

(*
   F_A^a (FAa): Takes a vector index A \[Element] {0,...,7} and produces an 8x8 matrix
   that maps between spinor and vector representations.
   
   Definition: FAa[[A+1]] = \[Eta]_{AA} * \[Tau][A].unit (as column vectors)
   The full FAa matrix has columns given by \[Tau][A].unit weighted by metric factor.
*)

(*FAa = Transpose[Table[etaAB[[A + 1, A + 1]] * (Q[A] . unit), {A, 0, 7}]];*)
FaA = Transpose[etaAB[[# + 1, # + 1]] * (Q[#] . unit)&/@Range[0, 7]];

(*
   F_a^A (FaA): The "inverse" map from spinor index to vector index.
   
   Definition: FaA[[A+1]] = unit^T . \[Sigma]8 . \[Tau]\:0304[A] (as row vectors)
   This is a list of 8 row vectors (each 1x8).
*)

(*FaA = Table[Transpose[unit] . \[Sigma]8 . OverBar[Q][A], {A, 0, 7}];*)
FAa = Transpose[unit] . \[Sigma]8 . OverBar[Q][#] & /@ Range[0, 7];

(* Verify orthogonality: FaA . FAa should give identity-like structure *)
(*FForthogonality = Simplify[Table[FaA[[A + 1]] . FAa[[All, B + 1]], {A, 0, 7}, {B, 0, 7}]];*)
FForthogonality = FaA . FAa===FAa . FaA===ID8;

(* -------------------------------------------------------------------------- *)
(*  13.3 Split Octonion Multiplication Constants                              *)
(* -------------------------------------------------------------------------- *)

(*
   The split octonion multiplication is encoded in structure constants:
       e_A * e_B = f_{ABC} e_C
   
   These can be computed from the Clifford algebra representation:
       f_{ABC} = unit^T . \[Tau]\:0304[A] . \[Tau][B] . \[Tau][C] . unit (with appropriate factors)
   
   The function below computes these structure constants.
*)

EA=Array[eA,8];

splitOctonionMultTable=Grid[Partition[
  Flatten[{{{times}, EA}, 
    Table[({{times}, 
        Table[Sum[
          FullSimplify[
           ExpandAll[ 
etaAB[[B, B]] EA[[C1]] FAa[[C1, c1]] Q[B - 1][[c1, d1]] FaA[[d1, B1]]]], {C1, 1, 8}, {c1, 1, 8}, {d1, 1, 8}], {B1, 1, 8}]} /. {times -> 
         ToExpression[\!\(\*
TagBox[
StyleBox[
RowBox[{"\"\<eA[\>\"", "<>", 
RowBox[{"ToString", "[", "B", "]"}], "<>", "\"\<]\>\""}],
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\)]}), {B, 1, 8}]}], 9], Frame -> All];


splitOctonionMult[A_, B_, C_] := Module[{result},
  result = Transpose[unit] . OverBar[Q][A] . Q[B] . Q[C] . unit;
  (* Apply metric factors for proper index placement *)
  etaAB[[A + 1, A + 1]] * etaAB[[B + 1, B + 1]] * etaAB[[C + 1, C + 1]] * result
];

(* -------------------------------------------------------------------------- *)
(*  13.4 Real and Imaginary Projections for Spinors                           *)
(* -------------------------------------------------------------------------- *)

(*
   The "real projection" extracts the component of a spinor along the
   octonionic identity direction (e\:2080). The complement gives the
   "imaginary" part (components along e\:2081,...,e\:2087).
   
   realProjection8 is a rank-1 projector: realProjection8 = |unit\:27e9\:27e8unit|\[Sigma]8
   Applied to a spinor \[Psi]: realProjection8.\[Psi] gives the "real" part.
   
   imaginaryPart8[\[Psi]] = \[Psi] - realProjection8.\[Psi] extracts the imaginary part.
*)

realProjection8 = KroneckerProduct[unit, \[Sigma]8 . unit];

(* Verify this is a projector: P\.b2 = P *)
realProjectionCheck = Simplify[realProjection8 . realProjection8 - realProjection8];

(* Function to extract imaginary part of an 8-component spinor *)
imaginaryPart8[psi_] := psi - realProjection8 . psi;

(*
   16-dimensional versions for the full Spin(4,4) spinor space.
   The projection acts block-diagonally on the two 8D subspaces.
*)

realProjection16 = ArrayFlatten[{{realProjection8, 0}, {0, realProjection8}}];

(* Function to extract imaginary part of a 16-component spinor *)
(* Note: Using 'spinor_' as parameter name to avoid conflicts with user-defined symbols *)
imaginaryPart16[spinor_] := spinor - realProjection16 . spinor;

(* Verify 16D projector property *)
realProjection16Check = Simplify[realProjection16 . realProjection16 - realProjection16];

(* -------------------------------------------------------------------------- *)
(*  13.5 Fundamental Identity for 8x8 Matrices                                *)
(* -------------------------------------------------------------------------- *)

(*
   The Fundamental Identity states that for any 8x8 matrix 'a':
   
       Tr[a] * I_8 = \[CapitalSigma]_{A=0}^{7} \[Eta]_{AA} * \[Tau][A] . a . \[Tau]\:0304[A]
   
   This identity is crucial for demonstrating completeness of the \[Tau]-algebra
   and for simplifying expressions in the Lagrangian construction.
   
   The function FundamentalIdentity8by8[a] computes:
       Tr[a] * I_8 - \[CapitalSigma]_{A} \[Eta]_{AA} * \[Tau][A] . a . \[Tau]\:0304[A]
   
   If the identity holds, this returns the zero matrix.
*)

FundamentalIdentity8by8[M_] := FullSimplify[
  ID8 * Tr[M] - Sum[etaAB[[1 + A1, 1 + A1]] * Q[A1] . M . OverBar[Q][A1], {A1, 0, 7}]
];

(*
   Test function: Verifies the fundamental identity for a specific matrix.
   Returns True if identity holds (result is zero matrix), False otherwise.
*)

testFundamentalIdentity[testMatrix_] := Module[{result},
  result = FundamentalIdentity8by8[testMatrix];
  AllTrue[Flatten[result], # === 0 &]
];

(* Run verification on a few standard matrices *)
fundamentalIdentityTest1 = testFundamentalIdentity[ID8];           (* Identity matrix *)
fundamentalIdentityTest2 = testFundamentalIdentity[\[Sigma]8];        (* Sigma matrix *)
fundamentalIdentityTest3 = testFundamentalIdentity[Q[1] . Q[2]]; (* Product of Q's *)

(* ========================================================================== *)
(*  SECTION 14: Complete 256-Element Basis via Pauli Kronecker Products       *)
(* ========================================================================== *)

(* 
   The space of 16x16 complex matrices is 256-dimensional.
   A complete basis is constructed from 4-fold Kronecker products of the
   four 2x2 basis matrices: {I_2, \[Sigma]_1, \[Sigma]_2, \[Sigma]_3}.
   
   Since 16 = 2^4, we need 4 Kronecker factors to build 16x16 matrices.
   The 256 = 4^4 basis elements are indexed by (a,b,c,d) \[Element] {0,1,2,3}^4:
   
       Basis16[a,b,c,d] = \[Sigma]_a \[CircleTimes] \[Sigma]_b \[CircleTimes] \[Sigma]_c \[CircleTimes] \[Sigma]_d
   
   where \[Sigma]_0 = I_2 (identity) and \[Sigma]_{1,2,3} are the standard Pauli matrices.
   
   Properties:
   - These 256 matrices are linearly independent
   - They satisfy Tr(Basis16[a,b,c,d] . Basis16[a',b',c',d']\[Dagger]) = 16 \[Delta]_{aa'} \[Delta]_{bb'} \[Delta]_{cc'} \[Delta]_{dd'}
   - Any 16x16 matrix can be expanded: M = (1/16) \[CapitalSigma] c_{abcd} Basis16[a,b,c,d]
     where c_{abcd} = Tr(Basis16[a,b,c,d]\[Dagger] . M)
*)

(* Standard Pauli matrices including identity *)
(* Note: Using STANDARD Pauli matrices (complex) for the general basis *)
(* \[Sigma]_0 = I_2, \[Sigma]_1 = PauliMatrix[1], \[Sigma]_2 = PauliMatrix[2], \[Sigma]_3 = PauliMatrix[3] *)
pauli[0] = IdentityMatrix[2];
pauli[1] = PauliMatrix[1];     (* {{0,1},{1,0}} *)
pauli[2] = PauliMatrix[2];     (* {{0,-I},{I,0}} *)
pauli[3] = PauliMatrix[3];     (* {{1,0},{0,-1}} *)

(* Real Pauli basis: using I*\[Sigma]_2 instead of \[Sigma]_2 to keep everything real *)
pauliReal[0] = IdentityMatrix[2];
pauliReal[1] = PauliMatrix[1];      (* {{0,1},{1,0}} *)
pauliReal[2] = I * PauliMatrix[2];  (* {{0,1},{-1,0}} - this is REAL *)
pauliReal[3] = PauliMatrix[3];      (* {{1,0},{0,-1}} *)

(* ========================================================================== *)
(*  Core Basis Generation Functions                                           *)
(* ========================================================================== *)

(* Basis16[a,b,c,d]: 4-fold Kronecker product using STANDARD Pauli matrices *)
(* Arguments: a,b,c,d \[Element] {0,1,2,3} *)
(* Returns: 16x16 matrix = \[Sigma]_a \[CircleTimes] \[Sigma]_b \[CircleTimes] \[Sigma]_c \[CircleTimes] \[Sigma]_d *)
Basis16[a_, b_, c_, d_] := KroneckerProduct[pauli[a], pauli[b], pauli[c], pauli[d]];

(* Basis16Real[a,b,c,d]: 4-fold Kronecker product using REAL Pauli basis *)
(* Returns: 16x16 REAL matrix (when all inputs are valid indices) *)
Basis16Real[a_, b_, c_, d_] := KroneckerProduct[pauliReal[a], pauliReal[b], pauliReal[c], pauliReal[d]];

(* Linear index mapping: Convert (a,b,c,d) to single index n \[Element] {0,...,255} *)
(* Formula: n = 64*a + 16*b + 4*c + d *)
Basis16Index[a_, b_, c_, d_] := 64*a + 16*b + 4*c + d;

(* Inverse mapping: Convert linear index n to (a,b,c,d) *)
Basis16FromIndex[n_] := Module[{a, b, c, d, r},
    a = Quotient[n, 64];
    r = Mod[n, 64];
    b = Quotient[r, 16];
    r = Mod[r, 16];
    c = Quotient[r, 4];
    d = Mod[r, 4];
    {a, b, c, d}
];

(* Get a basis element by linear index *)
Basis16ByIndex[n_] := Module[{indices},
    indices = Basis16FromIndex[n];
    Basis16[indices[[1]], indices[[2]], indices[[3]], indices[[4]]]
];

Basis16RealByIndex[n_] := Module[{indices},
    indices = Basis16FromIndex[n];
    Basis16Real[indices[[1]], indices[[2]], indices[[3]], indices[[4]]]
];

(* ========================================================================== *)
(*  Viewing and Display Functions                                             *)
(* ========================================================================== *)

(* Pretty label for a basis element *)
Basis16Label[a_, b_, c_, d_] := Module[{labels},
    labels = {"I", "\[Sigma]1", "\[Sigma]2", "\[Sigma]3"};
    StringJoin[labels[[a + 1]], " \[CircleTimes] ", labels[[b + 1]], " \[CircleTimes] ", labels[[c + 1]], " \[CircleTimes] ", labels[[d + 1]]]
];

(* Display a single basis matrix with label *)
ViewBasis16[a_, b_, c_, d_] := Module[{},
    Print["Basis16[", a, ",", b, ",", c, ",", d, "] = ", Basis16Label[a, b, c, d]];
    Print["Linear index: n = ", Basis16Index[a, b, c, d]];
    MatrixForm[Basis16[a, b, c, d]]
];

(* Display basis matrix by linear index *)
ViewBasis16ByIndex[n_] := Module[{indices},
    indices = Basis16FromIndex[n];
    ViewBasis16[indices[[1]], indices[[2]], indices[[3]], indices[[4]]]
];

(* ========================================================================== *)
(*  Generate All 256 Basis Matrices                                           *)
(* ========================================================================== *)

(* Generate all 256 basis elements as a list of {index, label, matrix} *)
GenerateAllBasis16[] := Table[
    {n, Basis16Label @@ Basis16FromIndex[n], Basis16ByIndex[n]},
    {n, 0, 255}
];

(* Generate only the real basis elements *)
GenerateAllBasis16Real[] := Table[
    {n, Basis16Label @@ Basis16FromIndex[n], Basis16RealByIndex[n]},
    {n, 0, 255}
];

(* Store all 256 matrices in an array (computed once for efficiency) *)
AllBasis16 := AllBasis16 = Table[Basis16ByIndex[n], {n, 0, 255}];
AllBasis16Real := AllBasis16Real = Table[Basis16RealByIndex[n], {n, 0, 255}];

(* ========================================================================== *)
(*  Index Tables and Summaries                                                *)
(* ========================================================================== *)

(* Generate a summary table showing all 256 indices and their labels *)
Basis16IndexTable[] := TableForm[
    Table[
        {n, Basis16FromIndex[n], Basis16Label @@ Basis16FromIndex[n]},
        {n, 0, 255}
    ],
    TableHeadings -> {None, {"n", "(a,b,c,d)", "\[Sigma]_a \[CircleTimes] \[Sigma]_b \[CircleTimes] \[Sigma]_c \[CircleTimes] \[Sigma]_d"}}
];

(* Compact table showing indices organized by first two Pauli indices *)
Basis16CompactTable[] := Grid[
    Prepend[
        Table[
            Prepend[
                Table[
                    {64*a + 16*b, 64*a + 16*b + 15},
                    {b, 0, 3}
                ],
                a
            ],
            {a, 0, 3}
        ],
        {"a\\b", 0, 1, 2, 3}
    ],
    Frame -> All
];

(* ========================================================================== *)
(*  Matrix Expansion and Decomposition                                        *)
(* ========================================================================== *)

(* Expand any 16x16 matrix M in the Pauli basis *)
(* Returns: List of 256 coefficients c_{abcd} such that M = (1/16) \[CapitalSigma] c_{abcd} Basis16[a,b,c,d] *)
ExpandInBasis16[M_] := Table[
    Tr[ConjugateTranspose[Basis16ByIndex[n]] . M] / 16,
    {n, 0, 255}
];

(* Reconstruct matrix from coefficients *)
ReconstructFromBasis16[coeffs_] := (1/16) * Sum[
    coeffs[[n + 1]] * Basis16ByIndex[n],
    {n, 0, 255}
];

(* Get non-zero components (useful for sparse matrices) *)
NonZeroComponents16[M_, tol_: 10^-10] := Module[{coeffs},
    coeffs = ExpandInBasis16[M];
    Select[
        Table[{n, Basis16FromIndex[n], coeffs[[n + 1]]}, {n, 0, 255}],
        Abs[#[[3]]] > tol &
    ]
];

(* ========================================================================== *)
(*  Verification of Basis Orthogonality                                       *)
(* ========================================================================== *)

(* Verify orthogonality: Tr(Basis16[i]\[Dagger] . Basis16[j]) = 16 \[Delta]_{ij} *)
VerifyBasis16Orthogonality[] := Module[{gram, expected},
    Print["Computing 256x256 Gram matrix (may take a moment)..."];
    gram = Table[
        Tr[ConjugateTranspose[Basis16ByIndex[i]] . Basis16ByIndex[j]],
        {i, 0, 255}, {j, 0, 255}
    ];
    expected = 16 * IdentityMatrix[256];
    gram == expected
];

(* ========================================================================== *)
(*  Relationship to Clifford Algebra Generators                               *)
(* ========================================================================== *)

(* 
   Express the Clifford generators T16A[n] in terms of Basis16[a,b,c,d]:
   This shows how the physical Clifford algebra sits inside the full 256D space.
*)
T16AInBasis16[n_] := NonZeroComponents16[T16A[n]];

(* ========================================================================== *)
(*  Export Public Symbols                                                     *)
(* ========================================================================== *)

End[]; (* `Private` *)

EndPackage[];

(* ========================================================================== *)
(*  VERIFICATION SECTION (Run after loading)                                  *)
(* ========================================================================== *)

(* To verify all algebraic relations, run: *)
(* 
Print["Verifying anti-commutation relations..."];
Print["8x8: ", DiracLordNash44`Private`verifyAntiCommutation8[]];
Print["16x16: ", DiracLordNash44`Private`verifyAntiCommutation16[]];
Print["Verifying spin generator commutation relations..."];
Print["[S,t]: ", DiracLordNash44`Private`verifySpinCommutation[]];
Print["[S,S]: ", DiracLordNash44`Private`verifySOCommutation[]];
*)

(* ========================================================================== *)
(*  EXAMPLE USAGE                                                             *)
(* ========================================================================== *)

(* 
(* After loading: Get["Dirac-Lord-Nash_4+4.wl"] *)

(* Access generators: *)
Q[1] // MatrixForm  (* 8x8 Clifford generator *)
T16A[0] // MatrixForm  (* 16x16 Clifford generator *)

(* Spin generators: *)
SAB8[1, 2] // MatrixForm  (* S^{12} as 8x8 matrix *)
SAB16[0, 1] // MatrixForm  (* S^{01} as 16x16 matrix *)

(* Metrics: *)
etaAB // MatrixForm  (* (4,4) signature metric *)
SpinorMetric8 // MatrixForm  (* 8x8 charge conjugation matrix *)

(* Build a Lagrangian: *)
(* For a type-1 spinor psi1 (8-component column vector): *)
psi1 = Array[Subscript[psi1, #] &, 8];
psiBar1 = DiracAdjoint8[psi1];
(* Kinetic term: psiBar1 . Q[mu] . (partial_mu psi1) *)

(* For a 16-component spinor psi (S1 \[CirclePlus] S2): *)
psi = Array[Subscript[psi, #] &, 16];
psiBar = DiracAdjoint16[psi];
(* Kinetic term: psiBar . T16A[mu] . (partial_mu psi) *)

(* ============== SECTION 13: 256-Element Basis Examples ============== *)

(* View a specific basis element by indices: *)
ViewBasis16[1, 0, 2, 3]  (* Shows \[Sigma]1 \[CircleTimes] I \[CircleTimes] \[Sigma]2 \[CircleTimes] \[Sigma]3 *)

(* View by linear index (n \[Element] {0,...,255}): *)
ViewBasis16ByIndex[42]   (* Shows the 42nd basis matrix *)

(* Generate a specific basis matrix: *)
Basis16[0, 0, 0, 0] // MatrixForm  (* = I_16, the identity *)
Basis16[1, 1, 1, 1] // MatrixForm  (* = \[Sigma]1 \[CircleTimes] \[Sigma]1 \[CircleTimes] \[Sigma]1 \[CircleTimes] \[Sigma]1 *)

(* Convert between index formats: *)
Basis16Index[1, 2, 3, 0]    (* Returns 108 *)
Basis16FromIndex[108]       (* Returns {1, 2, 3, 0} *)

(* Get ALL 256 basis matrices: *)
allBasis = GenerateAllBasis16[];   (* List of {n, label, matrix} *)
allBasis[[1]]                       (* First element: n=0, I\[CircleTimes]I\[CircleTimes]I\[CircleTimes]I *)
allBasis[[256]]                     (* Last element: n=255, \[Sigma]3\[CircleTimes]\[Sigma]3\[CircleTimes]\[Sigma]3\[CircleTimes]\[Sigma]3 *)

(* View summary table of all indices: *)
Basis16IndexTable[]  (* Shows all 256 indices with labels *)

(* Expand a matrix in the Pauli basis: *)
coeffs = ExpandInBasis16[T16A[1]];  (* Expand T16A[1] in Pauli basis *)
NonZeroComponents16[T16A[1]]        (* Show only non-zero components *)

(* Quick access to cached basis (computed once): *)
AllBasis16[[1]]   (* = Basis16[0,0,0,0] = I_16 *)
AllBasis16[[256]] (* = Basis16[3,3,3,3] = \[Sigma]3\[CircleTimes]\[Sigma]3\[CircleTimes]\[Sigma]3\[CircleTimes]\[Sigma]3 *)

(* Real basis (using I*\[Sigma]2 instead of \[Sigma]2): *)
Basis16Real[1, 2, 0, 3] // MatrixForm  (* All entries are REAL *)
AllBasis16Real[[100]]                   (* 100th REAL basis matrix *)
*)

(* ========================================================================== *)
(*  END OF PACKAGE                                                            *)
(* ========================================================================== *)




Print["Dirac-Lord-Nash_4+4 loaded successfully!  BUT, WARNING:  DO NOT USE IF YOU WANT A CORRECT RESULT!"]; 

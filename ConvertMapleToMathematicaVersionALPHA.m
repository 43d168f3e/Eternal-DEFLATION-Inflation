(* ::Package:: *)

(* ============================================================================ *)
(* Patrick L. Nash, Ph.D.     (c) 2022, under GPL ; do not remove this notice   *)
(* Professor, UTSA Physics and Astronomy, Retired (UTSA)                        *)
(* Patrick299Nash  at    gmail   ...                                            *)
(* Do not read if you are irascible; I apologize for the typos.                 *)
(* SOURCE: git clone https://github.com/43d168f3e/Eternal-DEFLATION-Inflation.git *)
(* "git@github.com:43d168f3e/Eternal-DEFLATION-Inflation.git"                   *)
(* here lies  ConvertMapleToMathematicaVersionALPHA.m                           *)
(* WARNING:  DO NOT USE IF YOU WANT A CORRECT RESULT                            *)
(* blame: PLN and friends (Claude 4.5  and Manus-Lite)                          *)
(* A robust Maple to Mathematica parser and translator it is NOT                *)
(* ============================================================================ *)

BeginPackage["ConvertMapleToMathematicaVersionALPHA`"];

ConvertMapleToMathematicaVersionALPHA::usage = "ConvertMapleToMathematicaVersionALPHA[str] converts a Maple syntax string to a Mathematica expression.";
ConvertMapleToMathematicaStringVersionALPHA::usage = "ConvertMapleToMathematicaStringVersionALPHA[str] converts a Maple syntax string to a Mathematica code string.";

Begin["`Private`"];

(* ============================================================================ *)
(* SECTION 1: LEXER                                                             *)
(* ============================================================================ *)

IsDigit[char_] := StringMatchQ[char, RegularExpression["[0-9]"]];
IsAlpha[char_] := StringMatchQ[char, RegularExpression["[a-zA-Z_]"]];
IsAlphaNum[char_] := StringMatchQ[char, RegularExpression["[a-zA-Z0-9_]"]];
IsSpace[char_] := StringMatchQ[char, RegularExpression["\\s"]];

mapleReservedWords = {
  "and", "assuming", "break", "by", "do", "done", "elif", "else", "end", 
  "for", "from", "if", "in", "local", "mod", "next", "not", "or", "proc", 
  "return", "then", "to", "while", "xor"
};

GetTokens[str_String] := Module[
  {chars, len, i, char, tokens = {}, numStr, idStr, strLit},
  
  chars = Characters[str];
  len = Length[chars];
  i = 1;
  
  While[i <= len,
    char = chars[[i]];
    
    Which[
      IsSpace[char],
      i++,
      
      IsDigit[char],
      numStr = char;
      i++;
      While[i <= len && IsDigit[chars[[i]]],
        numStr = numStr <> chars[[i]];
        i++;
      ];
      If[i <= len && chars[[i]] == "." && (i+1) <= len && IsDigit[chars[[i+1]]],
        numStr = numStr <> ".";
        i++;
        While[i <= len && IsDigit[chars[[i]]],
          numStr = numStr <> chars[[i]];
          i++;
        ];
      ];
      AppendTo[tokens, {"NUMBER", ToExpression[numStr]}],
      
      IsAlpha[char],
      idStr = char;
      i++;
      While[i <= len && IsAlphaNum[chars[[i]]],
        idStr = idStr <> chars[[i]];
        i++;
      ];
      If[MemberQ[mapleReservedWords, idStr],
        AppendTo[tokens, {"KEYWORD", idStr}],
        AppendTo[tokens, {"IDENTIFIER", idStr}]
      ],
      
      char == "\"",
      i++;
      strLit = "";
      While[i <= len && chars[[i]] != "\"",
        strLit = strLit <> chars[[i]];
        i++;
      ];
      If[i <= len, i++];
      AppendTo[tokens, {"STRING", strLit}],
      
      char == ":" && (i+1) <= len && chars[[i+1]] == "=",
      AppendTo[tokens, {"ASSIGN", ":="}];
      i += 2,
      
      char == "." && (i+1) <= len && chars[[i+1]] == ".",
      If[(i+2) <= len && chars[[i+2]] == ".",
        AppendTo[tokens, {"ELLIPSIS", "..."}];
        i += 3,
        AppendTo[tokens, {"RANGE", ".."}];
        i += 2
      ],
      
      char == "<" && (i+1) <= len && chars[[i+1]] == "=",
      AppendTo[tokens, {"LE", "<="}];
      i += 2,
      
      char == ">" && (i+1) <= len && chars[[i+1]] == "=",
      AppendTo[tokens, {"GE", ">="}];
      i += 2,
      
      char == "!" && (i+1) <= len && chars[[i+1]] == "=",
      AppendTo[tokens, {"NE", "!="}];
      i += 2,
      
      char == "+", AppendTo[tokens, {"PLUS", "+"}]; i++,
      char == "-", AppendTo[tokens, {"MINUS", "-"}]; i++,
      char == "*", AppendTo[tokens, {"STAR", "*"}]; i++,
      char == "/", AppendTo[tokens, {"SLASH", "/"}]; i++,
      char == "^", AppendTo[tokens, {"CARET", "^"}]; i++,
      char == "(", AppendTo[tokens, {"LPAREN", "("}]; i++,
      char == ")", AppendTo[tokens, {"RPAREN", ")"}]; i++,
      char == "{", AppendTo[tokens, {"LBRACE", "{"}]; i++,
      char == "}", AppendTo[tokens, {"RBRACE", "}"}]; i++,
      char == "[", AppendTo[tokens, {"LBRACKET", "["}]; i++,
      char == "]", AppendTo[tokens, {"RBRACKET", "]"}]; i++,
      char == "=", AppendTo[tokens, {"EQUALS", "="}]; i++,
      char == "<", AppendTo[tokens, {"LT", "<"}]; i++,
      char == ">", AppendTo[tokens, {"GT", ">"}]; i++,
      char == ",", AppendTo[tokens, {"COMMA", ","}]; i++,
      char == ";", AppendTo[tokens, {"SEMICOLON", ";"}]; i++,
      char == "$", AppendTo[tokens, {"DOLLAR", "$"}]; i++,
      
      char == "#",
      While[i <= len && chars[[i]] != "\n", i++];
      i++,
      
      True,
      i++
    ];
  ];
  
  AppendTo[tokens, {"EOF", "EOF"}];
  tokens
];

(* ============================================================================ *)
(* SECTION 2: FUNCTION MAPPING                                                  *)
(* ============================================================================ *)

MapleToMathematicaMap[funcName_String] := Switch[ToLowerCase[funcName],
  "abs", "Abs", "ceil", "Ceiling", "floor", "Floor", "round", "Round",
  "trunc", "IntegerPart", "frac", "FractionalPart", "signum", "Sign",
  "min", "Min", "max", "Max", "sqrt", "Sqrt", "surd", "Surd",
  "exp", "Exp", "ln", "Log", "log", "Log", "log10", "Log10", "lambertw", "ProductLog",
  "sin", "Sin", "cos", "Cos", "tan", "Tan", "cot", "Cot", "sec", "Sec", "csc", "Csc",
  "arcsin", "ArcSin", "arccos", "ArcCos", "arctan", "ArcTan", "arccot", "ArcCot", "arcsec", "ArcSec", "arccsc", "ArcCsc",
  "sinh", "Sinh", "cosh", "Cosh", "tanh", "Tanh", "coth", "Coth", "sech", "Sech", "csch", "Csch",
  "arcsinh", "ArcSinh", "arccosh", "ArcCosh", "arctanh", "ArcTanh", "arccoth", "ArcCoth", "arcsech", "ArcSech", "arccsch", "ArcCsch",
  "diff", "D", "int", "Integrate", "Int", "Integrate", "limit", "Limit", "sum", "Sum", "product", "Product", "series", "Series", "taylor", "Series",
  "dsolve", "DSolve", "desol", "DSolve", "pdsolve", "DSolve",
  "gamma", "Gamma", "lngamma", "LogGamma", "psi", "PolyGamma", "beta", "Beta", "binomial", "Binomial", "factorial", "Factorial", "doublefactorial", "Factorial2", "pochhammer", "Pochhammer",
  "besselj", "BesselJ", "bessely", "BesselY", "besseli", "BesselI", "besselk", "BesselK", "hankelh1", "HankelH1", "hankelh2", "HankelH2", "sphericalbesselj", "SphericalBesselJ", "sphericalbessely", "SphericalBesselY",
  "airyai", "AiryAi", "airybi", "AiryBi", "airy", "AiryAi",
  "erf", "Erf", "erfc", "Erfc", "erfi", "Erfi", "dawson", "DawsonF", "fresnelc", "FresnelC", "fresnels", "FresnelS",
  "elliptick", "EllipticK", "elliptice", "EllipticE", "ellipticf", "EllipticF", "ellipticpi", "EllipticPi",
  "jacobisn", "JacobiSN", "jacobicn", "JacobiCN", "jacobidn", "JacobiDN", "jacobicd", "JacobiCD", "jacobisd", "JacobiSD", "jacobind", "JacobiND", "jacobidc", "JacobiDC", "jacobinc", "JacobiNC", "jacobisc", "JacobiSC", "jacobins", "JacobiNS", "jacobids", "JacobiDS", "jacobics", "JacobiCS",
  "weierstrassp", "WeierstrassP", "weierstrasspprime", "WeierstrassPPrime", "weierstrasssigma", "WeierstrassSigma", "weierstrasszeta", "WeierstrassZeta",
  "zeta", "Zeta", "dilog", "PolyLog", "polylog", "PolyLog", "lerchphi", "LerchPhi",
  "hypergeom", "Hypergeometric2F1", "kummeru", "HypergeometricU", "kummerm", "Hypergeometric1F1", "whittakerm", "WhittakerM", "whittakerw", "WhittakerW", "meijerg", "MeijerG",
  "chebyshevt", "ChebyshevT", "chebyshevu", "ChebyshevU", "legendrep", "LegendreP", "legendreq", "LegendreQ", "laguerrel", "LaguerreL", "hermiteh", "HermiteH", "gegenbauerc", "GegenbauerC", "jacobip", "JacobiP",
  "fibonacci", "Fibonacci", "lucas", "LucasL", "bernoulli", "BernoulliB", "euler", "EulerE", "stirling1", "StirlingS1", "stirling2", "StirlingS2", "bell", "BellB", "catalan", "CatalanNumber",
  "ithprime", "Prime", "isprime", "PrimeQ", "nextprime", "NextPrime", "prevprime", "NextPrime", "ifactor", "FactorInteger", "igcd", "GCD", "ilcm", "LCM", "irem", "Mod", "iquo", "Quotient",
  "re", "Re", "im", "Im", "argument", "Arg", "conjugate", "Conjugate", "csgn", "Sign",
  "determinant", "Det", "det", "Det", "trace", "Tr", "rank", "MatrixRank", "transpose", "Transpose", "conjugatetranspose", "ConjugateTranspose", "norm", "Norm",
  "eigenvalues", "Eigenvalues", "eigenvectors", "Eigenvectors", "eigenvects", "Eigensystem", "characteristicpolynomial", "CharacteristicPolynomial",
  "ludecomposition", "LUDecomposition", "qrdecomposition", "QRDecomposition", "singularvalues", "SingularValueList", "svd", "SingularValueDecomposition", "jordanform", "JordanDecomposition", "schurdecomposition", "SchurDecomposition",
  "matrixexponential", "MatrixExp", "matrixlog", "MatrixLog",
  "expand", "Expand", "factor", "Factor", "simplify", "Simplify", "combine", "Simplify", "normal", "Together", "rationalize", "Rationalize", "evalf", "N", "collect", "Collect", "coeff", "Coefficient", "degree", "Exponent", "lcoeff", "Coefficient", "tcoeff", "Coefficient", "quo", "PolynomialQuotient", "rem", "PolynomialRemainder", "gcd", "PolynomialGCD", "lcm", "PolynomialLCM", "resultant", "Resultant", "discriminant", "Discriminant",
  "solve", "Solve", "fsolve", "NSolve", "isolve", "Solve", "minimize", "Minimize", "maximize", "Maximize",
  "nops", "Length", "seq", "Table", "type", "Head",
  "fourier", "FourierTransform", "invfourier", "InverseFourierTransform", "laplace", "LaplaceTransform", "invlaplace", "InverseLaplaceTransform", "ztrans", "ZTransform", "invztrans", "InverseZTransform",
  "pi", "Pi", "e", "E", "i", "I", "infinity", "Infinity",
  _, StringReplace[funcName, StartOfString ~~ x_ :> ToUpperCase[x]]
];

(* ============================================================================ *)
(* SECTION 3: PARSER                                                            *)
(* ============================================================================ *)

ParseTokens[tokens_List] := Module[
  {pos = 1, currentToken, eat, peek, parseExpression, parseEquation, 
   parseAddExp, parseMulExp, parsePowerExp, parseUnaryExp, parsePrimary, 
   parseArgs, parseList},
  
  currentToken := tokens[[pos]];
  peek[] := tokens[[pos]];
  
  eat[type_] := If[currentToken[[1]] == type,
    pos++;
    True,
    False
  ];
  
  parseExpression[] := parseEquation[];
  
  parseEquation[] := Module[{left, right},
    left = parseAddExp[];
    If[currentToken[[1]] == "EQUALS",
      eat["EQUALS"];
      right = parseAddExp[];
      {"Equation", left, right},
      left
    ]
  ];
  
  parseAddExp[] := Module[{node, right, op},
    node = parseMulExp[];
    While[MemberQ[{"PLUS", "MINUS"}, currentToken[[1]]],
      op = currentToken[[2]];
      eat[currentToken[[1]]];
      right = parseMulExp[];
      node = {"BinaryOp", op, node, right};
    ];
    node
  ];
  
  parseMulExp[] := Module[{node, right, op},
    node = parsePowerExp[];
    While[True,
      If[MemberQ[{"STAR", "SLASH"}, currentToken[[1]]],
        op = currentToken[[2]];
        eat[currentToken[[1]]];
        right = parsePowerExp[];
        node = {"BinaryOp", op, node, right},
        If[MemberQ[{"IDENTIFIER", "NUMBER", "LPAREN", "LBRACE"}, currentToken[[1]]],
          right = parsePowerExp[];
          node = {"BinaryOp", "*", node, right},
          Break[]
        ]
      ]
    ];
    node
  ];
  
  parsePowerExp[] := Module[{node, right},
    node = parseUnaryExp[];
    If[currentToken[[1]] == "CARET",
      eat["CARET"];
      right = parsePowerExp[];
      {"BinaryOp", "^", node, right},
      node
    ]
  ];
  
  parseUnaryExp[] := Module[{op, node},
    If[MemberQ[{"PLUS", "MINUS"}, currentToken[[1]]],
      op = currentToken[[2]];
      eat[currentToken[[1]]];
      node = parseUnaryExp[];
      {"UnaryOp", op, node},
      parsePrimary[]
    ]
  ];
  
  parsePrimary[] := Module[{token, node, name, args},
    token = currentToken;
    Switch[token[[1]],
      "NUMBER", eat["NUMBER"]; {"Number", token[[2]]},
      "IDENTIFIER",
      eat["IDENTIFIER"];
      name = token[[2]];
      If[currentToken[[1]] == "LPAREN",
        eat["LPAREN"];
        args = parseArgs[];
        eat["RPAREN"];
        {"Call", name, args},
        {"Identifier", name}
      ],
      "LPAREN", eat["LPAREN"]; node = parseExpression[]; eat["RPAREN"]; node,
      "LBRACE", parseList[],
      "ELLIPSIS", eat["ELLIPSIS"]; {"Identifier", "..."},
      "EOF", {"Error", "EOF"},
      _, eat[token[[1]]]; {"Error", token}
    ]
  ];
  
  parseList[] := Module[{elements},
    eat["LBRACE"];
    elements = parseArgs[];
    eat["RBRACE"];
    {"List", elements}
  ];
  
  parseArgs[] := Module[{args = {}, arg},
    If[currentToken[[1]] != "RPAREN" && currentToken[[1]] != "RBRACE",
      arg = parseExpression[];
      AppendTo[args, arg];
      While[currentToken[[1]] == "COMMA",
        eat["COMMA"];
        arg = parseExpression[];
        AppendTo[args, arg];
      ];
    ];
    args
  ];
  
  parseExpression[]
];

(* ============================================================================ *)
(* SECTION 4: TRANSLATOR                                                        *)
(* ============================================================================ *)

ProcessASTNode[nodeType_, nodeData_, childResults_] := Module[
  {funcName, cleanFuncName, mathFunc, argsStr, paramTransform},
  
  Switch[nodeType,
    "Number", ToString[nodeData],
    "Identifier", If[StringLength[nodeData] > 0 && StringTake[nodeData, 1] == "_", StringDrop[nodeData, 1], nodeData],
    "BinaryOp", "(" <> childResults[[1]] <> " " <> nodeData <> " " <> childResults[[2]] <> ")",
    "UnaryOp", nodeData <> "(" <> childResults[[1]] <> ")",
    "Equation", childResults[[1]] <> " == " <> childResults[[2]],
    "List", "{" <> StringRiffle[childResults, ", "] <> "}",
    "Call",
    funcName = nodeData;
    cleanFuncName = If[StringLength[funcName] > 0 && StringTake[funcName, 1] == "_", StringDrop[funcName, 1], funcName];
    mathFunc = MapleToMathematicaMap[cleanFuncName];
    paramTransform = Which[
      MemberQ[{"EllipticK", "EllipticE", "EllipticF", "EllipticPi"}, mathFunc] && Length[childResults] > 0,
      ReplacePart[childResults, -1 -> "(" <> childResults[[-1]] <> ")^2"],
      StringMatchQ[mathFunc, "Jacobi" ~~ __] && Length[childResults] == 2,
      {childResults[[1]], "(" <> childResults[[2]] <> ")^2"},
      mathFunc == "ArcTan" && Length[childResults] == 2,
      {childResults[[2]], childResults[[1]]},
      ToLowerCase[funcName] == "dilog",
      Prepend[childResults, "2"],
      True,
      childResults
    ];
    argsStr = StringRiffle[paramTransform, ", "];
    mathFunc <> "[" <> argsStr <> "]",
    _, "Error"
  ]
];

ToMathematicaString[ast_] := Module[
  {stack, outputStack, currentItem, node, nodeType, children, childCount, 
   childResults, i, result, nodeData},
  
  stack = {{ast, False}};
  outputStack = {};
  
  While[Length[stack] > 0,
    currentItem = Last[stack];
    node = currentItem[[1]];
    nodeType = node[[1]];
    
    If[currentItem[[2]],
      stack = Most[stack];
      {children, nodeData} = Switch[nodeType,
        "Number", {{}, node[[2]]},
        "Identifier", {{}, node[[2]]},
        "BinaryOp", {{node[[3]], node[[4]]}, node[[2]]},
        "UnaryOp", {{node[[3]]}, node[[2]]},
        "Equation", {{node[[2]], node[[3]]}, ""},
        "List", {node[[2]], ""},
        "Call", {node[[3]], node[[2]]},
        _, {{}, ""}
      ];
      childCount = Length[children];
      childResults = {};
      If[childCount > 0,
        Do[
          PrependTo[childResults, Last[outputStack]];
          outputStack = Most[outputStack];
          , {i, 1, childCount}
        ];
      ];
      result = ProcessASTNode[nodeType, nodeData, childResults];
      AppendTo[outputStack, result];
      ,
      stack[[Length[stack]]] = {node, True};
      children = Switch[nodeType,
        "Number", {},
        "Identifier", {},
        "BinaryOp", {node[[3]], node[[4]]},
        "UnaryOp", {node[[3]]},
        "Equation", {node[[2]], node[[3]]},
        "List", node[[2]],
        "Call", node[[3]],
        _, {}
      ];
      Do[
        AppendTo[stack, {children[[i]], False}];
        , {i, Length[children], 1, -1}
      ];
    ];
  ];
  
  If[Length[outputStack] > 0, Last[outputStack], "Error"]
];

(* ============================================================================ *)
(* SECTION 5: PUBLIC INTERFACE                                                  *)
(* ============================================================================ *)

ConvertMapleToMathematicaStringVersionALPHA[inputStr_String] := Module[{tokens, ast},
  tokens = GetTokens[inputStr];
  ast = ParseTokens[tokens];
  ToMathematicaString[ast]
];

ConvertMapleToMathematicaVersionALPHA[inputStr_String] := Module[{mathStr},
  mathStr = ConvertMapleToMathematicaStringVersionALPHA[inputStr];
  ToExpression[mathStr]
];

End[];
EndPackage[];

Print["ConvertMapleToMathematicaVersionALPHA loaded successfully!  BUT, WARNING:  DO NOT USE IF YOU WANT A CORRECT RESULT!"];

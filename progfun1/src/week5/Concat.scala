/**
 * reasoning about concat.
 * Verify the following properties:
 *  (xs ++ ys) ++ zs = xs ++ (ys ++ zs)   // associativity
 *         xs ++ Nil = xs                 // Nil is neutral element to the left
 *         Nil ++ xs = xs                 // Nil is neutral element to the right
 **/

// natural induction?
// if P(b) we can assume that for all integers n >= b if one has P(n), then P(n+1) also holds


// structural induction
// to prove a property P(xs) for all lists xs
//  show that P(Nil) holds - base case
//  for a list xs and some element x
//    if (Pxs) holds, then P(x::xs) also holds - induction step

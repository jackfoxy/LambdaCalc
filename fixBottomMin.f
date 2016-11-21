﻿tru = λt. λf. t;
fls = bottom;
c0 = bottom;
nil = bottom;
c1 = λs. λz. s z;
c2 = λs. λz. s (s z);
c3 = λs. λz. s (s (s z));
c4 = λs. λz. s (s (s (s z)));
c6 = λs. λz. s (s (s (s (s (s z)))));

c24 = λs. λz. s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))));

cond = λp. λt. λf. p t f;
iszro = λm. m (λx. fls) tru;
pair = λf. λs. λb. b f s;
fst = λp. p tru;
snd = λp. p fls;
plus = λm. λn. λs. λz. m s (n s z);
mult = λm. λn. m (plus n) c0;
prd = λm. fst (m (λp. pair (snd p) (plus c1 (snd p))) (pair c0 c0));
fact = λf. λx. cond (iszro x) c1 (mult x (f (prd x)));
id = λx. x;
h = fix fact;
l = h c3; /*stackoverflow*/



/*tests */
not = λa. a fls tru;
and = λb. λc. b c fls;
or = λb. λc. b tru c;
nand = λa. λb. not (and a b);
xor = λa. λb. and (nand a b) (or a b);
xnor = λa. λb. not (xor a b);
eql = λm. λn. and (iszro (m prd n)) (iszro (n prd m));

/*it is true that c1 = c1 (true)*/
xnor tru (eql c1 (cond tru c1 c0));
/*it is false that c1 = c1 (false)*/
xnor fls (eql c1 (cond tru c1 c0));


/*it is false that c3 = l (true)*/
xnor fls (eql c3 l);
/*it is true that c6 = l (true)*/
xnor tru (eql c6 l);
/*
l2 = h c2;
xnor tru (eql c2 l2);

l4 = h c4;
xnor tru (eql c24 l4); */
bottom = lambda t.lambda b. b;
fix = lambda f. (lambda x. f (lambda y. x x y)) (lambda x. f (lambda y. x x y));
tru = lambda t. lambda f. t;
fls = bottom;
c0 = bottom;
nil = bottom;
c1 = lambda s. lambda z. s z;
c2 = lambda s. lambda z. s (s z);
c3 = lambda s. lambda z. s (s (s z));
c4 = lambda s. lambda z. s (s (s (s z)));
c6 = lambda s. lambda z. s (s (s (s (s (s z)))));

c24 = lambda s. lambda z. s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))));

cond = lambda p. lambda t. lambda f. p t f;
iszro = lambda m. m (lambda x. fls) tru;
pair = lambda f. lambda s. lambda b. b f s;
fst = lambda p. p tru;
snd = lambda p. p fls;
plus = lambda m. lambda n. lambda s. lambda z. m s (n s z);
mult = lambda m. lambda n. m (plus n) c0;
prd = lambda m. fst (m (lambda p. pair (snd p) (plus c1 (snd p))) (pair c0 c0));
fact = lambda f. lambda x. cond (iszro x) c1 (mult x (f (prd x)));
id = lambda x. x;
h = fix fact;
l = h c3; /*stackoverflow*/



/*tests */
not = lambda a. a fls tru;
and = lambda b. lambda c. b c fls;
or = lambda b. lambda c. b tru c;
nand = lambda a. lambda b. not (and a b);
xor = lambda a. lambda b. and (nand a b) (or a b);
xnor = lambda a. lambda b. not (xor a b);
eql = lambda m. lambda n. and (iszro (m prd n)) (iszro (n prd m));

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
/* Examples for testing */

/* naive tests*/

/*
x/;
x;

lambda x. x;
(lambda x. x) (lambda x. x x); 

x=lambda x.x;
y=(lambda x.x)(lambda x.x); 

x y;

*/

fix = lambda f. (lambda x. f (lambda y. x x y))(lambda x. f (lambda y. x x y));
bottom = lambda t. lambda b. b;
/**/
tru = lambda t. lambda f. t;

fls = lambda t. lambda f. f;
c0 = lambda s. lambda z. z;
nil = lambda c. lambda n. n; 

/*fls = bottom;
c0 = bottom;
nil = bottom;*/

cond = lambda p. lambda t. lambda f. p t f;
iszro = lambda m. m (lambda x. fls) tru;

pair = lambda f. lambda s. lambda b. b f s;
fst = lambda p. p tru;
snd = lambda p. p fls;


/*list my attempt*/

cons = lambda h. lambda t.lambda c. c (pair h t);
head = lambda l. l fst;
/*isnil = lambda l. not (xnor (l tru) tru); */
tail = lambda l. l snd;

c1 = lambda s. lambda z. s z;
c3 = lambda s. lambda z. s (s (s z));

plus = lambda m. lambda n. lambda s. lambda z. m s (n s z);
mult = lambda m. lambda n. m (plus n) c0;

prd = lambda m. fst (m (lambda p. pair (snd p) (plus c1 (snd p))) (pair c0 c0));

fact = lambda f. lambda x. cond (iszro x) c1 (mult x (f (prd x)));

h = fix fact;
/*l = h c3; stackoverflow*/